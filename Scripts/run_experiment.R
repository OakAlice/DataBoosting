# Functions to actually run the experiment --------------------------------
get_species_config <- function(species, settings) {
  s <- settings[settings$Species == species, ]
  list(
    species = species,
    target  = s$minorityTarget,
    Hz      = s$Hz,
    window  = if (s$Hz > 1) 2 else 5,
    data_path   = file.path("Data", species),
    output_path = file.path("Output", species)
  )
}


prepare_species_data <- function(cfg) {
  
  raw_data <- fread(file.path(cfg$data_path, "Formatted_raw_data.csv"))
  raw_data[, row_id := .I]
  
  feature_path <- file.path(cfg$data_path, "Nothing_Feature_data.csv")
  key_path     <- file.path(cfg$data_path, "Nothing_Window_key.csv")
  
  if (file.exists(feature_path) && file.exists(key_path)) {
    message("Loading the features and window key made previously")
    feature_data <- fread(feature_path)
    key          <- fread(key_path)
  } else {
    message("No precomputed features/key, generating")
    res <- generate_features_master(raw_data,
                                    sample_rate     = cfg$Hz,
                                    overlap_setting = 0,
                                    window_setting  = cfg$window)
    feature_data <- res$features
    key          <- res$key
    fwrite(feature_data, feature_path)
    fwrite(key, key_path)
  }
  
  # overlap = 0, so each raw row is in at most one window and the key can sit on raw
  raw_data[key, on = "row_id", win_id := i.win_id]
  
  list(raw = raw_data, features = feature_data, key = key, feature_path = feature_path)
}



run_fold <- function(fold, method, dat, cfg, target_class, ID_groups, hyperparameters) {
  
  out_file <- file.path(cfg$output_path, "Binary", sprintf("%s_performance_%d.csv", method, fold))
  if (file.exists(out_file)) return(invisible(NULL))
  
  test_IDs <- ID_groups$ID[ID_groups$group == fold]
  
  boosted <- boost_training_data(
    training_data = dplyr::filter(dat$raw, !ID %in% test_IDs),
    metadata_cols, target_col,
    target_class     = target_class,
    method           = method,
    Hz               = cfg$Hz,
    prior_features   = dplyr::filter(dat$features, !ID %in% test_IDs)
  )
  
  all <- boosted$boosted_data # extract all the data
  
  # NOTE: The binaries here are hardcoded
  output   <- design_build_RF(all, type = "multiclass", metadata_cols, hyperparameters)
  metrics <- test_model(type = "multiclass", 
                        output$RF_model,
                        test_data = dplyr::filter(dat$features, ID %in% test_IDs),
                        metadata_cols, 
                        target_col)
  
  fwrite(metrics$metrics, out_file)
  invisible(metrics)
}

run_binary_experiment <- function(species, settings, dat, target_class, hyperparameters, boost_methods, k = 3) {
  
  cfg <- get_species_config(species, settings)
    
  # same ID split reused across all methods
  # k <- 3
  ID_groups <- make_ID_groups(dat$features, target_col = target_col, k = k,
                              priority_classes = target_class)
  
  grid <- expand.grid(method = boost_methods, fold = seq_len(k),
                      stringsAsFactors = FALSE)
  
  # j <- 7
  for (j in seq_len(nrow(grid))) {
    message(species, " | ", grid$method[j], " | fold ", grid$fold[j])
    run_fold(fold = grid$fold[j], 
             method = grid$method[j], 
             dat, 
             cfg, 
             target_class,
             ID_groups,
             hyperparameters)
  }
}

make_base_features <- function(species, settings){
  cfg <- get_species_config(species, settings)
  prepare_species_data(cfg)
}




run_volume_experiment <- function(species, settings, dat, target_class, hyperparameters, boost_methods, cut_conditions, downsample, target_col, k){
  
  ID_groups <- make_ID_groups(dat$features, target_col = target_col, k = k,
                              priority_classes = target_class)
  
  grid <- expand.grid(fold = seq_len(k), subsample = cut_conditions, method = boost_methods,
                      stringsAsFactors = FALSE)

  # subset the data
  other  <- dat$features %>% dplyr::filter(.data[[target_col]] != target_class)
  target <- dat$features %>% dplyr::filter(.data[[target_col]] == target_class)
  
  all_metrics <- list()
  for (j in seq_len(nrow(grid))) {
    message(species, " | ", grid$subsample[j], "% | ", grid$method[j], " | fold ", grid$fold[j])
    
    # held-out individuals for this fold
    test_IDs <- ID_groups$ID[ID_groups$group == grid$fold[j]]
   
    test  <- dat$features %>% dplyr::filter(ID %in% test_IDs)
    
    # this pulls out the feature data I want to use
    train <- subset_train_data(downsample, other, target, test_IDs, grid$subsample[j])
    # I then need to match it to the right raw data... 
    raw_data <- dat$raw[win_id %in% train$win_id]
    
    # and now boost the sampled training data...
    boosted <- boost_training_data(
      training_data    = dplyr::filter(raw_data, !ID %in% test_IDs),
      metadata_cols, target_col, target_class,
      method           = grid$method[j],
      Hz               = settings$Hz[settings$Species == species],
      prior_features   = dplyr::filter(train, !ID %in% test_IDs)
    )
    
    # and now see how it performs
    output <- design_build_RF(train, type = "multiclass", metadata_cols, hyperparameters)
    metrics <- test_model(type = "multiclass", 
                          output$RF_model,
                          test_data = test,
                          metadata_cols, 
                          target_col)
    
    train_prev <- mean(train[[target_col]] == target_class)
    test_prev  <- mean(test[[target_col]]  == target_class)
    
    # record the training prevalnce as well
    all_metrics[[j]] <- metrics$metrics %>%
      mutate(prop = grid$subsample[j], Species = species, fold = grid$fold[j], method = grid$method[j],
             train_prev = train_prev,
             test_prev  = test_prev,
             n_target_train = nrow(train),
             n_target_IDs   = dplyr::n_distinct(train$ID))
    
  }
  
  all_metrics <- rbindlist(all_metrics)
  fwrite(all_metrics, file.path("Output", species, "Volume", "Performance_metrics.csv"))
}


find_optimal_hyperparameters <- function(species, settings, dat){
  
  if (!file.exists(file.path("Data", species, "Hyperparameters.csv"))){
    # this function is to find the hyperparameters once and then reuse them for everything going foreward

    # clean the data and make a single RF
    output <- design_build_RF(dat$features, type, metadata_cols)
    
    hpo <- tibble::enframe(output$hyperparameters, name = "parameter", value = "value")
    fwrite(hpo, file.path("Data", species, "Hyperparameters.csv"))
  } else {
    print("loading previously calculated hyperparameters")
    hpo <- fread(file.path("Data", species, "Hyperparameters.csv"))
  }
  
  return(hpo)
}
