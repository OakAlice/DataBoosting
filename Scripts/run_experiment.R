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
  
  feature_path <- file.path(cfg$data_path, "Nothing_Feature_data.csv")
  
  if (file.exists(feature_path)) {
    message("Loading the features made previously: ", feature_path)
    feature_data <- fread(feature_path)
  } else {
    message("No precomputed features, generating: ", feature_path)
    feature_data <- generate_features_master(raw_data,
                                             sample_rate     = cfg$Hz,
                                             overlap_setting = 0,
                                             window_setting  = cfg$window)
    fwrite(feature_data, feature_path)
  }
  
  list(raw = raw_data, features = feature_data, feature_path = feature_path)
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




run_volume_experiment <- function(species, settings, dat, target_class, hyperparameters, cut_conditions, downsample, target_col, k){
  
  ID_groups <- make_ID_groups(dat$features, target_col = target_col, k = k,
                              priority_classes = target_class)
  
  grid <- expand.grid(subsample = cut_conditions, fold = seq_len(k),
                      stringsAsFactors = FALSE)
  
  # scrambling the data # a debug thing
  # dat$features[[target_col]] <- sample(dat$features[[target_col]])
  
  # subset the data
  other  <- dat$features %>% dplyr::filter(.data[[target_col]] != target_class)
  target <- dat$features %>% dplyr::filter(.data[[target_col]] == target_class)
  
  all_metrics <- list()
  for (j in seq_len(nrow(grid))) {
    message(species, " | ", grid$subsample[j], " | fold ", grid$fold[j])
    
    # held-out individuals for this fold
    test_IDs <- ID_groups$ID[ID_groups$group == grid$fold[j]]
    
    # I want to downsample the data to a given % of the orignal data, but with the 
    # imbalance and distribution across individuals preserved
    pool <- target %>% dplyr::filter(!ID %in% test_IDs)
   
    if (downsample == "samples"){
      
      n_total <- ceiling(nrow(pool) * (grid$subsample[j]/100))
     
      alloc <- pool %>%
        dplyr::count(ID, name = "n_orig") %>%
        mutate(exact  = n_orig / sum(n_orig) * n_total,
               n_take = floor(exact),
               n_take = n_take + (rank(-(exact - n_take), ties.method = "first") <= n_total - sum(n_take)),
               n_take = pmin(n_take, n_orig))
      
      target_train <- pool %>%
        left_join(dplyr::select(alloc, ID, n_take), by = "ID") %>%
        group_by(ID) %>%
        dplyr::slice(sample.int(dplyr::n(), dplyr::first(n_take))) %>%
        ungroup() %>%
        dplyr::select(-n_take)
      
      print(nrow(target_train))
      
      train <- bind_rows(other %>% dplyr::filter(!ID %in% test_IDs), target_train)
      test  <- dat$features %>% dplyr::filter(ID %in% test_IDs)
      
    } else {
      # this is by ID... removing IDs
      # retain only subsample% of the training individuals that have the target class
      
      target_IDs <- unique(pool$ID)
      n_keep     <- max(1, ceiling(length(target_IDs) * grid$subsample[j] / 100))
      keep_IDs   <- sample(target_IDs, n_keep)
      
      target_train <- pool %>% dplyr::filter(ID %in% keep_IDs)
      
      # other classes from the dropped individuals stay in training, so only the
      # target class is reduced
      train <- bind_rows(other %>% dplyr::filter(!ID %in% test_IDs), target_train)
      test  <- dat$features %>% dplyr::filter(ID %in% test_IDs)
    }
    
    # and now see how it performs
    output   <- design_build_RF(train, type = "multiclass", metadata_cols, hyperparameters)
    metrics <- test_model(type = "multiclass", 
                          output$RF_model,
                          test_data = test,
                          metadata_cols, 
                          target_col)
    
    train_prev <- mean(train[[target_col]] == target_class)
    test_prev  <- mean(test[[target_col]]  == target_class)
    
    # record the training prevalnce as well
    all_metrics[[j]] <- metrics$metrics %>%
      mutate(prop = grid$subsample[j], Species = species, fold = grid$fold[j],
             train_prev = train_prev,
             test_prev  = test_prev,
             n_target_train = nrow(target_train),
             n_target_IDs   = dplyr::n_distinct(target_train$ID))
    
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
