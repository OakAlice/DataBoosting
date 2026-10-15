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
  
  list(raw = raw_data, features = feature_data, key = key)
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




run_majority_trials <- function(species, Hz, raw_data, features, target_class, model_type, hyperparameters, boost_methods, cut_conditions, stratification, k){
  #' Wrapper function for trials downsampling and then augmenting the majority class
  #' 
  #' species: key name
  #' Hz (int): Sampling frequency
  #' raw_data: raw data
  #' features: precomputed features
  #' target_class: the class that is being stratificationd and then augmented
  #' model_type (RF | LDA): Which machine learning model architecture to use
  #' hyperparamters: settings for the model
  #' boost_methods: Augmentation methods to trial
  #' cut_conditions: which percentages of the target class to retain
  #' stratification (random | IDs | bouts): by which variable to stratify the cut
  #' k: fold replications (must be 2 or more)
  
  # set up for the experiment 
  ID_groups <- make_ID_groups(features, target_col = target_col, k = k, priority_classes = target_class)
  grid <- expand.grid(subsample = cut_conditions, method = boost_methods, stringsAsFactors = FALSE)
  out_dir <- file.path("Output", species, "Volume")
  
  # figuring out the prevalance of each class...
  prev_tbl <- function(x, nm, lv) {
    tibble(class = lv,
           !!nm := as.numeric(prop.table(table(factor(x$Activity, levels = lv)))))
  }
  
  all_metrics <- vector("list", nrow(grid))
  for (j in seq_len(nrow(grid))) {
    message(species, " | ", grid$subsample[j], "% | ", grid$method[j])
    
    path <- file.path(out_dir, paste0(grid$subsample[j], "_", grid$method[j], ".csv"))
    if (file.exists(path)){
      message("This combination has been completed")
      all_metrics[[j]] <- fread(path)   # keep it for the final save
      next
    }
    
    fold_metrics <- vector("list", k)
    for (f in seq_len(k)){ # this is the k number of folds (reps) -> must be greater than 2
      # held-out individuals for this fold (will not be augmented)
      test_IDs <- ID_groups$ID[ID_groups$group == f]
      test_features <- features %>% dplyr::filter(ID %in% test_IDs)
      
      # pulling out the training data and subsampling it to the cut_condition
      train <- features %>% dplyr::filter(!ID %in% test_IDs)
      selected_features <- subset_target_data(stratification, train, test_IDs, cut = grid$subsample[j], target_class)
      # and then match it to the right raw data... (this is messy but I need for some methods)
      selected_raw <- raw_data[win_id %in% unique(selected_features$win_id)]
      
      # and now boost the sampled training data...
      boosted <- boost_training_data(
        raw_data = selected_raw,
        feature_data = selected_features,
        metadata_cols, target_class,
        method = grid$method[j],
        Hz
      )
      
      # and now see how it performs
      output <- design_build_RF(data = boosted$boosted_data, type = "multiclass", metadata_cols, hyperparameters)
      metrics <- test_model(type = "multiclass", 
                            output$RF_model,
                            test_data = test_features,
                            metadata_cols)
      
      lv <- sort(unique(as.character(c(selected_features$Activity, boosted$boosted_data$Activity, test_features$Activity))))
      
      prev <- prev_tbl(selected_features, "train_prev", lv) %>%
        left_join(prev_tbl(boosted$boosted_data, "boost_prev", lv), by = "class") %>%
        left_join(prev_tbl(test_features, "test_prev", lv), by = "class")
      
      fold_metrics[[f]] <- metrics$metrics %>%
        mutate(class = as.character(Behaviour)) %>%
        left_join(prev, by = "class") %>%
        mutate(prop    = grid$subsample[j],
               Species = species,
               fold    = f,
               method  = grid$method[j])
    }
    
    # save these metrics temporarily (takes a long time to run and I want to save as I go)
    # this is ugly but means I save progress 
    all_metrics[[j]] <- rbindlist(fold_metrics)
    tmp <- paste0(path, ".tmp")
    fwrite(all_metrics[[j]], tmp)
    file.rename(tmp, path)   # a crash mid-write can't leave a partial file that counts as "completed"
  }
  
  # but then fo a final save as well for neatness sake
  all_metrics <- rbindlist(all_metrics)
  fwrite(all_metrics, file.path(out_dir, "Performance_metrics.csv"))
  all_metrics
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
