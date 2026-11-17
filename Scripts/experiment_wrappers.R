# Functions to actually run the experiment --------------------------------
# figuring out the prevalance of each class...
prev_tbl <- function(x, nm, lv) {
  tibble(class = lv,
         !!nm := as.numeric(prop.table(table(factor(x$Activity, levels = lv)))))
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

run_subsampling_trials <- function(species, Hz, raw_data, features, hyperparameters, metadata_cols, boost_methods, cut_conditions, stratification, k, n_reps = 10){
  #' Wrapper function for trials downsampling and then augmenting the majority class
  #' 
  #' species: key name
  #' Hz (int): Sampling frequency
  #' raw_data: raw data
  #' features: precomputed features
  #' hyperparamters: settings for the model
  #' boost_methods: Augmentation methods to trial
  #' cut_conditions: which percentages of the target class to retain
  #' stratification (random | IDs | bouts): by which variable to stratify the cut
  #' k: fold replications (must be 2 or more)
  #' metadata_cols: non-feature columns (passed explicitly rather than read from the global env)
   #' n_reps (int): number of subsampling replicates per fold
  
  # set up for the experiment 
  ID_groups <- make_ID_groups(features, k = k, seed = 42)
  grid <- expand.grid(subsample = cut_conditions, method = boost_methods, stringsAsFactors = FALSE)
  out_dir <- file.path("Output", species, paste0("Majority_", stratification))
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)  # otherwise the first fwrite fails
  
  all_metrics <- vector("list", nrow(grid))
  for (j in seq_len(nrow(grid))) {
    message(species, " | ", grid$subsample[j], " | ", grid$method[j])
    
    path <- file.path(out_dir, paste0(grid$subsample[j], "_", grid$method[j], ".csv"))
    if (file.exists(path)){
      message("This combination has been completed")
      all_metrics[[j]] <- fread(path)   # keep it for the final save
      next
    }
    
    beh_metrics <- vector("list", length(unique(features$Activity)))
    names(beh_metrics) <- unique(features$Activity)
    for (behaviour in unique(features$Activity)){
      message(behaviour)
      
      target_class <- behaviour # set this to be the target for the round
      if (grid$subsample[j] > nrow(features[features$Activity == behaviour])){
        next # if there aren't enough samples, just skip it
      }
        
      fold_metrics <- vector("list", k * n_reps)  # one slot per fold x replicate (was indexed by r only, so folds overwrote each other)
      idx <- 0L
      for (f in seq_len(k)){ # this is the k number of folds (reps) -> must be greater than 2
        # held-out individuals for this fold (will not be augmented)
        test_IDs <- ID_groups$ID[ID_groups$group == f]
        test_features <- features %>% dplyr::filter(ID %in% test_IDs)
        
        # pulling out the training data
        train <- features %>% dplyr::filter(!ID %in% test_IDs)
      
        # what I will be augmenting up to (recopvery of full daatset)
        target_count <- nrow(train[train$Activity == target_class]) # this is how mnay to augment up to
        
        # subsampling it multiple times
        for (r in seq_len(n_reps)){
          message("rep: ", r)
          idx <- idx + 1L
          selected_features <- subset_target_data(stratification, train, test_IDs, cut = grid$subsample[j], target_class)
          # and then match it to the right raw data... (this is messy but I need for some methods)
          selected_raw <- raw_data[win_id %in% unique(selected_features$win_id)]
          
          if (nrow(selected_features[selected_features$Activity == target_class])<10 & grid$method[j] == "SMOTE"){
            message("Too few samples for smote")
            all_metrics[[j]] <- NULL # have to skip it
            next
          }
          
          # and now boost the sampled training data if it can be done
          boosted <- boost_training_data(
            raw_data = selected_raw,
            feature_data = selected_features,
            metadata_cols, target_class, target_count,
            method = grid$method[j],
            Hz
          )
          
          output <- design_build_RF(data = boosted$boosted_data, 
                                    weighted = ifelse(grid$method[j] == "Weighted", TRUE, FALSE), 
                                    metadata_cols, hyperparameters)
          
          if (is.null(output)) next # just skip this whole thing if it didn't work (probably because no good columsn)
          
          metrics <- test_model(type = "multiclass", 
                                output$RF_model,
                                test_data = test_features,
                                metadata_cols)
          
          lv <- sort(unique(as.character(c(selected_features$Activity, boosted$boosted_data$Activity, test_features$Activity))))
          
          prev <- prev_tbl(selected_features, "train_prev", lv) %>%
            left_join(prev_tbl(boosted$boosted_data, "boost_prev", lv), by = "class") %>%
            left_join(prev_tbl(test_features, "test_prev", lv), by = "class")
          
          fold_metrics[[idx]] <- metrics$metrics %>%
            mutate(class = as.character(Behaviour)) %>%
            left_join(prev, by = "class") %>%
            mutate(prop    = grid$subsample[j],
                   Species = species,
                   Activity = behaviour,
                   fold    = f,
                   replicate = r,
                   method  = grid$method[j])
        }
      }
      
      # save these metrics temporarily (takes a long time to run and I want to save as I go)
      # this is ugly but means I save progress 
      beh_metrics[[behaviour]] <- rbindlist(fold_metrics)
    }
    
    all_metrics[[j]] <- rbindlist(beh_metrics, fill = TRUE)
    tmp <- paste0(path, ".tmp")
    fwrite(all_metrics[[j]], tmp)
    file.rename(tmp, path)
      
    }
    
  
  rbindlist(all_metrics, fill = TRUE)  # return everything (previously returned NULL)
}
