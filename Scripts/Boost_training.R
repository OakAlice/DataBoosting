# Function for boosting training data -------------------------------------
# NOTE: currently SMOTE and ADSYN only works if there is a single class being boosted
# currently have them all going to the mean of the majority and minority

# functions for each of the methods --------------------------------------

clean_raw_data <- function(training_data, metadata_cols, target_col) {
  # for cleaning the raw training data
  clean_cols <- removeBadFeatures(training_data, var_threshold = 0.3, corr_threshold = 0.9)
  training_data <- training_data %>%
    select(all_of(clean_cols), any_of(metadata_cols)) %>%
    na.omit()
  
  feat_cols <- setdiff(clean_cols, c(metadata_cols, target_col))
  X <- as.matrix(training_data[, feat_cols, drop = FALSE])
  y <- training_data[[target_col]]
  target_n <- round(mean(table(y)))
  
  return(list(training_data = training_data, feat_cols = feat_cols, y = y, X = X, target_n = target_n))
}

oversample_minority <- function(feature_data, target_col, minority_classes, target_n){
  # literally just duplicate rows from the target class
  # chose to sample it up to the mean (probably roughly half of what the majority class had)
  
  boosted_data <- feature_data %>%
    group_by(across(all_of(target_col))) %>%
    group_modify(~ {
      cls <- .y[[target_col]]
      n_current <- nrow(.x)
      
      if (cls %in% minority_classes && n_current < target_n) {
        bind_rows(
          .x,
          slice_sample(.x, n = target_n - n_current, replace = TRUE)
        )
      } else {
        .x
      }
    }) %>%
    ungroup()
  
  return(boosted_data)
}

smote_minority <- function(training_data, smote_method, metadata_cols, minority_classes){
  # cant handle NAs so have to clean first
  cleaned <- clean_raw_data(training_data, metadata_cols, target_col)
  training_data <- cleaned$training_data
  y <- cleaned$y
  aim <- cleaned$target_n
  feat_cols <- cleaned$feat_cols
  X <- cleaned$X
  
  n_min <- sum(y == minority_classes)
  n_needed <- aim - n_min
  K_chosen <- min(n_min - 1, 5)
  
  if (n_needed <= 0 || K_chosen < 1) return(NULL)
  
  syn <- tryCatch(
    if (smote_method == "SMOTE") {
      smotefamily::SMOTE(X = X, target = y, K = K_chosen,
                         dup_size = ceiling(n_needed / n_min))
    } else if (smote_method == "ADSYN") {
      smotefamily::ADAS(X = X, target = y, K = K_chosen)
    },
    error = function(e) { message(method, " failed: ", e$message); NULL }
  )
  if (is.null(syn)) return(NULL)
  
  synth_data <- syn$syn_data %>% filter(class == "target")
  if (nrow(synth_data) == 0) return(NULL)
  
  synth_data <- synth_data %>%
    slice_sample(n = min(n_needed, nrow(synth_data))) %>%
    rename(!!target_col := class) %>%
    mutate(ID = "Synth", Time = NA)
  
  # combine synthetic data with the original
  boosted_data <- rbind(training_data, synth_data)
  
  return (boosted_data)
}

noise_minority <- function(training_data, metadata_cols, target_col, minority_classes, sigma_frac = 0.1) {
  # random noise injection
  
  cleaned <- clean_raw_data(training_data, metadata_cols, target_col)
  training_data <- cleaned$training_data
  y <- cleaned$y
  target_n <- cleaned$target_n
  feat_cols <- cleaned$feat_cols
  X <- cleaned$X
  
  target_n <- round(mean(table(y)))
  
  synth_list <- lapply(intersect(minority_classes, unique(y)), function(cls) {
    X_min    <- X[y == cls, , drop = FALSE]
    n_needed <- target_n - nrow(X_min)
    if (n_needed <= 0) return(NULL)
    
    base <- X_min[sample.int(nrow(X_min), n_needed, replace = TRUE), , drop = FALSE]
    
    sds <- if (nrow(X_min) > 1) apply(X_min, 2, sd) else rep(0, ncol(X_min))
    sds[is.na(sds)] <- 0
    
    noise <- sweep(matrix(rnorm(length(base)), nrow = nrow(base)),
                   2, sds * sigma_frac, "*")
    
    out <- as.data.frame(base + noise)
    out[[target_col]] <- cls
    out$ID   <- "Synth"
    out$Time <- NA
    out
  })
  
  boosted_data <- bind_rows(training_data, bind_rows(synth_list))
  return (boosted_data)
}

DTW_minority <- function(raw_data, feature_data, target_col, minority_classes,
                         axis_cols, Hz, window_s, n_avg = 3) {
  
  win_len  <- window_s * Hz
  target_n <- round(mean(table(feature_data[[target_col]])))
  
  synth_raw_list <- lapply(minority_classes, function(cls) {
    
    n_needed <- target_n - sum(feature_data[[target_col]] == cls)
    if (n_needed <= 0) return(NULL)
    
    # non-overlapping raw windows of this class, within contiguous runs per ID
    cls_windows <- raw_data %>%
      mutate(.row = row_number()) %>%
      filter(.data[[target_col]] == cls) %>%
      group_by(ID, run = cumsum(c(1, diff(.row)) != 1)) %>%
      group_map(~ {
        m   <- as.matrix(.x[, axis_cols])
        n_w <- nrow(m) %/% win_len
        lapply(seq_len(n_w), function(i) m[((i - 1) * win_len + 1):(i * win_len), , drop = FALSE])
      }) %>%
      unlist(recursive = FALSE)
    
    if (length(cls_windows) == 0) return(NULL)
    k <- min(n_avg, length(cls_windows))
    
    lapply(seq_len(n_needed), function(i) {
      pick <- cls_windows[sample.int(length(cls_windows), k)]
      axes <- lapply(seq_along(axis_cols), function(a) {
        dtwclust::DBA(lapply(pick, function(m) m[, a]),
                      centroid = pick[[1]][, a], trace = FALSE)
      })
      m <- do.call(cbind, axes)
      colnames(m) <- axis_cols
      
      out <- as.data.frame(m)
      out$ID   <- paste0("Synth_", cls, "_", i)   # unique per window so none get merged
      out$Time <- seq_len(win_len) / Hz
      out[[target_col]] <- cls
      out
    }) %>% bind_rows()
  })
  
  synth_raw <- bind_rows(synth_raw_list)
  if (nrow(synth_raw) == 0) return(feature_data)
  
  synth_features <- generate_features_master(synth_raw,
                                             sample_rate = Hz,
                                             overlap_setting = 0,
                                             window_setting  = window_s)
  
  boosted_data <- bind_rows(feature_data, synth_features)
  return(boosted_data)
}


# main wrapper ------------------------------------------------------------
boost_training_data <- function(training_data, metadata_cols, target_col, minority_classes, method, Hz = NULL, path_to_features = NULL, test_ids = NULL){
  #' Function for boosting the minority representation in the training data... I hate this code, so ugly.
  #' training_data: dataframe of training feature data
  #' metadata_cols: string list of the metadata columns
  #' taget_col: string name of the target column
  #' minority_classes: list of classes (NOTE: only works for a single class at this point)
  #' method (Nothing | Oversample | Overlap# | SMOTE | ADSYN | Random_noise | DTW): defined method
  #' Hz: frequency of the sampling 
  #' path_to_features: In case these have already been generated, saves a lot of time. Ugly tho.
  
  if (method %in% c("Nothing", "Oversample", "SMOTE", "ADSYN", "Random_noise")){
    # these methods use the feature data directly (only generate once)
    if (file.exists(path_to_features)){
      feature_data <- fread(path_to_features)
      feature_data <- feature_data %>% dplyr::filter(!ID %in% test_IDs)
    } else {
      feature_data <- generate_features_master(training_data, 
                                                sample_rate = Hz, 
                                                overlap_setting = 0, 
                                                window_setting = ifelse(Hz > 1, 2, 5))
      fwrite(feature_data, path_to_features)
    }
  }
  
  if (method == "Nothing"){
    # just give this back to us
    boosted_data <- feature_data
    
  } else if (method == "Oversample"){
    target_n <- round(mean(table(feature_data[[target_col]])))
    boosted_data <- oversample_minority(feature_data, target_col, minority_classes, target_n)
    
  } else if (method == "SMOTE"){
    boosted_data <- smote_minority(training_data, smote_method = "SMOTE", metadata_cols, minority_classes)
    
  } else if (method == "ADSYN") {
    boosted_data <- smote_minority(training_data, smote_method = "ADSYN", metadata_cols, minority_classes)
    
  } else if (method == "Random_noise"){
    boosted_data <- noise_minority(training_data, metadata_cols, target_col, minority_classes)
  
  } else if (grep("Overlap", method) == 1){
    # overlap by the percentage defined in the string name
    # NOTE: have put some checks about whether the file exists to just save me time
    # this is bad code that doesnt fit in with the rest of the workflow uuughghghghgh
    
    overlap_percent <- as.numeric(gsub("Overlap", "", method))
    
    if (file.exists(file.path("Data", species, paste0("Overlap", overlap_percent, "_Feature_data.csv")))){
      boosted_data <- fread(file.path("Data", species, "Overlap50_Feature_data.csv"))
      
    } else { 
      data <- fread(file.path("Data", species, "Relabelled_raw_data.csv"))
      boosted_data <- generate_features_master(data = training_data,
                                               sample_rate_choice = Hz, 
                                               overlap_setting = overlap_percent, 
                                               window_setting = ifelse(Hz > 1, 2, 5))
      fwrite(boosted_data, file.path("Data", species, paste0("Overlap", overlap_percent, "_Feature_data.csv")))
    }
  
  } else if (method == "DTW") {
    boosted_data <- DTW_minority(raw_data, feature_data, target_col, minority_classes,
                             axis_cols, Hz, window_s, n_avg = 3)
    
   
   
  }
  
  
  
  
  return (boosted_data)
  
}
