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
  
  feat_cols <- setdiff(clean_cols, c(metadata_cols))
  X <- as.matrix(training_data[, ..feat_cols, drop = FALSE])
  y <- training_data[[target_col]]
  target_n <- round(mean(table(y)))
  
  return(list(training_data = training_data, feat_cols = clean_cols, y = y, X = X, target_n = target_n))
}

oversample_minority <- function(feature_data, target_col, target_class, target_n){
  # literally just duplicate rows from the target class
  # chose to sample it up to the mean (probably roughly half of what the majority class had)
  
  boosted_data <- feature_data %>%
    group_by(across(all_of(target_col))) %>%
    group_modify(~ {
      cls       <- .y[[target_col]]
      n_current <- nrow(.x)
      
      if (cls %in% target_class && n_current < target_n) {
        slice_sample(.x, n = target_n - n_current, replace = TRUE)
      } else {
        .x[0, ]   # zero rows, same columns
      }
    }) %>%
    ungroup()
  
  return(boosted_data)
}

smote_minority <- function(training_data, smote_method, metadata_cols, target_col,
                           target_class, k = 5, target_n = NULL) {
  
  # cant handle any NAs so have to clean the data first
  cleaned <- clean_raw_data(training_data, metadata_cols, target_col)
  y <- as.character(cleaned$y)
  X <- as.data.frame(cleaned$X)
  
  counts <- table(y)
  if (is.null(target_n)) target_n <- ceiling(max(counts) / 2)  # half the majority
  
  out <- lapply(target_class, function(cls) {
    n_cls <- counts[[cls]]
    have     <- sum(feature_data[[target_col]] == cls)
    n_needed <- target_n - have
    
    # class already at or above the mean: add extra_prop of its current count instead
    if (n_needed <= 0) n_needed <- ceiling(extra_prop * have)
    
    message("class ", cls, ": have = ", have, ", target_n = ", target_n, ", adding = ", n_needed)
    if (n_needed <= 0) return(NULL)   # only when the class has 0 windows
    
    
    k_use  <- min(k, n_cls - 1)
    y_bin  <- ifelse(y == cls, cls, "other")   # one-vs-rest so only this class is oversampled
    
    syn <- tryCatch(
      if (smote_method == "SMOTE") {
        smotefamily::SMOTE(X, y_bin, K = k_use, dup_size = ceiling(n_needed / n_cls))
      } else if (smote_method == "ADSYN") {
        smotefamily::ADAS(X, y_bin, K = k_use)
      },
      error = function(e) { message(smote_method, " failed for ", cls, ": ", e$message); NULL }
    )
    if (is.null(syn)) return(NULL)
    
    s <- syn$syn_data[syn$syn_data$class == cls, , drop = FALSE]
    if (nrow(s) == 0) return(NULL)
    if (nrow(s) > n_needed) s <- s[sample(nrow(s), n_needed), , drop = FALSE]  # trim to target
    s
  })
  
  synth_data <- data.table::rbindlist(out)
  if (nrow(synth_data) == 0) return(NULL)
  
  synth_data %>%
    dplyr::rename(!!target_col := class) %>%
    dplyr::mutate(ID = "Synth", Time = NA)
}

noise_minority <- function(training_data, metadata_cols, target_col, target_class, sigma_frac = 0.1) {
  # random noise injection
  
  cleaned <- clean_raw_data(training_data, metadata_cols, target_col)
  training_data <- cleaned$training_data
  y <- cleaned$y
  target_n <- cleaned$target_n
  X <- cleaned$X
  
  target_n <- round(mean(table(y)))
  
  synth_list <- lapply(intersect(target_class, unique(y)), function(cls) {
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
  
  boosted_data <- bind_rows(synth_list)
  return (boosted_data)
}

DTW_minority <- function(raw_data, feature_data, target_col, target_class, Hz, window_s, n_avg = 3) {
  
  axis_cols <- c("X", "Y", "Z") # hardcoded because I am a sloppy ugly coder. also a chud.
  
  win_len  <- window_s * Hz
  target_n <- round(mean(table(feature_data[[target_col]])))
  
  synth_raw_list <- lapply(target_class, function(cls) {
    
    have     <- sum(feature_data[[target_col]] == cls)
    n_needed <- target_n - have
    
    # class already at or above the mean: add extra_prop of its current count instead
    if (n_needed <= 0) n_needed <- ceiling(extra_prop * have)
    
    message("class ", cls, ": have = ", have, ", target_n = ", target_n, ", adding = ", n_needed)
    if (n_needed <= 0) return(NULL)   # only when the class has 0 windows
    
    
    # non-overlapping raw windows of this class, within contiguous runs per ID
    cls_windows <- raw_data %>%
      mutate(.row = row_number()) %>%
      dplyr::filter(.data[[target_col]] == cls) %>%
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
                      centroid = pick[[1]][, a], trace = FALSE )#,
                      #window.size = XXXXXX) # NOTE I need to actually tune this wtf
      })
      m <- do.call(cbind, axes)
      colnames(m) <- axis_cols
      
      out <- as.data.frame(m)
      out$ID   <- paste0("Synth_", cls, "_", i)   # unique per window so none get merged
      out$Time <- "NA"
      out[[target_col]] <- cls
      out
    }) %>% bind_rows()
  })
  
  synth_raw <- bind_rows(synth_raw_list)
  if (nrow(synth_raw) == 0) return(feature_data)
  
  res <- generate_features_master(data = synth_raw,
                                             sample_rate = Hz,
                                             overlap_setting = 0,
                                             window_setting  = window_s)
  synth_features <- res$features
  synth_features$ID <- "Synth" # so they dont get mixed in with the real data

  return(synth_features)
}



overlap_minority <- function(raw_data, sample_rate, metadata_cols, target_col, target_class, overlap_percent) {
  
  # Window parameters
  window_length <- if (sample_rate > 1) 2 else 5
  window_samples <- window_length * sample_rate
  overlap_samples <- round(overlap_percent / 100 * window_samples)
  step <- window_samples - overlap_samples
  
  # Find contiguous ID blocks
  runs <- rle(as.character(raw_data$ID))
  ends <- cumsum(runs$lengths)
  starts <- ends - runs$lengths + 1
  
  # Generate windows within each ID
  bounds <- do.call(rbind, Map(function(start, end) {
    n <- max(ceiling((end - start + 1 - overlap_samples) / step), 1)
    window_start <- start + (seq_len(n) - 1) * step
    
    data.frame(
      start = window_start,
      end = pmin(window_start + window_samples - 1, end)
    )
  }, starts, ends))
  
  # Keep only windows containing the minority class as the label
  # thought about doing it if any minority appeared but this is cleaner
  selected <- which(vapply(seq_len(nrow(bounds)), function(i) {
    labels <- raw_data[[target_col]][bounds$start[i]:bounds$end[i]]
    sum(labels %in% target_class) >= max(table(labels))
  }, logical(1)))
  
  # Extract selected windows
  raw_windows <- lapply(selected, function(i) {
    raw_data[bounds$start[i]:bounds$end[i], , drop = FALSE]
  })
  
  # Generate features
  features <- lapply(raw_windows, function(window) {
    
    res <- process_features_per_chunk(
      as.data.table(window),
      features_type = c("timeseries", "statistical"),
      window_length = window_length,
      sample_rate = sample_rate,
      overlap_percent = overlap_percent,
      available_axes = available_axes
    )
    res$features
  })
  
  bind_rows(features)
}

decomposition_minority <- function(raw_data, feature_data, target_col, target_class,
                                   Hz, window_s, trend_frac = 0.5, noise_frac = 0.1,
                                   amp_sd = 0.1, axis_cols = c("X", "Y", "Z")) {
  # decompose raw windows into trend / oscillation / residual, mix components
  # across different windows, recombine, then compute features
  target_n <- round(mean(table(feature_data[[target_col]])))
  win_len  <- window_s * Hz
  
  odd <- function(k) { k <- max(3, round(k)); if (k %% 2 == 0) k + 1 else k }
  k_trend <- odd(trend_frac * win_len)
  k_noise <- odd(noise_frac * win_len)
  
  # centred moving average with edge replication
  ma <- function(x, k) {
    h <- (k - 1) / 2; n <- length(x)
    y <- as.numeric(stats::filter(c(rep(x[1], h), x, rep(x[n], h)), rep(1 / k, k), sides = 2))
    y[(h + 1):(h + n)]
  }
  decomp <- function(m) {              # m: win_len x axes matrix
    trend <- apply(m, 2, ma, k = k_trend)
    det   <- m - trend
    seas  <- apply(det, 2, ma, k = k_noise)
    list(trend = trend, seas = seas, resid = det - seas)
  }
  
  synth_raw_list <- lapply(intersect(target_class, unique(raw_data[[target_col]])), function(cls) {
    
    have     <- sum(feature_data[[target_col]] == cls)
    n_needed <- target_n - have
    
    # class already at or above the mean: add extra_prop of its current count instead
    if (n_needed <= 0) n_needed <- ceiling(extra_prop * have)
    
    message("class ", cls, ": have = ", have, ", target_n = ", target_n, ", adding = ", n_needed)
    if (n_needed <= 0) return(NULL)   # only when the class has 0 windows
    
    
    # non-overlapping raw windows of this class, within contiguous runs per ID
    cls_windows <- raw_data %>%
      mutate(.row = row_number()) %>%
      dplyr::filter(.data[[target_col]] == cls) %>%
      group_by(ID, run = cumsum(c(1, diff(.row)) != 1)) %>%
      group_map(~ {
        m   <- as.matrix(.x[, axis_cols])
        n_w <- nrow(m) %/% win_len
        lapply(seq_len(n_w), function(i) m[((i - 1) * win_len + 1):(i * win_len), , drop = FALSE])
      }) %>%
      unlist(recursive = FALSE)
    
    if (length(cls_windows) < 2) return(NULL)
    parts <- lapply(cls_windows, decomp)
    
    lapply(seq_len(n_needed), function(i) {
      # trend, oscillation and residual each from a different random window
      idx <- sample.int(length(parts), 3, replace = length(parts) < 3)
      
      m <- parts[[idx[1]]]$trend +
        parts[[idx[2]]]$seas  * rnorm(1, 1, amp_sd) +
        parts[[idx[3]]]$resid * rnorm(1, 1, amp_sd)
      colnames(m) <- axis_cols
      
      out <- as.data.frame(m)
      out$ID   <- paste0("Synth_", cls, "_", i)   # unique per window so none get merged
      out$Time <- "NA"
      out[[target_col]] <- cls
      out
    }) %>% bind_rows()
  })
  
  synth_raw <- bind_rows(synth_raw_list)
  if (nrow(synth_raw) == 0) return(NULL)
  
  res <- generate_features_master(data = synth_raw,
                                             sample_rate = Hz,
                                             overlap_setting = 0,
                                             window_setting  = window_s)
  synth_features <- res$features
  synth_features$ID <- "Synth"
  return(synth_features)
}

rotation_minority <- function(raw_data, feature_data, target_col, target_class,
                              Hz, window_s, max_angle = 15, rot_axis = NULL,
                              axis_cols = c("X", "Y", "Z")) {
  # rotate real raw windows by small random angles, then compute features
  # rot_axis: NULL = random axis per window; or a unit vector e.g. c(0, 0, 1)
  target_n <- round(mean(table(feature_data[[target_col]])))
  win_len  <- window_s * Hz
  
  # Rodrigues rotation matrix
  rot_mat <- function(axis, theta) {
    axis <- axis / sqrt(sum(axis^2))
    K <- matrix(c(0, -axis[3], axis[2],
                  axis[3], 0, -axis[1],
                  -axis[2], axis[1], 0), 3, byrow = TRUE)
    diag(3) + sin(theta) * K + (1 - cos(theta)) * K %*% K
  }
  
  synth_raw_list <- lapply(intersect(target_class, unique(raw_data[[target_col]])), function(cls) {
    
    have     <- sum(feature_data[[target_col]] == cls)
    n_needed <- target_n - have
    
    # class already at or above the mean: add extra_prop of its current count instead
    if (n_needed <= 0) n_needed <- ceiling(extra_prop * have)
    
    message("class ", cls, ": have = ", have, ", target_n = ", target_n, ", adding = ", n_needed)
    if (n_needed <= 0) return(NULL)   # only when the class has 0 windows
    
    # non-overlapping raw windows of this class, within contiguous runs per ID
    cls_windows <- raw_data %>%
      mutate(.row = row_number()) %>%
      dplyr::filter(.data[[target_col]] == cls) %>%
      group_by(ID, run = cumsum(c(1, diff(.row)) != 1)) %>%
      group_map(~ {
        m   <- as.matrix(.x[, axis_cols])
        n_w <- nrow(m) %/% win_len
        lapply(seq_len(n_w), function(i) m[((i - 1) * win_len + 1):(i * win_len), , drop = FALSE])
      }) %>%
      unlist(recursive = FALSE)
    
    if (length(cls_windows) == 0) return(NULL)
    
    lapply(seq_len(n_needed), function(i) {
      m     <- cls_windows[[sample.int(length(cls_windows), 1)]]
      axis  <- if (is.null(rot_axis)) rnorm(3) else rot_axis
      theta <- runif(1, -max_angle, max_angle) * pi / 180
      m     <- m %*% t(rot_mat(axis, theta))     # rotate every row (sample)
      colnames(m) <- axis_cols
      
      out <- as.data.frame(m)
      out$ID   <- paste0("Synth_", cls, "_", i)   # unique per window so none get merged
      out$Time <- "NA"
      out[[target_col]] <- cls
      out
    }) %>% bind_rows()
  })
  
  synth_raw <- bind_rows(synth_raw_list)
  if (nrow(synth_raw) == 0) return(NULL)
  
  res <- generate_features_master(data = synth_raw,
                                             sample_rate = Hz,
                                             overlap_setting = 0,
                                             window_setting  = window_s)
  synth_features <- res$features
  
  synth_features$ID <- "Synth"
  return(synth_features)
}


# main wrapper ------------------------------------------------------------
boost_training_data <- function(training_data, metadata_cols, target_col, target_class, method, Hz = NULL, prior_features = NULL){
  #' Function for boosting the minority representation in the training data... I hate this code, so ugly.
  #' Requires input of the raw data and the feature data fml...
  #' training_data: dataframe of training feature data
  #' metadata_cols: string list of the metadata columns
  #' taget_col: string name of the target column
  #' target_class: list of classes (NOTE: only works for a single class at this point)
  #' method (Nothing | Oversample | Overlap# | SMOTE | ADSYN | Random_noise | DTW): defined method
  #' Hz: frequency of the sampling 
  #' path_to_features: In case these have already been generated, saves a lot of time. Ugly tho.
  
  target_n <- round(mean(table(prior_features[[target_col]])))
  
  if (method == "Nothing"){
    # just give this back to us
    synth_features <- prior_features
    
  } else if (method == "Oversample"){
    synth_features <- oversample_minority(prior_features, target_col, target_class, target_n)
    
  } else if (method == "SMOTE"){
    synth_features <- smote_minority(training_data = prior_features, 
                                     smote_method = "SMOTE", 
                                     metadata_cols, target_col, target_class,
                                     k = 5)
    
  } else if (method == "ADSYN") {
    synth_features <- smote_minority(prior_features, smote_method = "ADSYN", metadata_cols, target_col, target_class)
    
  } else if (method == "RandomNoise"){
    synth_features <- noise_minority(training_data = prior_features, 
                                     metadata_cols, target_col, target_class)
    
  } else if (method == "Decomposition"){
    synth_features <- decomposition_minority(training_data, prior_features, 
                                             target_col, target_class,
                                            Hz, window_s = ifelse(Hz > 1, 2, 5), 
                                            trend_frac = 0.5, noise_frac = 0.1, amp_sd = 0.1)
    synth_features <- synth_features %>% mutate(Time = prior_features$Time[1])
    
  } else if (method == "Rotation") {
    synth_features <- rotation_minority(raw_data = training_data, 
                                        feature_data = prior_features, 
                                        target_col, 
                                        target_class,
                                        Hz, 
                                        window_s = ifelse(Hz > 1, 2, 5),
                                        max_angle = 5)
    
    synth_features <- synth_features %>% mutate(Time = prior_features$Time[1])
    
  } else if (grepl("Overlap", method, fixed = TRUE)){
    # overlap by the percentage defined in the string name
    # NOTE: have put some checks about whether the file exists to just save me time

    overlap_percent <- as.numeric(gsub("Overlap", "", method))
    synth_features <- overlap_minority(training_data, sample_rate = Hz, metadata_cols, target_col, target_class, overlap_percent)

  } else if (method == "DTW") {
    synth_features <- DTW_minority(raw_data = training_data, 
                                   feature_data = prior_features, 
                                   target_col, target_class, Hz, 
                                   window_s = ifelse(Hz > 1, 2, 5), 
                                   n_avg = 3) # 1 changes nothing 
    synth_features <- synth_features %>% mutate(Time = prior_features$Time[1])
                              
  } else {
    print("Please choose from defined list of options.")
  }
  
  boosted_data <- unique(rbindlist(list(synth_features, prior_features),
                                   use.names = TRUE, fill = TRUE)) # this will also remove any possible duplicates
  
  return (list(boosted_data = boosted_data,
               synth_features = synth_features))
}
