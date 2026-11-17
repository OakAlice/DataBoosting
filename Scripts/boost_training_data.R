# Function for boosting training data -------------------------------------
# NOTE: currently SMOTE and ADSYN only works if there is a single class being boosted
# currently have them all going to the previous prevalance except overlap which will do its own thing
# this needs to be mroe clearly thought out.
# Probably filled with bugs and errors

clean_raw_data <- function(training_data, metadata_cols) {
  
  # for cleaning the raw training data
  clean_cols <- removeBadFeatures(training_data, var_threshold = 0.3, corr_threshold = 0.9)
  
  if (is.null(clean_cols) || length(clean_cols) == 0) return(NULL) # skip out of this fast
  
  training_data <- training_data %>%
    select(all_of(clean_cols), any_of(metadata_cols)) %>%
    na.omit()
  
  feat_cols <- setdiff(clean_cols, c(metadata_cols))
  X <- as.matrix(training_data[, ..feat_cols, drop = FALSE])
  y <- training_data$Activity
  
  return(list(training_data = training_data, feat_cols = clean_cols, y = y, X = X))
}

oversample_minority <- function(feature_data, target_class, target_n){
  # literally just duplicate rows from the target class
  # chose to sample it up to the mean (probably roughly half of what the majority class had)
  
  boosted_data <- feature_data %>%
    group_by(across(all_of("Activity"))) %>%
    group_modify(~ {
      cls       <- .y[["Activity"]]
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

overlap_minority <- function(raw_data, sample_rate, metadata_cols, target_class, overlap_percent) {
  
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
    L <- end - start + 1
    if (L < window_samples) return(NULL)               # drop short blocks
    n <- floor((L - window_samples) / step) + 1        # full windows only
    window_start <- start + (seq_len(n) - 1) * step
    data.frame(start = window_start, end = window_start + window_samples - 1)
  }, starts, ends))
  
  # Keep only windows containing the minority class as the label
  # thought about doing it if any minority appeared but this is cleaner
  is_t <- raw_data$Activity %in% target_class
  cs   <- c(0, cumsum(is_t))                               # running count of target samples
  n_t  <- cs[bounds$end + 1] - cs[bounds$start]            # target samples per window
  selected <- which(n_t / (bounds$end - bounds$start + 1) > 0.5)
  
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

noise_minority <- function(training_data, metadata_cols, target_class, target_n, sigma_frac = 0.1) {
  # random noise injection
  
  cleaned <- clean_raw_data(training_data, metadata_cols)
  training_data <- cleaned$training_data
  y <- cleaned$y
  X <- cleaned$X
  
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
    out[["Activity"]] <- cls
    out$ID   <- "Synth"
    out$Time <- NA
    out
  })
  
  boosted_data <- bind_rows(synth_list)
  return (boosted_data)
}

rotation_minority <- function(raw_data, feature_data, target_class, target_n,
                              Hz, window_s, max_angle = 15, rot_axis = NULL,
                              axis_cols = c("X", "Y", "Z")) {
  # rotate real raw windows by small random angles, then compute features
  # rot_axis: NULL = random axis per window; or a unit vector e.g. c(0, 0, 1)
  win_len  <- window_s * Hz
  
  # Rodrigues rotation matrix
  rot_mat <- function(axis, theta) {
    axis <- axis / sqrt(sum(axis^2))
    K <- matrix(c(0, -axis[3], axis[2],
                  axis[3], 0, -axis[1],
                  -axis[2], axis[1], 0), 3, byrow = TRUE)
    diag(3) + sin(theta) * K + (1 - cos(theta)) * K %*% K
  }
  
  synth_raw_list <- lapply(intersect(target_class, unique(raw_data$Activity)), function(cls) {
    
    have <- sum(feature_data$Activity == cls)
    n_needed <- target_n - have
    
    # class already at or above the mean: add extra_prop of its current count instead
    if (n_needed <= 0){ 
      message("Already at maximum count")
    }
    
    message("class ", cls, ": have = ", have, ", target_n = ", target_n, ", adding = ", n_needed)
    if (n_needed <= 0) return(NULL)   # only when the class has 0 windows
    
    # non-overlapping raw windows of this class, within contiguous runs per ID
    cls_windows <- raw_data %>%
      as_tibble() %>%
      mutate(.row = row_number()) %>%
      dplyr::filter(.data[["Activity"]] == cls) %>%
      group_by(ID, run = cumsum(c(1, diff(.row)) != 1)) %>%
      group_map(~ {
        m   <- as.matrix(.x[, axis_cols, drop = FALSE])
        n_w <- nrow(m) %/% win_len
        lapply(seq_len(n_w), function(i) m[((i - 1) * win_len + 1):(i * win_len), , drop = FALSE])
      }) %>%
      unlist(recursive = FALSE)
    
    if (length(cls_windows) == 0) {
      warning("No windows of length ", win_len, " for class '", cls, "'")
      cls_windows <- list()
      return(NULL)
    }
    
    # code courtesy of claude
    lapply(seq_len(n_needed), function(i) {
      m <- cls_windows[[sample.int(length(cls_windows), 1)]]
      axis <- if (is.null(rot_axis)) rnorm(3) else rot_axis
      theta <- runif(1, -max_angle, max_angle) * pi / 180
      m <- m %*% t(rot_mat(axis, theta))     # rotate every row (sample)
      colnames(m) <- axis_cols
      
      out <- as.data.frame(m)
      out$ID   <- paste0("Synth_", cls, "_", i)   # unique per window so none get merged
      out$Time <- "NA"
      out[["Activity"]] <- cls
      out
    }) %>% bind_rows()
  })
  
  synth_raw <- bind_rows(synth_raw_list)
  if (nrow(synth_raw) == 0) return(NULL)
  
  res <- generate_features_master(data = synth_raw, sample_rate = Hz, overlap_setting = 0, window_setting  = window_s)
  synth_features <- res$features
  
  synth_features$ID <- "Synth"
  return(synth_features)
}

smote_minority <- function(training_data, smote_method = c("SMOTE_within", "SMOTE_between"),
                           metadata_cols, target_class, target_n, k = 5) {
  
  # SMOTE_within:  synthesises inside each individual only (neighbours never cross IDs)
  # SMOTE_between: synthesises from the pooled samples of all individuals
  smote_method <- match.arg(smote_method)
  
  cleaned <- clean_raw_data(training_data, metadata_cols)
  y   <- as.character(cleaned$y)
  X   <- as.data.frame(cleaned$X)
  ids <- as.character(training_data[["ID"]])   # assumes rows align with cleaned$X / cleaned$y
  
  # Generate exactly n synthetic rows of class cls from the rows in `idx`
  gen_synth <- function(idx, cls, n, k) {
    n_min <- length(idx)
    if (n <= 0 || n_min < 2) return(NULL)
    k_use <- min(k, n_min - 1)
    
    # smotefamily oversamples the smaller class, so "other" must be larger.
    # Decoy rows come from all other-class rows and are discarded afterwards.
    oth_idx <- which(y != cls)
    if (length(oth_idx) == 0) return(NULL)
    want_oth <- n_min + 1
    oth_pick <- if (length(oth_idx) >= want_oth) sample(oth_idx, want_oth) else
      sample(oth_idx, want_oth, replace = TRUE)
    
    X_in <- rbind(X[idx, , drop = FALSE], X[oth_pick, , drop = FALSE])
    y_in <- c(rep(cls, n_min), rep("other", length(oth_pick)))
    
    syn <- tryCatch(
      smotefamily::SMOTE(X_in, y_in, K = k_use, dup_size = ceiling(n / n_min)),
      error = function(e) { message("SMOTE failed for ", cls, ": ", e$message); NULL }
    )
    if (is.null(syn)) return(NULL)
    
    s <- syn$syn_data[syn$syn_data$class == cls, , drop = FALSE]
    if (nrow(s) == 0) return(NULL)
    if (nrow(s) > n) s <- s[sample(nrow(s), n), , drop = FALSE]
    s
  }
  
  out <- lapply(target_class, function(cls) {
    cls_idx  <- which(y == cls)
    have     <- length(cls_idx)
    n_needed <- target_n - have   # synthetic rows needed to land on exactly target_n
    
    message("class ", cls, " (", smote_method, "): have = ", have,
            ", target_n = ", target_n, ", adding = ", max(n_needed, 0))
    if (n_needed <= 0) return(NULL)
    
    if (smote_method == "SMOTE_between") {
      return(gen_synth(cls_idx, cls, n_needed, k))
    }
    
    # SMOTE_within: split n_needed across individuals in proportion to their sample count
    id_counts <- table(ids[cls_idx])
    id_counts <- id_counts[id_counts >= 2]   # need >= 2 samples to interpolate
    if (length(id_counts) == 0) {
      message("  no individual has >= 2 samples of ", cls, ", skipping")
      return(NULL)
    }
    
    raw   <- n_needed * id_counts / sum(id_counts)
    quota <- floor(raw)
    left  <- n_needed - sum(quota)
    if (left > 0) {
      bump <- order(raw - quota, decreasing = TRUE)[seq_len(left)]
      quota[bump] <- quota[bump] + 1
    }
    
    res <- lapply(names(id_counts), function(id) {
      gen_synth(cls_idx[ids[cls_idx] == id], cls, quota[[id]], k)
    })
    s <- data.table::rbindlist(res)
    if (nrow(s) == 0) return(NULL)
    
    # top up any shortfall (failed IDs) from the eligible individuals
    short <- n_needed - nrow(s)
    if (short > 0) {
      message("  within-ID gave ", nrow(s), "/", n_needed, " for ", cls, ", topping up")
      pick <- sample(names(id_counts), short, replace = TRUE, prob = as.numeric(id_counts))
      extra <- lapply(unique(pick), function(id) {
        gen_synth(cls_idx[ids[cls_idx] == id], cls, sum(pick == id), k)
      })
      s <- data.table::rbindlist(c(list(s), extra))
    }
    if (nrow(s) > n_needed) s <- s[sample(nrow(s), n_needed), ]
    as.data.frame(s)
  })
  
  synth_data <- data.table::rbindlist(out)
  if (nrow(synth_data) == 0) return(NULL)
  
  synth_data %>%
    rename(!!"Activity" := class) %>%
    mutate(ID = "Synth", Time = NA)
}
      
      
DTW_minority <- function(raw_data, feature_data, target_class, Hz, window_s, n_avg = 3) {
  
  axis_cols <- c("X", "Y", "Z") # hardcoded because I am a sloppy ugly coder. also a chud.
  
  win_len  <- window_s * Hz
  target_n <- round(mean(table(feature_data[["Activity"]])))
  
  synth_raw_list <- lapply(target_class, function(cls) {
    
    have     <- sum(feature_data[["Activity"]] == cls)
    n_needed <- target_n - have
    
    # class already at or above the mean: add extra_prop of its current count instead
    if (n_needed <= 0){ 
      n_needed <- ceiling(0.2 * have) # this is hardcoded
      message("I'm arbitrarily adding 20% here because the mean condition is already met")
    }
    
    message("class ", cls, ": have = ", have, ", target_n = ", target_n, ", adding = ", n_needed)
    if (n_needed <= 0) return(NULL)   # only when the class has 0 windows
    
    # non-overlapping raw windows of this class, within contiguous runs per ID
    cls_windows <- raw_data %>%
      mutate(.row = row_number()) %>%
      dplyr::filter(.data[["Activity"]] == cls) %>%
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
      out[["Activity"]] <- cls
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

decomposition_minority <- function(raw_data, feature_data, target_class, target_n,
                                   Hz, window_s, trend_frac = 0.5, noise_frac = 0.1,
                                   amp_sd = 0.1, axis_cols = c("X", "Y", "Z")) {
  # decompose raw windows into trend / oscillation / residual, mix components
  # across different windows, recombine, then compute features
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
  
  synth_raw_list <- lapply(intersect(target_class, unique(raw_data[["Activity"]])), function(cls) {
    
    have     <- sum(feature_data[["Activity"]] == cls)
    n_needed <- target_n - have
    
    # class already at or above the mean: add extra_prop of its current count instead
    if (n_needed <= 0){ 
      n_needed <- ceiling(0.2 * have) # this is hardcoded
      message("I'm arbitrarily adding 20% here because the mean condition is already met")
    }
    
    message("class ", cls, ": have = ", have, ", target_n = ", target_n, ", adding = ", n_needed)
    if (n_needed <= 0) return(NULL)   # only when the class has 0 windows
    
    # non-overlapping raw windows of this class, within contiguous runs per ID
    cls_windows <- raw_data %>%
      mutate(.row = row_number()) %>%
      dplyr::filter(.data[["Activity"]] == cls) %>%
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
      out[["Activity"]] <- cls
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
boost_training_data <- function(raw_data, feature_data = NULL, metadata_cols, target_class, target_count, method, Hz = NULL){
  #' Function for boosting the minority representation in the training data... I hate this code, so ugly.
  #' Requires input of the raw data and the feature data fml...
  #' raw_data: dataframe of training feature data
  #' metadata_cols: string list of the metadata columns
  #' taget_col: string name of the target column
  #' target_class: list of classes (NOTE: only works for a single class at this point)
  #' method (Nothing | Oversample | Overlap# | SMOTE | ADSYN | Random_noise | DTW): defined method
  #' Hz: frequency of the sampling 
  #' path_to_features: In case these have already been generated, saves a lot of time. Ugly tho.
   
  # generate the feature data
  res <- generate_features_master(raw_data, sample_rate = Hz, overlap_setting = 0, window_setting = 2)
  feature_data <- res$features
  
  if (method == "Nothing"){ # Control 1
    # just give this back to us
    synth_features <- feature_data
    
  } else if (method == "Weighted"){ # Control 2
      # just give this back to us
      synth_features <- feature_data
  
  } else if (method == "Useless"){ # Control 3
    
    cleaned <- clean_raw_data(feature_data, metadata_cols)
    training_data <- cleaned$training_data
    y <- cleaned$y
    X <- cleaned$X
    
    synth_list <- lapply(intersect(target_class, unique(y)), function(cls) {
      X_min    <- X[y == cls, , drop = FALSE]
      n_needed <- target_count - nrow(X_min)
      if (n_needed <= 0) return(NULL)
      
      base <- X_min[sample.int(nrow(X_min), n_needed, replace = TRUE), , drop = FALSE]
      
      sds <- if (nrow(X_min) > 1) apply(X_min, 2, sd) else rep(0, ncol(X_min))
      sds[is.na(sds)] <- 0
      
      noise <- sweep(matrix(rnorm(length(base)), nrow = nrow(base)),
                     2, sds * 0.001, "*")
      
      out <- as.data.frame(base + noise)
      out[["Activity"]] <- cls
      out$ID   <- "Synth"
      out$Time <- NA
      out
    })
    
    synth_features <- bind_rows(synth_list)
    
  } else if (method %in% c("Oversample", "Scramble")){ # Control 4
    synth_features <- oversample_minority(feature_data, target_class, target_count)
    
  } else if (grepl("Overlap", method, fixed = TRUE)){
    overlap_percent <- as.numeric(gsub("Overlap", "", method))
    synth_features <- overlap_minority(raw_data, sample_rate = Hz, metadata_cols, target_class, overlap_percent)
    
  } else if (method == "RandomNoise"){
    synth_features <- noise_minority(training_data = feature_data, metadata_cols, target_class, target_count)
    
  } else if (method == "Rotation") {
    synth_features <- rotation_minority(raw_data, feature_data, target_class, target_n = target_count, Hz, 
                                        window_s = ifelse(Hz > 1, 2, 5),
                                        max_angle = 5) # how much it can rotate by... have made this conservative
    if (is.null(synth_features)) return (NULL)
    synth_features <- synth_features %>% mutate(Time = feature_data$Time[1])
    
    
  } else if (method == "Rotation_Useless") {
      synth_features <- rotation_minority(raw_data, feature_data, target_class, target_n = target_count, Hz, 
                                          window_s = ifelse(Hz > 1, 2, 5),
                                          max_angle = 0) # in this case, nothing
      if (is.null(synth_features)) return (NULL)
      synth_features <- synth_features %>% mutate(Time = feature_data$Time[1])
    
  } else if (grepl(method, "SMOTE_within")){
    
    if (nrow(feature_data[feature_data$Activity == target_class])<10){
      message("Too few samples for smote")
      return(NULL) # exit out if this can't be done # this is definitely going to error downstream UGH
      # TODO: Fix this
    }
    
    synth_features <- smote_minority(training_data = feature_data, 
                                     smote_method = "SMOTE_within", 
                                     metadata_cols, target_class, target_count,
                                     k = 5)
    
  } else if (grepl(method, "SMOTE_between")){
      
      if (nrow(feature_data[feature_data$Activity == target_class])<10){
        message("Too few samples for smote")
        return(NULL) # exit out if this can't be done # this is definitely going to error downstream UGH
        # TODO: Fix this
      }
      
      synth_features <- smote_minority(training_data = feature_data, 
                                       smote_method = "SMOTE_between", 
                                       metadata_cols, target_class, target_count,
                                       k = 5)
    
  } else if (method == "ADSYN") {
    synth_features <- smote_minority(feature_data, smote_method = "ADSYN", metadata_cols, target_class, target_count)
  
  } else if (method == "Decomposition"){
    synth_features <- decomposition_minority(raw_data, feature_data, 
                                             target_class, target_count,
                                            Hz, window_s = ifelse(Hz > 1, 2, 5), 
                                            trend_frac = 0.5, noise_frac = 0.1, amp_sd = 0.1)
    synth_features <- synth_features %>% mutate(Time = feature_data$Time[1])
  
  } else if (method == "DTW") {
    synth_features <- DTW_minority(raw_data = raw_data, 
                                   feature_data = feature_data, 
                                   target_class, Hz, 
                                   window_s = ifelse(Hz > 1, 2, 5), 
                                   n_avg = 3) # 1 changes nothing 
    synth_features <- synth_features %>% mutate(Time = feature_data$Time[1])
                              
  } else {
    
    print("Please choose from defined list of boosting options (in the config file).")
    
  }
  
  if (is.null(synth_features)) return (NULL)
  
  boosted_data <- unique(rbindlist(list(synth_features, feature_data),
                                   use.names = TRUE, fill = TRUE)) # this will also remove any possible duplicates
  
 if (method == "Scramble"){ # Control 2
  # oversample to bring to total count and then scramble the lables
  boosted_data$Activity <- sample(boosted_data$Activity)
 }
  
  return (list(boosted_data = boosted_data,
               synth_features = synth_features))
}
