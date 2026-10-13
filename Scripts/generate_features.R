# Generate Features -------------------------------------------------------

generate_features_master <- function(data, sample_rate, overlap_setting, window_setting){
  
  data <- as.data.table(data)
  data <- copy(data)[, row_id := .I]      # copy so the caller's raw_data isn't modified
  
  available_axes <- c("X", "Y", "Z")
  feats <- list(); keys <- list()
  
  for (id in unique(data$ID)){
    res <- process_features_per_chunk(data[ID == id],
                                      features_type = c("timeseries", "statistical"),
                                      window_length = window_setting,
                                      sample_rate = sample_rate,
                                      overlap_percent = overlap_setting,
                                      available_axes = available_axes)
    feats[[id]] <- res$features
    keys[[id]]  <- res$key
  }
  
  list(features = rbindlist(feats), key = rbindlist(keys))
}

# Function to process data for each ID
# had claude rewrite my old code for speed
process_features_per_chunk <- function(raw_data, features_type = c("statistical", "timeseries"),
                                       window_length, sample_rate, overlap_percent, available_axes) {
  raw_data <- as.data.table(raw_data)
  
  spw  <- as.integer(window_length * sample_rate)
  step <- max(1L, as.integer(round(spw * (1 - overlap_percent / 100))))
  n    <- nrow(raw_data)
  if (n < spw) return(list(features = data.table(), key = data.table()))
  
  nwin   <- (n - spw) %/% step + 1L              # complete windows only
  starts <- (seq_len(nwin) - 1L) * step + 1L
  idx    <- outer(0:(spw - 1L), starts, "+")     # spw x nwin index matrix
  win    <- function(v) { m <- v[idx]; dim(m) <- c(spw, nwin); m }
  
  half <- spw %/% 2L
  freq <- ((1:(half - 1L))) * sample_rate / spw      # excludes DC (bin 0)
  
  out <- list()
  
  for (ax in available_axes) {
    m   <- win(raw_data[[ax]])
    mu  <- colMeans2(m)
    d   <- m - rep(mu, each = spw)
    m2  <- colMeans2(d^2)
    m3  <- colMeans2(d^3)
    
    out[[paste0("mean_", ax)]] <- mu
    out[[paste0("max_",  ax)]] <- colMaxs(m)
    out[[paste0("min_",  ax)]] <- colMins(m)
    out[[paste0("sd_",   ax)]] <- sqrt(m2 * spw / (spw - 1))
    out[[paste0("sk_",   ax)]] <- m3 / m2^1.5 * ((spw - 1) / spw)^1.5   # e1071 type 3
    
    # FFT on mean-removed data, DC bin dropped
    mag <- abs(mvfft(d))[2:half, , drop = FALSE]
    pw  <- mag^2
    out[[paste0("mean_mag_",    ax)]] <- colMeans2(mag)
    out[[paste0("max_mag_",     ax)]] <- colMaxs(mag)
    out[[paste0("total_power_", ax)]] <- colSums2(pw)
    out[[paste0("peak_freq_", ax)]] <- freq[max.col(t(mag), ties.method = "first")]
    
    if ("timeseries" %in% features_type) {
      # cheap vectorised replacements for tsfeatures
      out[[paste0("acf1_", ax)]]    <- colSums2(d[-1, , drop = FALSE] * d[-spw, , drop = FALSE]) / colSums2(d^2)
      out[[paste0("cross_rate_", ax)]] <- colSums2(d[-1, , drop = FALSE] * d[-spw, , drop = FALSE] < 0) / (spw - 1)
      p  <- pw / rep(colSums2(pw), each = nrow(pw))
      pl <- p * log(p); pl[!is.finite(pl)] <- 0
      out[[paste0("spec_entropy_", ax)]] <- -colSums2(pl)
    }
  }
  
  # SMA / ODBA / VDBA
  A     <- as.matrix(as.data.frame(raw_data)[, available_axes, drop = FALSE])
  odba  <- win(rowSums(abs(A)))
  vdba  <- win(sqrt(rowSums(A^2)))
  out$SMA     <- colMeans2(odba)
  out$minODBA <- colMins(odba);  out$maxODBA <- colMaxs(odba)
  out$minVDBA <- colMins(vdba);  out$maxVDBA <- colMaxs(vdba)
  
  # window metadata
  win_ids <- paste0(raw_data$ID[1], "_", seq_len(nwin))
  meta <- list(win_id = win_ids, Time = raw_data$Time[starts],
               ID = raw_data$ID[starts], Activity = NA_character_)
  
  if ("Activity" %in% names(raw_data)) {
    f      <- factor(raw_data$Activity)
    am     <- win(as.integer(f))
    counts <- do.call(cbind, lapply(seq_along(levels(f)), function(k) colSums(am == k, na.rm = TRUE)))
    meta$Activity <- levels(f)[max.col(counts, ties.method = "first")]
  }
  
  key <- data.table(row_id = raw_data$row_id[as.vector(idx)],
                    win_id = rep(win_ids, each = spw))
  
  list(features = as.data.table(c(meta, out)), key = key)
}
