# Helper functions that dont otherwise fit into a different file ----------

# make_ID_groups <- function(feature_data, target_col = "Activity", k = 3,
#                            priority_classes = NULL, max_tries = 200) {
#   
#   # if any of the splits dont contain the validation class, my method dies. Ensure it always has
#   
#   dt <- as.data.frame(feature_data)
#   dt <- dt[dt$ID != "Synth", ] # synthetics never go in validation set
#   unique_IDs <- unique(dt$ID)
#   
#   # classes that must appear in every validation fold
#   must_have <- if (is.null(priority_classes)) unique(dt[[target_col]]) else priority_classes
#   
#   # which IDs contain each class
#   ids_by_class <- lapply(must_have, function(cls) unique(dt$ID[dt[[target_col]] == cls]))
#   names(ids_by_class) <- must_have
#   
#   # a class held by fewer than k IDs can't be in every fold
#   scarce <- names(ids_by_class)[lengths(ids_by_class) < k]
#   if (length(scarce) > 0) {
#     warning("Fewer than ", k, " IDs contain: ", paste(scarce, collapse = ", "),
#             ". Some folds will lack them.")
#   }
#   
#   # code courtesy of claude
#   group <- setNames(rep(NA_integer_, length(unique_IDs)), unique_IDs)
#   for (cls in names(sort(lengths(ids_by_class)))) {
#     ids <- sample(ids_by_class[[cls]])
#     unassigned <- ids[is.na(group[ids])]
#     already    <- table(factor(group[ids], levels = 1:k))     # balance with what's already placed
#     for (id in unassigned) {
#       f <- which.min(already)
#       group[id] <- f
#       already[f] <- already[f] + 1
#     }
#   }
#   
#   left <- names(group)[is.na(group)]
#   if (length(left) > 0) group[left] <- sample(rep(1:k, length.out = length(left)))
#   
#   data.frame(ID = names(group), group = as.integer(group), row.names = NULL)
# }

make_ID_groups <- function(features, k, id_col = "ID", seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  ids <- unique(features[[id_col]])
  if (length(ids) < k) stop("Fewer IDs (", length(ids), ") than k (", k, ")")
  
  data.frame(
    ID    = sample(ids),                    # shuffle
    group = rep_len(seq_len(k), length(ids)) # deal out 1..k evenly
  ) |> setNames(c(id_col, "group"))
}

reset_everything <- function() {
  #' this deletes everything except the raw data and the Hz for fresh runs 
  #' be careful!!!!!!!!!
  
  data_files <- list.files("Data", full.names = TRUE, recursive = TRUE)
  keep_files <- list.files("Data", full.names = TRUE, recursive = TRUE,
                             pattern = "Formatted_raw_data\\.csv$")
  keep_files <- c(keep_files, "Data/DatasetCharacteristics.csv")
  delete_files <- c(setdiff(data_files, keep_files),
                    list.files("Output", full.names = TRUE, recursive = TRUE))
  
  file.remove(delete_files) %>% invisible()
  message("cleared and ready for the next run :)")
}

subset_target_data <- function(stratification, train, test_IDs, cut, target_class){
  #' select some subset of the data to be retained
  
  target <- train %>% dplyr::filter(Activity == target_class) # this is the data we're messing with
  
  if (stratification == "windows"){
    # this will randomly select individual windows
    # I considered various rules for keeping all the individuals
    # but in the end decided to keep it truly random
    
    # thios is lame
    # cut_amount <- ceiling(nrow(target) * cut)
    target_selected <- target %>% slice_sample(n = cut)
    
  } else if (stratification == "IDs") {
    # this is by ID... removing IDs
    # retain only subsample of the training individuals that have the target class
    
    target_IDs <- unique(pool$ID)
    n_keep <- max(1, ceiling(length(target_IDs) * cut/ 100))
    keep_IDs <- sample(target_IDs, n_keep)
    target_selected <- target %>% dplyr::filter(ID %in% keep_IDs)
    
  } else if (stratification == "bouts"){  
    
    # I have hardcoded a rule for what will be considered as an observational bout (10x windows)
    # this would actually be different between every study/context
    target <- target %>%
      arrange(ID, Time) %>%
      group_by(ID) %>%
      mutate(diff = as.numeric(Time - lag(Time), units = "secs"))
    
    tab <- table(target$diff)
    mode_diff <- as.numeric(names(tab)[which.max(tab)])
    
    # bout_id restarts at 1 for every ID (cumsum is grouped), so make it unique across IDs
    target <- target %>%
      mutate(bout_id = cumsum(is.na(diff) | diff > (mode_diff * 10))) %>%
      ungroup() %>%
      mutate(bout_id = paste(ID, bout_id, sep = "_"))
    
    # and now select the proportion of bouts
    all_bouts   <- unique(target$bout_id)
    exact_bouts <- length(all_bouts) * cut / 100     # e.g. 0.1% of 10 bouts = 0.01 bouts
    n_full      <- floor(exact_bouts)                # whole bouts to keep
    frac        <- exact_bouts - n_full              # leftover fraction of one more bout
    
    ordered_bouts <- sample(all_bouts)               # random order
    keep_bouts    <- ordered_bouts[seq_len(n_full)]
    
    # the leftover fraction comes from a contiguous slice of one extra bout
    # (so 0.1% when 1% = 1 bout gives 10% of that bout, 0.5% gives 50%, and so on)
    partial <- NULL
    if (frac > 0 && n_full < length(all_bouts)) {
      pb <- target %>% dplyr::filter(bout_id == ordered_bouts[n_full + 1])
      n_keep <- max(1, round(nrow(pb) * frac))
      start  <- sample(nrow(pb) - n_keep + 1, 1)     # random start, contiguous windows
      partial <- pb[start:(start + n_keep - 1), ]
    }
    
    target_selected <- bind_rows(
      target %>% dplyr::filter(bout_id %in% keep_bouts),
      partial
    ) %>%
      select(-any_of(c("diff", "bout_id")))
    
    message(paste("total bouts:", length(all_bouts), "| requested:", round(exact_bouts, 3),
                  "| windows:", nrow(target_selected)))
  
  } else {
    print("stratification method unknwon.")
  }
  
  all <- bind_rows(train %>% dplyr::filter(Activity != target_class), target_selected)
  
  return(all)
}

prepare_species_data <- function(species, Hz) {
  
  raw_data <- fread(file.path("Data", species, "Formatted_raw_data.csv"))
  raw_data[, row_id := .I]
  
  feature_path <- file.path("Data", species, "Nothing_Feature_data.csv")
  key_path <- file.path("Data", species, "Nothing_Window_key.csv")
  
  if (file.exists(feature_path) && file.exists(key_path)) {
    message("Loading the features and window key made previously")
    feature_data <- fread(feature_path)
    key <- fread(key_path)
  } else {
    message("No precomputed features/key, generating")
    res <- generate_features_master(raw_data,
                                    sample_rate = Hz,
                                    overlap_setting = 0,
                                    window_setting = 2)
    feature_data <- res$features
    key <- res$key
    fwrite(feature_data, feature_path)
    fwrite(key, key_path)
  }
  
  raw_data[key, on = "row_id", win_id := i.win_id]
  
  list(raw = raw_data, features = feature_data, key = key)
}

