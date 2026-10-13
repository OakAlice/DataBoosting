make_ID_groups <- function(feature_data, target_col = "Activity", k = 3,
                           priority_classes = NULL, max_tries = 200) {
  
  # if any of the splits dont contain the validation class, my method dies. Ensure it always has
  
  dt <- as.data.frame(feature_data)
  dt <- dt[dt$ID != "Synth", ]                       # synthetics never go in validation
  unique_IDs <- unique(dt$ID)
  
  # classes that must appear in every validation fold
  must_have <- if (is.null(priority_classes)) unique(dt[[target_col]]) else priority_classes
  
  # which IDs contain each class
  ids_by_class <- lapply(must_have, function(cls) unique(dt$ID[dt[[target_col]] == cls]))
  names(ids_by_class) <- must_have
  
  # a class held by fewer than k IDs can't be in every fold
  scarce <- names(ids_by_class)[lengths(ids_by_class) < k]
  if (length(scarce) > 0) {
    warning("Fewer than ", k, " IDs contain: ", paste(scarce, collapse = ", "),
            ". Some folds will lack them.")
  }
  
  # 1) deterministic: deal the IDs of each class round-robin across folds,
  #    rarest class first, so every fold gets some of each
  group <- setNames(rep(NA_integer_, length(unique_IDs)), unique_IDs)
  for (cls in names(sort(lengths(ids_by_class)))) {
    ids <- sample(ids_by_class[[cls]])
    unassigned <- ids[is.na(group[ids])]
    already    <- table(factor(group[ids], levels = 1:k))     # balance with what's already placed
    for (id in unassigned) {
      f <- which.min(already)
      group[id] <- f
      already[f] <- already[f] + 1
    }
  }
  
  # 2) IDs with no must-have class: random fill
  left <- names(group)[is.na(group)]
  if (length(left) > 0) group[left] <- sample(rep(1:k, length.out = length(left)))
  
  data.frame(ID = names(group), group = as.integer(group), row.names = NULL)
}

cached <- function(path, compute) {
  if (file.exists(path)) return(fread(path))
  out <- compute()
  fwrite(out, path)
  out
}


reset_everything <- function() {
  data_files   <- list.files("Data", full.names = TRUE, recursive = TRUE)
  keep_files   <- list.files("Data", full.names = TRUE, recursive = TRUE,
                             pattern = "Formatted_raw_data\\.csv$")
  keep_files <- c(keep_files, "Data/DatasetCharacteristics.csv")
  delete_files <- c(setdiff(data_files, keep_files),
                    list.files("Output", full.names = TRUE, recursive = TRUE))
  
  file.remove(delete_files) %>% invisible()
}








subset_train_data <- function(downsample, other, target, test_IDs, cut){
  
  pool <- target %>% dplyr::filter(!ID %in% test_IDs)
  
  if (downsample == "samples"){
    
    n_total <- ceiling(nrow(pool) * (cut/100))
    
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
    
    train <- bind_rows(other %>% dplyr::filter(!ID %in% test_IDs), target_train)
    
  } else {
    # this is by ID... removing IDs
    # retain only subsample% of the training individuals that have the target class
    
    target_IDs <- unique(pool$ID)
    n_keep     <- max(1, ceiling(length(target_IDs) * cut/ 100))
    keep_IDs   <- sample(target_IDs, n_keep)
    
    target_train <- pool %>% dplyr::filter(ID %in% keep_IDs)
    
    # other classes from the dropped individuals stay in training, so only the
    # target class is reduced
    train <- bind_rows(other %>% dplyr::filter(!ID %in% test_IDs), target_train)
  }
  
  return(train)
}

