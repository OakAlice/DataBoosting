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
