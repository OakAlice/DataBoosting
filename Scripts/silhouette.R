find_silhouette_scores <- function(species, metadata_cols, max_per_class = 5000) {
  feature_data <- as.data.frame(fread(file.path("Data", species, "Nothing_Feature_data.csv")))
  sils <- generate_sil(feature_data, metadata_cols, target_col, species, max_per_class)
  fwrite(sils, file.path("Data", species, "Silhouette_scores.csv"))
  invisible(sils)
}

generate_sil <- function(feature_data, metadata_cols, target_col, species, max_per_class = 5000) {
  
  feat_cols <- setdiff(colnames(feature_data), c(metadata_cols, target_col))
  feat_cols <- feat_cols[vapply(feature_data[feat_cols], is.numeric, logical(1))]
  
  feature_data[feat_cols] <- lapply(feature_data[feat_cols], function(x) replace(x, !is.finite(x), NA))
  feat_cols <- feat_cols[vapply(feature_data[feat_cols],
                                function(x) isTRUE(sd(x, na.rm = TRUE) > 0), logical(1))]
  
  clean <- feature_data[complete.cases(feature_data[feat_cols]), ]
  
  # cap rows per class so dist() stays tractable (n^2 memory)
  if (!is.null(max_per_class)) {
    keep  <- unlist(lapply(split(seq_len(nrow(clean)), clean[[target_col]]),
                           function(i) if (length(i) > max_per_class) sample(i, max_per_class) else i))
    clean <- clean[sort(keep), ]
  }
  
  X <- scale(as.matrix(clean[feat_cols]))
  d <- dist(X)
  g <- factor(clean[[target_col]])
  
  # multiclass: each class's average width against all other classes jointly
  sil_multi <- silhouette(as.integer(g), d)
  
  # one-vs-rest: target class vs everything else, one binary silhouette per class
  ovr <- vapply(levels(g), function(cls) {
    s <- silhouette(as.integer(g == cls) + 1L, d)
    mean(s[s[, "cluster"] == 2L, "sil_width"])
  }, numeric(1))
  
  n_g <- as.integer(table(g))
  
  data.table(
    Species        = species,
    Activity       = levels(g),
    n              = n_g,
    proportion     = round(n_g / sum(n_g) * 100, 3),
    sil_multiclass = as.numeric(summary(sil_multi)$clus.avg.widths),
    sil_ovr        = unname(ovr),
    mean_sil       = mean(sil_multi[, "sil_width"])
  )
}
