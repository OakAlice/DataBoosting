find_silhouette_score <- function(feature_data, metadata_cols, target_col, species){
  
  feat_cols <- setdiff(colnames(feature_data), c(metadata_cols, target_col))
  feat_cols <- feat_cols[vapply(feature_data[feat_cols], is.numeric, logical(1))]
  
  # drop Inf -> NA, then zero-variance columns
  feature_data[feat_cols] <- lapply(feature_data[feat_cols],
                                    function(x) replace(x, !is.finite(x), NA))
  feat_cols <- feat_cols[vapply(feature_data[feat_cols],
                                function(x) isTRUE(sd(x, na.rm = TRUE) > 0), logical(1))]
  
  clean <- feature_data[complete.cases(feature_data[feat_cols]), ]
  
  X <- scale(as.matrix(clean[feat_cols]))
  d <- dist(X)
  
  g   <- factor(clean[[target_col]])
  sil <- silhouette(as.integer(g), d)
  
  data.table(
    Species   = species,
    behaviour = levels(g),
    n         = as.integer(table(g)),
    sil_width = as.numeric(summary(sil)$clus.avg.widths),
    mean_sil  = mean(sil[, "sil_width"])   # overall mean, repeated per row
  )
  
}
