find_silhouette_scores <- function(all_species, metadata_cols, target_col){
  
  sils <- list()
  for (species in all_species) {
    feature_data <- as.data.frame(fread(file.path("Data", species, "Nothing_Feature_data.csv")))
    sils[[species]] <- generate_sil(feature_data, metadata_cols, target_col, species)
  }
  sils <- rbindlist(sils)
  fwrite(sils, file.path("Data", "Silhouette_scores.csv"))

}


generate_sil <- function(feature_data, metadata_cols, target_col, species){
  
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
  n_g <- as.integer(table(g))
  
  data.table(
    Species    = species,
    behaviour  = levels(g),
    n          = n_g,
    proportion = round((n_g / sum(n_g)) * 100, 3),
    sil_width  = as.numeric(summary(sil)$clus.avg.widths),
    mean_sil   = mean(sil[, "sil_width"])
  )
}





