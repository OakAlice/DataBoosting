# Visulising the featue space with and without synthetic data -------------
# To understand why my methods are/aren't working, I need to visualise them in feature space.

plot_boosted_data <- function(boosted, metadata_cols = c("ID", "Time"),
                              n_neighbors = 15, min_dist = 0.1, seed = 1000) {
  
  boosted   <- as.data.frame(boosted)
 
  # shared numeric features only
  feat_cols <- intersect(names(boosted))
  feat_cols <- setdiff(feat_cols, metadata_cols)
  feat_cols <- feat_cols[vapply(boosted[feat_cols], is.numeric, logical(1)) &
                           vapply(test_data[feat_cols], is.numeric, logical(1))]
  
  # drop zero-variance features, then NA rows
  keep <- vapply(boosted[feat_cols], function(x) sd(x, na.rm = TRUE) > 0, logical(1))
  feat_cols <- feat_cols[keep %in% TRUE]
  
  boosted   <- boosted[complete.cases(boosted[feat_cols]), ]
  
  
  set.seed(seed)
  um <- umap(Xb, n_neighbors = n_neighbors, min_dist = min_dist, ret_model = TRUE)
  emb_b <- um$embedding
  emb_t <- umap_transform(Xt, um)
  
  df_b <- data.frame(UMAP1 = emb_b[, 1], UMAP2 = emb_b[, 2], ID = as.character(boosted$ID))
  
  Synth <- df_b %>% dplyr::filter(ID == "Synth")
  Real <- df_b %>% dplyr::filter(!ID == "Synth")
  
  ggplot() +
    geom_point(data = Real, aes(UMAP1, UMAP2, colour = ID), size = 1, alpha = 0.5) +
    geom_point(data = Synth, aes(UMAP1, UMAP2), shape = 8,
               colour = "black", size = 1.8, stroke = 0.5, alpha = 0.25) +
    labs(colour = "Train ID") +
    scale_colour_manual(values = fave_colours)+
    my_theme()
}

