# Visulising the featue space with and without synthetic data -------------
# To understand why my methods are/aren't working, I need to visualise them in feature space.

if(file.exists(file.path("Data", species, paste0(method, "_", i, "_Feature_data.csv")))){
  
  data <- fread(file.path("Data", species, paste0(method, "_", i, "_Feature_data.csv")))

  # Select only numeric feature columns (exclude Activity, ID, Time) and scale them
  feature_cols <- setdiff(names(data), c("Activity", "ID", "Time"))
  features     <- data[, ..feature_cols]  
  scaled <- scale(features)
  
  # reduce dimensionalirty for plotting
  pca  <- prcomp(scaled, center = FALSE, scale. = FALSE)
  pca_df <- as.data.frame(pca$x[, 1:3])
  pca_df$Activity <- data$Activity
  pca_df$ID <- data$ID
  
  # split the data
  real_df  <- pca_df %>% dplyr::filter(ID != "Synth")
  synth_df <- pca_df %>% dplyr::filter(ID == "Synth")
  
  # Variance explained
  var_exp <- round(summary(pca)$importance[2, 1:3] * 100, 1)
  
  # plot it in 2d
  p_activity <- ggplot() +
    geom_point(
      data = pca_df %>% dplyr::filter(ID != "Synth"),
      aes(PC1, PC2, colour = ID),
      size = 2.2, alpha = 0.75
    ) +
    geom_point(
      data = pca_df %>% dplyr::filter(ID == "Synth"),
      aes(PC1, PC2),
      colour = "black", shape = 8, size = 1, alpha = 0.5
    ) +
    labs(
      x = paste0("PC1 (", var_exp[1], "%)"),
      y = paste0("PC2 (", var_exp[2], "%)"),
      colour = "ID"
    ) +
    my_theme() +
    theme(legend.position = "right", panel.grid.minor = element_blank()) +
    facet_wrap(~Activity)
  
  ggsave(file.path("Output", species, paste0(method, "_", i, "_Feature_PCA_plot.png")),
         p_activity,
         bg = "white")

}

