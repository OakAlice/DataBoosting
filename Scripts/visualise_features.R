# Visulising the featue space with and without synthetic data -------------
# To understand why my methods are/aren't working, I need to visualise them in feature space.


generate_augmented_data <- function(all_species, boost_methods){
  
  grid <- expand.grid(method = setdiff(boost_methods, "Nothing"), species = all_species, stringsAsFactors = FALSE)
  for (j in seq_len(nrow(grid))) {
    message(grid$species[j], " | ", grid$method[j])
    cfg <- get_species_config(grid$species[j], settings)
    
    raw_data <- fread(file.path("Data", grid$species[j], "Formatted_raw_data.csv"))
    feature_data <- fread(file.path("Data", grid$species[j], "Nothing_Feature_data.csv"))
    
    boosted <- boost_training_data(
      raw_data,
      metadata_cols = c("ID", "Time", "Activity"), 
      target_col = "Activity",
      minority_classes = "target",
      method           = grid$method[j],
      Hz               = cfg$Hz,
      prior_features   = feature_data
    )
    
    synth <- boosted$synth_features
    fwrite(synth, file.path("Data", grid$species[j], "Boost", paste0(grid$species[j], "_", grid$method[j], ".csv")))
  }
}


plot_boosted_facets <- function(feature_data, boosted,
                                target_col = "Activity",
                                metadata_cols = c("ID", "Time", "Activity", "Method"),
                                n_neighbors = 15, min_dist = 0.1, seed = 1000,
                                ncol = 3) {
  
  feature_data <- as.data.frame(feature_data)
  boosted_list <- lapply(boosted, as.data.frame)
  
  # features shared by the reference and EVERY method
  feat_cols <- Reduce(intersect, c(list(names(feature_data)), lapply(boosted_list, names)))
  feat_cols <- setdiff(feat_cols, c(metadata_cols, target_col))
  is_num <- function(d) vapply(d[feat_cols], is.numeric, logical(1))
  ok <- Reduce(`&`, c(list(is_num(feature_data)), lapply(boosted_list, is_num)))
  feat_cols <- feat_cols[ok]
  feat_cols <- feat_cols[vapply(feature_data[feat_cols],
                                function(x) sd(x, na.rm = TRUE) > 0, logical(1))]
  
  feature_data <- feature_data[complete.cases(feature_data[feat_cols]), ]
  
  # fit once on the reference
  Xf <- scale(as.matrix(feature_data[feat_cols]))
  set.seed(seed)
  um <- umap(Xf, n_neighbors = n_neighbors, min_dist = min_dist, ret_model = TRUE)
  
  Real <- data.frame(UMAP1 = um$embedding[, 1], UMAP2 = um$embedding[, 2],
                     ID = as.character(feature_data$ID),
                     Activity = feature_data[[target_col]])
  
  # project each method's new rows with the reference scaling
  Synth <- bind_rows(lapply(names(boosted_list), function(m) {
    b  <- boosted_list[[m]]
    b  <- b[complete.cases(b[feat_cols]), ]
    Xb <- scale(as.matrix(b[feat_cols]),
                center = attr(Xf, "scaled:center"),
                scale  = attr(Xf, "scaled:scale"))
    emb <- umap_transform(Xb, um)
    data.frame(UMAP1 = emb[, 1], UMAP2 = emb[, 2], method = m)
  }))
  Synth$method <- factor(Synth$method, levels = names(boosted_list))
  
  ggplot() +
    # Real has no 'method' column, so it is repeated in every facet
    geom_point(data = Real %>% filter(Activity == "other"),
               aes(UMAP1, UMAP2, colour = ID),
               shape = 21, size = 2, alpha = 0.5) +
    geom_point(data = Real %>% filter(Activity == "target"),
               aes(UMAP1, UMAP2, colour = ID),
               size = 3, alpha = 1) +
    geom_point(data = Synth, aes(UMAP1, UMAP2),
               shape = 8, colour = "black", size = 1, stroke = 0.5, alpha = 0.5) +
    facet_wrap(~method, ncol = 2) +
    labs(colour = paste0("Opaque = target \nStars = synthetic \n\nTrain ID")) +
    scale_colour_manual(values = if (length(unique(Real$ID)) > 13) more_colours else fave_colours) +
    my_theme() +
    theme(legend.text  = element_text(size = 14),
          legend.title = element_text(size = 16))
}



