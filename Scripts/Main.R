# Part 1: Binary experiments ----------------------------------------------

# Set up ------------------------------------------------------------------
# setwd("C:/Users/oaw001/OneDrive - University of the Sunshine Coast/04.DataBoosting")
setwd("C:/Users/PC/OneDrive - University of the Sunshine Coast/04.DataBoosting")
source("Scripts/config.R") # file with all the config infomation for the analysis

# All trials --------------------------------------------------------------
# all_species <- setdiff(all_species, "Bonneau_Goat")
for (species in all_species) run_binary_experiment(species, settings, boost_methods, k = 3)
# everything saved to the data and output folders
for (species in all_species) run_volume_experiment(species, settings, boost_methods, k = 3)



# Generate synthetic data -------------------------------------------------
grid <- expand.grid(method = setdiff(boost_methods, "Nothing"), species = all_species, stringsAsFactors = FALSE)
for (j in seq_len(nrow(grid))) {
  message(grid$species[j], " | ", grid$method[j])
  cfg <- get_species_config(grid$species[j], settings)
  
  raw_data <- fread(file.path("Data", grid$species[j], "Relabelled_raw_data.csv"))
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

# Silhouette score --------------------------------------------------------
sils <- list()
for (species in all_species) {
  feature_data <- as.data.frame(fread(file.path("Data", species, "Nothing_Feature_data.csv")))
  sils[[species]] <- find_silhouette_score(feature_data, metadata_cols, target_col, species)
}
sils <- rbindlist(sils)
fwrite(sils, file.path("Output", "Silhouette_scores.csv"))


# Plots -------------------------------------------------------------------
for (species in all_species){
  message(species)
  feature_data <- fread(file.path("Data", species, "Nothing_Feature_data.csv"))
  synths  <- list.files(file.path("Data", species, "Boost"), full.names = TRUE)
  boosted <- lapply(synths, fread)
  names(boosted) <- word(basename(tools::file_path_sans_ext(synths)), 3, sep = "_")
  
  p <- plot_boosted_facets(feature_data, boosted,
                        target_col = "Activity",
                        metadata_cols = c("ID", "Time", "Activity", "Method"),
                        n_neighbors = 15, min_dist = 0.1, seed = 1000,
                        ncol = 3)
  
  ggsave(file.path("Output", species, "Figures", paste0(species, "_vis.png")), 
         p, width = 18, height = 11, dpi = 300)
}

# Analysis ----------------------------------------------------------------
# go to the analysis.R script for now... need to figure out...


