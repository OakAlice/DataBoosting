# Experiments understanding augmentation ----------------------------------

# Set up ------------------------------------------------------------------
# setwd("C:/Users/oaw001/OneDrive - University of the Sunshine Coast/04.DataBoosting")
setwd("C:/Users/PC/OneDrive - University of the Sunshine Coast/04.DataBoosting")
source("Scripts/config.R") # file with all the config infomation for the analysis

# reset_everything() # deletes everything except the raw data

# Determine the target classes --------------------------------------------
target_classes <- determine_targets(all_species, plot = FALSE) # if plot true makes a bar chart

# Generate the base features ----------------------------------------------
for (species in all_species) make_base_features(species, settings)

# Silhouette score --------------------------------------------------------
# for (species in all_species) find_silhouette_scores(all_species, metadata_cols, target_col) 

# Volume (majority) trials ------------------------------------------------
majority_classes <- target_classes %>% dplyr::filter(type == "Majority")
# part 1. finding the performance curve
for (species in all_species) run_volume_experiment(species, settings, majority_classes, cut_conditions, target_col, k = 3)



  
# Boosting trials ---------------------------------------------------------
minority_classes <- target_classes %>% dplyr::filter(type == "Minority")
for (species in all_species) run_binary_experiment(species, settings, minority_classes, boost_methods, k = 3)
# go to the analysis.R file to visualise etc.




# Visualise the synthetic data --------------------------------------------
for (species in all_species) generate_augmented_data(all_species, boost_methods)




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


