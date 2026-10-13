# Experiments understanding augmentation ----------------------------------

# Set up ------------------------------------------------------------------
# setwd("C:/Users/oaw001/OneDrive - University of the Sunshine Coast/04.DataBoosting")
setwd("C:/Users/PC/OneDrive - University of the Sunshine Coast/04.DataBoosting")
source("Scripts/config.R") # file with all the config infomation for the analysis

# reset_everything() # deletes everything except the raw data

# Determine the target classes --------------------------------------------
target_classes <- determine_targets(all_species, plot = FALSE) # if plot true makes a bar chart

# species <- "Galea_Cat"
# all_species <- setdiff(all_species, "Dickinson_Goat")
for (species in all_species){
  
  message(species)
  
  # Generate the base features ----------------------------------------------
  dat <- make_base_features(species, settings)

  # Fit a RF to all the data once -------------------------------------------
  # this is to determine the optimal hyperparameters rather than refitting every loop
  hyperparameters <- find_optimal_hyperparameters(species, settings, dat)

  # Silhouette score --------------------------------------------------------
  # find_silhouette_scores(all_species, metadata_cols, target_col) 

  # Volume (majority) trials ------------------------------------------------
  majority_classes <- target_classes %>% dplyr::filter(type == "Majority", Species == species) %>% pull(Activity)
  run_volume_experiment(species, settings, dat, target_class = majority_classes, hyperparameters, cut_conditions, downsample, target_col, k = 2) # must be 2 or more
  
  # Boosting trials ---------------------------------------------------------
  # minority_classes <- target_classes %>% dplyr::filter(type == "Minority", Species == species) %>% pull(Activity)
  # run_binary_experiment(species, settings, dat, minority_classes, hyperparameters, boost_methods, k = 2) # has to be 2 or more

  # Generate the data for visualisation -------------------------------------
  # generate_augmented_data(all_species, boost_methods)  
  
  
}


  # go to the analysis.R file to visualise etc.





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


