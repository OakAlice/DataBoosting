# Experiments understanding augmentation ----------------------------------

# Set up ------------------------------------------------------------------
# setwd("C:/Users/oaw001/OneDrive - University of the Sunshine Coast/04.DataBoosting")
setwd("C:/Users/PC/OneDrive - University of the Sunshine Coast/04.DataBoosting")
source("Scripts/config.R") # file with all the config infomation for the analysis

# reset_everything() # deletes everything except the raw data

# Determine the target classes --------------------------------------------
target_classes <- determine_targets(all_species, plot = FALSE) # if plot true makes a bar chart

# all_species <- setdiff(all_species, "Dunford_Cat")
for (species in all_species){
  
  message(species)
  
  # Generate the base features ----------------------------------------------
  dat <- make_base_features(species, settings)

  # Fit a RF to all the data once -------------------------------------------
  # hyperparameters <- find_optimal_hyperparameters(species, settings, dat)
  hyperparameters <- "default"
  
  # Volume (majority) trials ------------------------------------------------
  majority_class <- target_classes %>% dplyr::filter(type == "Majority", Species == species) %>% pull(Activity)
  run_majority_trials(species,
                      Hz = settings$Hz[settings$Species == species],
                      raw_data = dat$raw,
                      features = dat$features,
                      target_class = majority_class,
                      hyperparameters, metadata_cols, boost_methods, cut_conditions, stratification,
                      k = 3, # this is the number of test data folds -- must be 2 or more
                      n_reps = 2) # this is how many reps within each fold (~5-10 replicates ideal)

  # Boosting trials ---------------------------------------------------------
  # minority_class <- target_classes %>% dplyr::filter(type == "Minority", Species == species) %>% pull(Activity)
  # run_minority_trials(species,
  #                     Hz = settings$Hz[settings$Species == species],
  #                     raw_data = dat$raw,
  #                     features = dat$features,
  #                     target_class = minority_class,
  #                     hyperparameters, boost_methods, k = 3) # has to be 2 or more

  # Generate the data for visualisation -------------------------------------
  # generate_augmented_data(all_species, boost_methods)  
  
  # Silhouette score --------------------------------------------------------
  # find_silhouette_scores(all_species, metadata_cols, target_col) 
  
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


