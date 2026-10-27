# Experiments understanding augmentation ----------------------------------

# Set up ------------------------------------------------------------------
# setwd("C:/Users/oaw001/OneDrive - University of the Sunshine Coast/04.DataBoosting")
setwd("C:/Users/PC/OneDrive - University of the Sunshine Coast/04.DataBoosting")
source("Scripts/config.R") # file with all the config infomation for the analysis

# reset_everything() # deletes everything except the raw data

# Determine the target classes --------------------------------------------
target_classes <- determine_targets(all_species, settings, plot = FALSE) # if plot true makes a bar chart

# all_species <- "Dickinson_Goat"
# boost_methods <- "Overlap50"
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
                      n_reps = 3) # this is how many reps within each fold (~5-10 replicates ideal)
                      # when the stratification is windows, there is no stochasticity, so just rep once

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

