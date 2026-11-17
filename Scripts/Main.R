# Experiments understanding augmentation ----------------------------------

# Set up ------------------------------------------------------------------
# setwd("C:/Users/oaw001/OneDrive - University of the Sunshine Coast/04.DataBoosting")
setwd("C:/Users/PC/OneDrive - University of the Sunshine Coast/04.DataBoosting")
source("Scripts/config.R") # file with all the config infomation for the analysis

# reset_everything() # deletes everything except the raw data to clean dir for a fresh run

all_species <- "Dickinson_Goat"
boost_methods <- "SMOTE_between"
for (species in all_species){
  
  # Generate the base features ----------------------------------------------
  dat <- prepare_species_data(species, Hz = settings$Hz[settings$Species == species])

  # Fit a RF to all the data once -------------------------------------------
  # hyperparameters <- find_optimal_hyperparameters(species, settings, dat)
  hyperparameters <- "default" # fast option... note that optimisation no longer works with the code changes Ive made anyway
  
  # Volume (majority) trials ------------------------------------------------
  run_subsampling_trials(species,
                      Hz = settings$Hz[settings$Species == species],
                      raw_data = dat$raw,
                      features = dat$features,
                      hyperparameters, metadata_cols, boost_methods, cut_conditions, stratification,
                      k = 2, # this is the number of test data folds -- must be 2 or more fr code to work
                      n_reps = 2) # this is how many reps within each fold (~5-10 replicates ideal)
  
  # find the silhouette scores
  sils <- find_silhouette_scores(species, metadata_cols)
  fwrite(sils, file.path("Output", species, "Silhouette_scores.csv"))
  
}

# go to the analysis.R file to visualise and run stats etc.


