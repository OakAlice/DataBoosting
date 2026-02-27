# Script for doing nothing ------------------------------------------------

#| Just generate features directly from the original data

# Make the features -------------------------------------------------------
if (file.exists(file.path("Data", species, "Nothing_Feature_data.csv"))){
  print("basic features already generated")
} else { 
  data <- fread(file.path("Data", species, "Formatted_raw_data.csv"))
  generated_features_df <- generate_features_master(data, 
                                                    sample_rate = sample_rates[[species]], 
                                                    overlap_setting = 0, 
                                                    window_setting = ifelse(sample_rate > 1, 2, 5))
  fwrite(generated_features_df, file.path("Data", species, "Nothing_Feature_data.csv"))
}

# Make the model ----------------------------------------------------------
if (!file.exists(file.path("Output", species, paste0("Nothing_model_", i, ".rds")))) {
  
  data <- fread(file = file.path("Data", species, "Nothing_Feature_data.csv"))
  other_data <- data %>% dplyr::filter(!ID %in% test_IDs) 
  RF_model <- design_build_RF(other_data)  
  saveRDS(RF_model, file.path("Output", species, paste0("Nothing_model_", i, ".rds")))
  
} else {
  print("model already generated for this data")
}
