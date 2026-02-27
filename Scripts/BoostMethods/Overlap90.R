# regenerating the features, but this time with overlap -------------

#| Overlap between consecutive windows
#| In this case, 90% overlap

# Make the features -------------------------------------------------------
if (file.exists(file.path("Data", species, "Overlap90_Feature_data.csv"))){
  print("overlap 50 features already generated")
} else { 
  data <- fread(file.path("Data", species, "Formatted_raw_data.csv"))
  generated_features_df <- generate_features_master(data, 
                                                    sample_rate = sample_rates[[species]], 
                                                    overlap_setting = 90, 
                                                    window_setting = ifelse(sample_rate > 1, 2, 5))
  fwrite(generated_features_df, file.path("Data", species, "Overlap90_Feature_data.csv"))
}

# Make the model ----------------------------------------------------------
if (!file.exists(file.path("Output", species, paste0(method, "_model_", i, ".rds")))) {
  
  data <- fread(file = file.path("Data", species, paste0(method, "_Feature_data.csv")))
  other_data <- data %>% dplyr::filter(!ID %in% test_IDs) 
  RF_model <- design_build_RF(other_data)  
  saveRDS(RF_model, file.path("Output", species, paste0(method, "_model_", i, ".rds")))
  
} else {
  print("model already generated for this data")
}
