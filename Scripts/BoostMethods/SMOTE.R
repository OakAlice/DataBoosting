# Code for Synthetic Minority Over-Sampling Technique (SMOTE) -------------
# this is more complicated than the previous methods because the data can't just be 
# generated once, it can't go into the test data at all

#| Interpolating between points in the minority class
#| to generate "new" minority class points
#| Using the SMOTE function from the smotefamily package

if (!file.exists(file.path("Output", species, paste0(method, "_model_", i, ".rds")))) {
  
  data1 <- fread(file.path("Data", species, "Nothing_Feature_data.csv"))
  other_data <- data1 %>% dplyr::filter(!ID %in% test_IDs) 
  
  # gotta get rid of the bad columns because SMOTE cant cope with NA values
  clean_cols <- removeBadFeatures(other_data, var_threshold = 0.3, corr_threshold = 0.9)
  other_data <- other_data %>%
    select(c(!!!syms(clean_cols), "Activity", "ID", "Time")) %>% 
    na.omit()
  
  # calculate the aim (mean value) that we want to increase to
  aim <- other_data %>%
    count(Activity) %>%
    summarise(mean = round(mean(n))) %>%
    pull(mean)
  
  # pull out the minority classes
  minorities <- other_data %>%
    count(Activity) %>%
    dplyr::filter(n < aim) %>%
    pull(Activity)
  
  # SMOTE each of the minority classes independently
  synth_list <- lapply(minorities, function(minority_class) {
    
    minority_data <- other_data %>% dplyr::filter(Activity == minority_class)
    
    K_min <- nrow(minority_data) - 1
    K_chosen <- min(c(K_min, 5))
    minority_numeric <- minority_data %>% select(-Activity, -Time, -ID)
    
    # calculate by how much to increase so we don't aggressively overshoot
    dupe_by <- max(1, round(aim / nrow(minority_data)) - 1)
    
    dat_boosted <- smotefamily::SMOTE(X = minority_numeric,
                                      target = minority_data$Activity,
                                      K = K_chosen,
                                      dup_size = dupe_by)
    
    dat_boosted <- dat_boosted$syn_data
    dat_boosted$ID <- "Synth"
    dat_boosted$Time <- NA
    setnames(dat_boosted, "class", "Activity")
    
    # we have probably calculated too much so decrease it back down
    n_needed <- aim - nrow(minority_data)
    dat_boosted <- dat_boosted %>%
      slice_sample(n = min(n_needed, nrow(dat_boosted)))
    
    return(dat_boosted)
  })
  
  # combine all synthetic data and original
  other_data <- rbind(other_data, rbindlist(synth_list, fill = TRUE))
  
  # save this data so I can visualise it later
  fwrite(other_data, file.path("Data", species, paste0(method, "_", i, "_Feature_data.csv")))
  
  RF_model <- design_build_RF(other_data)  
  saveRDS(RF_model, file.path("Output", species, paste0(method, "_model_", i, ".rds")))
  
} else {
  print("model already made for this round")
}
