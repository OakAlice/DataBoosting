# Code for Synthetic Minority Over-Sampling Technique (SMOTE) -------------
# this is more complicated than the previous methods because the data can't just be 
# generated once, it can't go into the test data at all

#| Interpolating between points in the minority class
#| to generate "new" minority class points
#| Using the SMOTE function from the smotefamily package

if (!file.exists(file.path("Output", species, paste0(method, "_model_", i, ".csv")))) {
    
  # begin with the original feature data but just the non-test IDs
  data1 <- fread(file.path("Data", species, "Nothing_Feature_data.csv"))
  other_data <- data1 %>% dplyr::filter(!ID %in% test_IDs) 
  
  # first step is to clean out the useless features
  # needed to do this because the SMOTE cant handle NA values later on
  clean_cols <- removeBadFeatures(other_data, var_threshold = 0.3, corr_threshold = 0.9)
  other_data <- other_data %>%
    select(c(!!!syms(clean_cols), "Activity", "ID", "Time")) %>% 
    na.omit()
  
  # pull out the minority classes
  minorities <- other_data %>%
    count(Activity) %>%
    mutate(mean = round(mean(n)),0) %>%
    dplyr::filter(n < mean) %>%
    pull(Activity)
  
  minority_data <- other_data %>% dplyr::filter(Activity %in% minorities)
  
  # what value of K can I select
  K_min <- minority_data %>%
      count(Activity) %>%
      summarise(min = min(n)-1) %>%
      pull(min)
    
  # select numeric feature data
  minority_numeric <- minority_data %>% select(-Activity, -Time, -ID)
  
  # SMOTE the minority classes
  dat_boosted <- smotefamily::SMOTE(X = minority_numeric,
                                    target = minority_data$Activity,
                                    K = K_min,
                                    dup_size = 100
  )
  
  # add in faux metadata so it gels with the rest of my code
  dat_boosted <- dat_boosted$syn_data
  dat_boosted$ID <- "Synth"
  dat_boosted$Time <- NA
  setnames(dat_boosted, "class", "Activity") # update the name of the columm
  
  # add this back to the data
  other_data <- rbind(dat_boosted,
                other_data)
  
  # now generate the model
  RF_model <- design_build_RF(other_data)  
  saveRDS(RF_model, file.path("Output", species, paste0(method, "_model_", i, ".rds")))
  
} else {
  print("model already made for this round")
}