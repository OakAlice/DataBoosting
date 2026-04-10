# Method for Adaptive oversampling ----------------------------------------

#| Like SMOTE but with noise injection
#| Extra randomness prevents it from overfitting
#| Also will focus on sampling from where its needed most (harder edge cases)
#| However, it will only do one class at a time. Therefore, we need to loop over them
 
if (!file.exists(file.path("Output", species, paste0(method, "_model_", i, ".rds")))) {
  
  data1 <- fread(file.path("Data", species, "Nothing_Feature_data.csv"))
  other_data <- data1 %>% dplyr::filter(!ID %in% test_IDs) 
  
  # again, get rid of the NA columns though
  clean_cols <- removeBadFeatures(other_data, var_threshold = 0.3, corr_threshold = 0.9)
  other_data <- other_data %>%
    select(c(!!!syms(clean_cols), "Activity", "ID", "Time")) %>% 
    na.omit()
  
  # find the minority classes
  aim <- other_data %>%
    count(Activity) %>%
    summarise(mean = round(mean(n))) %>%
    pull(mean)
  
  minorities <- other_data %>%
    count(Activity) %>%
    dplyr::filter(n < aim) %>%
    pull(Activity)
  
  synth_list <- lapply(minorities, function(minority_class) {
    
    # recode target as binary: this class vs everything else
    binary_target <- ifelse(other_data$Activity == minority_class, 
                            minority_class, "majority")
    
    all_numeric <- other_data %>% select(-Activity, -Time, -ID)
    
    # how many can we sample
    K_chosen <- min(nrow(other_data %>% filter(Activity == minority_class)) - 1, 5)
    if (K_chosen < 1) print("K only 1, too small")
    
    dat_boosted <- tryCatch(
      smotefamily::ADAS(X = all_numeric,
                        target = binary_target,
                        K = K_chosen),
      error = function(e) {
        message("ADAS failed for '", minority_class, "': ", e$message)
        NULL
      }
    )
    
    if (is.null(dat_boosted)) return(NULL)
    
    synth_data <- dat_boosted$syn_data %>% 
      dplyr::filter(class == minority_class)
    if (nrow(synth_data) == 0) return(NULL)
    
    n_needed <- aim - sum(other_data$Activity == minority_class)
    synth_data <- synth_data %>% slice_sample(n = min(n_needed, nrow(synth_data)))
    
    synth_data$ID <- "Synth"
    synth_data$Time <- NA
    setnames(synth_data, "class", "Activity")
    
    return(synth_data)
  })
  
  synth_list <- Filter(Negate(is.null), synth_list)
  synth_list <- rbindlist(synth_list, fill = TRUE)
  other_data <- rbind(other_data, synth_list)
  
  # save this data so I can visualise it later
  fwrite(other_data, file.path("Data", species, paste0(method, "_", i, "_Feature_data.csv")))
  
  RF_model <- design_build_RF(other_data)    