# Main script for data boosting chapter -----------------------------------

# Set up ------------------------------------------------------------------
# setwd("C:/Users/oaw001/OneDrive - University of the Sunshine Coast/DataBoosting")
setwd("C:/Users/PC/OneDrive - University of the Sunshine Coast/04.DataBoosting")
source("Scripts/config.R") # file with all the config infomation for the analysis


# Running over each of the datasets ---------------------------------------
for (species in all_species){
  
  # species <- "Dunford_Cat"
  
  # create the save paths 
  data_path <- file.path("Data", species)
  output_path <- file.path("Output", species)
  
  # relabel the data to binary (target vs other)
  if (!file.exists(file.path(data_path, "Relabelled_raw_data.csv"))){
    # Generate the base features
    raw_data <- fread(file.path(data_path, "Formatted_raw_data.csv"))
    
    # select the minority class that I want to investigate...
    target <- settings$minorityTarget[settings$species == species]
    raw_data$Activity <- ifelse(data1$Activity == target, "target", "other")
    # and save this again
    fwrite(raw_data, file.path(data_path, "Relabelled_raw_data.csv"))
  } else {
    print("already relabelled")
    raw_data <- fread(file.path(data_path, "Relabelled_raw_data.csv"))
  }

  # now make the features # this has to happpen here for the test data to exist
  if (file.exists(file.path(data_path, "Nothing_Feature_data.csv"))){
    print("basic features already generated")
    feature_data <- fread(file.path(data_path, "Nothing_Feature_data.csv"))
  } else { 
    generated_features_df <- generate_features_master(data1, 
                                                      sample_rate = settings$Hz[settings$species == species], 
                                                      overlap_setting = 0, 
                                                      window_setting = ifelse(settings$Hz[settings$species == species] > 1, 2, 5))
    fwrite(generated_features_df, file.path(data_path, "Nothing_Feature_data.csv"))
    feature_data <- generated_features_df
  }
  
  # repeating the entire process 3 times, so need to split out the test data
  unique_IDs <- unique(feature_data$ID)
  ID_groups <- data.frame(
    ID = unique_IDs,
    group = sample(rep(1:3, length.out = length(unique_IDs)))
  )
  
  for (method in boost_methods){
    # method <- "DTW"
    print(method)
    for (i in 1:3){
      print(i)
      # define the test IDs for this round and subset out the data
      test_IDs <- ID_groups$ID[ID_groups$group == i]
      test_data <- feature_data %>% dplyr::filter(ID %in% test_IDs)
      
      # now boost the training samples
      training_data <- raw_data %>% dplyr::filter(!ID %in% test_IDs)
      boosted_data <- boost_training_data(training_data, 
                                          metadata_cols, 
                                          target_col, 
                                          minority_classes = c("target"), 
                                          method, 
                                          Hz = settings$Hz[settings$species == species], 
                                          # next things are if the base features have already been ganeerated
                                          path_to_features = file.path(data_path, "Nothing_Feature_data.csv"), 
                                          test_ids = test_IDs)
      
      # build a model with the boosted training data
      RF_model <- design_build_RF(boosted_data, type, metadata_cols)  
      
      # test this boosted model on the unchanged test data
      metrics <- test_model(type = "binary", RF_model, test_data, metadata_cols, target_col)
      
      # save the performance output
      fwrite(metrics$metrics, file.path("Output", species, paste0(method, "_performance_", i, ".csv")))
      
    }
  }
  
}

# Analysis ----------------------------------------------------------------
# just very basic for now
# collect all the results together
result_files <- list.files("Output", pattern = "_performance_", recursive = TRUE, full.names = TRUE)
results_df <- lapply(result_files, function(x){
  dat <- fread(x)
  dat$Species <- basename(dirname(x))
  dat$fold <- str_split(basename(tools::file_path_sans_ext(x)), "_", simplify = TRUE)[[3]]
  dat$method<- str_split(basename(tools::file_path_sans_ext(x)), "_", simplify = TRUE)[[1]]
  dat$Behaviour <- gsub("Class: ", "", dat$Behaviour)
  dat
})
result_df <- rbindlist(results_df)

result_df$method <- factor(result_df$method, levels = c(
  "Nothing", "Overlap50", "Overlap90", "Oversample", "SMOTE", "ADSYN"
))

summary_df <- result_df %>%
  group_by(Species, Behaviour, method) %>%
  summarise(
    n = n(),
    mean = mean(F1, na.rm = TRUE),
    se = sd(F1, na.rm = TRUE) / sqrt(n),
    ci = 1.96 * se, # gives us the confidence intervals (I think??)
    .groups = "drop"
  )

ggplot(summary_df,
       aes(x = method,
           y = mean,
           colour = Behaviour)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - ci,
                    ymax = mean + ci),
                width = 0.5) +
  labs(x = "Boosting Method",
       y = "Mean F1",
       colour = "Behaviour") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + 
  my_theme() + 
  scale_colour_manual(values = more_colours) +
  facet_wrap(~Species, ncol = 3)

