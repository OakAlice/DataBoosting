# Analysis to figure out the shoulder point for data volume ---------------
# How much training data do we need?

# Set up ------------------------------------------------------------------
# setwd("C:/Users/oaw001/OneDrive - University of the Sunshine Coast/DataBoosting")
setwd("C:/Users/PC/OneDrive - University of the Sunshine Coast/DataBoosting")

set.seed(1000)

pacman::p_load(
  tidyverse, 
  data.table,
  caret,
  future,
  future.apply,
  tsfeatures,
  rBayesianOptimization,
  ranger
)

source("Scripts/PerformanceTestingFunctions.R")
source("Scripts/PlotFunctions.R")
source("Scripts/FunctionsModelDesign_Volume.R")
source("Scripts/GenerateFeatures_Functions.R")

dataset_characterstics <- fread("Data/DatasetCharacteristics.csv")

# Running over each of the datasets ---------------------------------------
# all_species <- basename(list.dirs("Data", recursive = F))
all_species <- c("Studd_Squirrel", "Ladds_Seal", "Dunford_Cat", "HarveyCaroll_Pangolin", "Yu_Duck", "Sparkes_Koala")
for (species in all_species){
  
  # species <- "Sparkes_Koala"
  
  # generate the features
  data1 <- fread(file.path("Data", species, "Formatted_raw_data.csv"))
  if (file.exists(file.path("Data", species, "Nothing_Feature_data.csv"))){
    print("basic features already generated")
    feature_data <- fread(file.path("Data", species, "Nothing_Feature_data.csv"))
  } else { 
    data <- fread(file.path("Data", species, "Formatted_raw_data.csv"))
    feature_data <- generate_features_master(data, 
                                             sample_rate = dataset_characterstics$Hz[dataset_characterstics$species == species], # in Hz, 
                                             overlap_setting = 0, 
                                             window_setting = ifelse( dataset_characterstics$Hz[dataset_characterstics$species == species] > 1, 2, 5))
    fwrite(feature_data, file.path("Data", species, "Nothing_Feature_data.csv"))
  }
  
  # create the output directory
  if (!dir.exists(paste0("Output/", species, "/Volume"))){
    dir.create(paste0("Output/", species, "/Volume"))
  }

  # pull out the test data # this will be 20% of all individuals.
  if(!file.exists(file.path("Data", species, "Test_Feature_data.csv"))){
    unique_IDs <- unique(feature_data$ID)
    test_IDs <- sample(unique(feature_data$ID), ceiling(length(unique(feature_data$ID))*0.2))
    test_data <- feature_data %>% dplyr::filter(ID %in% test_IDs)
    fwrite(test_data, file.path("Data", species, "Test_Feature_data.csv"))
    explore_data <- feature_data %>% dplyr::filter(!ID %in% test_IDs)
    fwrite(explore_data, file.path("Data", species, "Explore_Feature_data.csv"))
  } else{
    print("reading in")
    test_data <- fread(file.path("Data", species, "Test_Feature_data.csv"))
    explore_data <- fread(file.path("Data", species, "Explore_Feature_data.csv"))
  }
  
  # generate all the combinations to run
  if(!file.exists(file.path("Data", species, "All_combinations.csv"))){
    ids <- unique(explore_data$ID)
    all_combs <- map_dfr(
      3:length(ids),
      ~ as.data.frame(t(combn(ids, .x)))
    )
    fwrite(all_combs, file.path("Data", species, "All_combinations.csv"))
  } else {
    all_combs <- fread(file.path("Data", species, "All_combinations.csv"))
  }
  
  # now run each of these combinations
  for (r in 1:nrow(all_combs)){
    row_vals <- as.character(all_combs[r, ])
    selected_ids <- as.character(row_vals[!is.na(row_vals) & row_vals != ""])
    selected_data <- explore_data[explore_data$ID %in% selected_ids]
    
    # to avoid hundreds of intermendiate files, I just left the data in the environment
    # do the model
    CV_filename <- file.path("Output", species, "Volume", paste0("trial_", r, "_CV.csv"))
    source("Scripts/Volume_CrossValModel.R")
  }
}







# read in the cross-validation results
output_dirs <- list.dirs("Output", recursive = FALSE, full.names = TRUE)
output_results <- lapply(output_dirs, function(x){
  name <- basename(x)
  output_files <- list.files(file.path(x, "Volume"), recursive = TRUE, full.names = TRUE, pattern = "trial_")
  CVs <- lapply(output_files, function(i){
    dat <- fread(i)
    
    # here's where I'd do soemthign to the data
  })
  CVs <- rbindlist(CVs)
  CVs$species <- name
  CVs
})
output_results <- rbindlist(output_results, fill = TRUE)
output_results <- output_results[!is.na(output_results$Class), ]

fwrite(output_results, "Output/All_performance.csv")




# just the overall
ggplot(output_results %>% dplyr::filter(Class %in% c("NonWeightedOverall")), 
       aes(x = Class, y = F1, fill = as.factor(total_inds))) +
  geom_boxplot() +
  labs(x = "Frequency", y = "F1 Score") +
  my_theme() + 
  scale_fill_manual(values = fave_colours) + 
  facet_wrap(~species)


# now the specific behaviours
ggplot(output_results%>% dplyr::filter(!Class %in% c("WeightedOverall", "NonWeightedOverall")), aes(x = Class, y = F1, fill = as.factor(total_inds))) +
  geom_boxplot() +
  labs(x = "Frequency", y = "F1 Score") +
  my_theme() + 
  scale_fill_manual(values = fave_colours) + 
  facet_wrap(Class~species, scales = "free")

model <- lmer(F1 ~ total_inds + (1 | species), data = results)
summary(model)




