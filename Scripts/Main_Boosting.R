# Main script for data boosting chapter -----------------------------------

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
  ranger,
  smotefamily
)

source("Scripts/PerformanceTestingFunctions.R")
source("Scripts/PlotFunctions.R")
source("Scripts/ModelDesign_Functions.R")
source("Scripts/GenerateFeatures_Functions.R")

sample_rates <- fread("Data/DatasetCharacteristics.csv")

boost_methods <- c("Nothing", 
                   "Oversample", 
                   # "Overlap50",
                   # "Overlap90", 
                   "SMOTE"
                   # "ADSYN"
                   )


# Delete all files from one or more conditions ----------------------------
# old_files <- list.files(file.path("Output"), full.names = TRUE, pattern = "ADSYN_", recursive = TRUE)
# keep_files <- list.files(file.path(base_path, "Data"), pattern = "Formatted_raw_data.csv", full.names = TRUE, recursive = TRUE)
# file.remove(old_files)

# Running over each of the datasets ---------------------------------------
# all_species <- basename(list.dirs("Data", recursive = F))
all_species <- c("Studd_Squirrel", "Ladds_Seal", "Dunford_Cat", "HarveyCaroll_Pangolin", "Yu_Duck", "Sparkes_Koala")
for (species in all_species){
  
  # species <- "Dunford_Cat"
  
  # create the output directory
  if (!dir.exists(paste0("Output/", species))){
    dir.create(paste0("Output/", species))
  }
  
  # Generate the features without anything
  data1 <- fread(file.path("Data", species, "Formatted_raw_data.csv"))
  if (file.exists(file.path("Data", species, "Nothing_Feature_data.csv"))){
    print("basic features already generated")
  } else { 
    data <- fread(file.path("Data", species, "Formatted_raw_data.csv"))
    generated_features_df <- generate_features_master(data, 
                                                      sample_rate = sample_rates[[species]], 
                                                      overlap_setting = 0, 
                                                      window_setting = ifelse(sample_rates[[species]] > 1, 2, 5))
    fwrite(generated_features_df, file.path("Data", species, "Nothing_Feature_data.csv"))
  }
  
  # repeating the entire process 3 times, so need to split out the test data
  data <- fread(file.path("Data", species, "Nothing_Feature_data.csv"))
  unique_IDs <- unique(data$ID)
  ID_groups <- data.frame(
    ID = unique_IDs,
    group = sample(rep(1:3, length.out = length(unique_IDs)))
  )
  
  # TODO: Change over all the save paths to be saved to the boosting output
  if(!dir.exists(file.path("Output", species, "Boost"))){
    dir.create(file.path("Output", species, "Boost"))
  }
  
  for (method in boost_methods){
    print(method)
    
    for (i in 1:3){
      print(i)
      # define the test IDs for this round
      test_IDs <- ID_groups$ID[ID_groups$group == i]
      
      # create a boosted model
      source(paste0("Scripts/BoostMethods/", method, ".R"))
      
      # visualise the feature space
      source("Scripts/VisualiseFeatureSpace.R")
      
      # now test the model
      test_data <- data %>% dplyr::filter(ID %in% test_IDs)
      source("Scripts/TestModel.R")
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

