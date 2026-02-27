# Main script for data boosting chapter -----------------------------------

# Set up ------------------------------------------------------------------
# setwd("C:/Users/oaw001/OneDrive - University of the Sunshine Coast/DataBoosting")
setwd("C:/Users/PC/OneDrive - University of the Sunshine Coast/DataBoosting")

set.seed(1000)

pacman::p_load(
  tidyverse, 
  data.table,
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


sample_rates <- list(Galea_Cat = 50, # could find a better way to store this, maybe read from csv...?
                     Dunford_Cat = 40,
                     Jeantet_Turtle = 20,
                     Ladds_Seal = 25,
                     Maekawa_Gull = 25,
                     Smit_Cat = 30,
                     Studd_Squirrel = 1,
                     Desantis_Rattlesnake = 1, 
                     Vehkaoja_Dog = 100,
                     Yu_Duck = 25,
                     HarveyCaroll_Pangolin = 50,
                     Pagano_Bear = 16,
                     Sparkes_Koala = 50,
                     Mauny_Goat = 5,
                     Clemente_Echidna = 10
                     )

boost_methods <- c("Nothing", 
                   "Oversample", 
                   # "Overlap50", 
                   # "Overlap90", 
                   "SMOTE"
                   )

# Running over each of the datasets ---------------------------------------
# all_species <- basename(list.dirs("Data", recursive = F))
all_species <- c("Studd_Squirrel", "Ladds_Seal", "Dunford_Cat", "HarveyCaroll_Pangolin", "Yu_Duck", "Sparkes_Koala")
for (species in all_species){
  
  # create the output directory
  if (!dir.exists(paste0("Output/", species))){
    dir.create(paste0("Output/", species))
  }
  
  # Generate the features without anything
  data1 <- fread(file.path("Data", species, "Formatted_raw_data.csv"))
  source(paste0("Scripts/BoostMethods/Nothing.R"))
  
  # repeating the entire process 3 times, so need to split out the test data
  data <- fread(file.path("Data", species, "Nothing_Feature_data.csv"))
  unique_IDs <- unique(data$ID)
  ID_groups <- data.frame(
    ID = unique_IDs,
    group = sample(rep(1:3, length.out = length(unique_IDs)))
  )
  
  for (method in boost_methods){
    print(method)
    
    for (i in 1:3){
      print(i)
      # define the test IDs for this round
      test_IDs <- ID_groups$ID[ID_groups$group == i]
      
      # create a boosted model
      source(paste0("Scripts/BoostMethods/", method, ".R"))
      
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
  dat
})
result_df <- rbindlist(results_df)

# plot the results
ggplot(result_df %>% dplyr::filter(Behaviour == "WeightedOverall"),
       aes(x = method, y = F1, colour = as.factor(method))) +
  geom_point(size = 2) +
  my_theme() +
  scale_colour_manual(values = fave_colours) +
  facet_wrap(~Species)
  

