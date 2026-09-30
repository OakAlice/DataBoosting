# All the configirations and settings for this analysis -------------------

pacman::p_load(
  tidyverse, 
  data.table,
  caret,
  future,
  future.apply,
  tsfeatures,
  rBayesianOptimization,
  ranger,
  smotefamily,
  dtwclust
)

source("Scripts/PerformanceTestingFunctions.R")
source("Scripts/ModelDesign_Functions.R")
source("Scripts/GenerateFeatures_Functions.R")

set.seed(1000)

settings <- fread("Data/DatasetCharacteristics.csv")

boost_methods <- c("Nothing", 
                   "Oversample", 
                   "Overlap50"
                   # "Overlap90", 
                   # "SMOTE",
                   # "ADSYN",
                   # "Rotation",
                   # "Decomposition",
                   # "GAN"
)

# all_species <- basename(list.dirs("Data", recursive = F))
all_species <- c("Studd_Squirrel", "Ladds_Seal", "Dunford_Cat", "HarveyCaroll_Pangolin", "Yu_Duck", "Sparkes_Koala")

# important variables
metadata_cols <- c("Activity", "ID", "Time")
target_col <- "Activity"

# File directory ----------------------------------------------------------
# for each of the defined species check data exists and create an output folder
lapply(all_species, function(species){
  
  # check whether the data exists
  
    # code here
  
  # create the output directory
  if (!dir.exists(paste0("Output/", species))){
    dir.create(paste0("Output/", species))
  }
  
  # and within that, a subfolder for the boosted data samples
  # if(!dir.exists(file.path("Output", species, "Boost"))){
  #   dir.create(file.path("Output", species, "Boost"))
  # }
  
  print("all good")
})


# Plotting ----------------------------------------------------------------
my_theme <- function() {
  theme_minimal(base_size = 10, base_family = "serif") +
    theme(
      panel.border = element_rect(color = "black", linewidth = 1.5, fill = NA),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background  = element_rect(fill = "transparent", colour = NA),
      panel.background = element_rect(fill = "transparent", colour = NA),
      axis.line = element_line(color = "black"),
      axis.ticks = element_line(color = "black"),
      axis.title = element_text(size = 20),
      axis.text = element_text(size = 10) 
    )
}
fave_colours <- c(
  "coral", "goldenrod2", "khaki2", 
  "aquamarine3", "turquoise2", "powderblue", "deepskyblue3", 
  "plum", "lightpink1", "lavenderblush2")


more_colours <- c(
  "firebrick3", "tomato", "coral", "darkorange2", "sienna3", "tan2",
  "goldenrod2", "lemonchiffon2", "khaki2", "gold1",
  "springgreen3", "darkseagreen4", "seagreen", "olivedrab4", "darkcyan", "aquamarine3", "palegreen3",
  "skyblue4", "deepskyblue3", "cornflowerblue", "powderblue", "royalblue4", "slateblue2", "lightslateblue", "lightcyan4",
  "purple4", "mediumpurple3", "mediumorchid", "plum", "orchid3", "thistle",
  "deeppink3", "hotpink", "lightpink1", "rosybrown1",
  "mistyrose3", "lavenderblush2", "seashell2"
)






# Delete all files from one or more conditions ----------------------------
# old_files <- list.files(file.path("Output"), full.names = TRUE, pattern = "ADSYN_", recursive = TRUE)
# keep_files <- list.files(file.path(base_path, "Data"), pattern = "Formatted_raw_data.csv", full.names = TRUE, recursive = TRUE)
# file.remove(old_files)

