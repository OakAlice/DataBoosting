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
  dtwclust,
  matrixStats,
  uwot,
  cluster,
  glmmTMB,
  marginaleffects
)

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
  "firebrick3", "coral", "goldenrod2", "khaki2", 
  "aquamarine3", "darkcyan", "turquoise2", "powderblue", "deepskyblue3", "slateblue2",
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



# Functions
source("Scripts/helpers.R")
source("Scripts/run_experiment.R")
source("Scripts/generate_features.R")
source("Scripts/boost_training_data.R")
source("Scripts/model_optimisation.R")
source("Scripts/performance_testing.R")
source("Scripts/silhouette.R")
source("Scripts/visualise_features.R")
source("Scripts/analysis.R")

# variables
set.seed(1000)

settings <- fread("Data/DatasetCharacteristics.csv")

boost_methods <- c("Nothing",
                   "Oversample",
                   "Overlap50",
                   "Overlap90",
                   "RandomNoise",
                   "Rotation",
                   "SMOTE",
                   "ADSYN",
                   "DTW",
                   "Decomposition"
)

all_species <- basename(list.dirs("Data", recursive = F))

available_axes <- c("X", "Y", "Z")
metadata_cols <- c("Activity", "ID", "Time")
target_col <- "Activity"
type <- "binary"

# File directory ----------------------------------------------------------
# for each of the defined species check data exists and create an output folder
lapply(all_species, function(species){
  
  # create the output directory
  if (!dir.exists(paste0("Output/", species))){
    dir.create(paste0("Output/", species))
  }
  
  # create where I save the synthetic data
  if (!dir.exists(paste0("Data/", species, "/Boost"))){
    dir.create(paste0("Data/", species, "/Boost"))
  }
  
  # and the figures
  if (!dir.exists(paste0("Output/", species, "/Figures"))){
    dir.create(paste0("Output/", species, "/Figures"))
  }

  print("all good")
})





# Delete all files from one or more conditions ----------------------------
# old_files <- list.files(file.path("Output"), full.names = TRUE, pattern = "ADSYN_", recursive = TRUE)
# keep_files <- list.files(file.path(base_path, "Data"), pattern = "Formatted_raw_data.csv", full.names = TRUE, recursive = TRUE)
# file.remove(old_files)

