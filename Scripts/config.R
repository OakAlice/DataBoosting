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
source("Scripts/choosing_target_class.R")
# source("Scripts/analysis.R")

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
                   "ADSYN" #,
                   #"DTW",
                   #"Decomposition"
)

all_species <- settings$Species

available_axes <- c("X", "Y", "Z")
metadata_cols <- c("Activity", "ID", "Time")
target_col <- "Activity"
type <- "multiclass"

cut_conditions <- c(1, 5, 10, 25, 100)
downsample <- "samples" # IDs

# File directory ----------------------------------------------------------
# for each of the defined species check data exists and create an output folder
for (species in all_species) {
  dirs <- c(file.path("Output", species, c("Binary", "Volume", "Figures")),
            file.path("Data", species, "Boost"))
  for (d in dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
}
