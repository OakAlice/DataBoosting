# Script for doing nothing ------------------------------------------------

# generate the features with no extra steps
data1 <- fread(file.path("Data", species, "Formatted_raw_data.csv"))
overlap_setting <- 0
source("Scripts/GenerateFeatures.R")

