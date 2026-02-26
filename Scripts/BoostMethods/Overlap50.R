# regenerating the features, but this time with overlap

# generate the features but with the different overlap
data1 <- fread(file.path("Data", species, "Formatted_raw_data.csv"))
overlap_setting <- 50
source("Scripts/GenerateFeatures.R")

