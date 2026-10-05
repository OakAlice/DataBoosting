# Main script for data boosting chapter -----------------------------------

# Set up ------------------------------------------------------------------
# setwd("C:/Users/oaw001/OneDrive - University of the Sunshine Coast/04.DataBoosting")
setwd("C:/Users/PC/OneDrive - University of the Sunshine Coast/04.DataBoosting")
source("Scripts/config.R") # file with all the config infomation for the analysis


all_species <- "Dunford_Cat"
species <- "Dunford_Cat"
boost_methods <- c("SMOTE", "ADSYN", "RandomNoise", "Oversample")


# All trials --------------------------------------------------------------
for (species in all_species) run_species(species, settings, boost_methods, k = 3)
# everything saved to the data and output folders


# Generate synthetic data -------------------------------------------------
grid <- expand.grid(method = boost_methods, species = all_species, stringsAsFactors = FALSE)
for (j in seq_len(nrow(grid))) {
  message(grid$species[j], " | ", grid$method[j])
  
  raw_data <- fread(file.path("Data", species, "Relabelled_raw_data.csv"))
  feature_data <- fread(file.path("Data", species, "Nothing_Feature_data.csv"))
  
  boosted <- boost_training_data(
    raw_data,
    metadata_cols = c("ID", "Time", "Activity"), 
    target_col = "Activity",
    minority_classes = "target",
    method           = grid$method[j],
    Hz               = cfg$Hz,
    prior_features   = feature_data
  )
    
  synth <- boosted$synth_features
  fwrite(synth, file.path("Data", species, "Boost", paste0(species, "_", grid$method[j], ".csv")))
}


# Plots -------------------------------------------------------------------
feature_data <- fread(file.path("Data", species, "Nothing_Feature_data.csv"))
synths  <- list.files(file.path("Data", species, "Boost"), full.names = TRUE)
boosted <- lapply(synths, fread)
names(boosted) <- word(basename(tools::file_path_sans_ext(synths)), 3, sep = "_")

p <- plot_boosted_facets(feature_data, boosted,
                      target_col = "Activity",
                      metadata_cols = c("ID", "Time", "Activity", "Method"),
                      n_neighbors = 15, min_dist = 0.1, seed = 1000,
                      ncol = 3)

ggsave(file.path("Output", species, "Figures", paste0(species, "_vis.png")), 
       p, width = 18, height = 11, dpi = 300)



# Analysis ----------------------------------------------------------------
# just very basic for now
result_files <- list.files("Output", pattern = "_performance_", recursive = TRUE, full.names = TRUE)
results_df <- lapply(result_files, function(x){
  dat <- fread(x)
  dat$Species <- basename(dirname(x))
  dat$fold <- str_split(basename(tools::file_path_sans_ext(x)), "_", simplify = TRUE)[[3]]
  dat$method<- str_split(basename(tools::file_path_sans_ext(x)), "_", simplify = TRUE)[[1]]
  dat$Behaviour <- gsub("Class: ", "", dat$Behaviour)
  dat
})
result_df <- rbindlist(results_df, fill = TRUE)

result_df$method <- factor(result_df$method, levels = c(
  "Nothing", "Oversample","Overlap50", "Overlap90", "RandomNoise", "SMOTE", "ADSYN",  "DTW"
))

summary_df <- result_df %>%
  group_by(Species, Behaviour, method) %>%
  summarise(
    n = n(),
    chance = mean(Prevalence, na.rm = TRUE),
    mean = mean(AUC_PR, na.rm = TRUE),
    se = sd(AUC_PR, na.rm = TRUE) / sqrt(n),
    ci = 1.96 * se, # gives us the confidence intervals (I think??)
    .groups = "drop"
  )

ggplot(summary_df,
       aes(x = method,
           y = mean,
           colour = method,
           group = Species)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = mean - ci,
                    ymax = mean + ci),
                linewidth = 1,
                width = 0.5) +
  geom_hline(data = summary_df, aes(yintercept = chance),
             linetype = "dashed", colour = "grey30", alpha = 0.7) +
  labs(x = "Boosting Method",
       y = "Mean AUC-PR") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none") + 
  my_theme() + 
  scale_colour_manual(values = fave_colours) +
  facet_wrap(~Species, ncol = 3)

