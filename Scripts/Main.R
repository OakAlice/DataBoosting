# Main script for data boosting chapter -----------------------------------

# Set up ------------------------------------------------------------------
# setwd("C:/Users/oaw001/OneDrive - University of the Sunshine Coast/04.DataBoosting")
setwd("C:/Users/PC/OneDrive - University of the Sunshine Coast/04.DataBoosting")
source("Scripts/config.R") # file with all the config infomation for the analysis

# this will generate all the results
for (species in all_species) run_species(species, settings, boost_methods, k = 3)




# make plots
plot <- plot_boosted_data(boosted, 
                          metadata_cols, 
                          n_neighbors = 15, min_dist = 0.1, seed = 42)


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

