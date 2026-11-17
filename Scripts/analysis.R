# Quick and dirty analysis of the results ---------------------------------
# very preliminary at this stage


boost_methods <- c(
  "Nothing",
  "Weighted",
  "Oversample",
  "Overlap50",
  "Overlap90",
  "RandomNoise",
  "Rotation",
  # "SMOTE_within",
  "SMOTE_between"
  #"DTW",
  #"Decomposition"
)

metrics <- list.files("Output", recursive = TRUE, full.names = TRUE)
majority_files <- metrics[grepl("/Majority_windows/", metrics)]
majority <- rbindlist(lapply(majority_files, fread))

# silhouette_files <- list.files("Output", pattern = "Silhouette_scores", recursive = TRUE, full.names = TRUE)
# sils <- rbindlist(lapply(silhouette_files, fread))

all_behs <- majority %>% dplyr::filter(Activity == Behaviour) 

# add in the baseline and silhouettes
full <- all_behs %>%
  # filter(method != "Nothing") %>%
  left_join(
    all_behs %>%
      filter(method == "Nothing") %>%
      select(Species, class, fold, replicate, prop, baseline = F1),
    by = c("Species", "class", "fold", "replicate", "prop"),
    relationship = "many-to-one"            # many methods per baseline row
  ) %>%
  mutate(change = F1 - baseline,
         relative_improvement = if_else(baseline > 0, change / baseline, NA_real_),
         sp_rep = str_c(Species, fold, sep = "_"),
         across(where(is.character), as.factor))

# full <- merge(full, sils, by = c("Activity", "Species"))

# summarise
summary <- full %>%
  group_by(Species, method, class, prop) %>%
  summarise(n = n(),
            mean_F1 = mean(F1, na.rm = TRUE),
            ci_F1   = 1.96 * sd(F1, na.rm = TRUE) / sqrt(n),
            .groups = "drop") %>%
  mutate(method = factor(method, levels = boost_methods))

summary2 <- full %>%
  group_by(Species, method, class, prop) %>%
  summarise(n = n(),
            mean_change = mean(change, na.rm = TRUE),
            ci_change   = 1.96 * sd(change, na.rm = TRUE) / sqrt(n),
            .groups = "drop") %>%
  mutate(method = factor(method, levels = boost_methods))



summary <- summary %>% dplyr::filter(Species == "Dickinson_Goat")
summary2 <- summary2 %>% dplyr::filter(Species == "Dickinson_Goat")


# graphs
ggplot(summary, aes(factor(prop), mean_F1, colour = method, group = method)) +
  geom_path(linewidth = 1, alpha = 0.5) +
  geom_point(data = ~ filter(.x, method != "Nothing"), size = 5, alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_F1 - ci_F1, ymax = mean_F1 + ci_F1),
                linewidth = 1, width = 0.1) +
  geom_point(data = ~ filter(.x, method == "Nothing"),
             shape = 8, colour = "black", size = 3, stroke = 1) +
  labs(x = "Retained windows", y = "F1") +
  scale_colour_manual(values = methods_colours) +
  facet_wrap(Species~class) +
  my_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
ggplot(summary2, aes(factor(prop), mean_change, colour = method, group = method)) +
  geom_path(linewidth = 1, alpha = 0.5) +
  geom_point(data = ~ filter(.x, method != "Nothing"), size = 5, alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_change - ci_change, ymax = mean_change + ci_change),
                linewidth = 1, width = 0.1) +
  geom_point(data = ~ filter(.x, method == "Nothing"),
             shape = 8, colour = "black", size = 3, stroke = 1) +
  labs(x = "Retained windows", y = "change in F1") +
  scale_colour_manual(values = methods_colours) +
  facet_wrap(Species~class) +
  my_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

full$prop_f <- factor(full$prop)
m1 <- glmmTMB(change ~ method * prop_f + (1 | Species:class) + (1 | sp_rep), data = full)
summary(m1)

# method-level gain, averaged over the behaviours present in the data
group_preds <- marginaleffects::avg_predictions(m1, by = "method", re.form = NA) %>%
  as_tibble() #%>%
  #dplyr::filter(!method == "Nothing") # include as a sanity check


ggplot(group_preds,
       aes(x = method,
           y = estimate,
           colour = method)) +
  geom_errorbar(aes(ymin = conf.low,
                    ymax = conf.high),
                linewidth = 1,
                width = 0.5) +
  geom_point(size = 5) +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             colour = "red") +
  labs(x = "Augmentation method",
       y = "Mean change in F1") +
  my_theme() + 
  scale_colour_manual(values = fave_colours) +
  theme(legend.position = "none")


# adding in more variables
m2 <- glmmTMB(change ~ method * prop_f + sil_ovr + (1 | Species:class) + (1 | sp_rep), data = full)
summary(m2)
