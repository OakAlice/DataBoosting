# Quick and dirty analysis of the results ---------------------------------
# very preliminary at this stage

volume_metrics <- list.files("Output", recursive = TRUE, full.names = TRUE)
volume_metrics <- lapply(volume_metrics, fread)
volume_metrics <- rbindlist(volume_metrics, fill = TRUE) %>% rename(Activity = Behaviour)

result_df <- merge(volume_metrics, target_classes, by = c("Species", "Activity"), all.x = TRUE)

summary <- result_df %>%
  group_by(Species, method, Activity, prop.x, type) %>%
  summarise(
    n = n(),
    
    mean = mean(Norm_AUC_PR, na.rm = TRUE),
    se   = sd(Norm_AUC_PR, na.rm = TRUE) / sqrt(n),
    ci   = 1.96 * se,
    
    prev    = mean(boost_prev, na.rm = TRUE),
    se_prev = sd(boost_prev, na.rm = TRUE) / sqrt(n),
    ci_prev = 1.96 * se_prev,
    
    mean_F1 = mean(F1, na.rm = TRUE),
    se_F1   = sd(F1, na.rm = TRUE) / sqrt(n),
    ci_F1   = 1.96 * se_F1,
    
    .groups = "drop"
  )

summary <- summary %>% dplyr::filter(type == "Majority") %>%
  mutate(method = factor(method, levels = c(boost_methods)))


# Plots -------------------------------------------------------------------
ggplot(summary,
       aes(x = prop.x, y = mean_F1, colour = method, group = method)) +
  geom_path(linewidth = 1, alpha = 0.5) +
  geom_point(data = ~ filter(.x, method != "Nothing"), size = 5, alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_F1 - ci_F1, ymax = mean_F1 + ci_F1),
                linewidth = 1, width = 0.5) +
  geom_point(data = ~ filter(.x, method == "Nothing"),
             shape = 8, colour = "black", size = 3, stroke = 1) +
  labs(x = "Percentage of retained bouts (%)", y = "F1") +
  my_theme() +
  scale_colour_manual(values = methods_colours) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Species)


# And trying do do some stats ---------------------------------------------
# code copied from the postprocessing chapter analysis that dave helped with

# this is the control condition
baseline <- result_df %>%
  filter(method == "Nothing") %>%
  select(Species, fold, prop.x, baseline = F1)

# and then results relative to the control
df <- result_df %>%
  dplyr::filter(Activity == target_class) %>%
  left_join(baseline, by = c("Species", "fold", "prop.x")) %>%
  mutate(relative_change = F1 - baseline,
         sp_rep = str_c(Species, "_", fold), #*** Reusing factor levels in random effects can cause issues, so I create unique factor levels...
         across(where(is.character), as.factor)) #*** I'd make everything that is still character into a factor, too

# remove no
df1 <- filter(df, method != "Nothing") %>%  #*** New df without "No"
  mutate(method = fct_drop(method)) # Drop unused factor levels

#m1 <- glmmTMB(relative_change ~ method + minorityRatio + (1|Species/fold), data = df1)
m1 <- glmmTMB(relative_change ~ method + (1|Species:fold), data = df1)
summary(m1) #*** `Mode` is now the base case, and all other estimates are adjustments relative to that. To get adjustments relative to zero, just remove the intercept.
m1.2 <- update(m1, ~. -1) #*** the `-1` removes the intercept
summary(m1.2)















# just very basic for now
result_files <- list.files("Output", pattern = "_performance_", recursive = TRUE, full.names = TRUE)
results_df <- lapply(result_files, function(x){
  dat <- fread(x)
  dat$Species <- basename(dirname(dirname(x)))
  dat$fold <- str_split(basename(tools::file_path_sans_ext(x)), "_", simplify = TRUE)[[3]]
  dat$method<- str_split(basename(tools::file_path_sans_ext(x)), "_", simplify = TRUE)[[1]]
  dat$Activity <- gsub("Class: ", "", dat$Behaviour)
  dat
})
result_df <- rbindlist(results_df, fill = TRUE)
result_df$method <- factor(result_df$method, levels = c(
  "Nothing", "Oversample","Overlap50", "Overlap90", "Rotation", "RandomNoise",
  "SMOTE", "ADSYN",  "DTW", "Decomposition"))

result_df <- merge(result_df, settings, by = "Species", all.x = TRUE)

# add in the labels for what class is what
result_df <- merge(result_df, target_classes, by = c("Species", "Activity"), all.x = TRUE)





# add in the silhouette scores
# sils <- fread(file.path("Output", "Silhouette_scores.csv")) %>% dplyr::filter(behaviour == "target")
# result_df <- merge(result_df, sils, by = "Species")


# Identify the high performing (ceiling) species
high_performance_species <- result_df %>%
  dplyr::filter(method == "Nothing") %>%
  group_by(Species) %>%
  summarise(mean = mean(AUC_PR, na.rm = TRUE)) %>%
  dplyr::filter(mean > 0.8) %>%
  pull(Species)

summary_df <- result_df %>%
  group_by(Species, Activity, method, type) %>%
  summarise(
    n = n(),
    chance = mean(Prevalence, na.rm = TRUE),
    
    mean_PR = mean(AUC_PR, na.rm = TRUE),
    se_PR = sd(AUC_PR, na.rm = TRUE) / sqrt(n),
    ci_PR = 1.96 * se_PR, # gives us the confidence intervals (I think??)
    
    mean_F1 = mean(F1, na.rm = TRUE),
    se_F1 = sd(F1, na.rm = TRUE) / sqrt(n),
    ci_F1 = 1.96 * se_F1, # gives us the confidence intervals (I think??)
    
    .groups = "drop"
  ) 

plot_df <- summary_df %>% 
  dplyr::filter(type == "Minority")
  #dplyr::filter(!Species %in% high_performance_species)


ggplot(plot_df,
       aes(x = method,
           y = mean_PR,
           colour = method,
           group = Species)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = mean_PR - ci_PR,
                    ymax = mean_PR + ci_PR),
                linewidth = 1,
                width = 0.5) +
  geom_hline(data = plot_df, aes(yintercept = chance),
             linetype = "dashed", colour = "grey30", alpha = 0.7) +
  labs(x = "Boosting Method",
       y = "Mean AUC-PR") +
  my_theme() + 
  scale_colour_manual(values = fave_colours) +
  facet_wrap(~Species, ncol = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")



ggplot(plot_df,
       aes(x = method,
           y = mean_F1,
           colour = method,
           group = Species)) +
  geom_point(size = 5) +
  geom_errorbar(aes(ymin = mean_F1 - ci_F1,
                    ymax = mean_F1 + ci_F1),
                linewidth = 1,
                width = 0.5) +
  geom_hline(data = plot_df, aes(yintercept = chance),
             linetype = "dashed", colour = "grey30", alpha = 0.7) +
  labs(x = "Boosting Method",
       y = "Mean F1") +
  my_theme() + 
  scale_colour_manual(values = fave_colours) +
  facet_wrap(~Species, ncol = 3) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")






dat <- result_df %>%                        # replicate-level scores, not summary_df
  mutate(method = relevel(factor(method), ref = "Nothing"),
         sil_c  = scale(mean_sil, scale = FALSE)[, 1])   # centre for interpretable main effects

m <- glmmTMB(
  AUC_PR ~ method * sil_c + (1 | Species) + (1 | Species),
  family = ordbeta(),                    # bounded 0-1, allows exact 0s and 1s
  data   = dat
)

summary(m)
car::Anova(m, type = "III")








# comments from Dave
baseline <- result_df %>%
  dplyr::filter(!Species %in% high_performance_species) %>% 
  filter(method == "Nothing") %>%
  select(Species, fold, baseline = AUC_PR)
df <- result_df %>%
  left_join(baseline, by = c("Species", "fold")) %>%
  mutate(relative_change = AUC_PR - baseline,
         sp_rep = str_c(Species, "_", fold), #*** Reusing factor levels in random effects can cause issues, so I create unique factor levels...
         smoothing_type = factor(method, levels = c(
           "Nothing", "Oversample","Overlap50", "Overlap90", "Rotation", "RandomNoise", 
           "SMOTE", "ADSYN",  "DTW", "Decomposition")), #*** do this in the code flow...
         across(where(is.character), as.factor)) #*** I'd make everything that is still character into a factor, too

# remove no
df1 <- filter(df, method != "Nothing") %>%  #*** New df without "No"
  mutate(method = fct_drop(method)) # Drop unused factor levels

#m1 <- glmmTMB(relative_change ~ method + minorityRatio + (1|Species/fold), data = df1)
m1 <- glmmTMB(relative_change ~ method + (1|Species:fold), data = df1)
summary(m1) #*** `Mode` is now the base case, and all other estimates are adjustments relative to that. To get adjustments relative to zero, just remove the intercept.
m1.2 <- update(m1, ~. -1) #*** the `-1` removes the intercept
summary(m1.2)

DHARMa::simulateResiduals(m1.2, plot = TRUE) #*** The diagnostics are horrible, at least for Normality...I don't know how we could fix those, though...given the data we have...maybe we just rely on the robustness of the method

#*** If you wanted an overall plot that illustrates the results for the fixed effects averaged over the random effects (mirroring your summary table)
group_preds <- marginaleffects::predictions(m1.2,
                                            re.form = NA) %>%   # Includes fixed + random effects
  as_tibble()

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
       y = "Mean change in AUC-PR (relative to no augmentation)") +
  my_theme() + 
  scale_colour_manual(values = fave_colours) +
  theme(legend.position = "none")


# testing the minority ratio more specifically
m1 <- glmmTMB(relative_change ~ method + minorityRatio + (1|Species:fold), data = df1)
m3 <- glmmTMB(relative_change ~ method * minorityRatio + (1|Species:fold), data = df1)
anova(m1, m3)  # does the slope differ by method?

library(emmeans)
emtrends(m3, ~ method, var = "minorityRatio") |> test(adjust = "holm")




