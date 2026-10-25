# Quick and dirty analysis of the results ---------------------------------
# very preliminary at this stage


# Load in the files -------------------------------------------------------
metrics <- list.files("Output", recursive = TRUE, full.names = TRUE)

# pulling out the majority files
minority_files <- metrics[grepl("/Minority/", metrics)]
majority_files <- metrics[grepl("/Majority/", metrics)]

# Majority ----------------------------------------------------------------
majority <- rbindlist(lapply(majority_files, fread), fill = TRUE)
result_df <- merge(majority, target_classes, by.x = c("Species", "class"), by.y = c("Species", "Activity"), all.x = TRUE)

summary <- result_df %>%
  group_by(Species, method, class, prop.x, type) %>%
  summarise(
    n = n(),
    mean_F1 = mean(F1, na.rm = TRUE),
    se_F1   = sd(F1, na.rm = TRUE) / sqrt(n),
    ci_F1   = 1.96 * se_F1,
    .groups = "drop"
  )
summary <- summary %>% dplyr::filter(type == "Majority") %>%
  mutate(method = factor(method, levels = c(boost_methods)))

# and representing the data slightly differently (as relative improvement)
types <- result_df %>% select(Species, class, type) %>% unique()
baseline <- result_df %>%
  filter(method == "Nothing") %>%
  select(Species, class, fold, prop.x, baseline = F1) %>% ungroup() %>%
  left_join(types, by = c("class", "Species")) %>%
  dplyr::filter(type == "Majority")

# and then results relative to the control
result_df2 <- result_df %>%
  dplyr::filter(type == "Majority") %>%
  left_join(baseline, by = c("Species", "class", "fold", "prop.x")) %>%
  mutate(change = F1 - baseline,
         sp_rep = str_c(Species, "_", fold),
         across(where(is.character), as.factor),
         relative_improvement = (change)/baseline) %>%
  select(Species, prop.x, fold, method, train_prev, boost_prev, F1, baseline, change, sp_rep, relative_improvement)

summary2 <- result_df2 %>% group_by(Species, method, prop.x) %>% 
  summarise(
    n = n(),
    mean = mean(change, na.rm = TRUE),
    se   = sd(change, na.rm = TRUE) / sqrt(n),
    ci   = 1.96 * se,
    .groups = "drop"
  )
  
  

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

ggplot(summary2,
       aes(x = prop.x, y = mean, colour = method, group = method)) +
  geom_path(linewidth = 1, alpha = 0.5) +
  geom_point(data = ~ filter(.x, method != "Nothing"), size = 5, alpha = 0.7) +
  geom_errorbar(aes(ymin = mean - ci, ymax = mean + ci),
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

# remove no
df1 <- filter(result_df2, method != "Nothing") %>%  #*** New df without "No"
  mutate(method = fct_drop(method)) # Drop unused factor levels

m1 <- glmmTMB(change ~ method * prop.x + (1|Species:fold), data = df1)
summary(m1) #*** `Mode` is now the base case, and all other estimates are adjustments relative to that. To get adjustments relative to zero, just remove the intercept.
m1.2 <- update(m1, ~. -1) #*** the `-1` removes the intercept
summary(m1.2)

#DHARMa::simulateResiduals(m1.2, plot = TRUE) 

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
       y = "Mean change in F1 (relative to control)") +
  my_theme() + 
  scale_colour_manual(values = fave_colours) +
  theme(legend.position = "none", axis.text.x = element_text(hjust = 1, vjust = 1, angle = 45)) + 
  facet_grid(~prop.x)




# Minority class ----------------------------------------------------------
minority <- rbindlist(lapply(minority_files, fread), fill = TRUE) %>% rename(Activity = Behaviour)
result_df <- merge(minority, target_classes, by = c("Species", "Activity"), all.x = TRUE)

summary <- result_df %>%
  group_by(Species, method, Activity, type) %>%
  summarise(
    mean_F1 = mean(F1, na.rm = TRUE),
    n_obs   = sum(!is.na(F1)),
    se_F1   = sd(F1, na.rm = TRUE) / sqrt(n_obs),
    ci_F1   = 1.96 * se_F1,
    .groups = "drop"
  )

summary <- summary %>% dplyr::filter(type == "Minority") %>%
  mutate(method = factor(method, levels = c(boost_methods)))


types <- result_df %>% select(Species, Activity, type) %>% unique()
baseline <- result_df %>%
  filter(method == "Nothing") %>%
  select(Species, Activity, fold, baseline = F1) %>% ungroup() %>%
  left_join(types, by = c("Activity", "Species"))

# and then results relative to the control
result_df2 <- result_df %>%
  dplyr::filter(type == "Minority") %>%
  left_join(baseline, by = c("Species", "Activity", "fold")) %>%
  mutate(change = F1 - baseline,
         sp_rep = str_c(Species, "_", fold),
         across(where(is.character), as.factor),
         relative_improvement = (change)/baseline) %>%
  select(Species, fold, method, train_prev, boost_prev, F1, baseline, change, sp_rep, relative_improvement)

summary2 <- result_df2 %>% group_by(Species, method) %>% 
  summarise(
    n = n(),
    mean = mean(change, na.rm = TRUE),
    se   = sd(change, na.rm = TRUE) / sqrt(n),
    ci   = 1.96 * se,
    .groups = "drop"
  )

ggplot(summary,
       aes(x = method, y = mean_F1, colour = method)) +
  geom_path(linewidth = 1, alpha = 0.5) +
  geom_point(size = 5, alpha = 0.7) +
  geom_errorbar(aes(ymin = mean_F1 - ci_F1, ymax = mean_F1 + ci_F1),
                linewidth = 1, width = 0.5) +
  labs(x = "Method", y = "F1") +
  my_theme() +
  scale_colour_manual(values = methods_colours) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~Species)





df1 <- filter(result_df2, method != "Nothing") %>%  #*** New df without "No"
  mutate(method = fct_drop(method)) # Drop unused factor levels

m1 <- glmmTMB(change ~ method + (1|Species:fold), data = df1)
summary(m1) #*** `Mode` is now the base case, and all other estimates are adjustments relative to that. To get adjustments relative to zero, just remove the intercept.
m1.2 <- update(m1, ~. -1) #*** the `-1` removes the intercept
summary(m1.2)

# DHARMa::simulateResiduals(m1.2, plot = TRUE) 

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
       y = "Mean change in F1 (relative to control)") +
  my_theme() + 
  scale_colour_manual(values = fave_colours) +
  theme(legend.position = "none", axis.text.x = element_text(hjust = 1, vjust = 1, angle = 45))




