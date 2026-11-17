# In this case study I will try to salvage the minority class in real data --------

species <- "Clemente_Impala"

# Generate case study data -----------------------------------------------
if(!file.exists(file.path("CaseStudy", species, "Feature_labelled_data.csv"))){
  labelled_data <- fread(file.path("CaseStudy", species, "Formatted_labelled_data.csv"))
  feature_data <- generate_features_master(labelled_data, sample_rate = 50, overlap_setting = 0, window_setting = 2)
  feature_data <- feature_data$features
  fwrite(feature_data, file.path("CaseStudy", species, "Feature_labelled_data.csv"))
} else {
  labelled_data <- fread(file.path("CaseStudy", species, "Formatted_labelled_data.csv"))
  feature_data <- fread(file.path("CaseStudy", species, "Feature_labelled_data.csv"))
}

# Explore the minority class ----------------------------------------------
class_counts <- feature_data %>% dplyr::count(ID, Activity)
ggplot(class_counts, aes(x = Activity, y = n, fill = as.factor(ID))) +
  geom_col(position = "stack") +
  scale_fill_manual(values = more_colours) + 
  labs(x = NULL, y = "Count", fill = "ID") +
  my_theme() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# pull the smallest class that still contains more than 3 individuals
minority_class <- feature_data %>%
  group_by(Activity) %>%
  summarise(n = n(), n_IDs = n_distinct(ID)) %>%
  dplyr::filter(n_IDs > 3) %>%
  arrange(n) %>%
  first() %>% 
  pull(Activity)

# alternatively, define it manually....
minority_class <- "Locomotion_Walk"

target_count <- feature_data %>% dplyr::count(Activity) %>% 
  summarise(mean = ceiling(mean(n))) %>% pull(mean)

# Augment the minority class ----------------------------------------------
# nothing
minority_nothing <- feature_data

# overlap
minority_overlap <- boost_training_data(
  raw_data = labelled_data,
  feature_data = feature_data,
  metadata_cols, 
  target_class = minority_class, 
  target_count = target_count,
  method = "Overlap90",
  Hz = 50
)
minority_overlap <- minority_overlap$boosted_data

# SMOTE
minority_SMOTE <- boost_training_data(
  raw_data = labelled_data,
  feature_data = feature_data,
  metadata_cols, 
  target_class = minority_class, 
  target_count = target_count,
  method = "SMOTE",
  Hz = 50
)

# make a dictionary
data_dict <- list("Nothing" = minority_nothing,
                  "Overlap" = minority_overlap,
                  "SMOTE" = minority_SMOTE)

# Prepare the unlabelled data ---------------------------------------------
if (!file.exists(file.path("CaseStudy", species, "Feature_unlabelled_data.csv"))){
  unlabelled_data <- fread(file.path("CaseStudy", species, "Formatted_unlabelled_data.csv"))
  unlab_feature_data <- generate_features_master(unlabelled_data, sample_rate = 50, overlap_setting = 0, window_setting = 2)
  unlab_feature_data <- unlab_feature_data$features
  fwrite(unlab_feature_data, file.path("CaseStudy", species, "Feature_unlabelled_data.csv"))
} else {
  unlab_feature_data <- fread(file.path("CaseStudy", species, "Feature_unlabelled_data.csv"))
}

# Build models from each of them ------------------------------------------
for (method in c("Nothing", "Overlap", "SMOTE")){
  print(method)
  
  if(file.exists(file.path("CaseStudy", species, paste0(method, "_predictions.csv")))){
    next
  }
  
  # extract
  data <- data_dict[[method]]
  
  if (method == "SMOTE"){
    data <- data$boosted_data
  }
  
  # tune
  if (!file.exists(file.path("CaseStudy", species, paste0(method, "_model.rds")))){
    output <- design_build_RF(data = data, weighted = FALSE, metadata_cols)
    
    message("Error: ", output$RF_model$prediction.error)
    
    saveRDS(output$RF_model, file.path("CaseStudy", species, paste0(method, "_model.rds")))
  } else {
    readRDS(file.path("CaseStudy", species, paste0(method, "_model.rds")))
  }
  
  # apply to the unlabelled data
  predictions <- predict(output$RF_model, data = unlab_feature_data)
  probabilities <- predictions$predictions
  predicted_classes <- factor(colnames(probabilities)[max.col(probabilities, ties.method = "first")],
                              levels = colnames(probabilities))
  metadata <- unlab_feature_data[, c("ID", "Time")]
  metadata <- cbind(metadata, predicted_classes)
  
  fwrite(metadata, file.path("CaseStudy", species, paste0(method, "_predictions.csv")))
  
}


# Compare the results -----------------------------------------------------
# Make behavioural budgets with the three methods
prediction_files <- list.files(paste0("CaseStudy/", species), pattern = "_predictions.csv", full.names = TRUE, recursive = TRUE)
predictions <- lapply(prediction_files, function(x){
  fread(x) %>% mutate(method = str_split(basename(x), "_", simplify = TRUE)[1])
})
predictions <- bind_rows(predictions)

per_second <- predictions %>%
  group_by(method) %>%
  arrange(Time, .by_group = TRUE) %>%
  ungroup()
budget <- bind_rows(predictions) %>%
  mutate(bins = floor_date(Time, "1 hour")) %>%
  dplyr::count(method, ID, bins, predicted_classes) %>%
  group_by(method, ID, bins) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

# define the colours specially so the non targets are desaturated
class_levels <- sort(unique(per_second$predicted_classes))   # or levels(...) if a factor
named_cols <- setNames(more_colours[seq_along(class_levels)], class_levels)
make_cols <- function(all_cols, minority_class, amount = 0.7, lighten = 0.3) {
  dull <- desaturate(lighten(all_cols, lighten), amount)  # muted, paler
  names(dull) <- names(all_cols)
  dull[minority_class] <- all_cols[minority_class]
  dull
}
cols <- make_cols(named_cols, minority_class)

# make the plots
per_second_plot <- per_second %>% dplyr::filter(Time > as.POSIXct("2024-06-29 21:00:00", tz = "UTC"),
                                        Time < as.POSIXct("2024-06-29 23:00:00", tz = "UTC"))
ggplot(per_second_plot, aes(x = Time, y = 1, fill = predicted_classes)) + 
  geom_tile() + 
  scale_fill_manual(values = more_colours) + 
  my_theme() + 
  facet_wrap(~method) + 
  theme(axis.text.y = element_blank(), axis.ticks = element_blank(), axis.title.y = element_blank())


budget_plot <- budget %>% dplyr::filter(bins > as.POSIXct("2024-07-10 00:00:00", tz = "UTC"),
                                   bins < as.POSIXct("2024-07-11 00:00:00", tz = "UTC"))
ggplot(budget_plot, aes(x = bins, y = prop, fill = predicted_classes)) + 
  geom_col(position = "stack") +
  scale_fill_manual(values = fave_colours) + 
  labs(x = NULL, y = "Proportion", fill = "Activity") +
  my_theme() + 
  facet_wrap(~method)








# Bonus helpers and stuff -------------------------------------------------
# temporary stuff to be deleted

dats <- list.files(file.path("CaseStudy", "DiCicco_Perentie", "unlabelled"), full.names = TRUE)
data <- lapply(dats, function(x){
  df <- fread(x)
  colnames(df) <- c("Time", "X", "Y", "Z")
  df$ID <- tools::file_path_sans_ext(basename(x))
  df
})
data  <- rbindlist(data)
fwrite(data, file.path("CaseStudy", "Formatted_unlabelled_data.csv"))


df <- fread(file.path("CaseStudy", "Clemente_Impala", "Collar_10_Female.csv"))
colnames(df) <- c("Time", "X", "Y", "Z")
df$Time <- as.POSIXct((df$Time - 719529)*86400, origin = "1970-01-01", tz = "UTC")
df$ID <- "Collar_10"
fwrite(df, file.path("CaseStudy", "Clemente_Impala", "Formatted_unlabelled_data.csv"))


