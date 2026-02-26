# Testing the model on the original data ----------------------------------

# load in the model
RF_model <- readRDS(file.path("Output", species, paste0(method, "_model_", i, ".rds")))

# extract the used features
clean_cols <- RF_model$forest$independent.variable.names

# get these out of the test data
test_feature_data <- as.data.table(test_data) # this was generated in the main script
complete_cases <- test_feature_data %>%
  select(all_of(c(clean_cols, "Activity", "ID", "Time"))) %>%
  na.omit()

numeric_testing_data <- complete_cases %>%
  select(all_of(clean_cols)) %>%
  as.matrix()
if (anyNA(numeric_testing_data)) message("Validation data contains missing values!")

testing_metadata <- complete_cases %>%
  select(Activity, ID, Time)
ground_truth_labels <- factor(testing_metadata$Activity)

# Make predictions
output <- predict(RF_model, data = numeric_testing_data)
predictions <- output$predictions
predictions_df <- cbind(testing_metadata, predictions)
predictions_df <- predictions_df %>% rename(true_class = Activity)

metrics <- compute_metrics(predicted_classes = as.factor(predictions_df$predictions), 
                           ground_truth_labels = as.factor(predictions_df$true_class))

fwrite(metrics$metrics, file.path("Output", species, paste0(method, "_performance_", i, ".csv")))
