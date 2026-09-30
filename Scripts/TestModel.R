# Testing the model on the original data ----------------------------------

test_model <- function(type, model, test_data, metadata_cols, target_col){
  #' Function to test the supervised machine learning random forest ranger model
  #' type ("binary" | "multiclass"): whether the model being tested is binary or multiclass
  #' model: the ranger random forest object
  #' test_data: dataframe of feature test data
  #' metadata_cols: list of non-feature columns
  #' target_col: column with the name of the target
  
  # extract the used features and get these out of the test data
  feat_cols <- model$forest$independent.variable.names
  test_feature_data <- as.data.table(test_data)
  complete_cases <- test_feature_data %>%
    select(all_of(c(feat_cols, metadata_cols))) %>%
    na.omit()
  
  # extract the components I need
  numeric_testing_data <- complete_cases %>%
    select(all_of(clean_cols)) %>%
    as.matrix()
  if (anyNA(numeric_testing_data)) message("Validation data contains missing values!")
  testing_metadata <- complete_cases %>% select(all_of(metadata_cols))
  ground_truth_labels <- factor(testing_metadata[[target_col]])
  
  # Make predictions
  output <- predict(RF_model, data = numeric_testing_data)
  predictions <- output$predictions
  predictions_df <- cbind(testing_metadata, predictions)
  predictions_df <- predictions_df %>% rename(true_class = all_of(target_col))
  
  metrics <- compute_metrics(predicted_classes = as.factor(predictions_df$predictions), 
                             ground_truth_labels = as.factor(predictions_df$true_class),
                             type = type)
  
  return(metrics)
}
