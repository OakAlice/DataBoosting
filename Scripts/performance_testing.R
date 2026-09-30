# Function for testing performance ----------------------------------------

# Plot of confusion matric ------------------------------------------------
generate_confusion_plot <- function(conf_matrix_padded, save_path) {
  
  # Extracting confusion matrix data and reshaping it
  conf_matrix_df <- as.data.frame(as.table(conf_matrix_padded))
  colnames(conf_matrix_df) <- c("Predicted", "Actual", "Count")
  
  # Repeat rows based on the Count column (i.e., add multiple rows for each count)
  conf_matrix_df_repeated <- conf_matrix_df[rep(1:nrow(conf_matrix_df), conf_matrix_df$Count), ]
  
  # Create a new column to classify the points as True Positive, False Positive, etc.
  conf_matrix_df_repeated$Type <- "Other"
  conf_matrix_df_repeated$Type[conf_matrix_df_repeated$Predicted == conf_matrix_df_repeated$Actual] <- "True Positive"
  conf_matrix_df_repeated$Type[conf_matrix_df_repeated$Predicted != conf_matrix_df_repeated$Actual] <- "False Positive"
  
  # Assign colors based on classification type
  conf_matrix_df_repeated$Color <- ifelse(conf_matrix_df_repeated$Type == "True Positive", "blue", "red")
  
  # Plotting with jitter
  confusion_plot <- ggplot(conf_matrix_df_repeated, aes(x = Predicted, y = Actual, color = Color)) +
    geom_jitter(width = 0.1, height = 0.1, alpha = 0.3, size = 2) +  # Add jitter with fixed point size
    scale_color_manual(values = c("blue", "red")) +
    labs(x = "Predicted Class", 
         y = "Actual Class") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  # Save the plot to a PDF
  ggsave(save_path,
         plot = confusion_plot, width = 16, height = 8)
}

# Function to compute confusion matrix metrics
compute_metrics <- function(predicted_classes, ground_truth_labels, type = "multiclass") {
  # Compute confusion matrix
  confusion_matrix <- table(predicted_classes, ground_truth_labels)
  all_classes <- union(levels(predicted_classes), levels(ground_truth_labels))
  
  # Create a padded confusion matrix (in case there are missing classes)
  conf_matrix_padded <- matrix(0, nrow = length(all_classes), ncol = length(all_classes),
                               dimnames = list(all_classes, all_classes))
  conf_matrix_padded[rownames(confusion_matrix), colnames(confusion_matrix)] <- confusion_matrix
  
  # now if binary or not
  confusion_mtx <- if (type == "binary") {
    caret::confusionMatrix(as.table(conf_matrix_padded), positive = "target")
  } else {
    caret::confusionMatrix(as.table(conf_matrix_padded))
  }
  
  byClass <- confusion_mtx$byClass
  if (is.null(dim(byClass))) {                 # binary -> named vector
    byClass <- t(as.matrix(byClass))
    rownames(byClass) <- "target"
  }
  
  metrics <- data.frame(
    Behaviour  = rownames(byClass),
    Accuracy   = byClass[, "Balanced Accuracy"],
    Sensitivity = byClass[, "Sensitivity"],
    Specificity = byClass[, "Specificity"],
    Precision  = byClass[, "Precision"],
    Recall     = byClass[, "Recall"],
    F1         = byClass[, "F1"],
    Detection_Rate = byClass[, "Detection Rate"],
    Prevalence = byClass[, "Prevalence"],
    row.names  = NULL
  )
  
  if (type == "multiclass"){
    # Add prevelance weighted metrics as the last row
    total_counts <- sum(metrics$Prevalence)
    # set all the NA values to 0
    metrics[is.na(metrics)] <- 0 # do not na.rm = TRUE otherwise the model can cheat and move towards not predicting classes at all
    overall_metrics <- data.frame(
      Behaviour   = "WeightedOverall",
      Accuracy    = confusion_mtx$overall["Accuracy"],   # still raw overall accuracy
      Precision   = sum(metrics$Precision * metrics$Prevalence) / total_counts,
      Recall      = sum(metrics$Recall * metrics$Prevalence) / total_counts,
      F1          = sum(metrics$F1 * metrics$Prevalence) / total_counts,
      Prevalence = NA,
      row.names  = NULL
    )
  
    metrics <- rbind(
      metrics,
      overall_metrics
    )
  }
  
  return(list(metrics = metrics,
              conf_matrix_padded = conf_matrix_padded))
}


test_model <- function(type, model, test_data, metadata_cols, target_col){
  #' Function to test the supervised machine learning random forest ranger model
  #' type ("binary" | "multiclass"): whether the model being tested is binary or multiclass
  #' model: the ranger random forest object
  #' test_data: dataframe of feature test data
  #' metadata_cols: list of non-feature columns
  #' target_col: column with the name of the target
  
  # extract the used features and get these out of the test data
  feat_cols <- model$forest$independent.variable.names
  test_data <- as.data.table(test_data)
  complete_cases <- test_data %>%
    select(all_of(c(feat_cols, metadata_cols))) %>%
    na.omit()
  
  # extract the components I need
  numeric_testing_data <- complete_cases %>%
    select(all_of(feat_cols)) %>%
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
