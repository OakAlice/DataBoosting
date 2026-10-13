# Function for testing performance ----------------------------------------

# Function to compute confusion matrix metrics (+ AUC-ROC / AUC-PR if probabilities supplied)
# probabilities: multiclass -> matrix/data.frame of class probabilities, colnames = class labels
#                binary     -> numeric vector of P("target"), or a matrix with a "target" column
compute_metrics <- function(predicted_classes, ground_truth_labels, type = c("multiclass", "binary"),
                            probabilities = NULL) {
  type <- match.arg(type)
  predicted_classes   <- as.factor(predicted_classes)
  ground_truth_labels <- as.factor(ground_truth_labels)
  
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
  # caret prefixes rownames with "Class: " for multiclass
  rownames(byClass) <- sub("^Class: ", "", rownames(byClass))
  
  metrics <- data.frame(
    Behaviour      = rownames(byClass),
    Accuracy       = byClass[, "Balanced Accuracy"],
    Sensitivity    = byClass[, "Sensitivity"],
    Specificity    = byClass[, "Specificity"],
    Precision      = byClass[, "Precision"],
    Recall         = byClass[, "Recall"],
    F1             = byClass[, "F1"],
    Detection_Rate = byClass[, "Detection Rate"],
    Prevalence     = byClass[, "Prevalence"],
    row.names      = NULL
  )
  
  # Threshold-independent metrics (one-vs-rest per class): need predicted probabilities
  if (!is.null(probabilities)) {
    if (is.null(dim(probabilities))) { # binary vector -> matrix
      probabilities <- cbind(target = probabilities)
    }
    probabilities <- as.matrix(probabilities)
    
    auc_one_vs_rest <- function(cls) {
      if (!cls %in% colnames(probabilities)) return(c(roc = NA_real_, pr = NA_real_))
      is_pos <- ground_truth_labels == cls
      scores <- probabilities[, cls]
      # AUC is undefined if the class is absent or is the only class present
      if (sum(is_pos) == 0 || sum(!is_pos) == 0) return(c(roc = NA_real_, pr = NA_real_))
      roc <- as.numeric(pROC::auc(pROC::roc(response = is_pos, predictor = scores,
                                            levels = c(FALSE, TRUE), direction = "<",
                                            quiet = TRUE)))
      pr  <- PRROC::pr.curve(scores.class0 = scores[is_pos],
                             scores.class1 = scores[!is_pos])$auc.integral
      c(roc = roc, pr = pr)
    }
    
    aucs <- vapply(metrics$Behaviour, auc_one_vs_rest, numeric(2))
    metrics$AUC_ROC <- aucs["roc", ]
    metrics$AUC_PR  <- aucs["pr", ]
  }
  
  # add the normalsoied as well
  metrics$Norm_AUC_PR <- (metrics$AUC_PR - metrics$Prevalence)/(1 - metrics$Prevalence)
  
  if (type == "multiclass") {
    # Add prevalence weighted metrics as the last row
    total_counts <- sum(metrics$Prevalence, na.rm = TRUE)
    # set all the NA values to 0
    metrics[is.na(metrics)] <- 0 # do not na.rm = TRUE otherwise the model can cheat and move towards not predicting classes at all
    
    wmean <- function(x) sum(x * metrics$Prevalence) / total_counts
    
    overall_metrics <- data.frame(
      Behaviour  = "WeightedOverall",
      Accuracy   = unname(confusion_mtx$overall["Accuracy"]),   # still raw overall accuracy
      Precision  = wmean(metrics$Precision),
      Recall     = wmean(metrics$Recall),
      F1         = wmean(metrics$F1),
      row.names  = NULL
    )
    if (!is.null(probabilities)) {
      overall_metrics$AUC_ROC <- wmean(metrics$AUC_ROC)
      overall_metrics$AUC_PR  <- wmean(metrics$AUC_PR)
      overall_metrics$Norm_AUC_PR  <- wmean(metrics$Norm_AUC_PR)
    }
    
    # columns not summarised overall (Sensitivity, Specificity, etc.) -> NA so rbind works
    overall_metrics[setdiff(names(metrics), names(overall_metrics))] <- NA
    metrics <- rbind(metrics, overall_metrics[names(metrics)])
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
  
  # just check that this hasnt happened
  if (nrow(complete_cases) < 1) {
    warning("No complete cases in test data")
    return(NULL)
  }
  
  # extract the components I need
  numeric_testing_data <- complete_cases %>%
    select(all_of(feat_cols)) %>%
    as.matrix()
  if (anyNA(numeric_testing_data)) message("Validation data contains missing values!")
  testing_metadata <- complete_cases %>% select(all_of(metadata_cols))
  ground_truth_labels <- factor(testing_metadata[[target_col]])

  # Make predictions
  output <- predict(model, data = numeric_testing_data)
  probabilities <- output$predictions # matrix: rows = samples, cols = classes
  predicted_classes <- factor(colnames(probabilities)[max.col(probabilities, ties.method = "first")],
                              levels = colnames(probabilities))
  
  predictions_df <- cbind(testing_metadata, predictions = predicted_classes)
  predictions_df <- predictions_df %>% rename(true_class = all_of(target_col))
  
  metrics <- compute_metrics(predicted_classes = as.factor(predictions_df$predictions), 
                             ground_truth_labels = as.factor(predictions_df$true_class),
                             type = type,
                             probabilities = probabilities)
  
  return(metrics)
}
