
select_features<- function(data){
  
  #top_predictors <- tryCatch({
    
    setDT(data)
    
    # remove bad features
    clean_cols <- removeBadFeatures(data, var_threshold = 0.3, corr_threshold = 0.9)
    clean_feature_data <- data %>%
      select(c(!!!syms(clean_cols), "Activity")) %>% 
      na.omit()
    
  # unnecessary because already using a rf
  #   clean_feature_data$Activity <- as.factor(clean_feature_data$Activity)
  #   
  #   # Fit RF on the training data to fidn the best predictor variables
  #   RF_model <- ranger(
  #     formula = Activity ~ .,
  #     data = clean_feature_data,
  #     num.trees = 500,
  #     mtry = floor(sqrt(ncol(clean_feature_data) - 1)),
  #     importance = "impurity",
  #     classification = TRUE
  #   )
  #   
  #   # Extract variable importance and select top 30
  #   imp <- importance(RF_model)
  #   top_predictors <- names(sort(imp, decreasing = TRUE))[1:30]
  #   
  # }, error = function(e) {
  #   message("Random forest failed: ", e$message)
  #   NULL  # return NULL if it fails
  # })
    
    top_predictors <- clean_cols
  
  return(top_predictors)
}

# remove redundant and NA columns
removeBadFeatures <- function(data, var_threshold, corr_threshold) {
  
  # Step 1: Calculate variance for numeric columns
  numeric_columns <- data[, .SD, .SDcols = setdiff(names(data), c("Activity", "ID", "Time"))]
  variances <- numeric_columns[, lapply(.SD, var, na.rm = TRUE)]
  selected_columns <- names(variances)[!is.na(variances) & variances > var_threshold]
  
  # Step 2: Remove highly correlated features
  numeric_columns <- numeric_columns[, ..selected_columns]
  corr_matrix <- cor(numeric_columns, use = "pairwise.complete.obs")
  high_corr <- caret::findCorrelation(corr_matrix, cutoff = corr_threshold)
  remaining_features <- setdiff(names(numeric_columns), names(numeric_columns)[high_corr])
  
  return(remaining_features)
}

run_cv_iteration <- function(train, validate, mtry_option, min.node.size_option, num.trees_option) {
  
  # have removed this for now because rf does it internally anyway
  # Feature selection
  # best_features <- select_features(data = train)
  # Subset to best predictors
  # train <- train %>%
  #   select(c(!!!syms(best_features), "Activity")) %>%
  #   na.omit()
  
  # Class weights
  class_freq <- table(train$Activity)
  
  class_weights <- 1 / class_freq
  class_weights <- class_weights / sum(class_weights)
  
  train <- train %>% select(-ID, -Time)
  train$Activity <- as.factor(train$Activity)
  
  # train a random forest
  RF_model <- ranger(
    Activity ~ .,
    data = train,
    num.trees       = num.trees_option,
    mtry            = mtry_option,
    min.node.size   = min.node.size_option,
    sample.fraction = 1,
    classification  = TRUE,
    class.weights   = class_weights,
    importance      = "impurity"
  )
  
  # recording which of the variables were the most important
  imp <- importance(RF_model)
  importance_df <- data.frame(
    feature    = names(imp),
    importance = as.numeric(imp),
    stringsAsFactors = FALSE
  ) %>% arrange(desc(importance))
  
  # Validation data
  # validation_data <- validate %>%
  #   select(c(!!!syms(best_features), "Activity")) %>%
  #   na.omit()
  
  num_validate <- validate %>% select(-ID, -Time, -Activity)
  ground_truth <- validate %>% select(Activity)
  
  # Predictions
  # newdata for RandomForest but just data for Ranger btw
  predictions <- predict(RF_model, data = num_validate)
  predictions <- predictions$predictions
  
  # Confusion matrix (padded to match dimensions)
  all_classes <- sort(union(unique(predictions), unique(ground_truth$Activity)))
  predicted_classes <- factor(unlist(predictions), levels = all_classes)
  ground_truth_labels <- factor(ground_truth$Activity, levels = all_classes)
  
  metrics_table <- NULL
  # Compute confusion matrix
  # If only one class present, have to deal with manually (otherwise faisl)
  if (length(unique(ground_truth_labels)) < 2) {
    
    # Create confusion table manually
    cm_table <- table(predicted_classes, ground_truth_labels)
    
    # Overall accuracy
    accuracy <- sum(diag(cm_table)) / sum(cm_table)
    
    cm <- list(
      table = cm_table,
      overall = c(Accuracy = accuracy),
      byClass = NA
    )
    
  } else {
    cm <- confusionMatrix(predicted_classes, ground_truth_labels)
  }
  cm$byClass[is.na(cm$byClass)] <- 0 # coerce all the NA to O - to penalise the model
  # Per-class metrics
  class_counts <- table(ground_truth_labels)
  byClass <- cm$byClass
  
  # (esp in small datasets there can only be a few behaviours unfortunately)
  # this needs to be calculated directly from the confunction matrix
  # in case there are too few behaviours
  tab <- cm$table
  classes <- rownames(tab)
  N <- sum(tab)
  
  per_class_metrics <- lapply(classes, function(cls) {
    
    TP <- tab[cls, cls]
    FP <- sum(tab[cls, ]) - TP
    FN <- sum(tab[, cls]) - TP
    TN <- N - TP - FP - FN
    
    Sensitivity <- TP / (TP + FN)
    Specificity <- TN / (TN + FP)
    Precision   <- TP / (TP + FP)
    Recall      <- Sensitivity
    F1          <- ifelse((Precision + Recall) == 0, NA,
                          2 * Precision * Recall / (Precision + Recall))
    BalancedAcc <- (Sensitivity + Specificity) / 2
    Prevalence  <- sum(tab[, cls]) / N
    
    data.frame(
      Class       = cls,
      Sensitivity = Sensitivity,
      Specificity = Specificity,
      Precision   = Precision,
      Recall      = Recall,
      F1          = F1,
      BalancedAcc = BalancedAcc,
      Prevalence  = Prevalence
    )
  })
  
  per_class_metrics <- do.call(rbind, per_class_metrics)
  row.names(per_class_metrics) <- NULL
  
  # set all the NA values to 0
  per_class_metrics[is.na(per_class_metrics)] <- 0
  
  # Overall metrics
  overall_metrics <- data.frame(
    Class = "NonWeightedOverall",
    Accuracy = cm$overall["Accuracy"],
    Sensitivity = mean(per_class_metrics$Sensitivity),
    Specificity = mean(per_class_metrics$Specificity),
    Precision   = mean(per_class_metrics$Precision),
    Recall      = mean(per_class_metrics$Recall),
    F1          = mean(per_class_metrics$F1),
    BalancedAcc = mean(per_class_metrics$BalancedAcc)
  )
  
  total_counts <- sum(per_class_metrics$Prevalence)
  overall_metrics2 <- data.frame(
    Class      = "WeightedOverall",
    Accuracy   = cm$overall["Accuracy"],   # still raw overall accuracy
    Sensitivity = sum(per_class_metrics$Sensitivity * per_class_metrics$Prevalence) / total_counts,
    Specificity = sum(per_class_metrics$Specificity * per_class_metrics$Prevalence) / total_counts,
    Precision   = sum(per_class_metrics$Precision * per_class_metrics$Prevalence) / total_counts,
    Recall      = sum(per_class_metrics$Recall * per_class_metrics$Prevalence) / total_counts,
    F1          = sum(per_class_metrics$F1 * per_class_metrics$Prevalence) / total_counts,
    BalancedAcc = sum(per_class_metrics$BalancedAcc * per_class_metrics$Prevalence) / total_counts
  )
  # stitch back toegtjer
  metrics_table <- bind_rows(per_class_metrics,overall_metrics,overall_metrics2)
  
  return(list(metrics_table = metrics_table,
              importance_df = importance_df))
}

# Main optimisation function
ModelOptimisation <- function(feature_data, mtry_option, min.node.size_option, num.trees_option) {
  
  # Split into folds
  unique_IDs <- unique(feature_data$ID)
  ID_groups <- data.frame(
    ID = unique_IDs,
    group = sample(rep(1:3, length.out = length(unique_IDs)))
  )
  
  # Run CV across k folds
  # in this case I had to remove K and set it to the max
  # in cases where there are only 2, it fails at 3
  results <- lapply(1:max(ID_groups$group), function(i) {
    print(i)
    # new individuals selected as the test for each fold
    test_ids <- ID_groups$ID[ID_groups$group == i]
    other <- feature_data %>% dplyr::filter(!ID %in% test_ids)
    test <- feature_data %>% dplyr::filter(ID %in% test_ids)
    
    tryCatch({
      run_cv_iteration(
        train = other,
        validate = test,
        mtry_option = mtry_option,
        min.node.size_option = min.node.size_option,
        num.trees_option = num.trees_option
      )
    }, error = function(e) {
      message("Issue in CV iteration ", i, ": ", e$message)
      return(NA)
    })
  })
  
  print(results)
  
  metrics_tables <- do.call(
    rbind,
    lapply(
      Filter(
        function(x) is.list(x) && "metrics_table" %in% names(x),
        results
      ),
      `[[`,
      "metrics_table"
    )
  )
  
  f1s <- metrics_tables$F1
  # replace the NA f1 values with a 0 to drag down the performance and penalise if it fails to predict the class
  f1s[is.na(f1s)] <- 0 
  f1 <- mean(f1s)
  
  # making this not a list with Score and Pred will trigger 
  # Error in This_Score_Pred$Score : $ operator is invalid for atomic vectors
  # because BayesianOptimisation is trying to call the Score object
  return(list(Score = f1, Pred = NA))
}
