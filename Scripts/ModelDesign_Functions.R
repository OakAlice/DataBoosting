# Master function that builds the model -----------------------------------
design_build_RF <- function(other_data){
  
  # Model design: hyperparameter tuning -------------------------------------
  # Based on a random forest, what hyperparamaters are best?
  bounds <- list(
    mtry = c(2, 20),
    max_depth = c(5, 30),
    number_trees = c(100, 1000)
  )
  
  # this is optimised for weighted F1 score
  results <- BayesianOptimization(
    FUN = function(number_trees, mtry, max_depth) {
      RFModelOptimisation(
        feature_data = other_data,
        number_trees = number_trees,
        mtry = mtry,
        max_depth = max_depth
      )
    },
    bounds = bounds,
    init_points = 2,
    n_iter = 5,
    acq = "ucb",
    kappa = 2.576 
  )
  
  # extract the best ones
  best_mtry <- round(results$Best_Par[["mtry"]],0)
  best_number_trees <- round(results$Best_Par[["number_trees"]],0)
  best_max_depth <- round(results$Best_Par[["max_depth"]],0)
  best_performance <- results$Best_Value # just for interest
  
  # Train an optimal model --------------------------------------------------
  other_feature_data <- as.data.table(other_data)
  clean_cols <- removeBadFeatures(other_feature_data, var_threshold = 0.3, corr_threshold = 0.9)
  training_data <- other_feature_data %>%
    select(c(!!!syms(clean_cols), "Activity")) %>%
    na.omit() %>%
    mutate(Activity = as.factor(Activity))
  
  if (best_mtry > length(clean_cols)-1){
    message("mtry too big, making max clean cols")
    flush.console()
    best_mtry <- length(clean_cols)
  }
  
  RF_model <- ranger(
    dependent.variable.name = "Activity",
    data = training_data,
    num.trees = best_number_trees,
    mtry = best_mtry,
    max.depth = best_max_depth,
    classification = TRUE
  ) 
  
  if(method == "Weighted"){
    # weight by class frequency
    class_freq <- table(training_data$Activity)
    class_weights <- 1 / class_freq
    class_weights <- class_weights / sum(class_weights)
    weight <- class_weights[training_data$Activity]
    
    RF_model <- ranger(
      dependent.variable.name = "Activity",
      data = training_data,
      num.trees = best_number_trees,
      mtry = best_mtry,
      max.depth = best_max_depth,
      classification = TRUE,
      case.weights = weight
    )
  }
  
  return(RF_model)   
}

# Hyperparameter Optimisation ---------------------------------------------

# remove redundant and NA columns
removeBadFeatures <- function(feature_data, var_threshold, corr_threshold) {
  
  # Step 1: Calculate variance for numeric columns
  numeric_columns <- feature_data[, .SD, .SDcols = !names(feature_data) %in% c("Activity", "ID", "Time")]
  variances <- numeric_columns[, lapply(.SD, var, na.rm = TRUE)]
  selected_columns <- names(variances)[!is.na(variances) & variances > var_threshold]
  
  if (!length(selected_columns) > 2){
    selected_columns <- names(variances)[!is.na(variances)] # include all of them then
  }
  
  # Step 2: Remove highly correlated features
  numeric_columns <- numeric_columns[, ..selected_columns]
  corr_matrix <- cor(numeric_columns, use = "pairwise.complete.obs")
  high_corr <- findCorrelation(corr_matrix, cutoff = corr_threshold)
  remaining_features <- setdiff(names(numeric_columns), names(numeric_columns)[high_corr])
  
  return(remaining_features)
}

# main call that splits data, generates function, and validates
RFModelOptimisation <- function(feature_data, number_trees, mtry, max_depth){
  
    # remove bad features
    feature_data <- as.data.table(feature_data)
    
    var <- ifelse(species == "Ferdinandy_Dog", 0.01, 0.3)
    
    clean_cols <- removeBadFeatures(feature_data, var_threshold = var, corr_threshold = 0.9)
    clean_feature_data <- feature_data %>%
      select(c(!!!syms(clean_cols), "Activity", "ID", "Time")) %>% 
      na.omit()
          
    if (mtry > length(clean_cols)-1){
      message("mtry too big, making max clean cols")
      flush.console()
      mtry <- length(clean_cols)
    }
  
    f1_scores <- list()  # List to store F1-scores
    
    # Repeat the process 3 times
    for (i in 1:3) {
      
        tryCatch({
          # Create training and validation data, split by ID
          # ensure that the synthetic data is not included in the test data
          if ("Synth" %in% unique(clean_feature_data$ID)){
            all_IDs <- unique(clean_feature_data$ID)
            real_IDs <- setdiff(all_IDs, "Synth")
            # Sample 30% of real IDs for test
            val_IDs <- sample(real_IDs, ceiling(0.3 * length(real_IDs)))
            
          } else {
            val_IDs <- sample(unique(clean_feature_data$ID), ceiling(0.3*length(unique(clean_feature_data$ID))))
          }
          
          validation_data <- clean_feature_data %>% dplyr::filter(ID %in% val_IDs)
          training_data <- clean_feature_data %>% dplyr::filter(!ID %in% val_IDs)                      
          
          # Separate into training and testing
          training_data <- training_data %>%
            select(-c(ID, Time)) %>%
            mutate(Activity = as.factor(Activity))
          
          validation_data <- validation_data %>%
            select(-c(ID, Time)) %>%
            mutate(Activity = as.factor(Activity))
          
        }, error = function(e) {
          message("Error in data splitting: ", e$message)
        })
      
      # Train RF model
      tryCatch({
        
        # weight by class frequency
        class_freq <- table(training_data$Activity)
        class_weights <- 1 / class_freq
        class_weights <- class_weights / sum(class_weights)
        weight <- class_weights[training_data$Activity]
        
        RF_model <- ranger(
          dependent.variable.name = "Activity",
          data = training_data,
          num.trees = number_trees,
          mtry = mtry,
          max.depth = max_depth,
          sample.fraction = 1, # select all the data (small datasets will fail otherwise)
          classification = TRUE,
          importance = "impurity",
          case.weights = weight
        )
  
      }, error = function(e) {
        message("Error in RF training: ", e$message)
        stop()
      })
      
      #### Validate the model
      tryCatch({
        numeric_validation_data <- as.matrix(validation_data[, !names(validation_data) %in% c("Activity"), with = FALSE])
        ground_truth_labels <- validation_data$Activity
        
        if (anyNA(numeric_validation_data)) {
          message("Validation data contains missing values!")
          flush.console()
        }
        
        numeric_validation_data <- as.data.frame(numeric_validation_data)
        predictions <- predict(RF_model, data = numeric_validation_data)
        predicted_classes <- predictions$predictions
        
      }, error = function(e) {
        message("Error in making predictions: ", e$message)
        flush.console()
        stop()
      })
      
      # Confusion matrix and performance metrics
      all_classes <- sort(union(unique(predicted_classes), unique(ground_truth_labels)))
      predicted_classes <- factor(unlist(predicted_classes), levels = all_classes)
      ground_truth_labels <- factor(ground_truth_labels, levels = all_classes)
      
      if (length(all_classes) < 2) {
        print("Only one class present ---- fix here")
        weighted_f1 <- NA
      } else {
      
        # make a confusion matrix
        confusion_matrix <- table(predicted_classes, ground_truth_labels)
        
        # Handling mismatched dimensions
        all_classes <- sort(union(colnames(confusion_matrix), rownames(confusion_matrix)))
        conf_matrix_padded <- matrix(0, 
                                     nrow = length(all_classes), 
                                     ncol = length(all_classes),
                                     dimnames = list(all_classes, all_classes))
        conf_matrix_padded[rownames(confusion_matrix), colnames(confusion_matrix)] <- confusion_matrix
        
        # Calculate F1 scores
        confusion_mtx <- confusionMatrix(conf_matrix_padded)
        byClass <- confusion_mtx$byClass
        
        if (is.matrix(byClass)) {
          # Compute weighted F1 (rather than the macro which is what I was doing before)
          
          f1 <- byClass[, "F1"]
          support <- rowSums(confusion_mtx$table)  # True instances per class
          
          f1[is.nan(f1)] <- 0 # if you leave NAs in, it fails. and if you na.rm() it gets too easy because classes are ommitted
          f1[is.na(f1)] <- 0
          weighted_f1 <- weighted.mean(f1, w = support)
          
        } else if (is.numeric(byClass) && "F1" %in% names(byClass)) {
          weighted_f1 <- byClass["F1"]
        } else {
          NA
        }
        
       }
      
      # Store the F1 score
      f1_scores[[i]] <- weighted_f1
    }
    
    #### Calculate average F1-scors
    # same NA problem
    f1s <- unlist(f1_scores)
    f1s[is.na(f1s)] <- 0
    average_macro_f1 <- mean(f1s)
    
    # no preds for this one
    return(list(Score = average_macro_f1, Pred = NA))
}
