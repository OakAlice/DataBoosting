# Master function that builds the model -----------------------------------
design_build_RF <- function(data, weighted = FALSE, metadata_cols, hyperparameters = NULL){
  
  # prep the data
  other_feature_data <- as.data.table(data)
  clean_cols <- removeBadFeatures(other_feature_data, var_threshold = 0.3, corr_threshold = 0.9)
  if (is.null(clean_cols) || length(clean_cols) == 0) return(NULL) # if there was no dfata, we need to skip out of this
  
  training_data <- other_feature_data %>%
    select(c(!!!syms(clean_cols), "Activity")) %>%
    na.omit() %>%
    mutate(Activity = as.factor(Activity))
  
  # choose the hyperparmaters
  # NOTE!!! I have removed the optimisation loop from this analyssi 
  if (hyperparameters == "default"){
    n_train <- nrow(training_data)
    p <- length(clean_cols)
    
    best_mtry <- max(2, min(p, ceiling(p / 3)))
    best_number_trees <- 500 
    best_max_depth <- if (n_train < 500) 8 else 15 
  
  } else { # if a dataframe with values has been input
    best_mtry <- floor(sqrt(length(clean_cols)))
    best_number_trees <- hyperparameters$value[hyperparameters$parameter == "best_number_trees"]
    best_max_depth <- hyperparameters$value[hyperparameters$parameter == "best_max_depth"]
  }
  
  # Train an optimal model --------------------------------------------------
  
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
    min.node.size = max(5, ceiling(0.01 * nrow(training_data))),
    classification = TRUE,
    probability = TRUE
  ) 
  
  # if(method == "Weighted"){
  #   # weight by class frequency
  #   class_freq <- table(training_data$Activity)
  #   class_weights <- 1 / class_freq
  #   class_weights <- class_weights / sum(class_weights)
  #   weight <- class_weights[training_data$Activity]
  #   
  #   RF_model <- ranger(
  #     dependent.variable.name = "Activity",
  #     data = training_data,
  #     num.trees = best_number_trees,
  #     mtry = best_mtry,
  #     max.depth = best_max_depth,
  #     classification = TRUE,
  #     case.weights = weight,
  #     probability = TRUE
  #   )
  # }
  
  return(list(RF_model = RF_model,
              hyperparameters = hyperparameters))   
}

# remove redundant and NA columns
removeBadFeatures <- function(feature_data, var_threshold, corr_threshold) {
  
  if (!is.data.frame(feature_data) || ncol(feature_data) == 0 || nrow(feature_data) == 0) {
    warning("Empty feature data; returning no features")
    return(character(0))
  }
  
  # sometimes empty data is coming in and erroring here...
  if (!is.data.frame(feature_data) || ncol(feature_data) == 0 || nrow(feature_data) == 0) {
    return(NULL)
  }
  
  # pick numeric columns by name, excluding metadata
  num_cols <- names(feature_data)[vapply(feature_data, is.numeric, logical(1))]
  num_cols <- setdiff(num_cols, c("Activity", "ID", "Time"))
  
  if (length(num_cols) == 0) stop("No numeric feature columns found")
  
  numeric_columns <- feature_data[, ..num_cols]
  
  variances <- sapply(numeric_columns, var, na.rm = TRUE)
  selected_columns <- names(variances)[!is.na(variances) & variances > var_threshold]
  
  if (length(selected_columns) <= 2) {
    selected_columns <- names(variances)[!is.na(variances)]
  }
  
  if (length(selected_columns) < 2) {
    warning("Fewer than 2 usable features; skipping correlation filter")
    return(selected_columns)
  }
  
  corr_matrix <- cor(numeric_columns[, ..selected_columns], use = "pairwise.complete.obs")
  high_corr <- findCorrelation(corr_matrix, cutoff = corr_threshold, names = TRUE)
  
  setdiff(selected_columns, high_corr)
}

