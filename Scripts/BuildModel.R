# Master script for building the models for any given species etc ---------

source(file = file.path("Scripts", "HPOFunctions.R"))
source(file = file.path("Scripts", "PerformanceTestingFunctions.R"))

if (!file.exists(file.path("Output", species, paste0(method, "_model_", i, ".csv")))) {
  
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
  
    saveRDS(RF_model, file.path("Output", species, paste0(method, "_model_", i, ".rds")))
    
} else {
  print("model already generated for this data")
}
