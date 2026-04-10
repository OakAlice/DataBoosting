# Script where we cross-validate a model to get a performance score and var ----
# Code --------------------------------------------------------------------
# define the bounds for hyperpatameter tuning
bounds <- list(
  mtry            = c(1L, 10L),      # number of variables tried at each split
  min.node.size   = c(1L, 20L),      # minimum observations in terminal nodes
  num.trees       = c(100L, 1000L)   # number of trees
)

if (!file.exists(CV_filename)){
  
  print("tuning and testing the model")
  
  # TODO: Fix this here
  data <- selected_data # this is read in from the main # TODo: Still need to chanfe this somehow!!!
  
  # tune the model on a small subset of the data
  test_ids <- sample(unique(selected_data$ID), ceiling(length(unique(selected_data$ID))*0.3))
  other <- data %>% dplyr::filter(!ID %in% test_ids)
  test <- data %>% dplyr::filter(ID %in% test_ids)
    
  bo_results <- NULL
  # Wrap the optimisation so it always tries to return a result even when we dont get a convergence
  bo_results <- tryCatch(
    {
      BayesianOptimization(
        FUN = function(mtry, min.node.size, num.trees) {
          ModelOptimisation(
            feature_data = other,
            mtry_option = mtry,
            min.node.size_option = min.node.size,
            num.trees_option  = num.trees
          )
        },
        bounds = bounds,
        init_points = 2,
        n_iter = 5,
        acq = "ucb",
        kappa = 2.576
      )
    },
    error = function(e) {
      message("Bayesian optimization failed: ", e$message)
      return(NULL)
    }
    )
    
    if(!is.null(bo_results)){
      # save best
      tuning <- data.frame(
        best_mtry            = bo_results$Best_Par[["mtry"]],
        best_min.node.size   = bo_results$Best_Par[["min.node.size"]],
        best_num.trees       = bo_results$Best_Par[["num.trees"]],
        best_performance     = bo_results$Best_Value
      )
      print("model optimised, moving into CV results now")
    } else {
      # give generic if it wasn't able to converge
      print("wasnt able to converge so gave generic hyperparameters")
      tuning <- data.frame(
        best_mtry            = 5,
        best_min.node.size   = 10,
        best_num.trees       = 500,
        best_performance     = NA
      )
    }
    
  # now use these to train the final model
  # use tuned params
  mtry_option <- tuning$best_mtry
  min.node.size_option <- tuning$best_min.node.size
  num.trees_option <- tuning$best_num.trees
  
  # now that we have tuned it, reset it to the full data
  other <- selected_data # all of the data
  test <- test_data # the same test data every time
  
  output <- run_cv_iteration(train = other, validate= test,
                             mtry_option, min.node.size_option, num.trees_option)
  
  output_scores <- output$metrics_table
  output_scores$total_inds <- length(unique(selected_data$ID))
    
  # save that
  fwrite(output_scores, file.path(CV_filename))
} else {
  print("already cross-validated - delete files if you want to redo")
}
