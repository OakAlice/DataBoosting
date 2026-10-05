# Functions to actually run the experiment --------------------------------
get_species_config <- function(Species, settings) {
  s <- settings[settings$species == Species, ]
  list(
    species = Species,
    target  = s$minorityTarget,
    Hz      = s$Hz,
    window  = if (s$Hz > 1) 2 else 5,
    data_path   = file.path("Data", Species),
    output_path = file.path("Output", Species)
  )
}


prepare_species_data <- function(cfg) {
  
  raw_data <- cached(file.path(cfg$data_path, "Relabelled_raw_data.csv"), function() {
    d <- fread(file.path(cfg$data_path, "Formatted_raw_data.csv"))
    d[, Activity := ifelse(Activity == cfg$target, "target", "other")]
    d
  })
  
  feature_path <- file.path(cfg$data_path, "Nothing_Feature_data.csv")
  feature_data <- cached(feature_path, function() {
    generate_features_master(raw_data,
                             sample_rate     = cfg$Hz,
                             overlap_setting = 0,
                             window_setting  = cfg$window)
  })
  
  list(raw = raw_data, features = feature_data, feature_path = feature_path)
}


run_fold <- function(fold, method, dat, cfg, ID_groups) {
  
  out_file <- file.path(cfg$output_path, sprintf("%s_performance_%d.csv", method, fold))
  if (file.exists(out_file)) return(invisible(NULL))
  
  test_IDs <- ID_groups$ID[ID_groups$group == fold]
  print(test_IDs) # debug
  
  boosted <- boost_training_data(
    dplyr::filter(dat$raw, !ID %in% test_IDs),
    metadata_cols, target_col,
    minority_classes = "target",
    method           = method,
    Hz               = cfg$Hz,
    prior_features   = dplyr::filter(dat$features, !ID %in% test_IDs)
  )
  
  boosted <- boosted$boosted_data # extract all the data
  
  # NOTE: The binaries here are hardcoded
  model   <- design_build_RF(boosted, type = "binary", metadata_cols)
  metrics <- test_model(type = "binary", 
                        model,
                        test_data = dplyr::filter(dat$features, ID %in% test_IDs),
                        metadata_cols, 
                        target_col)
  
  fwrite(metrics$metrics, out_file)
  invisible(metrics)
}

run_species <- function(species, settings, boost_methods, k = 3) {
  cfg <- get_species_config(species, settings)
  dat <- prepare_species_data(cfg)
  
  # same ID split reused across all methods
  ID_groups <- make_ID_groups(dat$features, target_col = target_col, k = k,
                              priority_classes = "target")
  
  grid <- expand.grid(method = boost_methods, fold = seq_len(k),
                      stringsAsFactors = FALSE)
  
  for (j in seq_len(nrow(grid))) {
    message(species, " | ", grid$method[j], " | fold ", grid$fold[j])
    run_fold(fold = grid$fold[j], 
             method = grid$method[j], 
             dat, 
             cfg, 
             ID_groups)
  }
}
