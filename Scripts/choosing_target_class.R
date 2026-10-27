# Quantify the minority class ------------------------------------------
determine_targets <- function(all_species, settings, plot = FALSE){

  if(!file.exists("Data/Targets.csv")){
    
    targets <- lapply(all_species, function(species){
      
      dat <- fread(file.path("Data", species, "Formatted_raw_data.csv"))
      sp_hz <- settings$Hz[settings$Species == species]
      
      if (plot == TRUE){
        counts <- dat %>% dplyr::count(Activity, ID)
        
        imb <- ggplot(counts, aes(x = Activity, y = n, fill = as.factor(ID))) +
          geom_col(position = "stack") +
          my_theme() + 
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
          labs(title = species)
        
        ggsave(file.path("Data", species, "Imbalance_plot.png"), imb)
      }
      
      total <- nrow(dat)
      
      class_counts <- dat %>%
        group_by(Activity) %>%
        summarise(n = n(), n_ids = n_distinct(ID))
      
      eligible <- filter(class_counts, n_ids >= 3)
      
      minority <- eligible %>%
        slice_min(n, n = 1, with_ties = FALSE) %>%
        mutate(prop = round((n/total) * 100, 3), type = "Minority")
      
      majority <- eligible %>%
        slice_max(n, n = 1, with_ties = FALSE) %>%
        mutate(prop = round((n/total) * 100, 3), type = "Majority")
      
      rbind(minority, majority) %>%
        mutate(Species = species,
               Hz = sp_hz,
               n_classes = nrow(class_counts),            # all classes
               n_individuals = n_distinct(dat$ID),
               total_min = round(total / sp_hz / 60, 1),
               imbalance = round(max(eligible$n) / min(eligible$n), 2))  # majority:minority, eligible classes only
    })
    
    targets <- rbindlist(targets)
    fwrite(targets, "Data/Targets.csv")
    
  } else {
    targets <- fread("Data/Targets.csv")
  }
  
  return(targets)
}
