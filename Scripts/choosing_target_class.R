# Quantify the minority class ------------------------------------------

determine_targets <- function(all_species, plot = FALSE){
  
  if(!file.exists("Data/Targets.csv")){
  
    targets <- lapply(all_species, function(species){
      
      dat <- fread(file.path("Data", species, "Formatted_raw_data.csv"))
      
      # make a plot
      if (plot== TRUE){
        dat <- fread(file.path("Data", species, "Formatted_raw_data.csv"))
      
        counts <- dat %>% dplyr::count(Activity, ID)
        
        imb <- ggplot(counts, aes(x = Activity, y = n, fill = as.factor(ID))) +
          geom_col(position = "stack") +
          my_theme() + 
          theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
          labs(title = species)
        
        ggsave(file.path("Data", species, "Imbalance_plot.png"), imb)
      }
      
      # make the data
      total <- nrow(dat)
      
      minority <- dat %>%
        group_by(Activity) %>%
        summarise(n = n(), n_ids = n_distinct(ID)) %>%
        filter(n_ids >= 3) %>%
        slice_min(n, n = 1) %>%
        mutate(prop = round((n/total) * 100, 3),
               type = "Minority") 
      
      majority <- dat %>%
        group_by(Activity) %>%
        summarise(n = n(), n_ids = n_distinct(ID)) %>%
        filter(n_ids >= 3) %>%
        slice_max(n, n = 1) %>%
        mutate(prop = round((n/total) * 100, 3),
               type = "Majority") 
      
      both <- rbind(minority, majority) %>%
        mutate(Species = species)
      
      both
    })
    
    targets <- rbindlist(targets)
    fwrite(targets, "Data/Targets.csv")
  
  } else {
    targets <- fread("Data/Targets.csv")
  }
  
  return(targets)
}


