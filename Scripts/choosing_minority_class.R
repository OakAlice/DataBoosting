# Plots to decide the minority class --------------------------------------
plots <- list()
for (species in all_species){

  dat <- fread(file.path("Data", species, "Formatted_raw_data.csv"))
  
  counts <- dat %>% dplyr::count(Activity, ID)
  
  imb <- ggplot(counts, aes(x = Activity, y = n, fill = as.factor(ID))) +
    geom_col(position = "stack") +
    my_theme() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
    labs(title = species)
    
  ggsave(file.path("Data", species, "Imbalance_plot.png"), imb)
  
  plots[[species]] <- imb
}
plots



# 2. Quantify the minority class ------------------------------------------
settings <- fread("Data/DatasetCharacteristics.csv")
for (species in all_species){
  
  target <- settings$minorityTarget[settings$species == species]
  
  dat <- fread(file.path("Data", species, "Formatted_raw_data.csv"))
  dat[, Activity := ifelse(Activity == target, "target", "other")]
  
  ratio <- dat %>% dplyr::count(Activity)
  ratio <- ratio$n[ratio$Activity == "target"] / ratio$n[ratio$Activity == "other"]
  
  settings$minorityRatio[settings$species == species] <- round(ratio * 100, 3)
  
}
fwrite(settings, "Data/DatasetCharacteristics.csv")

