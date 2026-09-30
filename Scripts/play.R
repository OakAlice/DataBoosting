
# Code for playing arounf -------------------------------------------------

all_species <- c("Galea_Cat", "Clemente_Echidna", "Studd_Squirrel", "Ladds_Seal", "Dunford_Cat", 
                 "HarveyCaroll_Pangolin", "Yu_Duck", "Sparkes_Koala")
for (species in all_species){
  
  print(species)
  
  dat <- fread(file.path("Data", species, "Nothing_Feature_data.csv"))
  
  counts <- dat %>% count(Activity, ID)
  
  ggplot(counts, aes(x = Activity, y = n, fill = ID)) +
    geom_col(position = "stack") +
    scale_fill_manual(values = more_colours) +
    my_theme() +
    ggtitle(species)
    
  
  print("done")
}
