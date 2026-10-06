
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


file <- "C:/Users/PC/OneDrive - University of the Sunshine Coast/04.DataBoosting/Data/Gaschk_Quoll/Gashk_Quoll_formatted.csv"
dat <- fread(file)
key <- "C:/Users/PC/OneDrive - University of the Sunshine Coast/04.DataBoosting/Data/Gaschk_Quoll/Quoll_Behaviour_ActNo.csv"
key <- fread(key)
dat <- merge(dat, key, by = "Activity") %>%
  select(-Activity) %>%
  rename(Activity = Activity2)
fwrite(dat, file)

dat %>% count(Activity)

counts <- dat %>% count(Activity, ID)

ggplot(counts, aes(x = Activity, y = n, fill = ID)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = more_colours) +
  my_theme()







species <- "Studd_Squirrel"

dat <- fread(file.path("Data", species, "Formatted_raw_data.csv"))
counts <- dat %>% group_by(Activity, ID) %>% dplyr::count()

ggplot(counts, aes(x = Activity, y = n, fill = ID)) +
  geom_col(position = "stack") +
  scale_fill_manual(values = more_colours) +
  my_theme() +
  ggtitle(species)


settings <- fread("Data/DatasetCharacteristics.csv")
target <- settings$minorityTarget[settings$species == species]
dat$Activity <- ifelse(dat$Activity == target, "target", "other")

dat <- dat %>% select(-FuncActivity, -BroadActivity)

fwrite(dat, file.path("Data", species, "Relabelled_raw_data.csv"))






