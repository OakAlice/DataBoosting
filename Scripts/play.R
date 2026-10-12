
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
























feature_data <- train #dat$features


feature_data <- as.data.frame(feature_data)

# features shared by the reference and EVERY method
feat_cols <- setdiff(colnames(feature_data), c(metadata_cols, target_col))
is_num <- function(d) vapply(d[feat_cols], is.numeric, logical(1))
feat_cols <- feat_cols[vapply(feature_data[feat_cols],
                              function(x) sd(x, na.rm = TRUE) > 0, logical(1))]
feature_data <- feature_data[complete.cases(feature_data[feat_cols]), ]

# fit once on the reference
Xf <- scale(as.matrix(feature_data[feat_cols]))
um <- umap(Xf,
           n_neighbors = 15,
           min_dist    = 0.5,
           metric      = "euclidean",
           n_components = 2,
           ret_model   = TRUE)

Real <- data.frame(UMAP1 = um$embedding[, 1], UMAP2 = um$embedding[, 2],
                   ID = as.character(feature_data$ID),
                   Activity = feature_data[[target_col]])

ggplot() +
  geom_point(data = Real,
             aes(UMAP1, UMAP2, colour = Activity),
             size = 2, alpha = 0.5) +
  my_theme() +
  theme(legend.text  = element_text(size = 14),
        legend.title = element_text(size = 16))





dat <- train #rbind(train, test)
counts <- dat %>% dplyr::count(Activity, ID)

ggplot(counts, aes(x = Activity, y = n, fill = as.factor(ID))) +
  geom_col(position = "stack") +
  my_theme() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) + 
  labs(title = species)



