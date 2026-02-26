# take the features that have already been generated and oversample from each of the classes


data1 <- fread(file.path("Data", species, "Nothing_Feature_data.csv"))

# this is how many to sample
target_n <- data1 %>%
  count(Activity) %>%
  summarise(mean = round(mean(n)),0) %>%
  pull(mean)

# and sample them
df_balanced <- data1 %>%
  group_by(Activity) %>%
  group_modify(~ {
    n_current <- nrow(.x)
    if (n_current >= target_n) {
      .x
    } else {
      bind_rows(
        .x,
        .x %>% slice_sample(n = target_n - n_current, replace = TRUE)
      )
    }
  }) %>%
  ungroup()

# and save this
fwrite(df_balanced, file.path("Data", species, "Oversample_Feature_data.csv"))
