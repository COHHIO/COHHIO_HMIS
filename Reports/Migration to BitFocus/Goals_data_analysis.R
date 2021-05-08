goals <- read_csv(here("random_data/goals_in_sp.csv"))
summary(goals)

glimpse(goals)

not_nulls_goals <- map(goals, ~sum(!is.na(.))) %>% as.data.frame() %>%
  pivot_longer(cols = everything(),
               names_to = "column_name",
               values_to = "not_null")
write_csv(not_nulls_goals, here("random_data/goals_data_summary.csv"))
