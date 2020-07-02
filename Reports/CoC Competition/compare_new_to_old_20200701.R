
new_scores <- read_csv("newscores.csv")
old_scores <- read_csv("oldscores.csv")

in_old_but_not_in_new <- anti_join(old_scores, new_scores, by = "AltProjectName")

compare <- new_scores %>%
  select(AltProjectName, "NewPoints" = ExitsToPHPoints) %>%
  left_join(old_scores %>%
              select(AltProjectName, "OldPoints" = ExitsToPHPoints),
            by = "AltProjectName") %>%
  mutate(Difference = NewPoints - OldPoints)


