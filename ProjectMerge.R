



load("images/COHHIOHMIS.RData")

x <- Project %>%
  filter(ProjectID %in% c(718, 719, 721, 1353, 1354)) %>%
  mutate(
    AltProjectID = case_when(
      ProjectID %in% c(718, 719, 721) ~ 3000,
      ProjectID %in% c(1353, 1354) ~ 3001
    ),
    AltProjectName = case_when(
      ProjectID %in% c(718, 719, 721) ~ "Butler SPC Combined",
      ProjectID %in% c(1353, 1354) ~ "Clark SPC Combined"
    )
  ) %>%
  select(ProjectID, AltProjectID, ProjectName, AltProjectName)
