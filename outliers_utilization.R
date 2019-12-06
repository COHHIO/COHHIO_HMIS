utilization_unit <- utilization_unit %>% ungroup()

names(utilization_unit) <- 
  c("ProjectID", "ProjectName", "ProjectType", "FilePeriod",
    "x1", "x2", "x3", "x4", "x5", "x6", "x7", "x8", "x9", "x10", "x11", "x12", 
    "x13", "x14", "x15", "x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23", 
    "x24")

utilization_unit <- utilization_unit %>%
  mutate(
    x1 = if_else(x1 %in% c("NaN", "Inf"), 0, x1),
    x2 = if_else(x2 %in% c("NaN", "Inf"), 0, x2),
    x3 = if_else(x3 %in% c("NaN", "Inf"), 0, x3),
    x4 = if_else(x4 %in% c("NaN", "Inf"), 0, x4),
    x5 = if_else(x5 %in% c("NaN", "Inf"), 0, x5),
    x6 = if_else(x5 %in% c("NaN", "Inf"), 0, x6),
    x7 = if_else(x7 %in% c("NaN", "Inf"), 0, x7),
    x8 = if_else(x8 %in% c("NaN", "Inf"), 0, x8),
    x9 = if_else(x9 %in% c("NaN", "Inf"), 0, x9),
    x10 = if_else(x10 %in% c("NaN", "Inf"), 0, x10),
    x11 = if_else(x11 %in% c("NaN", "Inf"), 0, x11),
    x12 = if_else(x12 %in% c("NaN", "Inf"), 0, x12),
    x13 = if_else(x13 %in% c("NaN", "Inf"), 0, x13),
    x14 = if_else(x14 %in% c("NaN", "Inf"), 0, x14),
    x15 = if_else(x15 %in% c("NaN", "Inf"), 0, x15),
    x16 = if_else(x16 %in% c("NaN", "Inf"), 0, x16),
    x17 = if_else(x17 %in% c("NaN", "Inf"), 0, x17),
    x18 = if_else(x18 %in% c("NaN", "Inf"), 0, x18),
    x19 = if_else(x19 %in% c("NaN", "Inf"), 0, x19),
    x20 = if_else(x20 %in% c("NaN", "Inf"), 0, x20),
    x21 = if_else(x21 %in% c("NaN", "Inf"), 0, x21),
    x22 = if_else(x22 %in% c("NaN", "Inf"), 0, x22),
    x23 = if_else(x23 %in% c("NaN", "Inf"), 0, x23),
    x24 = if_else(x24 %in% c("NaN", "Inf"), 0, x24)
  )

y <- utilization_unit %>%
  group_by(ProjectName, ProjectID, ProjectType) %>%
  filter(!is.na(FilePeriod)) %>%
  mutate(highest = case_when(
    x13 >= x14 &
      x13 >= x15 &
      x13 >= x16 &
      x13 >= x17 &
      x13 >= x18 &
      x13 >= x19 &
      x13 >= x20 &
      x13 >= x21 &
      x13 >= x22 &
      x13 >= x23 &
      x13 >= x24 ~ x13,
    x13 >= x14 &
      x13 >= x15 &
      x13 >= x16 &
      x13 >= x17 &
      x13 >= x18 &
      x13 >= x19 &
      x13 >= x20 &
      x13 >= x21 &
      x13 >= x22 &
      x13 >= x23 &
      x13 >= x24 ~ x13,    
    x14 >= x13 &
      x14 >= x15 &
      x14 >= x16 &
      x14 >= x17 &
      x14 >= x18 &
      x14 >= x19 &
      x14 >= x20 &
      x14 >= x21 &
      x14 >= x22 &
      x14 >= x23 &
      x14 >= x24 ~ x14,    
    x15 >= x14 &
      x15 >= x13 &
      x15 >= x16 &
      x15 >= x17 &
      x15 >= x18 &
      x15 >= x19 &
      x15 >= x20 &
      x15 >= x21 &
      x15 >= x22 &
      x15 >= x23 &
      x15 >= x24 ~ x15,    
    x16 >= x14 &
      x16 >= x15 &
      x16 >= x13 &
      x16 >= x17 &
      x16 >= x18 &
      x16 >= x19 &
      x16 >= x20 &
      x16 >= x21 &
      x16 >= x22 &
      x16 >= x23 &
      x16 >= x24 ~ x16,    
    x17 >= x14 &
      x17 >= x15 &
      x17 >= x16 &
      x17 >= x13 &
      x17 >= x18 &
      x17 >= x19 &
      x17 >= x20 &
      x17 >= x21 &
      x17 >= x22 &
      x17 >= x23 &
      x17 >= x24 ~ x17,    
    x18 >= x14 &
      x18 >= x15 &
      x18 >= x16 &
      x18 >= x17 &
      x18 >= x13 &
      x18 >= x19 &
      x18 >= x20 &
      x18 >= x21 &
      x18 >= x22 &
      x18 >= x23 &
      x18 >= x24 ~ x18,    
    x19 >= x14 &
      x19 >= x15 &
      x19 >= x16 &
      x19 >= x17 &
      x19 >= x18 &
      x19 >= x13 &
      x19 >= x20 &
      x19 >= x21 &
      x19 >= x22 &
      x19 >= x23 &
      x19 >= x24 ~ x19,    
    x20 >= x14 &
      x20 >= x15 &
      x20 >= x16 &
      x20 >= x17 &
      x20 >= x18 &
      x20 >= x19 &
      x20 >= x13 &
      x20 >= x21 &
      x20 >= x22 &
      x20 >= x23 &
      x20 >= x24 ~ x20,    
    x21 >= x14 &
      x21 >= x15 &
      x21 >= x16 &
      x21 >= x17 &
      x21 >= x18 &
      x21 >= x19 &
      x21 >= x20 &
      x21 >= x13 &
      x21 >= x22 &
      x21 >= x23 &
      x21 >= x24 ~ x21,    
    x22 >= x14 &
      x22 >= x15 &
      x22 >= x16 &
      x22 >= x17 &
      x22 >= x18 &
      x22 >= x19 &
      x22 >= x20 &
      x22 >= x21 &
      x22 >= x13 &
      x22 >= x23 &
      x22 >= x24 ~ x22,    
    x23 >= x14 &
      x23 >= x15 &
      x23 >= x16 &
      x23 >= x17 &
      x23 >= x18 &
      x23 >= x19 &
      x23 >= x20 &
      x23 >= x21 &
      x23 >= x22 &
      x23 >= x13 &
      x23 >= x24 ~ x23,    
    x24 >= x14 &
      x24 >= x15 &
      x24 >= x16 &
      x24 >= x17 &
      x24 >= x18 &
      x24 >= x19 &
      x24 >= x20 &
      x24 >= x21 &
      x24 >= x22 &
      x24 >= x13 &
      x24 >= x23 ~ x24
      
  ),
  lowest = case_when(
    x13 <= x14 &
      x13 <= x15 &
      x13 <= x16 &
      x13 <= x17 &
      x13 <= x18 &
      x13 <= x19 &
      x13 <= x20 &
      x13 <= x21 &
      x13 <= x22 &
      x13 <= x23 &
      x13 <= x24 ~ x13,
    x13 <= x14 &
      x13 <= x15 &
      x13 <= x16 &
      x13 <= x17 &
      x13 <= x18 &
      x13 <= x19 &
      x13 <= x20 &
      x13 <= x21 &
      x13 <= x22 &
      x13 <= x23 &
      x13 <= x24 ~ x13,    
    x14 <= x13 &
      x14 <= x15 &
      x14 <= x16 &
      x14 <= x17 &
      x14 <= x18 &
      x14 <= x19 &
      x14 <= x20 &
      x14 <= x21 &
      x14 <= x22 &
      x14 <= x23 &
      x14 <= x24 ~ x14,    
    x15 <= x14 &
      x15 <= x13 &
      x15 <= x16 &
      x15 <= x17 &
      x15 <= x18 &
      x15 <= x19 &
      x15 <= x20 &
      x15 <= x21 &
      x15 <= x22 &
      x15 <= x23 &
      x15 <= x24 ~ x15,    
    x16 <= x14 &
      x16 <= x15 &
      x16 <= x13 &
      x16 <= x17 &
      x16 <= x18 &
      x16 <= x19 &
      x16 <= x20 &
      x16 <= x21 &
      x16 <= x22 &
      x16 <= x23 &
      x16 <= x24 ~ x16,    
    x17 <= x14 &
      x17 <= x15 &
      x17 <= x16 &
      x17 <= x13 &
      x17 <= x18 &
      x17 <= x19 &
      x17 <= x20 &
      x17 <= x21 &
      x17 <= x22 &
      x17 <= x23 &
      x17 <= x24 ~ x17,    
    x18 <= x14 &
      x18 <= x15 &
      x18 <= x16 &
      x18 <= x17 &
      x18 <= x13 &
      x18 <= x19 &
      x18 <= x20 &
      x18 <= x21 &
      x18 <= x22 &
      x18 <= x23 &
      x18 <= x24 ~ x18,    
    x19 <= x14 &
      x19 <= x15 &
      x19 <= x16 &
      x19 <= x17 &
      x19 <= x18 &
      x19 <= x13 &
      x19 <= x20 &
      x19 <= x21 &
      x19 <= x22 &
      x19 <= x23 &
      x19 <= x24 ~ x19,    
    x20 <= x14 &
      x20 <= x15 &
      x20 <= x16 &
      x20 <= x17 &
      x20 <= x18 &
      x20 <= x19 &
      x20 <= x13 &
      x20 <= x21 &
      x20 <= x22 &
      x20 <= x23 &
      x20 <= x24 ~ x20,    
    x21 <= x14 &
      x21 <= x15 &
      x21 <= x16 &
      x21 <= x17 &
      x21 <= x18 &
      x21 <= x19 &
      x21 <= x20 &
      x21 <= x13 &
      x21 <= x22 &
      x21 <= x23 &
      x21 <= x24 ~ x21,    
    x22 <= x14 &
      x22 <= x15 &
      x22 <= x16 &
      x22 <= x17 &
      x22 <= x18 &
      x22 <= x19 &
      x22 <= x20 &
      x22 <= x21 &
      x22 <= x13 &
      x22 <= x23 &
      x22 <= x24 ~ x22,    
    x23 <= x14 &
      x23 <= x15 &
      x23 <= x16 &
      x23 <= x17 &
      x23 <= x18 &
      x23 <= x19 &
      x23 <= x20 &
      x23 <= x21 &
      x23 <= x22 &
      x23 <= x13 &
      x23 <= x24 ~ x23,    
    x24 <= x14 &
      x24 <= x15 &
      x24 <= x16 &
      x24 <= x17 &
      x24 <= x18 &
      x24 <= x19 &
      x24 <= x20 &
      x24 <= x21 &
      x24 <= x22 &
      x24 <= x13 &
      x24 <= x23 ~ x24
  )) %>%
  select(ProjectID, ProjectName, ProjectType, FilePeriod, lowest, highest)

y <- y %>%
  mutate(flag = case_when(
    FilePeriod < .2 ~ "2019 rate too low",
    FilePeriod > 1.1 ~ "2019 rate too high",
    lowest < .1 ~ "Lowest month less than 10%",
    highest > 1.2 ~ "Highest month greater than 120%"
  ))

write_csv(y, "HIC_outliers.csv")

non_participating <- Project %>%
  filter(HMISParticipatingProject == 0 &
           ProjectType %in% c(1, 2, 3, 8) &
           operating_between(., "10012018", FileEnd))

write_csv(non_participating, "non_participating.csv")
