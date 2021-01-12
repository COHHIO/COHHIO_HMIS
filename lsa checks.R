load("images/Data_Quality.RData")

a <- dq_main %>%
  filter(served_between(., "10012018", "09302020") &
           ProjectType %in% c(1, 2, 3, 8, 9, 13) &
           Issue %in% c(
             "Children Only Household",
             "Too Many Heads of Household",
             "No Head of Household"
           ))
write_csv(a, "hhs.csv")

a <- dq_main %>%
  filter(served_between(., "10012018", "09302020") &
           ProjectType %in% c(1, 2, 3, 8, 9, 13) &
           Issue %in% c(
             "Missing Client Location"
           ))
write_csv(a, "coclocation.csv")

a <- dq_main %>%
  filter(served_between(., "10012018", "09302020") &
           ProjectType %in% c(1, 2, 3, 8, 9, 13) &
           Issue %in% c(
             "Overlapping Project Stays"
           ))
write_csv(a, "overlaps.csv")


