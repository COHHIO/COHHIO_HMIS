load("images/Data_Quality.RData")
load("images/COHHIOHMIS.RData")
library(HMIS)
library(tidyverse)
library(lubridate)

a <- dq_main %>%
  filter(served_between(., "10012019", "09302020") &
           ProjectType %in% c(1, 2, 3, 8, 9, 13) &
           Issue %in% c(
             "Children Only Household",
             "Missing Relationship to Head of Household",
             "Too Many Heads of Household",
             "No Head of Household"
           ))
write_csv(a, "random_data/hhs.csv")

a <- dq_main %>%
  filter(served_between(., "10012019", "09302020") &
           ProjectType %in% c(1, 2, 3, 8, 9, 13) &
           Issue %in% c(
             "Missing Client Location"
           ))
write_csv(a, "random_data/coclocation.csv")

a <- dq_overlaps %>%
  rename("RecentProjectType" = "ProjectType") %>%
  left_join(Project[c("ProjectName", "ProjectType")], 
            by = c("PreviousProject" = "ProjectName")) %>%
  rename("PreviousProjectType" = "ProjectType") %>%
  filter(served_between(., "10012019", "09302020") &
           RecentProjectType %in% c(1, 2, 3, 8, 9, 13) &
           PreviousProjectType %in% c(1, 2, 3, 8, 9, 13) &
           Issue %in% c(
             "Overlapping Project Stays"
           ))

write_csv(a, "random_data/overlaps.csv")

today <- a
yesterday <- read_csv("random_data/overlaps1.csv")

new <- anti_join(today, yesterday)
new
fixed <- anti_join(yesterday, today)
fixed
