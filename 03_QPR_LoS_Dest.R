# this script uses the COHHIOHMIS data to populate the QPR.

library(tidyverse)
library(lubridate)
library(janitor)

load("images/COHHIOHMIS.RData")

# Successful Placement ----------------------------------------------------

smallProject <- Project %>%
  select(ProjectID,
         ProjectName,
         ProjectType)

smallEnrollment <- Enrollment %>%
  select(
    EnrollmentID,
    PersonalID,
    HouseholdID,
    ProjectID,
    RelationshipToHoH,
    CountyServed,
    EntryDate,
    ExitDate,
    ExitAdjust,
    Destination
  )

Destination <- smallEnrollment %>%
  left_join(smallProject, by = "ProjectID") %>%
  filter(!is.na(ExitDate)) %>%
  mutate(
    DestinationGroup = case_when(
      Destination %in% c(1, 2, 12, 13, 16, 18, 27, 14) ~ "Temporary",
      Destination %in% c(3, 10, 11, 19:23, 28, 31) ~ "Permanent",
      Destination %in% c(4:7, 15, 25:27, 29) ~ "Institutional",
      Destination %in% c(8, 9, 17, 24, 30, 99) ~ "Other"
    )
  )

# this is useless without dates- should be moved into the app
TotalHHExited <- Destination %>%
  filter((str_detect(HouseholdID, fixed("s_")) |
            (str_detect(HouseholdID, fixed("h_")) &
               RelationshipToHoH == 1)) &
           !is.na(ExitDate)) %>%
  group_by(ProjectName) %>%
  summarise(Leavers = n())


PSHandHPExits <- Destination %>%
  filter((DestinationGroup == "Permanent" | is.na(ExitDate)) &
           (
             str_detect(HouseholdID, fixed("s_")) |
               (str_detect(HouseholdID, fixed("h_")) &
                  RelationshipToHoH == 1)
           ))

#also useless without dates, should be moved into app
PSHandHPDestinationByProject <- PSHandHPExits %>%
  group_by(ProjectID) %>%
  summarise()




