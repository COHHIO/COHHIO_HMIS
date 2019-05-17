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

hmisbeds <- Inventory %>%
  filter(HMIS_participating_between(Inventory, FileStart, FileEnd)) %>%
  select(ProjectID) %>% unique()

hpOutreach <- Project %>% 
  filter(ProjectType %in% c(4, 12) &
           operating_between(., FileStart, FileEnd)) %>% 
  select(ProjectID)

x <- rbind(hmisbeds, hpOutreach)

smallProject <- smallProject %>% semi_join(x, by = "ProjectID") 

smallProject <- as.data.frame(smallProject)

smallEnrollment <- Enrollment %>%
  select(
    EnrollmentID,
    PersonalID,
    HouseholdID,
    ProjectID,
    RelationshipToHoH,
    CountyServed,
    EntryDate,
    MoveInDate,
    ExitDate,
    ExitAdjust,
    Destination
  ) %>%
  filter(str_detect(HouseholdID, fixed("s_")) |
           (str_detect(HouseholdID, fixed("h_")) &
              RelationshipToHoH == 1))

smallEnrollment <- as.data.frame(smallEnrollment)
# captures all leavers PLUS all ee's in either HP or PSH
# also limits records to singles and HoHs only
QPREEs <- smallProject %>%
  left_join(smallEnrollment, by = "ProjectID") %>%
  filter((!is.na(ExitDate) | ProjectType %in% c(3, 9, 12)) &
           served_between(., FileStart, FileEnd)) %>%
  mutate(
    DestinationGroup = case_when(
      Destination %in% c(1, 2, 12, 13, 14, 16, 18, 27) ~ "Temporary",
      Destination %in% c(3, 10, 11, 19:23, 28, 31) ~ "Permanent",
      Destination %in% c(4:7, 15, 25:27, 29) ~ "Institutional",
      Destination %in% c(8, 9, 17, 24, 30, 99) ~ "Other"
    ),
    EntryAdjust = case_when(
      ProjectType %in% c(1, 2, 8) ~ EntryDate,
      ProjectType %in% c(3, 9, 13) ~ MoveInDate),
    DaysinProject = difftime(ExitAdjust, EntryAdjust, units = "days")
  )

PermAndRetention <- QPREEs %>%
  filter((DestinationGroup == "Permanent" | is.na(ExitDate)) &
           ProjectType %in% c(3, 9, 12))

PermLeavers <- QPREEs %>%
  filter(DestinationGroup == "Permanent" &
           ProjectType %in% c(1, 2, 4, 8, 13))

# this is useless without dates- should be moved into the app
# for all project types except PSH and HP
TotalHHLeavers <- QPREEs %>%
  filter(!is.na(ExitDate)) %>%
  group_by(ProjectName) %>%
  summarise(Leavers = n())
# for PSH and HP only
TotalHHLeaversAndStayers <- QPREEs %>%
  group_by(ProjectName) %>%
  summarise(LeaversAndStayers = n())

#also useless without dates, should be moved into app
PermAndRetentionByProject <- PermAndRetention %>%
  group_by(ProjectName) %>%
  summarise(PermanentDestOrStayer = n())


# Length of Stay ----------------------------------------------------------



QPREEs <- QPREEs %>% 
  mutate(daysinproject = difftime(ExitAdjust, EntryAdjust, units = "days"))

QPREEs %>% group_by(ProjectID) %>% summarise(avg = mean(daysinproject))






