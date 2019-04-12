library(tidyverse)
library(lubridate)

load("data/COHHIOHMIS.Rdata")

rm(Affiliation, EmploymentEducation, EnrollmentCoC, Export, ProjectCoC, Services)

# run on a geography, not project(s)
smallEnrollment <- Enrollment %>%
  select(EnrollmentID, PersonalID, ProjectID, EntryDate, HouseholdID, 
         RelationshipToHoH, LivingSituation, DateToStreetESSH, 
         TimesHomelessPastThreeYears, MonthsHomelessPastThreeYears, 
         DisablingCondition, MoveInDate, UserCreating, CountyServed, ExitDate, 
         ExitAdjust, Destination)

smallProject <- Project %>%
  select(ProjectID, ProjectName, ProjectType)

singlyChronicAtEntry <- 
  smallEnrollment %>%
  filter(
      ((ymd(DateToStreetESSH) + years(1) <= ymd(EntryDate)) |
      (
        MonthsHomelessPastThreeYears %in% c(112, 113) &
          TimesHomelessPastThreeYears == 4
      )) &
      DisablingCondition == 1
  ) %>%
  mutate(ChronicAtEntry = 1)

allChronicAtEntry <- left_join(smallEnrollment, singlyChronicAtEntry) %>%
  group_by(HouseholdID) %>%
  mutate(ChronicHousehold = sum(ChronicAtEntry, na.rm = TRUE)) %>%
  filter(ChronicHousehold == 1) %>%
  ungroup() %>% select(-ChronicHousehold)

rm(singlyChronicAtEntry)

allChronicAtEntry <- left_join(allChronicAtEntry, smallProject, by = "ProjectID")

agedIntoChronicity <- allChronicAtEntry %>%
  filter(ProjectType %in% c(1, 8)) %>%
  mutate(
    DaysHomelessInProject = if_else(ProjectType %in% c(1, 2, 8),
                                    difftime(ymd(ExitAdjust),
                                             ymd(EntryDate),
                                             units = "days"),
                                    0),
    DaysBetweenHomeless = difftime(ymd(EntryDate),
                                   if_else(
                                     is.na(ymd(DateToStreetESSH)), 
                                     ymd(EntryDate), 
                                     ymd(DateToStreetESSH)
                                   ),
                                   units = "days"),
    ConsecutiveChronic = DaysBetweenHomeless +  DaysHomelessInProject >= 365
  ) %>%
  filter(ConsecutiveChronic == TRUE)
