# COHHIO_HMIS
# Copyright (C) 2019  Coalition on Homelessness and Housing in Ohio (COHHIO)
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details at 
#<https://www.gnu.org/licenses/>.

library(tidyverse)
library(lubridate)

load("images/COHHIOHMIS.Rdata")

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

allChronicAtEntry <- 
  full_join(
    smallEnrollment,
    singlyChronicAtEntry,
    by = c(
      "EnrollmentID",
      "PersonalID",
      "ProjectID",
      "EntryDate",
      "HouseholdID",
      "RelationshipToHoH",
      "LivingSituation",
      "DateToStreetESSH",
      "TimesHomelessPastThreeYears",
      "MonthsHomelessPastThreeYears",
      "DisablingCondition",
      "MoveInDate",
      "UserCreating",
      "CountyServed",
      "ExitDate",
      "ExitAdjust",
      "Destination"
    )
  ) %>%
  group_by(HouseholdID) %>%
  mutate(ChronicHousehold = sum(ChronicAtEntry, na.rm = TRUE)) %>%
  filter(ChronicHousehold == 1) %>%
  ungroup() %>% select(-ChronicHousehold)

rm(singlyChronicAtEntry)

allChronicAtEntry <- left_join(allChronicAtEntry, smallProject, by = "ProjectID")

agedIntoChronicity <- allChronicAtEntry %>%
  filter(ProjectType %in% c(1, 8)) %>%
  mutate(
    DaysHomelessInProject = difftime(ymd(ExitAdjust),
                                             ymd(EntryDate),
                                             units = "days"),
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

load("images/QPR_SPDATs.RData")


