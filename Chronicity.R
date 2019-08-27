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

rm(Affiliation,
   EmploymentEducation,
   EnrollmentCoC,
   Export,
   Exit,
   ProjectCoC,
   Services,
   Disabilities,
   Funder,
   Geography,
   HealthAndDV,
   IncomeBenefits,
   Inventory,
   Offers,
   Organization,
   Regions,
   Scores,
   Users,
   VeteranCE)

# run on a geography, not project(s)
smallEnrollment <- Enrollment %>%
  select(EnrollmentID, PersonalID, ProjectID, EntryDate, HouseholdID, 
         RelationshipToHoH, LivingSituation, DateToStreetESSH, 
         TimesHomelessPastThreeYears, MonthsHomelessPastThreeYears, 
         DisablingCondition, MoveInDate, UserCreating, CountyServed, ExitDate, 
         ExitAdjust, Destination)

smallProject <- Project %>%
  select(ProjectID, ProjectName, ProjectType)

# calculating chronicity on individual clients, adding a ChronicAtEntry marker

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

# pulling all EEs with the Chronic designation, creating a marker for whether
# each client is in a hh with someone who is Chronic, throwing out all clients
# who are neither Chronic themselves nor in a hh with someone who's Chronic.

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
  filter(ChronicHousehold > 0) %>%
  ungroup() %>% select(-ChronicHousehold)

rm(singlyChronicAtEntry)

# adding in Project data for convenience

allChronicAtEntry <- left_join(allChronicAtEntry, smallProject, by = "ProjectID")

# adds days in ES or SH projects to days homeless prior to entry and if it adds
# up to 365 or more, it marks the client as ConsecutiveChronic

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

rm(smallEnrollment, smallProject, Client, Enrollment)


