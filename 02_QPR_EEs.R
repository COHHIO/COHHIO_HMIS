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

# this script uses the COHHIOHMIS data to populate the QPR.

library(tidyverse)
library(lubridate)
library(plotly)
library(readxl)
library(janitor)

load("images/COHHIOHMIS.RData")

rm(Affiliation, Disabilities, EmploymentEducation, EnrollmentCoC, Exit, Export, 
   Funder, Geography, HealthAndDV, Offers, Organization, ProjectCoC, Scores, 
   Services, VeteranCE, Users)

goals <- read_xlsx("data/Goals.xlsx")

goals <- goals %>%
  gather(key = "ProjectType", 
         value = "Goal", 
         -SummaryMeasure, -Measure, -Operator) %>%
  mutate(ProjectType = as.numeric(ProjectType)) %>%
  filter(!is.na(Goal))


# Building QPR_EEs ----------------------------------------------------

smallProject <- Project %>%
  select(ProjectID,
         OrganizationName,
         ProjectAKA,
         ProjectName,
         ProjectType,
         GrantType,
         County,
         Region) 


hmisbeds <- Inventory %>%
  filter(HMIS_participating_between(Inventory, FileStart, FileEnd)) %>%
  select(ProjectID) %>% unique()

hpOutreach <- Project %>% 
  filter(ProjectType %in% c(4, 12) &
           operating_between(., FileStart, FileEnd)) %>% 
  select(ProjectID)

rm(Project, Inventory)

allHMISParticipating <- rbind(hmisbeds, hpOutreach)

rm(hmisbeds, hpOutreach)

# the problem with this is you're pulling in all the projects that participated
# during the fileperiod, which would include those who are no longer 
# participating, but have a year's worth of PIT data entered. Like 126 for 
# example. I am not sure how to remedy this except to literally move ALL of this
# to the app, which I think is too much.

smallProject <- smallProject %>% 
  semi_join(allHMISParticipating, by = "ProjectID") 

rm(allHMISParticipating)

smallProject <- smallProject %>%
  filter(!is.na(Region)) %>%
  mutate(
    FriendlyProjectName = if_else(is.na(ProjectAKA), ProjectName, ProjectAKA))

smallProject <- as.data.frame(smallProject)

smallEnrollment <- Enrollment %>% #only pulls in singles or HoHs
  select(
    EnrollmentID,
    PersonalID,
    HouseholdID,
    ProjectID,
    RelationshipToHoH,
    # CountyServed,
    EntryDate,
    MoveInDate,
    ExitDate,
    EntryAdjust,
    MoveInDateAdjust,
    ExitAdjust,
    Destination
  ) %>%
  filter(str_detect(HouseholdID, fixed("s_")) |
           (str_detect(HouseholdID, fixed("h_")) &
              RelationshipToHoH == 1))

smallEnrollment <- as.data.frame(smallEnrollment)

validation <- smallProject %>%
  left_join(smallEnrollment, by = "ProjectID") %>%
  select(ProjectID, ProjectName, ProjectType, EnrollmentID, PersonalID, HouseholdID,
         RelationshipToHoH, EntryDate, MoveInDate, ExitDate, Destination)

# captures all leavers PLUS stayers in either HP or PSH because we include those
# stayers in Permanent Destinations. This is used for LoS and Exits to PH.

QPR_EEs <- smallProject %>%
  left_join(smallEnrollment, by = "ProjectID") %>%
  filter((!is.na(ExitDate) | ProjectType %in% c(3, 9, 12)) &
           served_between(., FileStart, FileEnd) &
           RelationshipToHoH == 1) %>%
  mutate(
    DestinationGroup = case_when(
      Destination %in% c(1, 2, 12, 13, 14, 16, 18, 27) ~ "Temporary",
      Destination %in% c(3, 10, 11, 19:23, 28, 31) ~ "Permanent",
      Destination %in% c(4:7, 15, 25:27, 29) ~ "Institutional",
      Destination %in% c(8, 9, 17, 24, 30, 99) ~ "Other",
      is.na(Destination) ~ "Still in Program"
    ),
    Region = paste("Homeless Planning Region", Region),
    DaysinProject = difftime(ExitAdjust, EntryDate, units = "days")
  ) %>% 
  filter(stayed_between(., FileStart, FileEnd))

RRHEnterers <- smallProject %>%
  left_join(smallEnrollment, by = "ProjectID") %>%
  filter(ProjectType == 13 &
           entered_between(., FileStart, FileEnd) &
           RelationshipToHoH == 1) %>%
  mutate(
    DaysToHouse = difftime(MoveInDateAdjust, EntryDate, units = "days"),
    Region = paste("Homeless Planning Region", Region),
    DaysinProject = difftime(ExitAdjust, EntryAdjust, units = "days")
  )

smallMainstreamBenefits <- IncomeBenefits %>%
  select(InsuranceFromAnySource, BenefitsFromAnySource,
         DataCollectionStage, EnrollmentID, InformationDate) %>%
  group_by(EnrollmentID) %>%
  slice(which.max(InformationDate)) %>%
  ungroup()


QPR_MainstreamBenefits <- smallProject %>%
  left_join(smallEnrollment, by = "ProjectID") %>%
  filter(exited_between(., FileStart, FileEnd) &
           RelationshipToHoH == 1) %>%
  left_join(smallMainstreamBenefits, by = "EnrollmentID") %>%
  select(ProjectName, FriendlyProjectName, PersonalID, HouseholdID, EntryDate,
         EntryAdjust, MoveInDate, MoveInDateAdjust, ExitDate, ExitAdjust,
         InsuranceFromAnySource, BenefitsFromAnySource, DataCollectionStage, 
         InformationDate) %>%
  arrange(ProjectName, HouseholdID)

incomeMostRecent <- IncomeBenefits %>%
  select(IncomeFromAnySource, TotalMonthlyIncome, DataCollectionStage, 
         EnrollmentID, InformationDate) %>%
  group_by(EnrollmentID) %>%
  slice(which.max(InformationDate)) %>%
  ungroup() %>%
  mutate(RecentIncome = TotalMonthlyIncome) %>%
  select(EnrollmentID, RecentIncome)

incomeAtEntry <- IncomeBenefits %>%
  select(IncomeFromAnySource, TotalMonthlyIncome, DataCollectionStage, 
         EnrollmentID, InformationDate) %>%
  group_by(EnrollmentID) %>%
  slice(which.min(InformationDate)) %>%
  ungroup() %>%
  mutate(EntryIncome = TotalMonthlyIncome) %>%
  select(EnrollmentID, EntryIncome)

smallIncomeDiff <- 
  full_join(incomeAtEntry, incomeMostRecent, by = "EnrollmentID")

QPR_Income <- smallProject %>%
  left_join(smallEnrollment, by = "ProjectID") %>%
  filter(served_between(., FileStart, FileEnd) &
           RelationshipToHoH == 1) %>%
  left_join(smallIncomeDiff, by = "EnrollmentID") %>%
  select(ProjectName, FriendlyProjectName, PersonalID, HouseholdID, EntryDate,
         EntryAdjust, MoveInDate, MoveInDateAdjust, ExitDate, ExitAdjust,
         EntryIncome, RecentIncome) %>%
  mutate(Difference = RecentIncome - EntryIncome) %>%
  arrange(ProjectName, HouseholdID)

rm(Client, 
   Enrollment, 
   smallEnrollment,
   smallProject, 
   Regions, 
   smallMainstreamBenefits,
   incomeMostRecent,
   incomeAtEntry,
   smallIncomeDiff,
   IncomeBenefits)

save.image("images/QPR_EEs.RData")

rm(list = ls())


# somecolors <- c("#7156e9", "#56B4E9", "#56e98c", "#e98756", "#e9d056", "#ba56e9",
#                 "#e95684")
# somemorecolors <- c('#f0f9e8','#ccebc5','#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe',
#                     '#08589e')

# RECURRENCE #

# Enrollments to Compare Against ------------------------------------------

# ReportStart <- format.Date(ymd("20190101"), "%m-%d-%Y")
# ReportEnd <- format.Date(mdy("06302019"), "%m-%d-%Y")
# LookbackStart <- format.Date(mdy(ReportStart) - years(1), "%m-%d-%Y")
# LookbackEnd <- format.Date(mdy(ReportEnd) - years(1), "%m-%d-%Y")
# 
# exitedToPH <- QPR_EEs %>%
#   filter(Destination %in% c(3, 10, 11, 19, 20, 21, 22, 23, 26, 28, 31) &
#            ymd(ExitDate) >= mdy(LookbackStart) &
#            ymd(ExitDate) <= mdy(LookbackEnd) &
#            ProjectType %in% c(1, 2, 3, 4, 8, 9, 10, 13))
# 
# earliestExitsToPH <- exitedToPH %>%
#   group_by(PersonalID) %>%
#   summarise(ExitDate = min(ymd(ExitDate)))
# 
# x <- semi_join(exitedToPH, earliestExitsToPH, by = c("PersonalID", "ExitDate"))
# 
# get_dupes(x, PersonalID) %>% view()

# so how do you break the tie when a client has two exits to PH on the same day?

# Qualifying Recurrences --------------------------------------------------



