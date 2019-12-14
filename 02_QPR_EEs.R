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
library(janitor)

load("images/COHHIOHMIS.RData")

rm(Affiliation, CaseManagers, Disabilities, EmploymentEducation, EnrollmentCoC, 
   Exit, Export, Funder, HealthAndDV, Offers, ProjectCoC, Scores, VeteranCE, 
   Users, Referrals, stray_services, Inventory)

# decided to continue to use a separate file for Goals (instead of building it
# in a tribble) because this way the CoC team can review it more easily.
goals <- read_csv("data/Goals.csv", col_types = "cccdddddddd")

goals <- goals %>%
  gather(key = "ProjectType", 
         value = "Goal", 
         -SummaryMeasure, -Measure, -Operator) %>%
  mutate(ProjectType = as.numeric(ProjectType)) %>%
  filter(!is.na(Goal))

# Building qpr_leavers ----------------------------------------------------

smallProject <- Project %>%
  select(ProjectID,
         OrganizationName,
         OperatingStartDate,
         OperatingEndDate,
         ProjectAKA,
         ProjectName,
         ProjectType,
         HMISParticipatingProject,
         GrantType,
         ProjectCounty,
         ProjectRegion) %>%
  filter(HMISParticipatingProject == 1 &
           operating_between(., FileStart, FileEnd) &
           !is.na(ProjectRegion)) %>%
  mutate(
    FriendlyProjectName = if_else(is.na(ProjectAKA), ProjectName, ProjectAKA))

rm(Project)

smallEnrollment <- Enrollment %>% 
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
  ) 

validation <- smallProject %>%
  left_join(smallEnrollment, by = "ProjectID") %>%
  select(
    ProjectID,
    ProjectName,
    ProjectType,
    EnrollmentID,
    PersonalID,
    HouseholdID,
    RelationshipToHoH,
    EntryDate,
    MoveInDate,
    MoveInDateAdjust,
    ExitDate,
    Destination
  ) %>%
  filter(!is.na(EntryDate))

smallEnrollment <- smallEnrollment %>%
  filter(str_detect(HouseholdID, fixed("s_")) |
           (str_detect(HouseholdID, fixed("h_")) &
              RelationshipToHoH == 1)) #<- only pulls in hohs and singles

# captures all leavers PLUS stayers in either HP or PSH because we include those
# stayers in Permanent Destinations. This is used for LoS and Exits to PH.

qpr_leavers <- smallProject %>%
  left_join(smallEnrollment, by = "ProjectID") %>%
  filter((!is.na(ExitDate) | ProjectType %in% c(3, 9, 12)) &
           served_between(., FileStart, FileEnd) &
           RelationshipToHoH == 1) %>%
  mutate(
    DestinationGroup = case_when(
      Destination %in% c(1, 2, 12, 13, 14, 16, 18, 27, 32, 35) ~ "Temporary",
      Destination %in% c(3, 10, 11, 19:23, 28, 31, 33, 34, 36) ~ "Permanent",
      Destination %in% c(4:7, 15, 25:27, 29) ~ "Institutional",
      Destination %in% c(8, 9, 17, 24, 30, 37, 99) ~ "Other",
      is.na(Destination) ~ "Still in Program"
    ),
    ProjectRegion = paste("Homeless Planning Region", ProjectRegion),
    DaysinProject = difftime(ExitAdjust, EntryDate, units = "days")
  ) %>% 
  filter(stayed_between(., FileStart, FileEnd))

qpr_rrh_enterers <- smallProject %>%
  left_join(smallEnrollment, by = "ProjectID") %>%
  filter(ProjectType == 13 &
           entered_between(., FileStart, FileEnd) &
           RelationshipToHoH == 1) %>%
  mutate(
    DaysToHouse = difftime(MoveInDateAdjust, EntryDate, units = "days"),
    Region = paste("Homeless Planning Region", ProjectRegion),
    DaysinProject = difftime(ExitAdjust, EntryAdjust, units = "days")
  )

smallMainstreamBenefits <- IncomeBenefits %>%
  select(InsuranceFromAnySource, BenefitsFromAnySource,
         DataCollectionStage, EnrollmentID, InformationDate) %>%
  group_by(EnrollmentID) %>%
  slice(which.max(InformationDate)) %>% # most recent answer per Enrollment
  ungroup()


qpr_benefits <- smallProject %>%
  left_join(smallEnrollment, by = "ProjectID") %>%
  filter(exited_between(., FileStart, FileEnd) &
           RelationshipToHoH == 1) %>%
  left_join(smallMainstreamBenefits, by = "EnrollmentID") %>%
  select(ProjectName, FriendlyProjectName, PersonalID, HouseholdID, EntryDate,
         EntryAdjust, MoveInDate, MoveInDateAdjust, ExitDate, ExitAdjust,
         InsuranceFromAnySource, BenefitsFromAnySource, DataCollectionStage, 
         InformationDate, ProjectRegion, ProjectCounty, ProjectType) %>%
  mutate(ProjectType = case_when(
    ProjectType == 1 ~ "Emergency Shelters", 
    ProjectType == 2 ~ "Transitional Housing", 
    ProjectType == 3 ~ "Permanent Supportive Housing", 
    ProjectType == 4 ~ "Street Outreach", 
    ProjectType == 8 ~ "Safe Haven",
    ProjectType == 9 ~ "Permanent Supportive Housing", 
    ProjectType == 12 ~ "Prevention",  
    ProjectType == 13 ~ "Rapid Rehousing"
  )) %>%
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

qpr_income <- smallProject %>%
  left_join(smallEnrollment, by = "ProjectID") %>%
  filter(served_between(., FileStart, FileEnd) &
           RelationshipToHoH == 1) %>%
  left_join(smallIncomeDiff, by = "EnrollmentID") %>%
  select(ProjectName, FriendlyProjectName, PersonalID, HouseholdID, EntryDate,
         EntryAdjust, MoveInDate, MoveInDateAdjust, ExitDate, ExitAdjust,
         EntryIncome, RecentIncome, ProjectRegion, ProjectCounty, ProjectType) %>%
  mutate(
    Difference = RecentIncome - EntryIncome,
    ProjectType = case_when(
      ProjectType == 1 ~ "Emergency Shelters",
      ProjectType == 2 ~ "Transitional Housing",
      ProjectType == 3 ~ "Permanent Supportive Housing",
      ProjectType == 4 ~ "Street Outreach",
      ProjectType == 8 ~ "Safe Haven",
      ProjectType == 9 ~ "Permanent Supportive Housing",
      ProjectType == 12 ~ "Prevention",
      ProjectType == 13 ~ "Rapid Rehousing"
    )
  ) %>% 
  arrange(ProjectName, HouseholdID)

qpr_spending <- Services %>%
  left_join(Enrollment,
            by = c("EnrollmentID", "PersonalID",
                   "ServiceProvider" = "ProjectName")) %>%
  left_join(smallProject, by = c("ProjectID", "ProjectType")) %>%
  select(
    PersonalID,
    OrganizationName,
    ProjectName,
    ProjectRegion,
    ProjectType,
    Amount,
    Description,
    RelationshipToHoH,
    ServiceStartDate,
    EntryDate,
    MoveInDateAdjust,
    ExitDate
  ) %>% 
  filter((ProjectType == 13 & !is.na(MoveInDateAdjust) | 
           ProjectType == 12) &
           RelationshipToHoH == 1 &
           !is.na(Amount)) %>%
  select(-RelationshipToHoH)
  
rm(Client, 
   Enrollment, 
   Organization,
   Services,
   smallEnrollment,
   smallProject, 
   regions, 
   smallMainstreamBenefits,
   incomeMostRecent,
   incomeAtEntry,
   smallIncomeDiff,
   IncomeBenefits)

save.image("images/QPR_EEs.RData")



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
# exitedToPH <- qpr_leavers %>%
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



