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

load("images/COHHIOHMIS.RData")

rm(Affiliation, Disabilities, EmploymentEducation, EnrollmentCoC, Exit,
   Export, Funder, HealthAndDV, IncomeBenefits, Offers, Organization, 
   ProjectCoC, Scores, Services, Users, stray_services)

Veterans <- Client %>%
  filter(VeteranStatus == 1) %>%
  select(PersonalID, AmIndAKNative, Asian, BlackAfAmerican, NativeHIOtherPacific,
         White, RaceNone, Ethnicity, Gender)

VeteranHHs <- Veterans %>%
  left_join(Enrollment, by = "PersonalID") %>%
  select(PersonalID, ProjectID, EntryDate, HouseholdID, 
         RelationshipToHoH, LivingSituation, LengthOfStay, LOSUnderThreshold,
         PreviousStreetESSH, DateToStreetESSH, TimesHomelessPastThreeYears,
         MonthsHomelessPastThreeYears, DisablingCondition, DateOfEngagement,
         MoveInDate, VAMCStation, CountyServed, CountyPrior, ExitDate, 
         Destination, OtherDestination, ExitAdjust, AgeAtEntry)

VeteranHHs <- Project %>%
  select(ProjectID, OrganizationName, OperatingStartDate, OperatingEndDate,
         ProjectType, GrantType, ProjectName, ProjectAKA, RegionName, Region) %>%
  right_join(VeteranHHs, by = "ProjectID")

VeteranHHs <- VeteranHHs %>%
  left_join(VeteranCE, by = c("PersonalID"))


CurrentVeterans <- VeteranHHs %>%
  filter((ProjectType %in% c(1, 2, 4, 8, 12) & (
    ymd(EntryDate) <= today() &
      (is.na(ExitDate) | ymd(ExitDate) > today())
  )) |
    (ProjectType %in% c(3, 9, 13) & (
      ymd(MoveInDate) <= today() &
        (is.na(ExitDate) |
           ymd(ExitDate) > today())
    )))

CurrentVeteranCounts <- CurrentVeterans %>%
  filter(ProjectType %in% c(1, 2, 4, 8)) %>%
  group_by(ProjectName, RegionName) %>%
  summarise(Veterans = n()) %>%
  ungroup()

VeteranEngagement <- CurrentVeterans %>%
  filter(ProjectType %in% c(1, 2, 4, 8)) %>%
  mutate(EngagementStatus = case_when(
    !is.na(PHTrack) & PHTrack != "None" &
      ymd(ExpectedPHDate) >= today() ~ "Has Current Housing Plan",
    is.na(PHTrack) | PHTrack == "None" |
      (!is.na(PHTrack) & (ymd(ExpectedPHDate) < today() |
                            is.na(ExpectedPHDate))) ~ "No Current Housing Plan"
  )) %>%
  select(ProjectName, ProjectType, RegionName, PersonalID, PHTrack, 
         ExpectedPHDate, EngagementStatus)

VetEngagementSummary <- VeteranEngagement %>%
  group_by(ProjectName, ProjectType, RegionName, EngagementStatus) %>%
  summarise(CurrentVeteranCount = n()) %>%
  spread(key = EngagementStatus, value = CurrentVeteranCount) %>%
  rename(HasCurrentHousingPlan = `Has Current Housing Plan`,
         NoCurrentHousingPlan = `No Current Housing Plan`) 

VetEngagementSummary[is.na(VetEngagementSummary)] <- 0 

VetEngagementSummary <- VetEngagementSummary %>%
  mutate(
    Summary =
      case_when(
        HasCurrentHousingPlan == 0 &
          NoCurrentHousingPlan == 1 ~
          "This veteran has no current Housing Plan",
        HasCurrentHousingPlan == 0 &
          NoCurrentHousingPlan > 1  ~
          "None of these veterans have current Housing Plans",
        HasCurrentHousingPlan == 1 &
          NoCurrentHousingPlan == 0 ~
          "This veteran has a current Housing Plan!",
        HasCurrentHousingPlan > 1 &
          NoCurrentHousingPlan == 0  ~
          "All veterans in this project have current Housing Plans!",
        HasCurrentHousingPlan == 1 &
          NoCurrentHousingPlan > 0 ~
          paste(HasCurrentHousingPlan, 
                "of these veterans has a current Housing Plan"),
        HasCurrentHousingPlan > 1 &
          NoCurrentHousingPlan > 0 ~
          paste(HasCurrentHousingPlan, 
                "of these veterans have current Housing Plans")
      )
  )

rm(Client, CaseManagers, Enrollment, Inventory, Project, Regions, VeteranCE, 
   Veterans, CurrentVeterans, VeteranEngagement, VeteranHHs)

save.image("images/Veterans.RData")


