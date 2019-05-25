library(tidyverse)
library(lubridate)

load("images/COHHIOHMIS.RData")

rm(Affiliation, Disabilities, EmploymentEducation, EnrollmentCoC, Exit,
   Export, Funder, Geography, HealthAndDV, IncomeBenefits, Offers,
   Organization, ProjectCoC, Scores, Services, Users)

Veterans <- Client %>%
  filter(VeteranStatus == 1) %>%
  select(PersonalID, AmIndAKNative, Asian, BlackAfAmerican, NativeHIOtherPacific,
         White, RaceNone, Ethnicity, Gender)

VeteranHHs <- Veterans %>%
  left_join(Enrollment, by = "PersonalID") %>%
  select(PersonalID, EnrollmentID, ProjectID, EntryDate, HouseholdID, 
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
  left_join(VeteranCE, by = c("PersonalID" = "ClientID"))


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

rm(Client, Enrollment, Inventory, Project, Regions, VeteranCE, Veterans)

save.image("images/Veterans.RData")

