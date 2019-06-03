library(tidyverse)
library(janitor)
library(lubridate)

load("images/COHHIOHMIS.RData")


# Providers to Check ------------------------------------------------------

hmisParticipatingCurrent <- Project %>%
  left_join(Inventory, by = "ProjectID") %>%
  filter(HMIS_participating_between(., FileStart, FileEnd) &
         operating_between(., FileStart, FileEnd) &
         (GrantType != "HOPWA" | is.na(GrantType))) %>%
  select(ProjectID, OrganizationID, OperatingStartDate, OperatingEndDate,
         ProjectType, GrantType, FacilityType, ProjectName, ProjectAKA,
         OrganizationName, County, Region) %>% unique()
    
# Clients to Check --------------------------------------------------------

servedInDateRange <- Enrollment %>%
  filter(served_between(., FileStart, FileEnd)) %>%
  left_join(Client, by = "PersonalID") %>%
  select(PersonalID, FirstName, NameDataQuality, SSN, SSNDataQuality,
         DOB, DOBDataQuality, AmIndAKNative, Asian, BlackAfAmerican,
         NativeHIOtherPacific, White, RaceNone, Ethnicity, Gender, VeteranStatus,
         EnrollmentID, ProjectID, EntryDate, HouseholdID, RelationshipToHoH,
         LivingSituation, LengthOfStay, LOSUnderThreshold, PreviousStreetESSH,
         DateToStreetESSH, TimesHomelessPastThreeYears, MonthsHomelessPastThreeYears,
         DisablingCondition, DateOfEngagement, MoveInDate, EEType, CountyServed,
         CountyPrior, ExitDate, Destination, ExitAdjust, AgeAtEntry) %>%
  inner_join(hmisParticipatingCurrent, by = "ProjectID")

# Missing Data ------------------------------------------------------------

missingUDEs <- servedInDateRange %>%
  mutate(
    Issue = case_when(
      is.na(DOB) | DOBDataQuality == 99 ~ "Missing DOB",
      NameDataQuality == 99 ~ "Missing Name DQ",
      SSN == "missing" ~ "Missing SSN",
      RaceNone == 1 ~ "Missing Race",
      Ethnicity == 99 ~ "Missing Ethnicity",
      Gender == 99 ~ "Missing Gender",
      VeteranStatus == 99 ~ "Missing Veteran Status"
    )
  ) %>%
  filter(!is.na(Issue)) %>%
  select(PersonalID, ProjectName, Issue, EntryDate, MoveInDate, ExitDate, HouseholdID,
         RelationshipToHoH, ProjectType, County, Region)

missingEnrollment <- Enrollment %>%
  filter(
    RelationshipToHoH == 99 |
      LivingSituation == 99 |
      LengthOfStay == 99 |
      is.na(PreviousStreetESSH) | # needs more logic
      is.na(DateToStreetESSH) | # needs more logic
      is.na(TimesHomelessPastThreeYears) |
      is.na(MonthsHomelessPastThreeYears) |
      is.na(DisablingCondition)
  )
