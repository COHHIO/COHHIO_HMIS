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
# Missing UDEs
missingUDEs <- servedInDateRange %>%
  mutate(
    Issue = case_when(
      FirstName == "Missing" ~ "Missing Name DQ",
      FirstName %in% c("DKR", "Partial") ~ "Incomplete or DKR Name",
      DOBDataQuality == 99 ~ "Missing DOB",
      DOBDataQuality %in% c(2, 8, 9) ~ "DKR or Approx DOB",
      AgeAtEntry < 0 | AgeAtEntry > 95 ~ "Unlikely DOB or Entry Date",
      SSN == "Missing" ~ "Missing SSN",
      SSN %in% c("Invalid or Incomplete", "DKR") ~ "Invalid or Incomplete SSN",
      RaceNone == 99 ~ "Missing Race",
      RaceNone %in% c(8, 9) ~ "DKR Race",
      Ethnicity == 99 ~ "Missing Ethnicity",
      Ethnicity %in% c(8, 9) ~ "DKR Ethnicity",
      Gender == 99 ~ "Missing Gender",
      Gender %in% c(8, 9) ~ "DKR Gender",
      (AgeAtEntry >= 18 | is.na(AgeAtEntry)) & 
        VeteranStatus == 99 ~ "Missing Veteran Status",
      (AgeAtEntry >= 18 | is.na(AgeAtEntry)) & 
        VeteranStatus %in% c(8, 9) ~ "DKR Veteran Status",
      (AgeAtEntry >= 18 | is.na(AgeAtEntry)) & 
        RelationshipToHoH == 1 &
        VeteranStatus == 0 &
        Destination %in% c(19, 28) ~ "Check Veteran Status for Accuracy"
    )
  ) %>%
  filter(!is.na(Issue)) %>%
  select(PersonalID, ProjectName, Issue, EntryDate, MoveInDate, ExitDate, 
         HouseholdID, RelationshipToHoH, ProjectType, County, Region)

# Missing Enrollment ------------------------------------------------------

missingEnrollment <- servedInDateRange %>%
  mutate(
    Issue = case_when(
    RelationshipToHoH == 99 ~ "Missing Relationship to HoH",
      LivingSituation == 99 |
      LengthOfStay == 99 |
      is.na(PreviousStreetESSH) | # needs more logic
      is.na(DateToStreetESSH) | # needs more logic
      is.na(TimesHomelessPastThreeYears) |
      is.na(MonthsHomelessPastThreeYears) |
      is.na(DisablingCondition)
  ))


# Children Only HH
# Missing HoH
# Multiple HoHs
# Missing or Incorrect SSN
# Missing Data at Entry
# Missing Destination
# Missing SSVF Data
# Missing PATH Data at Entry
# Incorrect PATH Contact Date
# Missing PATH Contact End Date
# Missing PATH Contacts
# Missing PATH Data at Exit
# Questionable Housing Data
# Duplicate EE
# Future Entry Exit
# Incorrect EE Type
# HoHs Entering PH without SPDATs
# HoHs in Shelter without a SPDAT
# Missing Income at Entry
# Missing Income at Exit
# Missing Health Ins at Entry
# Missing NCBs at Entry
# Missing NCBs at Exit
# Disability Subs Not Matching
# Old Disability Type
# SSI/SSDI but no Disability (Q)
# Non HoHs w Svcs or Referrals
# Unpaired Needs
# Service Date Before Entry
# Open Service/Referral
# Unmet Needs
# AP No Recent Referrals
# Need Status Referral Outcomes
# Veterans with No Referral
# Side Door
# Old Outstanding Referrals

