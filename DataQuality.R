library(tidyverse)
library(janitor)
library(lubridate)

load("images/COHHIOHMIS.RData")


# Providers to Check ------------------------------------------------------


hmisParticipatingCurrent <- Project %>%
  left_join(Inventory, by = "ProjectID") %>%
  filter(ProjectType == 12 |
           (
             HMIS_participating_between(., FileStart, FileEnd) &
               operating_between(., FileStart, FileEnd) &
               (GrantType != "HOPWA" | is.na(GrantType))
           )) %>%
  select(
    ProjectID,
    OrganizationID,
    OperatingStartDate,
    OperatingEndDate,
    ProjectType,
    GrantType,
    FacilityType,
    ProjectName,
    ProjectAKA,
    OrganizationName,
    County,
    Region
  ) %>% unique()
    
# Clients to Check --------------------------------------------------------

servedInDateRange <- Enrollment %>%
  filter(served_between(., FileStart, FileEnd)) %>%
  left_join(Client, by = "PersonalID") %>%
  select(PersonalID, FirstName, NameDataQuality, SSN, SSNDataQuality,
         DOB, DOBDataQuality, AmIndAKNative, Asian, BlackAfAmerican,
         NativeHIOtherPacific, White, RaceNone, Ethnicity, Gender, VeteranStatus,
         EnrollmentID, ProjectID, EntryDate, HouseholdID, RelationshipToHoH,
         LivingSituation, LengthOfStay, LOSUnderThreshold, PreviousStreetESSH,
         DateToStreetESSH, TimesHomelessPastThreeYears, 
         MonthsHomelessPastThreeYears, DisablingCondition, DateOfEngagement, 
         MoveInDate, EEType, CountyServed, CountyPrior, ExitDate, Destination, 
         ExitAdjust, AgeAtEntry, DateCreated = DateCreated.x) %>%
  inner_join(hmisParticipatingCurrent, by = "ProjectID")

# Missing UDEs ------------------------------------------------------------

missingUDEs <- servedInDateRange %>%
  mutate(
    Issue = case_when(
      FirstName == "Missing" ~ "Missing Name DQ",
      FirstName %in% c("DKR", "Partial") ~ "Incomplete or DKR Name",
      DOBDataQuality == 99 ~ "Missing DOB",
      DOBDataQuality %in% c(2, 8, 9) ~ "DKR or Approx DOB",
      AgeAtEntry < 0 |
        AgeAtEntry > 95 ~ "Incorrect DOB or Entry Date",
      SSN == "Missing" ~ "Missing SSN",
      SSN %in% c("Invalid or Incomplete", "DKR") ~ "Invalid SSN",
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
  select(
    HouseholdID,
    PersonalID,
    ProjectName,
    Issue,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region
  )

# Household Issues --------------------------------------------------------

childrenOnly <- servedInDateRange %>%
  group_by(HouseholdID, ProjectType, ProjectName, County, Region) %>%
  summarise(
    hhMembers = n(),
    maxAge = max(AgeAtEntry),
    maxPersonalID = max(PersonalID),
    minEntryDate = min(EntryDate),
    Issue = "Children Only Household"
  ) %>%
  filter(maxAge < 18) %>%
  ungroup() %>%
  select(HouseholdID,
         maxPersonalID,
         ProjectName,
         Issue,
         EntryDate = minEntryDate,
         ProjectType)

noHoH <- servedInDateRange %>%
  group_by(HouseholdID, ProjectType, ProjectName, County, Region) %>%
  summarise(
    hasHoH = if_else(min(RelationshipToHoH, na.rm = TRUE) != 1,
                     FALSE,
                     TRUE),
    maxPersonalID = max(PersonalID),
    minEntryDate = min(EntryDate),
    Issue = "No Head of Household"
  ) %>%
  filter(hasHoH == FALSE) %>%
  ungroup() %>%
  select(HouseholdID,
         maxPersonalID,
         ProjectName,
         Issue,
         EntryDate = minEntryDate,
         ProjectType)

tooManyHoHs <- servedInDateRange %>%
  filter(RelationshipToHoH == 1) %>%
  group_by(HouseholdID, ProjectType, ProjectName, County, Region) %>%
  summarise(
    HoHsinHousehold = n(),
    maxPersonalID = max(PersonalID),
    minEntryDate = min(EntryDate),
    Issue = "Too Many Heads of Household"
  ) %>%
  filter(HoHsinHousehold > 1) %>%
  ungroup() %>%
  select(HouseholdID,
         maxPersonalID,
         ProjectName,
         Issue,
         EntryDate = minEntryDate,
         ProjectType)

householdIssues <- rbind(tooManyHoHs, noHoH, childrenOnly)
rm(tooManyHoHs, noHoH, childrenOnly)


# Missing Data at Entry ---------------------------------------------------
# Living Situation
# Length of Stay
# LoSUnderThreshold
# PreviousStreetESSH
# DateToStreetESSH
# TimesHomelessPastThreeYears
# MonthsHomelessPastThreeYears
# DisablingCondition
# MoveInDate
# CountyServed
# CountyPrior


# Check Eligibility, Project Type, Residence Prior
checkEligibility <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    ProjectID,
    ProjectType,
    ProjectName,
    EntryDate,
    MoveInDate,
    ExitDate,
    RelationshipToHoH,
    LivingSituation,
    LengthOfStay,
    LOSUnderThreshold
  ) %>%
  filter(RelationshipToHoH == 1 &
           (ProjectType %in% c(2, 3, 9, 10, 13) & # PTC that requires LH status
           (
             is.na(LivingSituation) |
               (
                 LivingSituation %in% c(4:7, 15, 24, 26) & # institution
                   (
                     !LengthOfStay %in% c(2, 3, 11, 12) | # <90 days
                       is.na(LengthOfStay) |
                       
                       LOSUnderThreshold == FALSE | # LH prior
                       is.na(LOSUnderThreshold)
                   ) 
               ) |
               (
                 LivingSituation %in% c(2:3, 8, 9, 12:14, 19:23, 25) &
                   # not homeless
                   (
                     !LengthOfStay %in% c(10, 11) |  # <1 week
                       is.na(LengthOfStay) |
                       LOSUnderThreshold == FALSE |
                       is.na(LOSUnderThreshold) # LH prior
                   ) 
               )
           ))|
           (ProjectType == 12 &
              !LivingSituation %in% c(8, 9, 12:14, 19:23, 25))) %>%
  mutate(Issue = "Check Eligibility")

# Missing PATH Data at Entry
# Missing Destination
# Missing SSVF Data
# Incorrect PATH Contact Date
# Missing PATH Contact End Date
# Missing PATH Contacts
# Missing PATH Data at Exit

# Duplicate EE

duplicateEEs <- get_dupes(servedInDateRange, PersonalID, ProjectID, EntryDate) %>%
  mutate(Issue = "Duplicate Entry Exits") %>%
  select(HouseholdID, PersonalID, ProjectName, Issue, EntryDate, MoveInDate, 
         ExitDate, ProjectType, County, Region)

# Future Entry Exit

futureEEs <- servedInDateRange %>%
  filter(ymd(EntryDate) > ymd_hms(DateCreated) &
           (ProjectType %in% c(1, 2, 4, 8) |
           (ProjectType %in% c(3, 9) & ymd(EntryDate) > mdy("10012017"))))  %>%
  mutate(Issue = "Future Entry Date") %>%
  select(HouseholdID, PersonalID, ProjectName, Issue, DateCreated, EntryDate, 
         MoveInDate, ExitDate, ProjectType, County, Region)
  
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

