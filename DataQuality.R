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
         DateToStreetESSH, TimesHomelessPastThreeYears, AgeAtEntry,
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
    minPersonalID = min(PersonalID),
    minEntryDate = min(EntryDate),
    Issue = "Children Only Household"
  ) %>%
  filter(maxAge < 18) %>%
  ungroup() %>%
  select(HouseholdID,
         minPersonalID,
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
    minPersonalID = min(PersonalID),
    minEntryDate = min(EntryDate),
    Issue = "No Head of Household"
  ) %>%
  filter(hasHoH == FALSE) %>%
  ungroup() %>%
  select(HouseholdID,
         minPersonalID,
         ProjectName,
         Issue,
         EntryDate = minEntryDate,
         ProjectType)

tooManyHoHs <- servedInDateRange %>%
  filter(RelationshipToHoH == 1) %>%
  group_by(HouseholdID, ProjectType, ProjectName, County, Region) %>%
  summarise(
    HoHsinHousehold = n(),
    minPersonalID = min(PersonalID),
    minEntryDate = min(EntryDate),
    Issue = "Too Many Heads of Household"
  ) %>%
  filter(HoHsinHousehold > 1) %>%
  ungroup() %>%
  select(HouseholdID,
         minPersonalID,
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
missingLivingSituation <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    ProjectID,
    ProjectType,
    ProjectName,
    EntryDate,
    MoveInDate,
    ExitDate,
    AgeAtEntry,
    RelationshipToHoH,
    LivingSituation,
    LengthOfStay,
    LOSUnderThreshold,
    PreviousStreetESSH,
    DateToStreetESSH,
    MonthsHomelessPastThreeYears,
    TimesHomelessPastThreeYears
  ) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           ymd(EntryDate) > mdy("10012016") &
           ((
             ProjectType %in% c(1, 4, 8, 12) & # ES, OUT, HP, and SH
               (
                 is.na(DateToStreetESSH) |
                   is.na(MonthsHomelessPastThreeYears) |
                   is.na(TimesHomelessPastThreeYears) |
                   MonthsHomelessPastThreeYears %in% c(8, 9, 99) |
                   TimesHomelessPastThreeYears %in% c(8, 9, 99) |
                   is.na(LivingSituation) |
                   LivingSituation %in% c(8, 9, 99)
               )
           ) |
             (
               ProjectType %in% c(2, 3, 9, 10, 13) & # TH, PSH, RRH
                 (
                   is.na(LivingSituation) |
                     LivingSituation %in% c(8, 9, 99) |
                     (
                       LivingSituation %in% c(2:9, 12:15, 19:26) &
                         # institution or not homeless
                         (is.na(LOSUnderThreshold) |
                            is.na(PreviousStreetESSH))
                     )
                 )
             ))) %>%
  mutate(Issue = "Incomplete Living Situation")

DKRLivingSituation <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    ProjectID,
    ProjectType,
    ProjectName,
    EntryDate,
    MoveInDate,
    ExitDate,
    AgeAtEntry,
    RelationshipToHoH,
    LivingSituation,
    LengthOfStay,
    LOSUnderThreshold,
    PreviousStreetESSH,
    DateToStreetESSH,
    MonthsHomelessPastThreeYears,
    TimesHomelessPastThreeYears
  ) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           ymd(EntryDate) > mdy("10012016") &
           (
             MonthsHomelessPastThreeYears %in% c(8, 9) |
               TimesHomelessPastThreeYears %in% c(8, 9) |
               LivingSituation %in% c(8, 9)
             
           )
  ) %>%
  mutate(Issue = "DKR Living Situation")

# DisablingCondition at Entry

missingDisabilities <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    HouseholdID,
    RelationshipToHoH,
    DisablingCondition,
    County,
    Region
  ) %>%
  filter(DisablingCondition == 99 |
      is.na(DisablingCondition)) %>%
  mutate(Issue = "Missing Disabling Condition")

smallDisabilities <- Disabilities %>%
  filter(DataCollectionStage == 1, DisabilityResponse != 0) %>%
  select(PersonalID, DisabilitiesID, EnrollmentID, InformationDate, 
         IndefiniteAndImpairs)

incongruentDisabilities <- servedInDateRange %>%
  select(PersonalID,
         EnrollmentID,
         AgeAtEntry,
         ProjectName,
         EntryDate,
         HouseholdID,
         RelationshipToHoH,
         DisablingCondition,
         County,
         Region) %>%
  left_join(smallDisabilities, by = c("PersonalID", "EnrollmentID")) %>%
  group_by(PersonalID,
           EnrollmentID,
           AgeAtEntry,
           ProjectName,
           EntryDate,
           HouseholdID,
           RelationshipToHoH,
           DisablingCondition,
           County,
           Region) %>%
  filter(IndefiniteAndImpairs %in% c(0, 1), DisablingCondition %in% c(0, 1)) %>%
  summarise(HasLongDurationSub = max(IndefiniteAndImpairs)) %>%
  ungroup() %>%
  mutate(Issue = case_when(
    DisablingCondition != HasLongDurationSub ~ 
      "Client's subassessments don't match the Yes/No"
  )) %>%
  filter(!is.na(Issue))

#e INCORRECT, DON'T USE FOR REAL UNTIL THE EXPORT IS FIXED
missingDisabilitySubs <- servedInDateRange %>%
  select(PersonalID,
         EnrollmentID,
         AgeAtEntry,
         ProjectName,
         EntryDate,
         HouseholdID,
         RelationshipToHoH,
         DisablingCondition,
         County,
         Region) %>%
  left_join(smallDisabilities, by = c("PersonalID", "EnrollmentID")) %>%
  filter(DisablingCondition == 1 & is.na(DisabilitiesID)) %>%
  mutate(Issue = "Missing Disability Subs")

## NOT SURE IF THIS EVEN MATTERS ANYMORE?
missingLongDuration <- servedInDateRange %>%
  select(PersonalID,
         EnrollmentID,
         AgeAtEntry,
         ProjectName,
         EntryDate,
         HouseholdID,
         RelationshipToHoH,
         DisablingCondition,
         County,
         Region) %>%
  left_join(smallDisabilities, by = c("PersonalID", "EnrollmentID")) %>%
  filter(IndefiniteAndImpairs == 99) %>%
  mutate(Issue = "Long Duration not answered in subassessment") %>%
  filter(!is.na(Issue))

# MoveInDate
# CountyServed
# CountyPrior



# Check Eligibility, Project Type, Residence Prior ------------------------

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
    AgeAtEntry,
    RelationshipToHoH,
    LivingSituation,
    LengthOfStay,
    LOSUnderThreshold,
    PreviousStreetESSH
  ) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) & 
           ymd(EntryDate) > mdy("10012016") &
           (ProjectType %in% c(2, 3, 9, 10, 13) & # PTCs that require LH status
           (
             is.na(LivingSituation) |
               (
                 LivingSituation %in% c(4:7, 15, 24) & # institution
                   (
                     !LengthOfStay %in% c(2, 3, 10, 11) | # <90 days
                       is.na(LengthOfStay) |
                       PreviousStreetESSH == 0 | # LH prior
                       is.na(PreviousStreetESSH)
                   ) 
               ) |
               (
                 LivingSituation %in% c(2:3, 8, 9, 12:14, 19:23, 25, 26) &
                   # not homeless
                   (
                     !LengthOfStay %in% c(10, 11) |  # <1 week
                       is.na(LengthOfStay) |
                       PreviousStreetESSH == 0 | # LH prior
                       is.na(PreviousStreetESSH)
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



# Duplicate EEs -----------------------------------------------------------

duplicateEEs <- get_dupes(servedInDateRange, PersonalID, ProjectID, EntryDate) %>%
  mutate(Issue = "Duplicate Entry Exits") %>%
  select(HouseholdID, PersonalID, ProjectName, Issue, EntryDate, MoveInDate, 
         ExitDate, ProjectType, County, Region)


# Future Entry Exits ------------------------------------------------------
# PSHs in the old days before Move In Dates would definitely have been entering
# their clients prior to their Entry Date since back then the Entry Date was the
# day they moved in. So they're excused from this prior to Move In Date's existence.

futureEEs <- servedInDateRange %>%
  filter(ymd(EntryDate) > ymd_hms(DateCreated) &
           (ProjectType %in% c(1, 2, 4, 8) |
           (ProjectType %in% c(3, 9) & ymd(EntryDate) > mdy("10012017"))))  %>%
  mutate(Issue = "Future Entry Date") %>%
  select(HouseholdID, PersonalID, ProjectName, Issue, DateCreated, EntryDate, 
         MoveInDate, ExitDate, ProjectType, County, Region)
  
# Incorrect Entry Exit Type -----------------------------------------------



# HoHs Entering PH without SPDATs -----------------------------------------



# HoHs in Shelter without a SPDAT -----------------------------------------



# Missing Income at Entry -------------------------------------------------



# Missing Income at Exit --------------------------------------------------



# Missing Health Ins at Entry ---------------------------------------------



# Missing NCBs at Entry ---------------------------------------------------



# Missing NCBs at Exit ----------------------------------------------------



# Disability Subs Not Matching --------------------------------------------



# Old Disability Type -----------------------------------------------------



# SSI/SSDI but no Disability (Q) ------------------------------------------



# Non HoHs w Svcs or Referrals --------------------------------------------



# Unpaired Needs ----------------------------------------------------------



# Service Date Before Entry -----------------------------------------------



# Unmet Needs -------------------------------------------------------------



# AP No Recent Referrals --------------------------------------------------



# Need Status Referral Outcomes -------------------------------------------



# Veterans with No Referral -----------------------------------------------



# Side Door ---------------------------------------------------------------



# Old Outstanding Referrals -----------------------------------------------



# Service Date Before Entry -----------------------------------------------


