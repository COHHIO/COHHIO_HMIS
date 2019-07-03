library(tidyverse)
library(janitor)
library(lubridate)
start <- now()
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
    ),
    Type = case_when(
        FirstName == "Missing" ~ "Error",
        FirstName %in% c("DKR", "Partial") ~ "Warning",
        DOBDataQuality == 99 ~ "Error",
        DOBDataQuality %in% c(2, 8, 9) ~ "Warning",
        AgeAtEntry < 0 |
          AgeAtEntry > 95 ~ "Error",
        SSN == "Missing" ~ "Error",
        SSN %in% c("Invalid or Incomplete", "DKR") ~ "Error",
        RaceNone == 99 ~ "Error",
        RaceNone %in% c(8, 9) ~ "Warning",
        Ethnicity == 99 ~ "Error",
        Ethnicity %in% c(8, 9) ~ "Warning",
        Gender == 99 ~ "Error",
        Gender %in% c(8, 9) ~ "Warning",
        (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
          VeteranStatus == 99 ~ "Error",
        (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
          VeteranStatus %in% c(8, 9) ~ "Warning",
        (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
          RelationshipToHoH == 1 &
          VeteranStatus == 0 &
          Destination %in% c(19, 28) ~ "Warning"
    )
  ) %>%
  filter(!is.na(Issue)) %>%
  select(
    HouseholdID,
    PersonalID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region
  )

# Household Issues --------------------------------------------------------

childrenOnly <- servedInDateRange %>%
  filter(GrantType != "RHY") %>% # not checking for children-only hhs for RHY
  group_by(HouseholdID, ProjectType, ProjectName, County, Region) %>%
  summarise(
    hhMembers = n(),
    maxAge = max(AgeAtEntry),
    PersonalID = min(PersonalID),
    EntryDate = min(EntryDate),
    MoveInDate = min(MoveInDate),
    ExitDate = min(ExitDate),
    Issue = "Children Only Household"
  ) %>%
  filter(maxAge < 18) %>%
  ungroup() %>%
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
         Region)

noHoH <- servedInDateRange %>%
  group_by(HouseholdID, ProjectType, ProjectName, County, Region) %>%
  summarise(
    hasHoH = if_else(min(RelationshipToHoH, na.rm = TRUE) != 1,
                     FALSE,
                     TRUE),
    PersonalID = min(PersonalID),
    EntryDate = min(EntryDate),
    MoveInDate = min(MoveInDate),
    ExitDate = min(ExitDate),
    Issue = "No Head of Household"
  ) %>%
  filter(hasHoH == FALSE) %>%
  ungroup() %>%
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

tooManyHoHs <- servedInDateRange %>%
  filter(RelationshipToHoH == 1) %>%
  group_by(HouseholdID, ProjectType, ProjectName, County, Region) %>%
  summarise(
    HoHsinHousehold = n(),
    PersonalID = min(PersonalID),
    EntryDate = min(EntryDate),
    MoveInDate = min(MoveInDate),
    ExitDate = min(ExitDate),
    Issue = "Too Many Heads of Household"
  ) %>%
  filter(HoHsinHousehold > 1) %>%
  ungroup() %>%
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

householdIssues <- rbind(tooManyHoHs, noHoH, childrenOnly) %>%
  mutate(Type = "Error")
rm(tooManyHoHs, noHoH, childrenOnly)

# Missing Data at Entry ---------------------------------------------------
# Living Situation,  Length of Stay, LoSUnderThreshold, PreviousStreetESSH,
# DateToStreetESSH, TimesHomelessPastThreeYears, MonthsHomelessPastThreeYears

missingLivingSituationDetail <- servedInDateRange %>%
  select(
    PersonalID,
    HouseholdID,
    EnrollmentID,
    ProjectID,
    ProjectType,
    ProjectName,
    EntryDate,
    MoveInDate,
    ExitDate,
    AgeAtEntry,
    County,
    Region,
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
                     (LivingSituation %in% c(2:9, 12:15, 19:26) &
                        # institution or not homeless
                        (
                          is.na(LOSUnderThreshold) |
                            is.na(PreviousStreetESSH)
                        ))
                 )
             ))) %>%
  mutate(Issue = "Incomplete Living Situation", Type = "Error")

missingLivingSituation <- missingLivingSituationDetail %>%
  select(
    HouseholdID,
    PersonalID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region
  )


DKRLivingSituationDetail <- servedInDateRange %>%
  select(
    PersonalID,
    HouseholdID,
    EnrollmentID,
    ProjectID,
    ProjectType,
    ProjectName,
    EntryDate,
    MoveInDate,
    ExitDate,
    AgeAtEntry,
    County,
    Region,
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
  mutate(Issue = "DKR Living Situation", Type = "Warning")

DKRLivingSituation <- DKRLivingSituationDetail %>%
  select(
    HouseholdID,
    PersonalID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region
  )

# DisablingCondition at Entry

missingDisabilitiesDetail <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    HouseholdID,
    RelationshipToHoH,
    DisablingCondition,
    County,
    Region
  ) %>%
  filter(DisablingCondition == 99 |
      is.na(DisablingCondition)) %>%
  mutate(Issue = "Missing Disabling Condition", Type = "Error")

missingDisabilities <- missingDisabilitiesDetail %>%
  select(
    HouseholdID,
    PersonalID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region
  )

smallDisabilities <- Disabilities %>%
  filter(DataCollectionStage == 1, DisabilityResponse != 0) %>%
  select(PersonalID, DisabilitiesID, EnrollmentID, InformationDate, 
         IndefiniteAndImpairs)

incongruentDisabilitiesDetail <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    HouseholdID,
    RelationshipToHoH,
    DisablingCondition,
    County,
    Region
  ) %>%
  left_join(smallDisabilities, by = c("PersonalID", "EnrollmentID")) %>%
  group_by(
    PersonalID,
    EnrollmentID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDate,
    ExitDate,
    HouseholdID,
    ProjectType,
    RelationshipToHoH,
    DisablingCondition,
    County,
    Region
  ) %>%
  filter(IndefiniteAndImpairs %in% c(0, 1),
         DisablingCondition %in% c(0, 1)) %>%
  summarise(HasLongDurationSub = max(IndefiniteAndImpairs)) %>%
  ungroup() %>%
  mutate(
    Issue = case_when(
      DisablingCondition != HasLongDurationSub ~
        "Disability subassessments don't match the Yes/No"
    ),
    Type = "Error"
  ) %>%
  filter(!is.na(Issue)) 

incongruentDisabilities <- incongruentDisabilitiesDetail %>%
  select(
    HouseholdID,
    PersonalID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region
  )

# INCORRECT, DON'T USE FOR REAL UNTIL THE EXPORT IS FIXED
missingDisabilitySubs <- servedInDateRange %>%
  select(PersonalID,
         EnrollmentID,
         AgeAtEntry,
         ProjectName,
         EntryDate,
         MoveInDate,
         ExitDate,
         HouseholdID,
         RelationshipToHoH,
         ProjectType,
         DisablingCondition,
         County,
         Region) %>%
  left_join(smallDisabilities, by = c("PersonalID", "EnrollmentID")) %>%
  filter(DisablingCondition == 1 & is.na(DisabilitiesID)) %>%
  mutate(Issue = "Missing Disability Subs", Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region
  )

## NOT SURE IF THIS EVEN MATTERS ANYMORE?
missingLongDuration <- servedInDateRange %>%
  select(PersonalID,
         EnrollmentID,
         AgeAtEntry,
         ProjectName,
         EntryDate,
         MoveInDate,
         ExitDate,     
         ProjectType,
         HouseholdID,
         RelationshipToHoH,
         DisablingCondition,
         County,
         Region) %>%
  left_join(smallDisabilities, by = c("PersonalID", "EnrollmentID")) %>%
  filter(IndefiniteAndImpairs == 99) %>%
  mutate(Issue = "Long Duration not answered in subassessment",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region
  )

# MoveInDate
# check that these aren't just bad data from WellSky
# also check the ART report to see what logic I'm using exactly
incorrectMoveInDate <- servedInDateRange %>%
  filter(ProjectType %in% c(3, 9, 12),
         (ymd(MoveInDate) < ymd(EntryDate) |
           ymd(MoveInDate) > ymd(ExitDate))) %>%
  mutate(Issue = "Incorrect Move In Date",
         Type = "Error") %>%
  select(HouseholdID,
         PersonalID,
         ProjectName,
         Issue,
         Type,
         EntryDate,
         MoveInDate,
         ExitDate,
         ProjectType,
         County,
         Region) 
  
# CountyServed

missingCountyServed <- servedInDateRange %>%
  filter(is.na(CountyServed)) %>%
  mutate(Issue = "Missing County Served",
         Type = "Error") %>%
  select(HouseholdID,
         PersonalID,
         ProjectName,
         Issue,
         Type,
         EntryDate,
         MoveInDate,
         ExitDate,
         ProjectType,
         County,
         Region) 

# CountyPrior
# check to see if all hh members have to answer this or if just adults or all?
missingCountyPrior <- servedInDateRange %>%
  filter(is.na(CountyPrior)) %>%
  mutate(Issue = "Missing County Prior",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region
  ) 

# Check Eligibility, Project Type, Residence Prior ------------------------

checkEligibility <- servedInDateRange %>%
  select(
    PersonalID,
    HouseholdID,
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
    County,
    Region
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
  mutate(Issue = "Check Eligibility", Type = "Warning") %>%
  select(
    HouseholdID,
    PersonalID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region
  )

# Missing PATH Data at Entry
# Missing Destination

# Missing SSVF Data
# Incorrect PATH Contact Date
# Missing PATH Contact End Date
# Missing PATH Contacts
# Missing PATH Data at Exit



# Duplicate EEs -----------------------------------------------------------

duplicateEEs <- get_dupes(servedInDateRange, PersonalID, ProjectID, EntryDate) %>%
  mutate(Issue = "Duplicate Entry Exits", Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region
  )


# Future Entry Exits ------------------------------------------------------
# PSHs in the old days before Move In Dates would definitely have been entering
# their clients prior to their Entry Date since back then the Entry Date was the
# day they moved in. So they're excused from this prior to Move In Date's existence.

futureEEs <- servedInDateRange %>%
  filter(ymd(EntryDate) > ymd_hms(DateCreated) &
           (ProjectType %in% c(1, 2, 4, 8) |
           (ProjectType %in% c(3, 9) & ymd(EntryDate) > mdy("10012017"))))  %>%
  mutate(Issue = "Future Entry Date", Type = "Warning") %>%
  select(
    HouseholdID,
    PersonalID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region
  )
  
# Incorrect Entry Exit Type -----------------------------------------------
# check ART report for exact logic. This is an approximation.
incorrectEntryExitType <- servedInDateRange %>%
  filter(EEType != "HUD" & is.na(GrantType)) %>%
  mutate(Issue = "Incorrect EE Type",
         Type = "Error") %>%
  select(HouseholdID,
         PersonalID,
         ProjectName,
         Issue,
         Type,
         EntryDate,
         MoveInDate,
         ExitDate,
         ProjectType,
         County,
         Region)

# HoHs Entering PH without SPDATs -----------------------------------------

EEsWithSPDATs <- left_join(servedInDateRange, Scores, by = "PersonalID") %>%
  select(PersonalID, RelationshipToHoH, EnrollmentID, ProjectID, EntryDate,
         MoveInDate, ExitDate, HouseholdID, ProjectType, ProjectName, County,
         Region, SPDATRecordID, SPDATProvider, StartDate, Score) %>%
  filter(ymd(StartDate) + years(1) > ymd(EntryDate) & # score is < 1 yr old
    ymd(EntryDate) >= ymd(StartDate)) %>% # score is on or before Entry Date
  group_by(EnrollmentID) %>%
  mutate(MaxScoreDate = max(ymd(StartDate))) %>%
  filter(ymd(StartDate) == ymd(MaxScoreDate)) %>%
  mutate(MaxScore = max(Score)) %>%
  filter(Score == MaxScore) %>%
  distinct() %>%
  ungroup() %>%
  select(-MaxScoreDate, -MaxScore) %>%
  mutate(ScoreAdjusted = if_else(is.na(Score), 0, Score))

enteredPHwithoutSPDAT <- 
  anti_join(servedInDateRange, EEsWithSPDATs, by = "EnrollmentID") %>%
  filter(ProjectType %in% c(3, 9, 13)) %>%
  mutate(Issue = "HoHs Entering PH w/o SPDAT",
         Type = "Warning") %>%
  select(HouseholdID,
         PersonalID,
         ProjectName,
         Issue,
         Type,
         EntryDate,
         MoveInDate,
         ExitDate,
         ProjectType,
         County,
         Region)

# HoHs in Shelter without a SPDAT -----------------------------------------
# this is a little different than the ART report; it only flags stayers
# since users can't do anything about leavers
LHwithoutSPDAT <- 
  anti_join(servedInDateRange, EEsWithSPDATs, by = "EnrollmentID") %>%
  filter(ProjectType %in% c(1, 2, 4) &
         ymd(EntryDate) < today() - days(8) &
            is.na(ExitDate)) %>%
  mutate(Issue = "HoHs in ES, TH, SH w/o SPDAT",
         Type = "Warning") %>%
  select(HouseholdID,
         PersonalID,
         ProjectName,
         Issue,
         Type,
         EntryDate,
         MoveInDate,
         ExitDate,
         ProjectType,
         County,
         Region)

# Missing Income at Entry -------------------------------------------------
IncomeBenefits <- IncomeBenefits %>% select(-DateCreated)
missingIncome <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(PersonalID, HouseholdID, AgeAtEntry, ProjectName, EntryDate,
         MoveInDate, ExitDate, ProjectType, County, Region, DataCollectionStage,
         TotalMonthlyIncome, IncomeFromAnySource) %>%
  filter(DataCollectionStage == 1 & 
           (AgeAtEntry > 17 | 
              is.na(AgeAtEntry)) &
           (IncomeFromAnySource == 99 | 
              is.na(IncomeFromAnySource))) %>%
  mutate(Issue = "Income Missing",
         Type = "Error") %>%
  select(HouseholdID,
         PersonalID,
         ProjectName,
         Issue,
         Type,
         EntryDate,
         MoveInDate,
         ExitDate,
         ProjectType,
         County,
         Region)

conflictingIncomeYN <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    PersonalID,
    HouseholdID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region,
    DataCollectionStage,
    TotalMonthlyIncome,
    IncomeFromAnySource,
    Earned,
    EarnedAmount,
    Unemployment,
    UnemploymentAmount,
    SSI,
    SSIAmount,
    SSDI,
    SSDIAmount,
    VADisabilityService,
    VADisabilityServiceAmount,
    VADisabilityNonService,
    VADisabilityNonServiceAmount,
    PrivateDisability,
    PrivateDisabilityAmount,
    WorkersComp,
    WorkersCompAmount,
    TANF,
    TANFAmount,
    GA,
    GAAmount,
    SocSecRetirement,
    SocSecRetirementAmount,
    Pension,
    PensionAmount,
    ChildSupport,
    ChildSupportAmount,
    Alimony,
    AlimonyAmount,
    OtherIncomeSource,
    OtherIncomeAmount,
    OtherIncomeSourceIdentify
  ) %>%
  filter(DataCollectionStage == 1 &
           (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
           ((
             IncomeFromAnySource == 1 &
               Earned + Unemployment + SSI + SSDI + VADisabilityService +
               VADisabilityNonService + PrivateDisability + WorkersComp +
               TANF + GA + SocSecRetirement + Pension + ChildSupport +
               Alimony + OtherIncomeSource == 0
           ) |
             (
               IncomeFromAnySource == 0 &
                 Earned + Unemployment + SSI + SSDI + VADisabilityService +
                 VADisabilityNonService + PrivateDisability + WorkersComp +
                 TANF + GA + SocSecRetirement + Pension + ChildSupport +
                 Alimony + OtherIncomeSource > 0
             )
           )) %>%
  mutate(Issue = "Conflicting Income yes/no",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region
  )

# I think this turns up with 0 records bc they're calculating the TMI from the
# subs instead of using the field itself. Understandable but that means I'll 
# have to pull the TMI data in through RMisc. :(
conflictingIncomeDollars <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    PersonalID,
    HouseholdID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region,
    DataCollectionStage,
    TotalMonthlyIncome,
    IncomeFromAnySource,
    Earned,
    EarnedAmount,
    Unemployment,
    UnemploymentAmount,
    SSI,
    SSIAmount,
    SSDI,
    SSDIAmount,
    VADisabilityService,
    VADisabilityServiceAmount,
    VADisabilityNonService,
    VADisabilityNonServiceAmount,
    PrivateDisability,
    PrivateDisabilityAmount,
    WorkersComp,
    WorkersCompAmount,
    TANF,
    TANFAmount,
    GA,
    GAAmount,
    SocSecRetirement,
    SocSecRetirementAmount,
    Pension,
    PensionAmount,
    ChildSupport,
    ChildSupportAmount,
    Alimony,
    AlimonyAmount,
    OtherIncomeSource,
    OtherIncomeAmount,
    OtherIncomeSourceIdentify
  ) %>%
  filter(
    DataCollectionStage == 1 &
      (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
      TotalMonthlyIncome != EarnedAmount + UnemploymentAmount + SSIAmount + 
      SSDIAmount + VADisabilityServiceAmount + VADisabilityNonServiceAmount +
      PrivateDisabilityAmount + WorkersCompAmount + TANFAmount +
      GAAmount + SocSecRetirementAmount + PensionAmount +
      ChildSupportAmount + AlimonyAmount + OtherIncomeAmount
  ) %>%
  mutate(Issue = "Conflicting Income Amounts",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region
  )

# Missing Income at Exit --------------------------------------------------

IncomeBenefits <- IncomeBenefits %>% select(-DateCreated)
missingIncome <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(PersonalID, HouseholdID, AgeAtEntry, ProjectName, EntryDate,
         MoveInDate, ExitDate, ProjectType, County, Region, DataCollectionStage,
         TotalMonthlyIncome, IncomeFromAnySource) %>%
  filter(DataCollectionStage == 3 & 
           (AgeAtEntry > 17 | 
              is.na(AgeAtEntry)) &
           (IncomeFromAnySource == 99 | 
              is.na(IncomeFromAnySource))) %>%
  mutate(Issue = "Income Missing",
         Type = "Error") %>%
  select(HouseholdID,
         PersonalID,
         ProjectName,
         Issue,
         Type,
         EntryDate,
         MoveInDate,
         ExitDate,
         ProjectType,
         County,
         Region)

conflictingIncomeYN <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    PersonalID,
    HouseholdID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region,
    DataCollectionStage,
    TotalMonthlyIncome,
    IncomeFromAnySource,
    Earned,
    EarnedAmount,
    Unemployment,
    UnemploymentAmount,
    SSI,
    SSIAmount,
    SSDI,
    SSDIAmount,
    VADisabilityService,
    VADisabilityServiceAmount,
    VADisabilityNonService,
    VADisabilityNonServiceAmount,
    PrivateDisability,
    PrivateDisabilityAmount,
    WorkersComp,
    WorkersCompAmount,
    TANF,
    TANFAmount,
    GA,
    GAAmount,
    SocSecRetirement,
    SocSecRetirementAmount,
    Pension,
    PensionAmount,
    ChildSupport,
    ChildSupportAmount,
    Alimony,
    AlimonyAmount,
    OtherIncomeSource,
    OtherIncomeAmount,
    OtherIncomeSourceIdentify
  ) %>%
  filter(DataCollectionStage == 3 &
           (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
           ((
             IncomeFromAnySource == 1 &
               Earned + Unemployment + SSI + SSDI + VADisabilityService +
               VADisabilityNonService + PrivateDisability + WorkersComp +
               TANF + GA + SocSecRetirement + Pension + ChildSupport +
               Alimony + OtherIncomeSource == 0
           ) |
             (
               IncomeFromAnySource == 0 &
                 Earned + Unemployment + SSI + SSDI + VADisabilityService +
                 VADisabilityNonService + PrivateDisability + WorkersComp +
                 TANF + GA + SocSecRetirement + Pension + ChildSupport +
                 Alimony + OtherIncomeSource > 0
             )
           )) %>%
  mutate(Issue = "Conflicting Income yes/no",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region
  )

# I think this turns up with 0 records bc they're calculating the TMI from the
# subs instead of using the field itself. Understandable but that means I'll 
# have to pull the TMI data in through RMisc. :(
conflictingIncomeDollars <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    PersonalID,
    HouseholdID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region,
    DataCollectionStage,
    TotalMonthlyIncome,
    IncomeFromAnySource,
    Earned,
    EarnedAmount,
    Unemployment,
    UnemploymentAmount,
    SSI,
    SSIAmount,
    SSDI,
    SSDIAmount,
    VADisabilityService,
    VADisabilityServiceAmount,
    VADisabilityNonService,
    VADisabilityNonServiceAmount,
    PrivateDisability,
    PrivateDisabilityAmount,
    WorkersComp,
    WorkersCompAmount,
    TANF,
    TANFAmount,
    GA,
    GAAmount,
    SocSecRetirement,
    SocSecRetirementAmount,
    Pension,
    PensionAmount,
    ChildSupport,
    ChildSupportAmount,
    Alimony,
    AlimonyAmount,
    OtherIncomeSource,
    OtherIncomeAmount,
    OtherIncomeSourceIdentify
  ) %>%
  filter(
    DataCollectionStage == 3 &
      (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
      TotalMonthlyIncome != EarnedAmount + UnemploymentAmount + SSIAmount + 
      SSDIAmount + VADisabilityServiceAmount + VADisabilityNonServiceAmount +
      PrivateDisabilityAmount + WorkersCompAmount + TANFAmount +
      GAAmount + SocSecRetirementAmount + PensionAmount +
      ChildSupportAmount + AlimonyAmount + OtherIncomeAmount
  ) %>%
  mutate(Issue = "Conflicting Income Amounts",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region
  )

# Overlapping Enrollment/Move In Dates ------------------------------------

# this only pulls the most recent EE in the overlap and I think that's fine but
# some users won't like being flagged for it if it's someone else's fault
# but you can't tell whose fault it is from the data so...

stagingOverlaps <- servedInDateRange %>%
  select(
    HouseholdID,
    PersonalID,
    ProjectName,
    EntryDate,
    MoveInDate,
    ExitDate,
    ExitAdjust,
    ProjectType,
    County,
    Region
  ) %>%
  mutate(
    MoveInDateAdjust = if_else(
      # only counts movein dates between entry & exit
      ymd(EntryDate) <= ymd(MoveInDate) &
        ymd(MoveInDate) <= ExitAdjust &
        ProjectType %in% c(3, 9, 13),
      MoveInDate,
      NULL
    ),
    EntryAdjust = case_when(
      #for PSH and RRH, EntryAdjust = MoveInDate
      ProjectType %in% c(1, 2, 4, 8, 12) ~ EntryDate,
      ProjectType %in% c(3, 9, 13) &
        !is.na(MoveInDateAdjust) ~ MoveInDateAdjust,
      ProjectType %in% c(3, 9, 13) &
        is.na(MoveInDateAdjust) ~ EntryDate
    ),
    ExitAdjust = ExitAdjust - days(1),
    # bc a client can exit&enter same day
    LiterallyInProject = if_else(
      ProjectType %in% c(3, 9, 13),
      interval(MoveInDateAdjust, ExitAdjust),
      interval(EntryAdjust, ExitAdjust)
    ),
    Issue = "Overlapping Project Stays",
    Type = "Error"
  ) %>%
  filter(!is.na(LiterallyInProject)) %>%
  get_dupes(., PersonalID) %>%
  group_by(PersonalID) %>%
  arrange(PersonalID, EntryAdjust) %>%
  mutate(
    PreviousEntryAdjust = lag(EntryAdjust),
    PreviousExitAdjust = lag(ExitAdjust)
  ) %>%
  filter(!is.na(PreviousEntryAdjust)) %>%
  ungroup()

overlaps <- stagingOverlaps %>%
  mutate(
    PreviousStay = interval(PreviousEntryAdjust, PreviousExitAdjust),
    Overlap = int_overlaps(LiterallyInProject, PreviousStay)
  ) %>%
  filter(Overlap == TRUE) %>%
  select(
    HouseholdID,
    PersonalID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDate,
    ExitDate,
    ProjectType,
    County,
    Region
  )

rm(stagingOverlaps)

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



# All together now --------------------------------------------------------

DataQualityHMIS <- rbind(checkEligibility,
                         duplicateEEs,
                         futureEEs,
                         householdIssues,
                         incongruentDisabilities,
                         missingDisabilities,
                         missingDisabilitySubs,
                         missingLivingSituation,
                         missingLongDuration,
                         missingUDEs,
                         incorrectMoveInDate,
                         missingCountyServed,
                         missingCountyPrior,
                         incorrectEntryExitType)

stagingDQErrors <- DataQualityHMIS %>%
  filter(Type == "Error") %>%
  group_by(ProjectName, Issue, Type, ProjectType, County, Region) %>%
  summarise(Count = n())

end <- now()
total <- end - start
total

