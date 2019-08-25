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
library(janitor)
library(lubridate)
# library(plotly)
# start <- now()
load("images/COHHIOHMIS.RData")

# Providers to Check ------------------------------------------------------

hmisParticipatingCurrent <- Project %>%
  left_join(Inventory, by = "ProjectID") %>%
  filter(ProjectID %in% c(1775, 1695) | ProjectType %in% c(4, 12) | # including 
           # Diversion, Unsheltered, PATH Outreach, and Prevention projects
           (
             HMIS_participating_between(., FileStart, FileEnd) &
               operating_between(., FileStart, FileEnd) &
               (GrantType != "HOPWA" | is.na(GrantType)) # excluding HOPWA
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
    "ProviderCounty" = County,
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
         MoveInDate, MoveInDateAdjust, EEType, CountyServed, CountyPrior, 
         ExitDate, Destination, ExitAdjust, AgeAtEntry, DateCreated = DateCreated.x, UserCreating) %>%
  inner_join(hmisParticipatingCurrent, by = "ProjectID")

# Missing UDEs ------------------------------------------------------------

missingUDEs <- servedInDateRange %>%
  mutate(
    Issue = case_when(
      FirstName == "Missing" ~ "Missing Name Data Quality",
      FirstName %in% c("DKR", "Partial") ~ "Incomplete or DKR Name",
      DOBDataQuality == 99 ~ "Missing Date of Birth Data Quality",
      DOBDataQuality %in% c(2, 8, 9) ~ "DKR or Approx. Date of Birth",
      AgeAtEntry < 0 |
        AgeAtEntry > 95 ~ "Incorrect Date of Birth or Entry Date",
      SSN == "Missing" ~ "Missing SSN",
      SSN == "Invalid" ~ "Invalid SSN",
      SSN == "DKR" ~ "Don't Know/Refused SSN",
      SSN == "Incomplete" ~ "Incomplete SSN",
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
      Issue %in% c("Missing Name Data Quality",
                   "Missing DOB",
                   "Incorrect Date of Birth or Entry Date",
                   "Missing SSN",
                   "Invalid SSN",
                   "Missing Race",
                   "Missing Ethnicity",
                   "Missing Gender",
                   "Missing Veteran Status"
                   ) ~ "Error",
      Issue %in% c("Incomplete or DKR Name",
                  "DKR or Approx. Date of Birth",
                  "DKR Race",
                  "DKR Ethnicity",
                  "DKR Gender",
                  "DKR Veteran Status",
                  "Check Veteran Status for Accuracy") ~ "Warning"
    )
  ) %>%
  filter(!is.na(Issue)) %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )

# Household Issues --------------------------------------------------------

childrenOnly <- servedInDateRange %>%
  filter(GrantType != "RHY" | is.na(GrantType)) %>% # not checking for children-only hhs for RHY
  group_by(HouseholdID) %>%
  summarise(
    hhMembers = n(),
    maxAge = max(AgeAtEntry),
    PersonalID = min(PersonalID)
  ) %>%
  filter(maxAge < 18) %>%
  ungroup() %>%
  left_join(servedInDateRange, by = c("PersonalID", "HouseholdID")) %>%
  mutate(Issue = "Children Only Household",
         Type = "Error") %>%
  select(
         HouseholdID,
         PersonalID,
         EnrollmentID,
         ProjectName,
         Issue,
         Type,
         EntryDate,
         MoveInDateAdjust,
         ExitDate,
         ProjectType,
         CountyServed,
         ProviderCounty,
         Region,
         UserCreating)

noHoH <- servedInDateRange %>%
  group_by(HouseholdID) %>%
  summarise(
    hasHoH = if_else(min(RelationshipToHoH, na.rm = TRUE) != 1,
                     FALSE,
                     TRUE),
    PersonalID = min(PersonalID)
  ) %>%
  filter(hasHoH == FALSE) %>%
  ungroup() %>%
  left_join(servedInDateRange, by = c("PersonalID", "HouseholdID")) %>%
  mutate(Issue = "No Head of Household",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating)

tooManyHoHs <- servedInDateRange %>%
  filter(RelationshipToHoH == 1) %>%
  group_by(HouseholdID) %>%
  summarise(HoHsinHousehold = n(),
            PersonalID = min(PersonalID)) %>%
  filter(HoHsinHousehold > 1) %>%
  ungroup() %>%
  left_join(servedInDateRange, by = c("PersonalID", "HouseholdID")) %>%
  mutate(Issue = "Too Many Heads of Household",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating)

householdIssues <- rbind(tooManyHoHs, noHoH, childrenOnly)

rm(tooManyHoHs, noHoH, childrenOnly)

# Missing Data at Entry ---------------------------------------------------
# Living Situation,  Length of Stay, LoSUnderThreshold, PreviousStreetESSH,
# DateToStreetESSH, TimesHomelessPastThreeYears, MonthsHomelessPastThreeYears

missingLivingSituationDetail <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    EnrollmentID,
    ProjectID,
    ProjectType,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    AgeAtEntry,
    CountyServed,
    ProviderCounty,
    Region,
    RelationshipToHoH,
    LivingSituation,
    LengthOfStay,
    LOSUnderThreshold,
    PreviousStreetESSH,
    DateToStreetESSH,
    MonthsHomelessPastThreeYears,
    TimesHomelessPastThreeYears,
    UserCreating
  ) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           ymd(EntryDate) > mdy("10012016") & # not req'd prior to this
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
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
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
    MoveInDateAdjust,
    ExitDate,
    AgeAtEntry,
    CountyServed,
    ProviderCounty,
    Region,
    RelationshipToHoH,
    LivingSituation,
    LengthOfStay,
    LOSUnderThreshold,
    PreviousStreetESSH,
    DateToStreetESSH,
    MonthsHomelessPastThreeYears,
    TimesHomelessPastThreeYears,
    UserCreating
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
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )

# DisablingCondition at Entry

missingDisabilitiesDetail <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    HouseholdID,
    RelationshipToHoH,
    DisablingCondition,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  ) %>%
  filter(DisablingCondition == 99 |
      is.na(DisablingCondition)) %>%
  mutate(Issue = "Missing Disabling Condition", Type = "Error")

missingDisabilities <- missingDisabilitiesDetail %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )

smallDisabilities <- Disabilities %>%
  filter(DataCollectionStage == 1, DisabilityResponse != 0) %>%
  select(PersonalID, DisabilitiesID, EnrollmentID, InformationDate, 
         IndefiniteAndImpairs)

conflictingDisabilitiesDetail <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    HouseholdID,
    RelationshipToHoH,
    DisablingCondition,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  ) %>%
  left_join(smallDisabilities, by = c("PersonalID", "EnrollmentID")) %>%
  group_by(
    PersonalID,
    EnrollmentID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    HouseholdID,
    ProjectType,
    RelationshipToHoH,
    DisablingCondition,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  ) %>%
  filter(IndefiniteAndImpairs %in% c(0, 1),
         DisablingCondition %in% c(0, 1)) %>%
  summarise(HasLongDurationSub = max(IndefiniteAndImpairs)) %>%
  ungroup() %>%
  mutate(
    Issue = case_when(
      DisablingCondition != HasLongDurationSub ~
        "Conflicting Disability yes/no"
    ),
    Type = "Error"
  ) %>%
  filter(!is.na(Issue)) 

conflictingDisabilities <- conflictingDisabilitiesDetail %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )

# INCORRECT, DON'T USE FOR REAL UNTIL THE EXPORT IS FIXED
missingDisabilitySubs <- servedInDateRange %>%
  select(PersonalID,
         EnrollmentID,
         AgeAtEntry,
         ProjectName,
         EntryDate,
         MoveInDateAdjust,
         ExitDate,
         HouseholdID,
         RelationshipToHoH,
         ProjectType,
         DisablingCondition,
         CountyServed,
         ProviderCounty,
         Region,
         UserCreating) %>%
  left_join(smallDisabilities, by = c("PersonalID", "EnrollmentID")) %>%
  filter(DisablingCondition == 1 & is.na(DisabilitiesID)) %>%
  mutate(Issue = "Missing Disability Subs", Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )

## NOT SURE IF THIS EVEN MATTERS ANYMORE?
missingLongDuration <- servedInDateRange %>%
  select(PersonalID,
         EnrollmentID,
         AgeAtEntry,
         ProjectName,
         EntryDate,
         MoveInDateAdjust,
         ExitDate,     
         ProjectType,
         HouseholdID,
         RelationshipToHoH,
         DisablingCondition,
         CountyServed,
         ProviderCounty,
         Region,
         UserCreating) %>%
  left_join(smallDisabilities, by = c("PersonalID", "EnrollmentID")) %>%
  filter(IndefiniteAndImpairs == 99) %>%
  mutate(Issue = "Disabilities: missing Long Duration in subassessment",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )

# MoveInDate
# check that these aren't just bad data from WellSky
# also check the ART report to see what logic I'm using exactly
incorrectMoveInDate <- servedInDateRange %>%
  filter(ProjectType %in% c(3, 9, 13),
         (ymd(MoveInDate) < ymd(EntryDate) |
           ymd(MoveInDate) > ymd(ExitDate))) %>%
  mutate(Issue = "Incorrect Move In Date",
         Type = "Error") %>%
  select(HouseholdID,
         PersonalID,
         EnrollmentID,
         ProjectName,
         Issue,
         Type,
         EntryDate,
         "MoveInDateAdjust" = MoveInDate,
         ExitDate,
         ProjectType,
         CountyServed,
         ProviderCounty,
         Region,
         UserCreating) 
  
# CountyServed

missingCountyServed <- servedInDateRange %>%
  filter(is.na(CountyServed)) %>%
  mutate(Issue = "Missing County Served",
         Type = "Error") %>%
  select(HouseholdID,
         PersonalID,
         EnrollmentID,
         ProjectName,
         Issue,
         Type,
         EntryDate,
         MoveInDateAdjust,
         ExitDate,
         ProjectType,
         CountyServed,
         ProviderCounty,
         Region,
         UserCreating) 

# CountyPrior
# check to see if all hh members have to answer this or if just adults or all?
missingCountyPrior <- servedInDateRange %>%
  filter(is.na(CountyPrior),
         RelationshipToHoH == 1) %>%
  mutate(Issue = "Missing County Prior",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  ) 

# Check Eligibility, Project Type, Residence Prior ------------------------

checkEligibility <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    EnrollmentID,
    ProjectID,
    ProjectType,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    AgeAtEntry,
    RelationshipToHoH,
    LivingSituation,
    LengthOfStay,
    LOSUnderThreshold,
    PreviousStreetESSH,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
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
              !LivingSituation %in% c(8, 9, 12:14, 19:23, 25)) |
           (ProjectType %in% c(8, 4) & # Outreach and Safe Havens
              !LivingSituation == 16)) # unsheltered only

smallEligibility <- checkEligibility %>%
  select(PersonalID, ProjectName, ProjectType, LivingSituation, EntryDate,
         ExitDate, LengthOfStay, LOSUnderThreshold, PreviousStreetESSH) %>%
  mutate(ResidencePrior = case_when(
    LivingSituation == 1 ~ "Emergency shelter, incl hotel or motel paid for with
    emergency shelter voucher",
    LivingSituation == 2 ~ "Transitional housing for homeless persons",
    LivingSituation == 3 ~ "Permanent housing (other than RRH) for formerly 
    homeless persons",
    LivingSituation == 4 ~ "Psychiatric hospital or other psychiatric facility",
    LivingSituation == 5 ~ "Substance abuse treatment facility or detox center",
    LivingSituation == 6 ~ "Hospital or other residential non-psychiatric medical
    facility",
    LivingSituation == 7 ~ "Jail, prison or juvenile detention facility",
    LivingSituation == 8 ~ "Client doesn't know",
    LivingSituation == 9 ~ "Client refused",
    LivingSituation == 12 ~ "Staying or living in a family member's room, 
    apartment, or house",
    LivingSituation == 13 ~ "Staying or living in a friend's room, apartment or
    house",
    LivingSituation == 14 ~ "Hotel or motel paid for without emergency shelter 
    voucher",
    LivingSituation == 15 ~ "Foster care home or foster care group home",
    LivingSituation == 16 ~ "Place not meant for habitation",
    LivingSituation == 18 ~ "Safe Haven",
    LivingSituation == 19 ~ "Rental by client, with VASH subsidy",
    LivingSituation == 20 ~ "Rental by client, with other housing subsidy (incl
    RRH)",
    LivingSituation == 21 ~ "Owned by client, with ongoing housing subsidy",
    LivingSituation == 22 ~ "Rental by client, no ongoing housing subsidy",
    LivingSituation == 23 ~ "Owned by client, no ongoing housing subsidy",
    LivingSituation == 24 ~ "Long-term care facility or nursing home",
    LivingSituation == 25 ~ "Rental by client, with GPD TIP subsidy",
    LivingSituation == 26 ~ "Residential project or halfway house with no 
    homeless criteria",
    LivingSituation == 27 ~ "Interim housing",
    LivingSituation == 99 ~ "Data not collected"
  ),
  LengthOfStay = case_when(
    LengthOfStay == 2 ~ "One week or more but less than one month",
    LengthOfStay == 3 ~ "One month or more but less than 90 days",
    LengthOfStay == 4 ~ "90 days or more but less than one year",
    LengthOfStay == 5 ~ "One year or longer",
    LengthOfStay == 8 ~ "Client doesn't know",
    LengthOfStay == 9 ~ "Client refused",
    LengthOfStay == 10 ~ "One night or less",
    LengthOfStay == 11 ~ "Two to six nights",
    LengthOfStay == 99 ~ "Data not collected"
  ))

checkEligibility <- checkEligibility %>%
  mutate(Issue = "Check Eligibility", Type = "Warning") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )

# Missing PATH Data at Entry
# Missing Destination
missingDestination <- servedInDateRange %>%
  filter(!is.na(ExitDate) & 
           (is.na(Destination) | Destination %in% c(99, 30))) %>%
  mutate(Issue = "Missing Destination",
         Type = "Warning") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )

dkrDestination <- servedInDateRange %>%
  filter(Destination %in% c(8,9)) %>%
  mutate(Issue = "Don't Know/Refused Destination",
         Type = "Warning") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )
# Missing SSVF Data
# Incorrect PATH Contact Date
# Missing PATH Contact End Date
# Missing PATH Contacts
# Missing PATH Data at Exit



# Duplicate EEs -----------------------------------------------------------
# this could be more nuanced
duplicateEEs <- get_dupes(servedInDateRange, PersonalID, ProjectID, EntryDate) %>%
  mutate(Issue = "Duplicate Entry Exits", Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
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
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )
  
# Incorrect Entry Exit Type -----------------------------------------------
# check ART report for exact logic. This is an approximation. Also be sure
# to include Project 1695 = "Standard"
incorrectEntryExitType <- servedInDateRange %>%
  filter(
    (
      is.na(GrantType) & 
        !grepl("GPD", ProjectName) & 
        !grepl("HCHV", ProjectName) & 
        ProjectID != 1695 &
        EEType != "HUD"
    ) |
      ((
        GrantType == "SSVF" | 
          grepl("GPD", ProjectName) | 
          grepl("HCHV", ProjectName)
      ) &
        EEType != "VA") |
      (GrantType == "RHY" & EEType != "RHY") |
      (GrantType == "PATH" & EEType != "PATH") |
      (ProjectID == 1695 & EEType != "Standard")
  ) %>% 
  mutate(Issue = "Incorrect Entry Exit Type",
         Type = "Error") %>%
  select(HouseholdID,
         PersonalID,
         EnrollmentID,
         ProjectName,
         Issue,
         Type,
         EntryDate,
         MoveInDateAdjust,
         ExitDate,
         ProjectType,
         CountyServed,
         ProviderCounty,
         Region,
         UserCreating)

# HoHs Entering PH without SPDATs -----------------------------------------

EEsWithSPDATs <- left_join(servedInDateRange, Scores, by = "PersonalID") %>%
  select(PersonalID, EnrollmentID, RelationshipToHoH, EntryDate, ExitAdjust, 
         SPDATRecordID, SPDATProvider, StartDate, Score) %>%
  filter(ymd(StartDate) + years(1) > ymd(EntryDate) & # score is < 1 yr old
    ymd(StartDate) < ymd(ExitAdjust)) %>%  # score is prior to Exit
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
  filter(ProjectType %in% c(3, 9, 13),
         ymd(EntryDate) > ymd("20190101") & # only looking at 1/1/2019 forward
           RelationshipToHoH == 1) %>% # HoHs only
  mutate(Issue = "HoHs Entering PH without SPDAT",
         Type = "Warning") %>%
  select(HouseholdID,
         PersonalID,
         EnrollmentID,
         ProjectName,
         Issue,
         Type,
         EntryDate,
         MoveInDateAdjust,
         ExitDate,
         ProjectType,
         CountyServed,
         ProviderCounty,
         Region,
         UserCreating)

# HoHs in Shelter without a SPDAT -----------------------------------------
# this is a little different than the ART report; it only flags stayers
# since users can't do anything about leavers
LHwithoutSPDAT <- 
  anti_join(servedInDateRange, EEsWithSPDATs, by = "EnrollmentID") %>%
  filter(
    ProjectType %in% c(1, 2, 4, 8) &
      RelationshipToHoH == 1 &
      ymd(EntryDate) < today() - days(8) &
      is.na(ExitDate) &
      ymd(EntryDate) > ymd("20190101")
  ) %>% 
  mutate(Issue = "HoHs in shelter or Transitional Housing for 8+ days without SPDAT",
         Type = "Warning") %>%
  select(HouseholdID,
         PersonalID,
         EnrollmentID,
         ProjectName,
         Issue,
         Type,
         EntryDate,
         MoveInDateAdjust,
         ExitDate,
         ProjectType,
         CountyServed,
         ProviderCounty,
         Region,
         UserCreating)

SPDATCreatedOnNonHoH <- EEsWithSPDATs %>%
  left_join(servedInDateRange, by = c("PersonalID", 
                                      "EnrollmentID",
                                      "RelationshipToHoH",
                                      "EntryDate",
                                      "ExitAdjust")) %>%
  filter(RelationshipToHoH != 1) %>%
  mutate(Issue = "SPDAT Created on a Non-Head-of-Household",
         Type = "Warning") %>%
  select(HouseholdID,
         PersonalID,
         EnrollmentID,
         ProjectName,
         Issue,
         Type,
         EntryDate,
         MoveInDateAdjust,
         ExitDate,
         ProjectType,
         CountyServed,
         ProviderCounty,
         Region,
         UserCreating)

# Missing Income at Entry -------------------------------------------------
IncomeBenefits <- IncomeBenefits %>% select(-DateCreated)
missingIncomeAtEntry <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    PersonalID,
    HouseholdID,
    EnrollmentID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    DataCollectionStage,
    TotalMonthlyIncome,
    IncomeFromAnySource,
    UserCreating
  ) %>%
  filter(DataCollectionStage == 1 &
           (AgeAtEntry > 17 |
              is.na(AgeAtEntry)) &
           (IncomeFromAnySource == 99 |
              is.na(IncomeFromAnySource))) %>%
  mutate(Issue = "Income Missing at Entry",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )

conflictingIncomeYN <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
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
    OtherIncomeSourceIdentify,
    UserCreating
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
  mutate(Issue = "Conflicting Income yes/no at Entry",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )

# I think this turns up with 0 records bc they're calculating the TMI from the
# subs instead of using the field itself. Understandable but that means I'll 
# have to pull the TMI data in through RMisc. :( - OR we kill TMI altogether.
conflictingIncomeAmountAtEntry <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
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
    OtherIncomeSourceIdentify,
    UserCreating
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
  mutate(Issue = "Conflicting Income Amounts at Entry",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )

# Missing Income at Exit --------------------------------------------------

missingIncomeAtExit <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(PersonalID, HouseholdID, EnrollmentID, AgeAtEntry, ProjectName, 
         EntryDate, MoveInDateAdjust, ExitDate, ProjectType, CountyServed, 
         ProviderCounty, Region, DataCollectionStage, TotalMonthlyIncome, 
         IncomeFromAnySource, UserCreating) %>%
  filter(DataCollectionStage == 3 & 
           (AgeAtEntry > 17 | 
              is.na(AgeAtEntry)) &
           (IncomeFromAnySource == 99 | 
              is.na(IncomeFromAnySource))) %>%
  mutate(Issue = "Income Missing at Exit",
         Type = "Error") %>%
  select(HouseholdID,
         PersonalID,
         EnrollmentID,
         ProjectName,
         Issue,
         Type,
         EntryDate,
         MoveInDateAdjust,
         ExitDate,
         ProjectType,
         CountyServed,
         ProviderCounty,
         Region,
         UserCreating)

conflictingIncomeYN <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
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
    OtherIncomeSourceIdentify,
    UserCreating
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
  mutate(Issue = "Conflicting Income yes/no at Exit",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )

# I think this turns up with 0 records bc they're calculating the TMI from the
# subs instead of using the field itself. Understandable but that means I'll 
# have to pull the TMI data in through RMisc. :(
conflictingIncomeAmountAtExit <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
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
    OtherIncomeSourceIdentify,
    UserCreating
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
  mutate(Issue = "Conflicting Income Amounts at Exit",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )

# Overlapping Enrollment/Move In Dates ------------------------------------

# this only pulls the most recent EE in the overlap and I think that's fine but
# some users won't like being flagged for it if it's someone else's fault
# but you can't tell whose fault it is from the data so...

stagingOverlaps <- servedInDateRange %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ExitAdjust,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  ) %>%
  mutate(
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
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )

rm(stagingOverlaps)

# Missing Health Ins at Entry ---------------------------------------------

missingHealthInsurance <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(PersonalID, EnrollmentID, HouseholdID, AgeAtEntry, ProjectName, EntryDate,
         MoveInDateAdjust, ExitDate, ProjectType, CountyServed, ProviderCounty, Region, 
         DataCollectionStage, InsuranceFromAnySource, UserCreating) %>%
  filter(DataCollectionStage == 1 & 
           (InsuranceFromAnySource == 99 | 
              is.na(InsuranceFromAnySource))) %>%
  mutate(Issue = "Health Insurance Missing at Entry",
         Type = "Error") %>%
  select(HouseholdID,
         PersonalID,
         EnrollmentID,
         ProjectName,
         Issue,
         Type,
         EntryDate,
         MoveInDateAdjust,
         ExitDate,
         ProjectType,
         CountyServed,
         ProviderCounty,
         Region,
         UserCreating)

conflictingHealthInsuranceYN <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    DataCollectionStage,
    InsuranceFromAnySource,
    Medicaid,
    SCHIP,
    VAMedicalServices,
    EmployerProvided,
    COBRA,
    PrivatePay,
    StateHealthIns,
    IndianHealthServices,
    OtherInsurance,
    HIVAIDSAssistance,
    ADAP,
    UserCreating
  ) %>%
  filter(DataCollectionStage == 1 &
           ((
             InsuranceFromAnySource == 1 &
               Medicaid + SCHIP + VAMedicalServices + EmployerProvided +
               COBRA + PrivatePay + StateHealthIns + IndianHealthServices +
               OtherInsurance + HIVAIDSAssistance + ADAP == 0
           ) |
             (
               InsuranceFromAnySource == 0 &
                 Medicaid + SCHIP + VAMedicalServices + EmployerProvided +
                 COBRA + PrivatePay + StateHealthIns + IndianHealthServices +
                 OtherInsurance + HIVAIDSAssistance + ADAP > 0
             )
           )) %>%
  mutate(Issue = "Conflicting Health Insurance yes/no at Entry",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )

# Missing NCBs at Entry ---------------------------------------------------

missingNCBsAtEntry <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    DataCollectionStage,
    BenefitsFromAnySource,
    UserCreating
  ) %>%
  filter(
    DataCollectionStage == 1 &
      (AgeAtEntry > 17 |
         is.na(AgeAtEntry)) &
      (BenefitsFromAnySource == 99 |
         is.na(BenefitsFromAnySource))
  ) %>%
  mutate(Issue = "Non-cash Benefits Missing at Entry",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )

conflictingNCBsAtEntry <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    DataCollectionStage,
    BenefitsFromAnySource,
    SNAP,
    WIC,
    TANFChildCare,
    TANFTransportation,
    OtherTANF,
    OtherBenefitsSource,
    OtherBenefitsSourceIdentify,
    UserCreating
  ) %>%
  filter(DataCollectionStage == 1 &
           (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
           ((
             BenefitsFromAnySource == 1 &
               SNAP + WIC + TANFChildCare + TANFTransportation + OtherTANF +
               OtherBenefitsSource == 0
           ) |
             (
               BenefitsFromAnySource == 0 &
                 SNAP + WIC + TANFChildCare + TANFTransportation + OtherTANF +
                 OtherBenefitsSource > 0
             )
           )) %>%
  mutate(Issue = "Conflicting Non-cash Benefits yes/no at Entry",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )

# Missing NCBs at Exit ----------------------------------------------------
missingNCBsAtEntry <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    DataCollectionStage,
    BenefitsFromAnySource,
    UserCreating
  ) %>%
  filter(
    DataCollectionStage == 3 &
      (AgeAtEntry > 17 |
         is.na(AgeAtEntry)) &
      (BenefitsFromAnySource == 99 |
         is.na(BenefitsFromAnySource))
  ) %>%
  mutate(Issue = "Non-cash Benefits Missing at Exit",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )

conflictingNCBsAtEntry <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    AgeAtEntry,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    DataCollectionStage,
    BenefitsFromAnySource,
    SNAP,
    WIC,
    TANFChildCare,
    TANFTransportation,
    OtherTANF,
    OtherBenefitsSource,
    OtherBenefitsSourceIdentify,
    UserCreating
  ) %>%
  filter(DataCollectionStage == 3 &
           (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
           ((
             BenefitsFromAnySource == 1 &
               SNAP + WIC + TANFChildCare + TANFTransportation + OtherTANF +
               OtherBenefitsSource == 0
           ) |
             (
               BenefitsFromAnySource == 0 &
                 SNAP + WIC + TANFChildCare + TANFTransportation + OtherTANF +
                 OtherBenefitsSource > 0
             )
           )) %>%
  mutate(Issue = "Conflicting Non-cash Benefits yes/no at Exit",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )


# SSI/SSDI but no Disability (Q) ------------------------------------------
smallIncome <- IncomeBenefits %>%
  select(EnrollmentID, PersonalID, SSI, SSDI)

checkDisabilityForAccuracy <- servedInDateRange %>%
  select(HouseholdID,
         PersonalID,
         EnrollmentID,
         ProjectName,
         EntryDate,
         MoveInDateAdjust,
         ExitDate,
         ProjectType,
         CountyServed,
         ProviderCounty,
         Region, 
         AgeAtEntry, 
         DisablingCondition,
         UserCreating) %>%
  left_join(smallIncome, by = c("EnrollmentID", "PersonalID")) %>%
  mutate(SSI = if_else(is.na(SSI), 0, SSI),
         SSDI = if_else(is.na(SSDI), 0, SSDI)) %>%
  filter(SSI + SSDI > 0 & DisablingCondition == 0 & AgeAtEntry > 17) %>%
  select(-DisablingCondition, -SSI, -SSDI, -AgeAtEntry) %>%
  unique() %>%
  mutate(Issue = "Client with No Disability Receiving SSI/SSDI (could be ok)",
         Type = "Warning")

# Non HoHs w Svcs or Referrals --------------------------------------------
# I have a feeling not all Referrals are coming in.
Referrals <- Services %>%
  filter(RecordType == 161)
# I have a feeling not all the Services are coming in.
Services <- Services %>%
  filter(RecordType %in% c(141, 143, 151, 144, 152))

servicesOnHHMembers <- servedInDateRange %>%
  select(HouseholdID,
         PersonalID,
         EnrollmentID,
         ProjectName,
         EntryDate,
         MoveInDateAdjust,
         ExitDate,
         ProjectType,
         CountyServed,
         ProviderCounty,
         Region,
         RelationshipToHoH,
         UserCreating) %>%
  filter(RelationshipToHoH != 1) %>%
  semi_join(Services, by = c("PersonalID", "EnrollmentID")) %>%
  mutate(Issue = "Service Transaction on a non Head of Household",
         Type = "Warning") %>% 
  select(-RelationshipToHoH)

# why isn't this catching any records, i don't know.
referralsOnHHMembers <- servedInDateRange %>%
  select(HouseholdID,
         PersonalID,
         EnrollmentID,
         ProjectName,
         EntryDate,
         MoveInDateAdjust,
         ExitDate,
         ProjectType,
         CountyServed,
         ProviderCounty,
         Region,
         RelationshipToHoH,
         UserCreating) %>%
  filter(RelationshipToHoH != 1) %>%
  semi_join(Referrals, by = c("PersonalID", "EnrollmentID")) %>%
  mutate(Issue = "Referral on a non Head of Household",
         Type = "Warning") %>%
  select(-RelationshipToHoH)


# Unpaired Needs ----------------------------------------------------------
# can't get this from the CSV Export

# Service Date Before Entry -----------------------------------------------
# can't get this from the CSV Export

# Unmet Needs -------------------------------------------------------------
# can't get this from the CSV Export

# AP No Recent Referrals --------------------------------------------------
# can't get this from the CSV export

# AP entering project stays -----------------------------------------------
smallProject <- Project %>% select(ProjectID, 
                                   ProjectName, 
                                   Region,
                                   "ProviderCounty" = County)

APsWithEEs <- Enrollment %>%
  filter(ProjectType ==  14) %>%
  mutate(Issue = "Access Point with Entry Exits",
         Type = "Error") %>%
  left_join(smallProject, by = "ProjectID") %>%
  select(HouseholdID, PersonalID, EnrollmentID, ProjectName, Issue, Type, 
         EntryDate, MoveInDateAdjust, ExitDate, ProjectType, CountyServed, 
         ProviderCounty, Region, UserCreating)

# Need Status Referral Outcomes -------------------------------------------
# can't get this from the HUD CSV Export

# Veterans with No Referral -----------------------------------------------
# can't get this from the HUD CSV Export


# Side Door ---------------------------------------------------------------
# can't get this from the HUD CSV Export


# Old Outstanding Referrals -----------------------------------------------
# can't get this from the HUD CSV Export


# Service Date Before Entry -----------------------------------------------
# can't get this from the CSV Export

# Diversion Incorrect Destination -----------------------------------------
# Currently, the BoS is collecting Diversion via EEs on a single provider
# shared by all access points in the system. This is changing soon.

diversionEnrollments <- servedInDateRange %>%
  filter(ProjectID == 1775) %>%
  select(HouseholdID, PersonalID, EnrollmentID, ProjectID, ProjectName, 
         ProjectType, EntryDate, MoveInDateAdjust, ExitDate, RelationshipToHoH, 
         LivingSituation, AgeAtEntry, EEType, Destination, 
         CountyServed, ProviderCounty, Region, UserCreating)

diversionIncorrectDestination <- diversionEnrollments %>%
  filter(Destination %in% c(1:3, 8:9, 16, 18, 24, 31, 99)) %>%
  mutate(Issue = "Incorrect Destination or Not a Diversion",
         Type = "Error") %>%
  select(HouseholdID, PersonalID, EnrollmentID, ProjectName, Issue, Type, 
         EntryDate, MoveInDateAdjust, ExitDate, ProjectType, CountyServed, 
         ProviderCounty,Region, UserCreating)

# Diversion Exit Date Missing or Incorrect --------------------------------
diversionIncorrectExitDate <- filter(diversionEnrollments,
       ymd(EntryDate) != ymd(ExitDate) | is.na(ExitDate)) %>%
  mutate(Issue = "Incorrect Exit Date",
         Type = "Error") %>%
  select(HouseholdID, PersonalID, EnrollmentID, ProjectName, Issue, Type, 
         EntryDate, MoveInDateAdjust, ExitDate, ProjectType, CountyServed, 
         ProviderCounty, Region, UserCreating)

# Diversion Missing # in HH -----------------------------------------------
# can't get this from the CSV Export

# Diversion No Provider in CM Record --------------------------------------
# can't get this from the CSV Export

# Diversion Missing CM ----------------------------------------------------
# can't get this from the CSV Export

# Diversion Entered all HH members ----------------------------------------
diversionEnteredHHMembers <- filter(diversionEnrollments,
                                    str_starts(HouseholdID, "h_")) %>%
  mutate(Issue = "Entered Household Members on a Diversion",
         Type = "Error") %>%
  select(
    HouseholdID,
    PersonalID,
    EnrollmentID,
    ProjectName,
    Issue,
    Type,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    ProjectType,
    CountyServed,
    ProviderCounty,
    Region,
    UserCreating
  )

# Unsheltered Incorrect Residence Prior -----------------------------------
unshelteredEnrollments <- servedInDateRange %>%
  filter(ProjectID == 1695) %>%
  select(PersonalID, EnrollmentID, ProjectID, ProjectName, ProjectType,
         HouseholdID, EntryDate, MoveInDateAdjust, ExitDate, RelationshipToHoH, 
         LivingSituation, AgeAtEntry, EEType, Destination, CountyServed, 
         ProviderCounty, UserCreating, LivingSituation, Region)

unshelteredNotUnsheltered <- unshelteredEnrollments %>%
  filter(LivingSituation != 16) %>%
  mutate(Type = "Error",
         Issue = "Wrong Provider (Not Unsheltered)") %>%
  select(HouseholdID, PersonalID, EnrollmentID, ProjectName, Issue, Type, 
         EntryDate, MoveInDateAdjust, ExitDate, ProjectType, CountyServed, Region, 
         ProviderCounty, UserCreating)

# Unsheltered Incorrect Provider in CM Record -----------------------------
# can't get this from the HUD CSV export 

# Missing End Date on Outreach Contact ------------------------------------

# Unsheltered Missing Outreach Contact Note -------------------------------

# Unsheltered Missing Outreach Contact Record -----------------------------

# Unsheltered Outreach Contact Incorrect Start Date -----------------------


# Unsheltered 30+ Days Exited w No Referral -------------------------------


# Unsheltered Currently Unsheltered 30+ Days w No Referral ----------------


# Unsheltered No Case Manager ---------------------------------------------


# All together now --------------------------------------------------------

DataQualityHMIS <- rbind(
  APsWithEEs,
  checkDisabilityForAccuracy,
  checkEligibility,
  conflictingDisabilities,
  conflictingHealthInsuranceYN,
  conflictingIncomeAmountAtEntry,
  conflictingIncomeAmountAtExit,
  conflictingIncomeYN,
  conflictingNCBsAtEntry,
  DKRLivingSituation,
  duplicateEEs,
  enteredPHwithoutSPDAT,
  futureEEs,
  householdIssues,
  incorrectEntryExitType,
  incorrectMoveInDate,
  LHwithoutSPDAT,
  missingDestination,
  dkrDestination,
  missingCountyServed,
  missingCountyPrior,
  missingDisabilities,
  missingDisabilitySubs,
  missingIncomeAtEntry,
  missingIncomeAtExit,
  missingHealthInsurance,
  missingLivingSituation,
  missingLongDuration,
  missingNCBsAtEntry,
  missingUDEs,
  overlaps,
  referralsOnHHMembers,
  servicesOnHHMembers,
  SPDATCreatedOnNonHoH
) %>%
  filter(!ProjectName %in% c("Diversion from Homeless System", 
                             "Unsheltered Clients - OUTREACH")) %>%
  left_join(Users, by = "UserCreating") %>%
  select(-UserID, -UserName)

unshelteredDataQuality <- rbind(
  checkDisabilityForAccuracy,
  conflictingDisabilities,
  DKRLivingSituation,
  duplicateEEs,
  futureEEs,
  householdIssues,
  incorrectEntryExitType,
  LHwithoutSPDAT,
  missingCountyPrior,
  missingDestination,
  dkrDestination,
  missingCountyServed,
  missingDisabilities,
  missingDisabilitySubs,
  missingLivingSituation,
  missingUDEs,
  overlaps,
  referralsOnHHMembers,
  SPDATCreatedOnNonHoH,
  unshelteredNotUnsheltered
) %>% filter(ProjectName == "Unsheltered Clients - OUTREACH") %>%
  left_join(Users, by = "UserCreating") %>%
  select(-UserID, -UserName)

diversionDataQuality <- rbind(
  diversionEnteredHHMembers,
  diversionIncorrectDestination,
  diversionIncorrectExitDate
)

# UNTIL WELLSKY FIXES THEIR EXPORT: ---------------------------------------

DataQualityHMIS <- DataQualityHMIS %>%
  filter(!Issue %in% c(
    "Conflicting Disability yes/no at Entry",
    "Conflicting Disability yes/no at Exit",
    "Conflicting Health Insurance yes/no at Entry",                       
    "Conflicting Health Insurance yes/no at Exit",                       
    "Conflicting Income yes/no at Entry",                                
    "Conflicting Income yes/no at Exit",                                
    "Conflicting Non-cash Benefits yes/no at Entry",
    "Conflicting Non-cash Benefits yes/no at Exit",
    "Missing Disability Subs"
    
  )) 


# SOMETHING I'M TRYING ----------------------------------------------------

DataQualityHMIS <- DataQualityHMIS %>%
  select(HouseholdID, PersonalID, ProjectName, Issue, Type, EntryDate,
         MoveInDateAdjust, ExitDate)

# Clean up the house ------------------------------------------------------

rm(Affiliation, Client, Disabilities, EmploymentEducation, Enrollment,
   EnrollmentCoC, Exit, Export, Funder, Geography, HealthAndDV, IncomeBenefits,
   Inventory, Offers, Organization, Project, ProjectCoC, Regions, Scores,
   Users, VeteranCE, diversionEnrollments, EEsWithSPDATs, FileEnd, FileStart, 
   FilePeriod, hmisParticipatingCurrent, Referrals, servedInDateRange, Services,
   smallDisabilities, smallIncome, smallProject, unshelteredEnrollments, 
   updatedate, APsWithEEs, checkDisabilityForAccuracy, checkEligibility, 
   conflictingDisabilities, conflictingHealthInsuranceYN, 
   conflictingIncomeAmountAtEntry, conflictingIncomeAmountAtExit,
   conflictingIncomeYN, conflictingNCBsAtEntry, diversionEnteredHHMembers,
   diversionIncorrectDestination, diversionIncorrectExitDate, DKRLivingSituation,
   duplicateEEs, enteredPHwithoutSPDAT, futureEEs, householdIssues,
   incorrectEntryExitType, incorrectMoveInDate, LHwithoutSPDAT,
   missingCountyPrior, missingCountyServed, missingDisabilities,
   missingDisabilitySubs, missingHealthInsurance, missingIncomeAtEntry,
   missingIncomeAtExit, missingLivingSituation, missingLongDuration,
   missingNCBsAtEntry, missingUDEs, overlaps, referralsOnHHMembers,
   servicesOnHHMembers, unshelteredNotUnsheltered, conflictingDisabilitiesDetail,
   missingDisabilitiesDetail, missingLivingSituationDetail, 
   DKRLivingSituationDetail)

dqProviders <- sort(DataQualityHMIS$ProjectName) %>% unique()

save.image("images/Data_Quality.RData")
# 
# end <- now()
# end - start
# 
rm(list = ls())
# 
# # Errors by Provider ------------------------------------------------------
# 
# stagingDQErrors <- DataQualityHMIS %>%
#   filter(Type == "Error") %>%
#   group_by(ProjectName, Issue, Type, ProjectType, ProviderCounty, Region) %>%
#   summarise(Count = n())
# 
# stagingDQWarnings <- DataQualityHMIS %>%
#   filter(Type == "Warning") %>%
#   group_by(ProjectName, Issue, Type, ProjectType, ProviderCounty, Region) %>%
#   summarise(Count = n())
# 
# plotErrors <- stagingDQErrors %>%
#   group_by(ProjectName) %>%
#   summarise(Errors = sum(Count)) %>%
#   ungroup() %>%
#   arrange(desc(Errors))
# 
# plotErrors$hover <-
#   with(
#     plotErrors,
#     paste(Errors, "Errors")
#   )
# 
# errorsProviderPlot <- plot_ly(
#   plotErrors,
#   x = ~ ProjectName,
#   y = ~ Errors,
#   type = 'bar',
#   text = ~ hover,
#   marker = list(
#     color = 'rgb(158,202,225)',
#     line = list(color = 'rgb(8,48,107)',
#                 width = 1.5)
#   )
# ) %>%
#   layout(
#     title = "HMIS Errors by Provider",
#     xaxis = list(
#       title = ~ ProjectName,
#       categoryorder = "array",
#       categoryarray = ~ Errors
#     ),
#     yaxis = list(title = "All Errors")
#   )
# 
# # Error Types Plot --------------------------------------------------------
# errorTypes <- stagingDQErrors %>%
#   group_by(Issue) %>%
#   summarise(Errors = sum(Count)) %>%
#   ungroup() %>%
#   arrange(desc(Errors))
# 
# errorTypePlot <- plot_ly(
#   errorTypes,
#   x = ~ Issue,
#   y = ~ Errors,
#   type = 'bar',
#   marker = list(
#     color = 'rgb(158,202,225)',
#     line = list(color = 'rgb(8,48,107)',
#                 width = 1.5)
#   )
# ) %>%
#   layout(
#     title = "HMIS Errors Across the Ohio BoS CoC",
#     xaxis = list(
#       title = ~ Issue,
#       categoryorder = "array",
#       categoryarray = ~ Errors
#     ),
#     yaxis = list(title = "All Errors")
#   )
# 
# # Widespread Issues -------------------------------------------------------
# 
#  widespreadIssue <- DataQualityHMIS %>%
#    select(Issue, ProjectName, Type) %>%
#    unique() %>%
#    group_by(Issue, Type) %>%
#    summarise(HowManyProjects = n()) %>%
#    arrange(desc(HowManyProjects))
# 
# # Warnings by Provider ----------------------------------------------------
# 
#  plotWarnings <- stagingDQWarnings %>%
#    group_by(ProjectName) %>%
#    summarise(Warnings = sum(Count)) %>%
#    ungroup() %>%
#    arrange(desc(Warnings))
#  
#  plotWarnings$hover <-
#    with(
#      plotWarnings,
#      paste(Warnings, "Warnings")
#    )
#  
#  warningsProviderPlot <- plot_ly(
#    plotWarnings,
#    x = ~ ProjectName,
#    y = ~ Warnings,
#    type = 'bar',
#    text = ~ hover,
#    marker = list(
#      color = 'rgb(158,202,225)',
#      line = list(color = 'rgb(8,48,107)',
#                  width = 1.5)
#    )
#  ) %>%
#    layout(
#      title = "HMIS Warnings by Provider",
#      xaxis = list(
#        title = ~ ProjectName,
#        categoryorder = "array",
#        categoryarray = ~ Warnings
#      ),
#      yaxis = list(title = "All Warnings")
#    )
#  
# 
# # Warning Types -----------------------------------------------------------
# 
#  warningTypes <- stagingDQWarnings %>%
#    group_by(Issue) %>%
#    summarise(Warnings = sum(Count)) %>%
#    ungroup() %>%
#    arrange(desc(Warnings))
#  
#  warningTypePlot <- plot_ly(
#    warningTypes,
#    x = ~ Issue,
#    y = ~ Warnings,
#    type = 'bar',
#    marker = list(
#      color = 'rgb(158,202,225)',
#      line = list(color = 'rgb(8,48,107)',
#                  width = 1.5)
#    )
#  ) %>%
#    layout(
#      title = "HMIS Warnings Across the Ohio BoS CoC",
#      xaxis = list(
#        title = ~ Issue,
#        categoryorder = "array",
#        categoryarray = ~ Warnings
#      ),
#      yaxis = list(title = "All Warnings")
#    )
#  
 