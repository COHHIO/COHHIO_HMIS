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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at 
#<https://www.gnu.org/licenses/>.

library(tidyverse)
library(janitor)
library(lubridate)

load("images/COHHIOHMIS.RData")

rm(
  Affiliation,
  EmploymentEducation,
  EnrollmentCoC,
  Exit,
  Export,
  Funder,
  Geography,
  HealthAndDV,
  Offers,
  ProjectCoC,
  Regions,
  VeteranCE
)

# Providers to Check ------------------------------------------------------

hmisParticipatingCurrent <- Project %>%
  left_join(Inventory, by = "ProjectID") %>%
  filter(ProjectID %in% c(1775, 1695) | ProjectType %in% c(4, 6, 9, 12) | 
           # including Diversion, Unsheltered, PATH Outreach & SO, Prevention,
           # and PH - Housing Only projects
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

rm(Inventory, Organization)
    
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
         ExitDate, Destination, ExitAdjust, DateCreated = DateCreated.x, 
         UserCreating, ClientEnrolledInPATH, LengthOfStay, DateOfPATHStatus, 
         ReasonNotEnrolled) %>%
  inner_join(hmisParticipatingCurrent, by = "ProjectID")

rm(Client, FileStart, FileEnd, FilePeriod, hmisParticipatingCurrent)

# The Variables That We Want ----------------------------------------------

vars_we_want <- c("HouseholdID", 
                  "PersonalID", 
                  "ProjectName", 
                  "Issue", 
                  "Type", 
                  "EntryDate",
                  "MoveInDateAdjust", 
                  "ExitDate",
                  "UserCreating")

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
                   "Missing Date of Birth Data Quality",
                   "Missing SSN",
                   "Invalid SSN",
                   "Missing Race",
                   "Missing Ethnicity",
                   "Missing Gender",
                   "Missing Veteran Status"
                   ) ~ "Error",
      Issue %in% c("Incomplete or DKR Name",
                  "DKR or Approx. Date of Birth",
                  "Don't Know/Refused SSN",
                  "DKR Race",
                  "DKR Ethnicity",
                  "DKR Gender",
                  "DKR Veteran Status",
                  "Check Veteran Status for Accuracy") ~ "Warning"
    )
  ) %>%
  filter(!is.na(Issue)) %>%
  select(vars_we_want)

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
  select(vars_we_want)

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
  select(vars_we_want)

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
  select(vars_we_want)

householdIssues <- rbind(tooManyHoHs, noHoH, childrenOnly)

rm(tooManyHoHs, noHoH, childrenOnly)

# Missing Data at Entry ---------------------------------------------------
# Living Situation,  Length of Stay, LoSUnderThreshold, PreviousStreetESSH,
# DateToStreetESSH, TimesHomelessPastThreeYears, MonthsHomelessPastThreeYears

missingApproxDateHomeless <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    ProjectID,
    ProjectType,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    AgeAtEntry,
    RelationshipToHoH,
    DateToStreetESSH,
    UserCreating
  ) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           ymd(EntryDate) >= mdy("10012016") &
           is.na(DateToStreetESSH)) %>% 
  mutate(Issue = "Missing Approximate Date Homeless", Type = "Error") %>%
  select(vars_we_want)

missingResidencePrior <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    ProjectID,
    ProjectType,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    AgeAtEntry,
    RelationshipToHoH,
    LivingSituation,
    UserCreating
  ) %>% 
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           (is.na(LivingSituation) | LivingSituation == 99)) %>% 
  mutate(Issue = "Missing Residence Prior", 
         Type = "Error") %>%
  select(vars_we_want)

dkrResidencePrior <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    ProjectID,
    ProjectType,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    AgeAtEntry,
    RelationshipToHoH,
    LivingSituation,
    UserCreating
  ) %>% 
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           LivingSituation %in% c(8, 9)) %>% 
  mutate(Issue = "Don't Know/Refused Residence Prior", 
         Type = "Warning") %>%
  select(vars_we_want)

missingLoS <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    ProjectID,
    ProjectType,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    AgeAtEntry,
    RelationshipToHoH,
    LengthOfStay,
    UserCreating
  ) %>% 
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           (is.na(LengthOfStay) | LengthOfStay == 99)) %>% 
  mutate(Issue = "Missing Residence Prior", 
         Type = "Error") %>%
  select(vars_we_want)

dkrLoS <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    ProjectID,
    ProjectType,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    AgeAtEntry,
    RelationshipToHoH,
    LengthOfStay,
    UserCreating
  ) %>% 
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           LengthOfStay %in% c(8, 9)) %>% 
  mutate(Issue = "Don't Know/Refused Residence Prior", 
         Type = "Warning") %>%
  select(vars_we_want)
  
missingMonthsTimesHomeless <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    ProjectID,
    ProjectType,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    AgeAtEntry,
    RelationshipToHoH,
    MonthsHomelessPastThreeYears,
    TimesHomelessPastThreeYears,
    UserCreating
  ) %>% 
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           ymd(EntryDate) >= mdy("10012016") &
           (is.na(MonthsHomelessPastThreeYears) |
              is.na(TimesHomelessPastThreeYears) |
              MonthsHomelessPastThreeYears == 99 |
              TimesHomelessPastThreeYears == 99)) %>% 
  mutate(Issue = "Missing Months or Times Homeless", 
         Type = "Error") %>%
  select(vars_we_want)

dkrMonthsTimesHomeless <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    ProjectID,
    ProjectType,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    AgeAtEntry,
    RelationshipToHoH,
    MonthsHomelessPastThreeYears,
    TimesHomelessPastThreeYears,    
    UserCreating
  ) %>% 
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           ymd(EntryDate) >= mdy("10012016") &
           (MonthsHomelessPastThreeYears %in% c(8, 9) |
               TimesHomelessPastThreeYears %in% c(8, 9))
  ) %>% 
  mutate(Issue = "Don't Know/Refused Months or Times Homeless", 
         Type = "Warning") %>%
  select(vars_we_want)

# THIS IS RETURNING FALSE POSITIVES. I think something is up with the HUD CSV
# Export's LOSUnderThreshold logic. I put in a case with WS.

missingLivingSituationData <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
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
           ymd(EntryDate) >= mdy("10012016") & # not req'd prior to this
           ProjectType %in% c(2, 3, 6, 9, 10, 12, 13) &
           (
             (
               LivingSituation %in% c(15, 6, 7, 24, 4, 5) &
                 LengthOfStay %in% c(2, 3, 10, 11) &
                 (is.na(LOSUnderThreshold) |
                    is.na(PreviousStreetESSH))
             ) |
               (
                 LivingSituation %in% c(2, 3, 12, 13, 14, 15, 19,
                                        20, 21, 22, 23, 25, 26) &
                   LengthOfStay %in% c(10, 11) &
                   (is.na(LOSUnderThreshold) |
                      is.na(PreviousStreetESSH))
               )
           )
  ) %>% 
  mutate(Issue = "Incomplete Living Situation Data", Type = "Error") %>%
  select(vars_we_want)

# DKRLivingSituationDetail <- servedInDateRange %>%
#   select(
#     PersonalID,
#     HouseholdID,
#     EnrollmentID,
#     ProjectID,
#     ProjectType,
#     ProjectName,
#     EntryDate,
#     MoveInDateAdjust,
#     ExitDate,
#     AgeAtEntry,
#     CountyServed,
#     ProviderCounty,
#     Region,
#     RelationshipToHoH,
#     LivingSituation,
#     LengthOfStay,
#     LOSUnderThreshold,
#     PreviousStreetESSH,
#     DateToStreetESSH,
#     MonthsHomelessPastThreeYears,
#     TimesHomelessPastThreeYears,
#     UserCreating
#   ) %>%
#   filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
#            ymd(EntryDate) > mdy("10012016") &
#            (
#              MonthsHomelessPastThreeYears %in% c(8, 9) |
#                TimesHomelessPastThreeYears %in% c(8, 9) |
#                LivingSituation %in% c(8, 9)
#              
#            )
#   ) %>%
#   mutate(Issue = "DKR Living Situation", Type = "Warning")
# 
# DKRLivingSituation <- DKRLivingSituationDetail %>%
#   select(vars_we_want)

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
  select(vars_we_want)


smallDisabilities <- Disabilities %>%
  filter(DataCollectionStage == 1 & 
           ((DisabilityType == 10 & DisabilityResponse %in% c(1:3)) | 
           (DisabilityType != 10 & DisabilityResponse == 1))
           ) %>%
  mutate(IndefiniteAndImpairs =
           if_else(DisabilityType %in% c(6, 8), 1, IndefiniteAndImpairs)) %>%
  select(PersonalID, DisabilitiesID, EnrollmentID, InformationDate, 
         DisabilityType, IndefiniteAndImpairs)

# Developmental & HIV/AIDS get automatically IndefiniteAndImpairs = 1 per FY2020

rm(Disabilities)

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
    UserCreating
  ) %>%
  left_join(smallDisabilities, by = c("PersonalID", "EnrollmentID")) %>%
  filter(DisablingCondition %in% c(0, 1)) %>%
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
    UserCreating
  ) %>%  
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
  select(vars_we_want)

rm(conflictingDisabilitiesDetail)

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
  select(vars_we_want)

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
  select(vars_we_want)

rm(smallDisabilities)

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
         ProjectName,
         Issue,
         Type,
         EntryDate,
         "MoveInDateAdjust" = MoveInDate,
         ExitDate, 
         UserCreating) 
  
# CountyServed

missingCountyServed <- servedInDateRange %>%
  filter(is.na(CountyServed)) %>%
  mutate(Issue = "Missing County Served",
         Type = "Error") %>%
  select(vars_we_want) 

# CountyPrior
# check to see if all hh members have to answer this or if just adults or all?
missingCountyPrior <- servedInDateRange %>%
  filter(is.na(CountyPrior),
         RelationshipToHoH == 1) %>%
  mutate(Issue = "Missing County Prior",
         Type = "Error") %>%
  select(vars_we_want) 

# Check Eligibility, Project Type, Residence Prior ------------------------

checkEligibility <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
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
  select(vars_we_want)


# Missing Destination
missingDestination <- servedInDateRange %>%
  filter(!is.na(ExitDate) & 
           (is.na(Destination) | Destination %in% c(99, 30))) %>%
  mutate(Issue = "Missing Destination",
         Type = "Warning") %>%
  select(vars_we_want)

dkrDestination <- servedInDateRange %>%
  filter(Destination %in% c(8,9)) %>%
  mutate(Issue = "Don't Know/Refused Destination",
         Type = "Warning") %>%
  select(vars_we_want)

# Missing SSVF Data

# Percent of AMI (HoH Only)
# Last Permanent Address	
# SOAR	
# Year Entered Service	
# Theater of Operations	
# Branch	
# Discharge Status	
# VAMC Station Number	 
# HP Targeting Criteria


# Missing PATH Data -------------------------------------------------------

#* Length of Stay in Res Prior
### adult, PATH-enrolled, and:
### Length of Stay is null or DNC -> error -OR-
### Length of Stay is DKR -> warning

smallProject <- Project %>% select(ProjectID, 
                                   ProjectName, 
                                   Region,
                                   "ProviderCounty" = County)

path_missing_los_res_prior <- servedInDateRange %>%
  select(PersonalID, HouseholdID, ProjectID, EntryDate, MoveInDateAdjust,
         ExitDate, UserCreating, AgeAtEntry, ClientEnrolledInPATH, 
         LengthOfStay, EEType) %>%
  left_join(smallProject, by = "ProjectID") %>%
  filter(EEType == "PATH" &
         AgeAtEntry > 17 &
         ClientEnrolledInPATH == 1 &
         (is.na(LengthOfStay)| LengthOfStay == 99)) %>%
  mutate(Issue = "Missing Residence Prior Length of Stay (PATH)",
         Type = "Error") %>%
  select(vars_we_want)


#* Engagement at Entry/Exit
### adult, PATH-enrolled, Date of Engagement is null -> error


#* Status Determination at Exit
### adult, PATH-Enrolled is not null 
### Date of Status Determ is null -> error
path_status_determination <- servedInDateRange %>%
  select(PersonalID, HouseholdID, ProjectID, EntryDate, MoveInDateAdjust,
         ExitDate, UserCreating, AgeAtEntry, ClientEnrolledInPATH, 
         DateOfPATHStatus, EEType) %>%
  left_join(smallProject, by = "ProjectID") %>%
  filter(EEType == "PATH" &
           AgeAtEntry > 17 &
           !is.na(ClientEnrolledInPATH) &
           is.na(DateOfPATHStatus)) %>%
  mutate(Issue = "Missing Date of PATH Status",
         Type = "Error") %>%
  select(vars_we_want)

#* PATH Enrolled at Entry/Exit
### adult and:
### PATH Enrolled null or DNC -> error -OR-

path_enrolled_missing <- servedInDateRange %>%
  select(PersonalID, HouseholdID, ProjectID, EntryDate, MoveInDateAdjust,
         ExitDate, UserCreating, AgeAtEntry, ClientEnrolledInPATH, EEType) %>%
  left_join(smallProject, by = "ProjectID") %>%
  filter(EEType == "PATH" &
           AgeAtEntry > 17 &
           (ClientEnrolledInPATH == 99 |
              is.na(ClientEnrolledInPATH))) %>% 
  mutate(Issue = "Missing PATH Enrollment",
         Type = "Error") %>%
  select(vars_we_want)

#* Not Enrolled Reason
### adult
### PATH Enrolled = No
### Reason is null -> error

path_reason_missing <- servedInDateRange %>%
  select(PersonalID, HouseholdID, ProjectID, EntryDate, MoveInDateAdjust,
         ExitDate, UserCreating, AgeAtEntry, ClientEnrolledInPATH, EEType,
         ReasonNotEnrolled) %>%
  left_join(smallProject, by = "ProjectID") %>%
  filter(EEType == "PATH" &
           AgeAtEntry > 17 &
           ClientEnrolledInPATH == 0 &
              is.na(ReasonNotEnrolled)) %>% 
  mutate(Issue = "Missing Reason Not PATH Enrolled",
         Type = "Error") %>%
  select(vars_we_want)

#* Connection with SOAR at Exit
### adult 
### Connection w/ SOAR is null or DNC -> error -OR-
### Connection w/ SOAR DKR -> warning

smallIncomeSOAR <- IncomeBenefits %>%
  select(PersonalID, EnrollmentID, ConnectionWithSOAR, DataCollectionStage) %>%
  filter(DataCollectionStage == 3)

path_SOAR_missing_at_exit <- servedInDateRange %>%
  select(
    PersonalID,
    EnrollmentID,
    HouseholdID,
    ProjectID,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    UserCreating,
    AgeAtEntry,
    ClientEnrolledInPATH,
    EEType
  ) %>%
  left_join(smallProject, by = "ProjectID") %>%
  left_join(smallIncomeSOAR, by = c("PersonalID", "EnrollmentID")) %>%
  filter(EEType == "PATH" &
           AgeAtEntry > 17 &
           DataCollectionStage == 3 &
           is.na(ConnectionWithSOAR)) %>%
  mutate(Issue = "Missing Connection with SOAR at Exit",
         Type = "Error") %>%
  select(vars_we_want)

rm(smallIncomeSOAR)

# Missing PATH Contacts
## client is adult/hoh and has no contact record in the EE -> error

# Incorrect PATH Contact Date
## client is adult/hoh, has a contact record, and the first record in the EE 
## does not equal the Entry Date ->  error

# Missing PATH Contact End Date
## client is adult/hoh, has a contact record, and the End Date is null -> error


# Duplicate EEs -----------------------------------------------------------
# this could be more nuanced
duplicateEEs <- get_dupes(servedInDateRange, PersonalID, ProjectID, EntryDate) %>%
  mutate(Issue = "Duplicate Entry Exits", Type = "Error") %>%
  select(vars_we_want)


# Future Entry Exits ------------------------------------------------------
# PSHs in the old days before Move In Dates would definitely have been entering
# their clients prior to their Entry Date since back then the Entry Date was the
# day they moved in. So they're excused from this prior to Move In Date's existence.

futureEEs <- servedInDateRange %>%
  filter(ymd(EntryDate) > ymd_hms(DateCreated) &
           (ProjectType %in% c(1, 2, 4, 8) |
           (ProjectType %in% c(3, 9) & ymd(EntryDate) >= mdy("10012017"))))  %>%
  mutate(Issue = "Future Entry Date", Type = "Warning") %>%
  select(vars_we_want)
  
# Incorrect Entry Exit Type -----------------------------------------------
# check ART report for exact logic. 
incorrectEntryExitType <- servedInDateRange %>%
  filter(
    (
      is.na(GrantType) & 
        !grepl("GPD", ProjectName) & 
        !grepl("HCHV", ProjectName) & 
        !grepl("VET ", ProjectName) &
        ProjectID != 1695 &
        EEType != "HUD"
    ) |
      ((
        GrantType == "SSVF" | 
          grepl("GPD", ProjectName) | 
          grepl("HCHV", ProjectName) |
          grepl("VET ", ProjectName)
      ) &
        EEType != "VA") |
      (GrantType == "RHY" & EEType != "RHY") |
      (GrantType == "PATH" & EEType != "PATH") |
      (ProjectID == 1695 & EEType != "Standard")
  ) %>% 
  mutate(Issue = "Incorrect Entry Exit Type",
         Type = "Error") %>%
  select(vars_we_want)

# HoHs Entering PH without SPDATs -----------------------------------------

EEsWithSPDATs <-
  left_join(servedInDateRange, Scores, by = "PersonalID") %>%
  select(
    PersonalID,
    EnrollmentID,
    RelationshipToHoH,
    EntryDate,
    ExitAdjust,
    ScoreDate,
    Score
  ) %>%
  filter(ymd(ScoreDate) + years(1) > ymd(EntryDate) &
           # score is < 1 yr old
           ymd(ScoreDate) < ymd(ExitAdjust)) %>%  # score is prior to Exit
  group_by(EnrollmentID) %>%
  mutate(MaxScoreDate = max(ymd(ScoreDate))) %>%
  filter(ymd(ScoreDate) == ymd(MaxScoreDate)) %>%
  mutate(MaxScore = max(Score)) %>%
  filter(Score == MaxScore) %>%
  distinct() %>%
  ungroup() %>%
  select(-MaxScoreDate,-MaxScore) %>%
  mutate(ScoreAdjusted = if_else(is.na(Score), 0, Score))

rm(Scores)

enteredPHwithoutSPDAT <- 
  anti_join(servedInDateRange, EEsWithSPDATs, by = "EnrollmentID") %>%
  filter(ProjectType %in% c(3, 9, 13),
         ymd(EntryDate) > ymd("20190101") & # only looking at 1/1/2019 forward
           RelationshipToHoH == 1) %>% # HoHs only
  mutate(Issue = "HoHs Entering PH without SPDAT",
         Type = "Warning") %>%
  select(vars_we_want)

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
  select(vars_we_want)

SPDATCreatedOnNonHoH <- EEsWithSPDATs %>%
  left_join(servedInDateRange, by = c("PersonalID", 
                                      "EnrollmentID",
                                      "RelationshipToHoH",
                                      "EntryDate",
                                      "ExitAdjust")) %>%
  filter(RelationshipToHoH != 1) %>%
  mutate(Issue = "SPDAT Created on a Non-Head-of-Household",
         Type = "Warning") %>%
  select(vars_we_want)

rm(EEsWithSPDATs)

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
  select(vars_we_want)

smallIncome <- IncomeBenefits %>%
  select(PersonalID, EnrollmentID, Earned, Unemployment, SSI, SSDI, 
         VADisabilityService, VADisabilityNonService, PrivateDisability,
         WorkersComp, TANF, GA, SocSecRetirement, Pension, ChildSupport,
         Alimony, OtherIncomeSource, DataCollectionStage)

smallIncome[is.na(smallIncome)] <- 0

smallIncome <- smallIncome %>% full_join(IncomeBenefits[c("PersonalID",
                                                          "EnrollmentID",
                                                          "DataCollectionStage",
                                                          "TotalMonthlyIncome",
                                                          "IncomeFromAnySource")],
                                         by = c("PersonalID", 
                                                "EnrollmentID",
                                                "DataCollectionStage"))

incomeSubs <- servedInDateRange[c("PersonalID",
                                "EnrollmentID",
                                "HouseholdID",
                                "AgeAtEntry",
                                "ProjectName",
                                "EntryDate",
                                "MoveInDateAdjust",
                                "ExitDate",
                                "ProjectType",
                                "UserCreating")] %>%
  left_join(smallIncome, by = c("PersonalID", "EnrollmentID")) %>%
  mutate(
    IncomeCount =
      Earned +
      Unemployment +
      SSI +
      SSDI +
      VADisabilityService +
      VADisabilityNonService +
      PrivateDisability +
      WorkersComp +
      TANF +
      GA +
      SocSecRetirement +
      Pension +
      ChildSupport +
      Alimony +
      OtherIncomeSource
  )


conflictingIncomeYNatEntry <- incomeSubs %>%
  filter(DataCollectionStage == 1 &
           (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
           ((IncomeFromAnySource == 1 &
               IncomeCount == 0) |
              (IncomeFromAnySource == 0 &
                 IncomeCount > 0)
           )) %>% 
  mutate(Issue = "Conflicting Income yes/no at Entry",
         Type = "Error") %>%
  select(vars_we_want)

# Not calculating Conflicting Income Amounts bc they're calculating the TMI from the
# subs instead of using the field itself. Understandable but that means I would 
# have to pull the TMI data in through RMisc OR we kill TMI altogether. (We
# decided to kill TMI altogether.)

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
  select(vars_we_want)

conflictingIncomeYNatExit <- incomeSubs %>%
  filter(DataCollectionStage == 3 &
           (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
           ((IncomeFromAnySource == 1 &
               IncomeCount == 0) |
              (IncomeFromAnySource == 0 &
                 IncomeCount > 0)
           )) %>% 
  mutate(Issue = "Conflicting Income yes/no at Exit",
         Type = "Error") %>%
  select(vars_we_want)

rm(incomeSubs)

# conflictingIncomeYN <- servedInDateRange %>%
#   left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
#   select(
#     PersonalID,
#     EnrollmentID,
#     HouseholdID,
#     AgeAtEntry,
#     ProjectName,
#     EntryDate,
#     MoveInDateAdjust,
#     ExitDate,
#     ProjectType,
#     CountyServed,
#     ProviderCounty,
#     Region,
#     DataCollectionStage,
#     TotalMonthlyIncome,
#     IncomeFromAnySource,
#     Earned,
#     EarnedAmount,
#     Unemployment,
#     UnemploymentAmount,
#     SSI,
#     SSIAmount,
#     SSDI,
#     SSDIAmount,
#     VADisabilityService,
#     VADisabilityServiceAmount,
#     VADisabilityNonService,
#     VADisabilityNonServiceAmount,
#     PrivateDisability,
#     PrivateDisabilityAmount,
#     WorkersComp,
#     WorkersCompAmount,
#     TANF,
#     TANFAmount,
#     GA,
#     GAAmount,
#     SocSecRetirement,
#     SocSecRetirementAmount,
#     Pension,
#     PensionAmount,
#     ChildSupport,
#     ChildSupportAmount,
#     Alimony,
#     AlimonyAmount,
#     OtherIncomeSource,
#     OtherIncomeAmount,
#     OtherIncomeSourceIdentify,
#     UserCreating
#   ) %>%
#   filter(DataCollectionStage == 3 &
#            (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
#            ((
#              IncomeFromAnySource == 1 &
#                Earned + Unemployment + SSI + SSDI + VADisabilityService +
#                VADisabilityNonService + PrivateDisability + WorkersComp +
#                TANF + GA + SocSecRetirement + Pension + ChildSupport +
#                Alimony + OtherIncomeSource == 0
#            ) |
#              (
#                IncomeFromAnySource == 0 &
#                  Earned + Unemployment + SSI + SSDI + VADisabilityService +
#                  VADisabilityNonService + PrivateDisability + WorkersComp +
#                  TANF + GA + SocSecRetirement + Pension + ChildSupport +
#                  Alimony + OtherIncomeSource > 0
#              )
#            )) %>%
#   mutate(Issue = "Conflicting Income yes/no at Exit",
#          Type = "Error") %>%
#   select(vars_we_want)

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
      ProjectType %in% c(1, 2, 8, 12) | 
        ProjectName == "Unsheltered Clients - OUTREACH" ~ EntryDate,
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
    PreviousExitAdjust = lag(ExitAdjust),
    PreviousProject = lag(ProjectName)
  ) %>%
  filter(!is.na(PreviousEntryAdjust)) %>%
  ungroup()

rrh_overlaps <- servedInDateRange %>%
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
    ExitAdjust = ExitAdjust - days(1),
    # bc a client can exit&enter same day
    InProject = interval(EntryDate, ExitAdjust),
    Issue = "Overlapping Project Stays",
    Type = "Error"
  ) %>%
  filter(ProjectType == 13) %>%
  get_dupes(., PersonalID) %>%
  group_by(PersonalID) %>%
  arrange(PersonalID, EntryDate) %>%
  mutate(
    PreviousEntry = lag(EntryDate),
    PreviousExit = lag(ExitAdjust),
    PreviousProject = lag(ProjectName)
  ) %>%
  filter(!is.na(PreviousEntry)) %>%
  ungroup() %>%
  mutate(
    PreviousStay = interval(PreviousEntry, PreviousExit),
    Overlap = int_overlaps(InProject, PreviousStay)
  ) %>%
  filter(Overlap == TRUE) %>%
  select(vars_we_want, PreviousProject)

psh_overlaps <- servedInDateRange %>%
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
    ExitAdjust = ExitAdjust - days(1),
    # bc a client can exit&enter same day
    InProject = interval(EntryDate, ExitAdjust),
    Issue = "Overlapping Project Stays",
    Type = "Error"
  ) %>%
  filter(ProjectType == 3) %>%
  get_dupes(., PersonalID) %>%
  group_by(PersonalID) %>%
  arrange(PersonalID, EntryDate) %>%
  mutate(
    PreviousEntry = lag(EntryDate),
    PreviousExit = lag(ExitAdjust),
    PreviousProject = lag(ProjectName)
  ) %>%
  filter(!is.na(PreviousEntry)) %>%
  ungroup() %>%
  mutate(
    PreviousStay = interval(PreviousEntry, PreviousExit),
    Overlap = int_overlaps(InProject, PreviousStay)
  ) %>%
  filter(Overlap == TRUE) %>%
  select(vars_we_want, PreviousProject)

# forAmanda <- stagingOverlaps %>%
#   mutate(
#     PreviousStay = interval(PreviousEntryAdjust, PreviousExitAdjust),
#     Overlap = int_overlaps(LiterallyInProject, PreviousStay)
#   ) %>%
#   filter(Overlap == TRUE) %>%
#   select(PersonalID, dupe_count, ProjectName, EntryDate, MoveInDateAdjust,
#          ExitDate, UserCreating, LiterallyInProject, PreviousStay, Overlap)

overlaps <- stagingOverlaps %>%
  mutate(
    PreviousStay = interval(PreviousEntryAdjust, PreviousExitAdjust),
    Overlap = int_overlaps(LiterallyInProject, PreviousStay)
  ) %>%
  filter(Overlap == TRUE) %>%
  select(vars_we_want, PreviousProject)

overlaps <- rbind(overlaps, rrh_overlaps, psh_overlaps)

# write_csv(forAmanda, "data/OverlapsToChase.csv")

rm(stagingOverlaps,
   # forAmanda,
   rrh_overlaps,
   psh_overlaps)

# Missing Health Ins ------------------------------------------------------

missingHealthInsuranceatEntry <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(PersonalID, EnrollmentID, HouseholdID, AgeAtEntry, ProjectName, 
         EntryDate, MoveInDateAdjust, ExitDate, ProjectType, DataCollectionStage, 
         InsuranceFromAnySource, UserCreating) %>%
  filter(DataCollectionStage == 1 & 
           (InsuranceFromAnySource == 99 | 
              is.na(InsuranceFromAnySource))) %>%
  mutate(Issue = "Health Insurance Missing at Entry",
         Type = "Error") %>%
  select(vars_we_want)

missingHealthInsuranceatExit <- servedInDateRange %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(PersonalID, EnrollmentID, HouseholdID, AgeAtEntry, ProjectName, 
         EntryDate, MoveInDateAdjust, ExitDate, ProjectType, DataCollectionStage, 
         InsuranceFromAnySource, UserCreating) %>%
  filter(DataCollectionStage == 3 & 
           (InsuranceFromAnySource == 99 | 
              is.na(InsuranceFromAnySource))) %>%
  mutate(Issue = "Health Insurance Missing at Exit",
         Type = "Error") %>%
  select(vars_we_want)

healthInsuranceSubs <- servedInDateRange %>%
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
    DataCollectionStage,
    InsuranceFromAnySource,
    Medicaid,
    Medicare,
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
  mutate(SourceCount = Medicaid + SCHIP + VAMedicalServices + EmployerProvided +
           COBRA + PrivatePay + StateHealthIns + IndianHealthServices +
           OtherInsurance + Medicare)

conflictingHealthInsuranceYNatEntry <- healthInsuranceSubs %>%
  filter(DataCollectionStage == 1 &
           ((
             InsuranceFromAnySource == 1 &
               SourceCount == 0
           ) |
             (
               InsuranceFromAnySource == 0 &
                 SourceCount > 0
             )
           )) %>%
  mutate(Issue = "Conflicting Health Insurance yes/no at Entry",
         Type = "Error") %>%
  select(vars_we_want)

conflictingHealthInsuranceYNatExit <- healthInsuranceSubs %>%
  filter(DataCollectionStage == 3 &
           ((
             InsuranceFromAnySource == 1 &
               SourceCount == 0
           ) |
             (
               InsuranceFromAnySource == 0 &
                 SourceCount > 0
             )
           )) %>%
  mutate(Issue = "Conflicting Health Insurance yes/no at Exit",
         Type = "Error") %>%
  select(vars_we_want)

rm(healthInsuranceSubs)

# Missing NCBs at Entry ---------------------------------------------------

NCBSubs <- IncomeBenefits %>%
  select(PersonalID, EnrollmentID, DataCollectionStage, SNAP, WIC, TANFChildCare, TANFTransportation,
         OtherTANF, OtherBenefitsSource)

NCBSubs[is.na(NCBSubs)] <- 0

NCBSubs <- NCBSubs %>%
  full_join(IncomeBenefits[c("PersonalID",
                             "EnrollmentID",
                             "DataCollectionStage",
                             "BenefitsFromAnySource")],
            by = c("PersonalID",
                   "EnrollmentID",
                   "DataCollectionStage"))

NCBSubs <- servedInDateRange %>%
  left_join(NCBSubs, by = c("PersonalID", "EnrollmentID")) %>%
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
    UserCreating
  ) %>%
  mutate(BenefitCount = SNAP + WIC + TANFChildCare + TANFTransportation + 
           OtherTANF + OtherBenefitsSource) %>%
  select(PersonalID, EnrollmentID, DataCollectionStage, BenefitsFromAnySource, 
         BenefitCount)

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
  select(vars_we_want)

conflictingNCBsAtEntry <- servedInDateRange %>%
  left_join(NCBSubs, by = c("PersonalID", "EnrollmentID")) %>%
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
    BenefitCount,
    UserCreating
  ) %>%
  filter(DataCollectionStage == 1 &
           (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
           ((
             BenefitsFromAnySource == 1 &
               BenefitCount == 0
           ) |
             (
               BenefitsFromAnySource == 0 &
                 BenefitCount > 0
             )
           )) %>%
  mutate(Issue = "Conflicting Non-cash Benefits yes/no at Entry",
         Type = "Error") %>%
  select(vars_we_want)

# Missing NCBs at Exit ----------------------------------------------------
missingNCBsAtExit <- servedInDateRange %>%
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
  select(vars_we_want)

conflictingNCBsAtExit <- servedInDateRange %>%
  left_join(NCBSubs, by = c("PersonalID", "EnrollmentID")) %>%
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
    BenefitCount,
    UserCreating
  ) %>%
  filter(DataCollectionStage == 3 &
           (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
           ((
             BenefitsFromAnySource == 1 &
               BenefitCount == 0
           ) |
             (
               BenefitsFromAnySource == 0 &
                 BenefitCount > 0
             )
           )) %>%
  mutate(Issue = "Conflicting Non-cash Benefits yes/no at Exit",
         Type = "Error") %>%
  select(vars_we_want)

rm(NCBSubs)

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
         Type = "Warning") %>%
  select(vars_we_want)

rm(IncomeBenefits, smallIncome)

# Non HoHs w Svcs or Referrals --------------------------------------------
# SSVF projects should be showing this as an Error, whereas non-SSVF projects 
# should be showing it as a warning, and only back to Feb of 2018.
servicesOnHHMembers <- servedInDateRange %>%
  select(
    HouseholdID,
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
    UserCreating,
    GrantType
  ) %>%
  filter(
    RelationshipToHoH != 1 &
      ymd(EntryDate) >= mdy("02012019") &
      (GrantType != "SSVF" | is.na(GrantType))
  ) %>%
  semi_join(Services, by = c("PersonalID", "EnrollmentID")) %>%
  mutate(Issue = "Service Transaction on a non Head of Household",
         Type = "Warning") %>%
  select(vars_we_want)

servicesOnHHMembersSSVF <- servedInDateRange %>%
  select(
    HouseholdID,
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
    UserCreating,
    GrantType
  ) %>%
  filter(
    RelationshipToHoH != 1 &
      GrantType == "SSVF"
  ) %>%
  semi_join(Services, by = c("PersonalID", "EnrollmentID")) %>%
  mutate(Issue = "Service Transaction on a non Head of Household (SSVF)",
         Type = "Error") %>%
  select(vars_we_want)

rm(Services)

# # why isn't this catching any records, i don't know.
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
         UserCreating,
         GrantType) %>%
  filter(RelationshipToHoH != 1 &
           (GrantType != "SSVF"  | is.na(GrantType))) %>%
  semi_join(Referrals, by = c("PersonalID")) %>%
  mutate(Issue = "Referral on a non Head of Household",
         Type = "Warning") %>%
  select(vars_we_want)

referralsOnHHMembersSSVF <- servedInDateRange %>%
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
         UserCreating,
         GrantType) %>%
  filter(RelationshipToHoH != 1 &
           GrantType == "SSVF") %>%
  semi_join(Referrals, by = c("PersonalID")) %>%
  mutate(Issue = "Referral on a non Head of Household",
         Type = "Error") %>%
  select(vars_we_want)


# Unpaired Needs ----------------------------------------------------------
# can't get this from the CSV Export

# Stray Services (fall outside EE) ----------------------------------------
# Because a lot of these records are stray Services due to there being no
# Entry Exit at all, this can't be shown in the same data set as all the other 
# errors. I'm going to have to make this its own thing somehow. :(
stray_services <- stray_services %>% 
  mutate(Issue = "Service Not Attached to an Entry Exit",
         Type = "Warning") %>%
  select(PersonalID, ServiceProvider, ServiceStartDate, Issue, Type)

# Unmet Needs -------------------------------------------------------------
# can't get this from the CSV Export

# AP No Recent Referrals --------------------------------------------------


# AP entering project stays -----------------------------------------------

APsWithEEs <- Enrollment %>%
  filter(ProjectType == 14) %>%
  mutate(Issue = "Access Point with Entry Exits",
         Type = "Error") %>%
  left_join(smallProject, by = "ProjectID") %>%
  select(vars_we_want)

rm(Enrollment)

# Need Status Referral Outcomes -------------------------------------------
# would need to pull in the Needs records to calculate this


# Side Door ---------------------------------------------------------------
# use Referrals, get logic from ART report- it's pretty lax I think


# Old Outstanding Referrals -----------------------------------------------
# CW says ProviderCreating should work instead of Referred-From Provider
# Using ProviderCreating instead. Either way, I feel this should go in the 
# Provider Dashboard, not the Data Quality report.
old_outstanding_referrals <- Referrals %>%
  filter(!is.na(ReferralOutcome) &
           ReferralDate < today() - days(14)) %>%
  select(PersonalID, ReferralDate, ProviderCreating)

rm(Referrals)

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
  select(vars_we_want)

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
  conflictingHealthInsuranceYNatEntry,
  conflictingHealthInsuranceYNatExit,
  conflictingIncomeYNatEntry,
  conflictingIncomeYNatExit,
  conflictingNCBsAtEntry,
  conflictingNCBsAtExit,
  dkrDestination,
  dkrLoS,
  dkrMonthsTimesHomeless,
  dkrResidencePrior,
  duplicateEEs,
  enteredPHwithoutSPDAT,
  futureEEs,
  householdIssues,
  incorrectEntryExitType,
  incorrectMoveInDate,
  LHwithoutSPDAT,
  missingApproxDateHomeless,
  missingCountyServed,
  missingCountyPrior,  
  missingDestination,
  missingDisabilities,
  missingDisabilitySubs,  
  missingHealthInsuranceatEntry,
  missingHealthInsuranceatExit,  
  missingIncomeAtEntry,
  missingIncomeAtExit,
  # missingLivingSituationData, #(waiting on WS ticket)
  missingLoS,
  missingLongDuration,
  missingMonthsTimesHomeless,
  missingNCBsAtEntry,
  missingNCBsAtExit,
  missingResidencePrior,
  missingUDEs,
  path_enrolled_missing,
  path_missing_los_res_prior,
  path_reason_missing,
  path_SOAR_missing_at_exit,
  path_status_determination,
  referralsOnHHMembers,
  referralsOnHHMembersSSVF,
  servicesOnHHMembers,
  servicesOnHHMembersSSVF,
  SPDATCreatedOnNonHoH
) %>%
  filter(!ProjectName %in% c(
    "Diversion from Homeless System",
    "Unsheltered Clients - OUTREACH"
  )) %>%
  left_join(Users, by = "UserCreating") %>%
  select(-UserID, -UserName)
  

unshelteredDataQuality <- rbind(
  checkDisabilityForAccuracy,
  # conflictingDisabilities,
  dkrDestination,
  dkrMonthsTimesHomeless,
  dkrResidencePrior,
  dkrLoS,  
  duplicateEEs,
  futureEEs,
  householdIssues,
  incorrectEntryExitType,
  LHwithoutSPDAT,
  missingApproxDateHomeless,
  # missingCountyPrior,
  missingDestination,
  missingCountyServed,
  # missingDisabilities,
  # missingDisabilitySubs,
#  missingLivingSituationData, 
  missingLoS,
  missingMonthsTimesHomeless,
  missingResidencePrior,
  missingUDEs,
  # overlaps,
  referralsOnHHMembers,
  SPDATCreatedOnNonHoH,
  unshelteredNotUnsheltered
) %>%
  filter(ProjectName == "Unsheltered Clients - OUTREACH") %>%
  left_join(Users, by = "UserCreating") %>%
  select(-UserID, -UserName) 

rm(Users)

# UNTIL WELLSKY FIXES THEIR EXPORT: ---------------------------------------

DataQualityHMIS <- DataQualityHMIS %>%
  filter(
    !Issue %in% c(
      "Conflicting Disability yes/no",
      # "Conflicting Disability yes/no at Exit",
      "Conflicting Health Insurance yes/no at Entry",
      "Conflicting Health Insurance yes/no at Exit",
      "Conflicting Income yes/no at Entry",
      "Conflicting Income yes/no at Exit",
      "Conflicting Non-cash Benefits yes/no at Entry",
      "Conflicting Non-cash Benefits yes/no at Exit",
      "Missing Disability Subs",
      "Incomplete Living Situation Data",
      "Missing Approximate Date Homeless",
      "Missing Months or Times Homeless",
      "Check Eligibility",
      "Don't Know/Refused Residence Prior",
      "Don't Know/Refused Months or Times Homeless",
      "Health Insurance Missing at Entry",
      "Health Insurance Missing at Exit",
      "Income Missing at Entry",
      "Income Missing at Exit",
      "Missing Residence Prior",
      "Non-cash Benefits Missing at Entry",
      "Non-cash Benefits Missing at Exit",
      "SPDAT Created on a Non-Head-of-Household"
    )
  )

ReportStart <- "10012018"
ReportEnd <- format.Date(today(), "%m-%d-%Y")

cocDataQualityHMIS <- DataQualityHMIS %>%
  filter(served_between(., ReportStart, ReportEnd)) %>%
  left_join(Project[c("ProjectID", "ProjectName")], by = "ProjectName")

rm(ReportStart, ReportEnd)

# Clean up the house ------------------------------------------------------

rm(
  APsWithEEs,
  checkDisabilityForAccuracy,
  checkEligibility,
  conflictingDisabilities,
  conflictingHealthInsuranceYNatEntry,
  conflictingHealthInsuranceYNatExit,
  conflictingIncomeYNatEntry,
  conflictingIncomeYNatExit,
  conflictingNCBsAtEntry,
  conflictingNCBsAtExit,
  dkrMonthsTimesHomeless,
  dkrResidencePrior,
  dkrDestination,
  dkrLoS,
  duplicateEEs,
  enteredPHwithoutSPDAT,
  futureEEs,
  householdIssues,
  incorrectEntryExitType,
  incorrectMoveInDate,
  LHwithoutSPDAT,
  missingApproxDateHomeless,
  missingCountyPrior,
  missingCountyServed,
  missingDestination,
  missingDisabilities,
  missingDisabilitiesDetail,
  missingDisabilitySubs,
  missingHealthInsuranceatEntry,
  missingHealthInsuranceatExit,
  missingIncomeAtEntry,
  missingIncomeAtExit,
  missingLivingSituationData,
  missingLongDuration,
  missingLoS,
  missingMonthsTimesHomeless,
  missingNCBsAtEntry,
  missingNCBsAtExit,
  missingResidencePrior,
  missingUDEs,
  path_enrolled_missing,
  path_missing_los_res_prior,
  path_reason_missing,
  path_SOAR_missing_at_exit,
  path_status_determination,
  Project,
  referralsOnHHMembers,
  referralsOnHHMembersSSVF,
  servedInDateRange,
  servicesOnHHMembers,
  servicesOnHHMembersSSVF,
  smallProject,
  SPDATCreatedOnNonHoH,
  unshelteredEnrollments,
  unshelteredNotUnsheltered,
  update_date
)

dqProviders <- sort(DataQualityHMIS$ProjectName) %>% unique()

save.image("images/Data_Quality.RData")

 