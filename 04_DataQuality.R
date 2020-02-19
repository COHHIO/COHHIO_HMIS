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
# <https://www.gnu.org/licenses/>.

library(tidyverse)
library(janitor)
library(lubridate)
library(scales)

load("images/COHHIOHMIS.RData")

rm(
  Affiliation,
  EmploymentEducation,
  EnrollmentCoC,
  Exit,
  Export,
  Funder,
  Offers,
  ProjectCoC,
  regions,
  VeteranCE
)

# Providers to Check ------------------------------------------------------

projects_current_hmis <- Project %>%
  left_join(Inventory, by = "ProjectID") %>%
  filter(
    ProjectID == 1695 | (
      HMISParticipatingProject == 1 &
        str_detect(ProjectName, "zz", negate = TRUE) &
        # <- since we can't trust projects_current_hmis
        operating_between(., FileStart, FileEnd) &
        (GrantType != "HOPWA" |
           is.na(GrantType)) # excluding HOPWA
    )
  ) %>%
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
    ProjectCounty,
    ProjectRegion
  ) %>% unique()

rm(Inventory, Organization)

# Clients to Check --------------------------------------------------------

served_in_date_range <- Enrollment %>%
  filter(served_between(., FileStart, FileEnd)) %>%
  left_join(Client %>%
              select(-DateCreated), by = "PersonalID") %>%
  select(
    PersonalID,
    FirstName,
    NameDataQuality,
    SSN,
    SSNDataQuality,
    DOB,
    DOBDataQuality,
    AmIndAKNative,
    Asian,
    BlackAfAmerican,
    NativeHIOtherPacific,
    White,
    RaceNone,
    Ethnicity,
    Gender,
    VeteranStatus,
    EnrollmentID,
    ProjectID,
    EntryDate,
    HouseholdID,
    RelationshipToHoH,
    LivingSituation,
    LengthOfStay,
    LOSUnderThreshold,
    PreviousStreetESSH,
    DateToStreetESSH,
    TimesHomelessPastThreeYears,
    AgeAtEntry,
    MonthsHomelessPastThreeYears,
    DisablingCondition,
    DateOfEngagement,
    MoveInDate,
    MoveInDateAdjust,
    EEType,
    CountyServed,
    CountyPrior,
    ExitDate,
    Destination,
    ExitAdjust,
    DateCreated,
    UserCreating,
    ClientEnrolledInPATH,
    LengthOfStay,
    DateOfPATHStatus,
    ReasonNotEnrolled,
    ClientLocation,
    PHTrack,
    ExpectedPHDate
  ) %>%
  inner_join(projects_current_hmis, by = "ProjectID")

DV <- HealthAndDV %>%
  filter(DataCollectionStage == 1) %>%
  select(EnrollmentID, DomesticViolenceVictim, WhenOccurred, CurrentlyFleeing)

served_in_date_range <- served_in_date_range %>%
  left_join(DV, by = "EnrollmentID")

rm(FileStart, FileEnd, FilePeriod, DV)

# The Variables That We Want ----------------------------------------------

vars_prep <- c(
  "HouseholdID",
  "PersonalID",
  "ProjectName",
  "ProjectType",
  "EntryDate",
  "MoveInDateAdjust",
  "ExitDate",
  "UserCreating",
  "ProjectRegion"
)

vars_we_want <- c(vars_prep,
                  "Issue",
                  "Type")

# Missing UDEs ------------------------------------------------------------

dq_name <- served_in_date_range %>%
  # filter(FirstName != "ok") %>%
  mutate(
    Issue = case_when(
      FirstName == "Missing" ~ 
        "Missing Name Data Quality",
      FirstName %in% c("DKR", "Partial") ~
        "Incomplete or Don't Know/Refused Name"
    ),
    Type = case_when(
      Issue == "Missing Name Data Quality" ~ "Error",
      Issue == "Incomplete or Don't Know/Refused Name" ~ "Warning"
    )
  ) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

dq_dob <- served_in_date_range %>%
  mutate(
    Issue = case_when(
      is.na(DOB) & DOBDataQuality %in% c(1, 2) ~ "Missing DOB",
      DOBDataQuality == 99 ~ "Missing Date of Birth Data Quality",
      DOBDataQuality %in% c(2, 8, 9) ~ "Don't Know/Refused or Approx. Date of Birth",
      AgeAtEntry < 0 |
        AgeAtEntry > 95 ~ "Incorrect Date of Birth or Entry Date"
    ),
    Type = case_when(
      Issue %in% c(
        "Missing DOB",
        "Incorrect Date of Birth or Entry Date",
        "Missing Date of Birth Data Quality"
      ) ~ "Error",
      Issue ==  "Don't Know/Refused or Approx. Date of Birth" ~ "Warning"
    )
  ) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

dq_ssn <- served_in_date_range %>%
  mutate(
    Issue = case_when(
      SSN == "Missing" ~ "Missing SSN",
      SSN == "Invalid" ~ "Invalid SSN",
      SSN == "DKR" ~ "Don't Know/Refused SSN",
      SSN == "Incomplete" ~ "Incomplete SSN"
    ),
    Type = case_when(
      Issue %in% c("Missing SSN", "Invalid SSN") ~ "Error",
      Issue == "Don't Know/Refused SSN" ~ "Warning"
    )
  ) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

dq_race <- served_in_date_range %>%
  mutate(
    Issue = case_when(
      RaceNone == 99 ~ "Missing Race",
      RaceNone %in% c(8, 9) ~ "Don't Know/Refused Race"
    ),
    Type = case_when(
      Issue == "Missing Race" ~ "Error",
      Issue == "Don't Know/Refused Race" ~ "Warning"
    )
  ) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

dq_ethnicity <- served_in_date_range %>%
  mutate(
    Issue = case_when(
      Ethnicity == 99 ~ "Missing Ethnicity",
      Ethnicity %in% c(8, 9) ~ "Don't Know/Refused Ethnicity"
    ),
    Type = case_when(
      Issue == "Missing Ethnicity" ~ "Error",
      Issue == "Don't Know/Refused Ethnicity" ~ "Warning"
    )
  ) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

dq_gender <- served_in_date_range %>%
  mutate(
    Issue = case_when(
      Gender == 99 ~ "Missing Gender",
      Gender %in% c(8, 9) ~ "Don't Know/Refused Gender"
    ),
    Type = case_when(
      Issue == "Missing Gender" ~ "Error",
      Issue == "Don't Know/Refused Gender" ~ "Warning"
    )
  ) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

dq_veteran <- served_in_date_range %>%
  mutate(
    Issue = case_when(
      (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
        VeteranStatus == 99 ~ "Missing Veteran Status",
      (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
        VeteranStatus %in% c(8, 9) ~ "Don't Know/Refused Veteran Status",
      (AgeAtEntry >= 18 | is.na(AgeAtEntry)) &
        RelationshipToHoH == 1 &
        VeteranStatus == 0 &
        Destination %in% c(19, 28) ~ "Check Veteran Status for Accuracy"
    ),
    Type = case_when(
      Issue == "Missing Veteran Status" ~ "Error",
      Issue %in% c(
        "Don't Know/Refused Veteran Status",
        "Check Veteran Status for Accuracy"
      ) ~ "Warning"
    )
  ) %>%
  filter(!is.na(Issue)) %>%
  select(all_of(vars_we_want))

# Missing Client Location -------------------------------------------------
# only pulls in Data Collection Stage 1 CoCCode bc none of our
# reporting looks at this at multiple data collection stages

# also this is not included in the final bc I'm waiting on a WS ticket
missing_client_location <- served_in_date_range %>%
  filter(is.na(ClientLocation),
         RelationshipToHoH == 1) %>%
  mutate(Type = "High Priority",
         Issue = "Missing Client Location") %>%
  select(all_of(vars_we_want))

# Household Issues --------------------------------------------------------

hh_children_only <- served_in_date_range %>%
  filter(GrantType != "RHY" |
           is.na(GrantType)) %>% # not checking for children-only hhs for RHY
  group_by(HouseholdID) %>%
  summarise(
    hhMembers = n(),
    maxAge = max(AgeAtEntry),
    PersonalID = min(PersonalID)
  ) %>%
  filter(maxAge < 18) %>%
  ungroup() %>%
  left_join(served_in_date_range, by = c("PersonalID", "HouseholdID")) %>%
  mutate(Issue = "Children Only Household",
         Type = "High Priority") %>%
  select(all_of(vars_we_want))

hh_no_hoh <- served_in_date_range %>%
  group_by(HouseholdID) %>%
  summarise(hasHoH = if_else(min(RelationshipToHoH) != 1,
                             FALSE,
                             TRUE),
            PersonalID = min(PersonalID)) %>%
  filter(hasHoH == FALSE) %>%
  ungroup() %>%
  left_join(served_in_date_range, by = c("PersonalID", "HouseholdID")) %>%
  mutate(Issue = "No Head of Household",
         Type = "High Priority") %>%
  select(all_of(vars_we_want))

hh_too_many_hohs <- served_in_date_range %>%
  filter(RelationshipToHoH == 1) %>%
  group_by(HouseholdID) %>%
  summarise(HoHsinHousehold = n(),
            PersonalID = min(PersonalID)) %>%
  filter(HoHsinHousehold > 1) %>%
  ungroup() %>%
  left_join(served_in_date_range, by = c("PersonalID", "HouseholdID")) %>%
  mutate(Issue = "Too Many Heads of Household",
         Type = "High Priority") %>%
  select(all_of(vars_we_want))

hh_issues <- rbind(hh_too_many_hohs, hh_no_hoh, hh_children_only)

rm(hh_too_many_hohs, hh_no_hoh, hh_children_only)

# Missing Data at Entry ---------------------------------------------------
# Living Situation,  Length of Stay, LoSUnderThreshold, PreviousStreetESSH,
# DateToStreetESSH, TimesHomelessPastThreeYears, MonthsHomelessPastThreeYears

missing_approx_date_homeless <- served_in_date_range %>%
  select(
    all_of(vars_prep),
    EnrollmentID,
    ProjectID,
    AgeAtEntry,
    RelationshipToHoH,
    LOSUnderThreshold,
    DateToStreetESSH,
    PreviousStreetESSH
  ) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           ymd(EntryDate) >= mdy("10012016") &
           is.na(DateToStreetESSH) &
           LOSUnderThreshold == 1 &
           PreviousStreetESSH == 1
  ) %>%
  mutate(Issue = "Missing Approximate Date Homeless", Type = "Error") %>%
  select(all_of(vars_we_want))

missing_previous_street_ESSH <- served_in_date_range %>%
  select(
    all_of(vars_prep),
    AgeAtEntry,
    RelationshipToHoH,
    DateToStreetESSH,
    PreviousStreetESSH,
    LOSUnderThreshold
  ) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           ymd(EntryDate) >= mdy("10012016") &
           is.na(PreviousStreetESSH) &
           LOSUnderThreshold == 1
  ) %>%
  mutate(Issue = "Missing Previously From Street, ES, or SH (Length of Time Homeless questions)",
         Type = "Error") %>%
  select(all_of(vars_we_want))

missing_residence_prior <- served_in_date_range %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         RelationshipToHoH,
         LivingSituation) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           (is.na(LivingSituation) | LivingSituation == 99)) %>%
  mutate(Issue = "Missing Residence Prior",
         Type = "Error") %>%
  select(all_of(vars_we_want))

dkr_residence_prior <- served_in_date_range %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         RelationshipToHoH,
         LivingSituation) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           LivingSituation %in% c(8, 9)) %>%
  mutate(Issue = "Don't Know/Refused Residence Prior",
         Type = "Warning") %>%
  select(all_of(vars_we_want))

missing_LoS <- served_in_date_range %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         RelationshipToHoH,
         LengthOfStay) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           (is.na(LengthOfStay) | LengthOfStay == 99)) %>%
  mutate(Issue = "Missing Length of Stay",
         Type = "Error") %>%
  select(all_of(vars_we_want))

dkr_LoS <- served_in_date_range %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         RelationshipToHoH,
         LengthOfStay) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           LengthOfStay %in% c(8, 9)) %>%
  mutate(Issue = "Don't Know/Refused Residence Prior",
         Type = "Warning") %>%
  select(all_of(vars_we_want))

missing_months_times_homeless <- served_in_date_range %>%
  select(
    all_of(vars_prep),
    AgeAtEntry,
    RelationshipToHoH,
    MonthsHomelessPastThreeYears,
    TimesHomelessPastThreeYears
  ) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           ymd(EntryDate) >= mdy("10012016") &
           ProjectType %in% c(1, 4, 8) &
           (
             is.na(MonthsHomelessPastThreeYears) |
               is.na(TimesHomelessPastThreeYears) |
               MonthsHomelessPastThreeYears == 99 |
               TimesHomelessPastThreeYears == 99
           )
  ) %>%
  mutate(Issue = "Missing Months or Times Homeless",
         Type = "Error") %>%
  select(all_of(vars_we_want))

dkr_months_times_homeless <- served_in_date_range %>%
  select(
    all_of(vars_prep),
    AgeAtEntry,
    RelationshipToHoH,
    MonthsHomelessPastThreeYears,
    TimesHomelessPastThreeYears
  ) %>%
  filter((RelationshipToHoH == 1 | AgeAtEntry > 17) &
           ymd(EntryDate) >= mdy("10012016") &
           (
             MonthsHomelessPastThreeYears %in% c(8, 9) |
               TimesHomelessPastThreeYears %in% c(8, 9)
           )
  ) %>%
  mutate(Issue = "Don't Know/Refused Months or Times Homeless",
         Type = "Warning") %>%
  select(all_of(vars_we_want))

detail_missing_living_situation <- served_in_date_range %>%
  select(
    all_of(vars_prep),
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
           ymd(EntryDate) >= mdy("10012016") &
           # not req'd prior to this
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
  select(all_of(vars_we_want))

dkr_living_situation <- served_in_date_range %>%
  select(
    PersonalID,
    HouseholdID,
    EnrollmentID,
    ProjectID,
    ProjectType,
    ProjectName,
    ProjectRegion,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    AgeAtEntry,
    CountyServed,
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
  mutate(Issue = "Don't Know/Refused Living Situation", Type = "Warning") %>%
  select(all_of(vars_we_want))

# DisablingCondition at Entry

detail_missing_disabilities <- served_in_date_range %>%
  select(all_of(vars_prep),
         AgeAtEntry,
         RelationshipToHoH,
         DisablingCondition) %>%
  filter(DisablingCondition == 99 |
           is.na(DisablingCondition)) %>%
  mutate(Issue = "Missing Disabling Condition", Type = "Error")

missing_disabilities <- detail_missing_disabilities %>%
  select(all_of(vars_we_want))


smallDisabilities <- Disabilities %>%
  filter(DataCollectionStage == 1 &
           ((DisabilityType == 10 &
               DisabilityResponse %in% c(1:3)) |
              (DisabilityType != 10 & DisabilityResponse == 1)
           )) %>%
  mutate(
    IndefiniteAndImpairs =
      case_when(
        DisabilityType %in% c(6, 8) ~ 1,
        TRUE ~ IndefiniteAndImpairs)
  ) %>%
  select(
    PersonalID,
    DisabilitiesID,
    EnrollmentID,
    InformationDate,
    DisabilityType,
    IndefiniteAndImpairs
  )

# Developmental & HIV/AIDS get automatically IndefiniteAndImpairs = 1 per FY2020

rm(Disabilities)

conflicting_disabilities <- served_in_date_range %>%
  select(all_of(vars_prep),
         EnrollmentID,
         AgeAtEntry,
         RelationshipToHoH,
         DisablingCondition) %>%
  left_join(
    smallDisabilities %>%
      filter(IndefiniteAndImpairs == 1),
    by = c("PersonalID", "EnrollmentID")
  ) %>% 
  filter((DisablingCondition == 0 & !is.na(DisabilitiesID)) |
           (DisablingCondition == 1 & is.na(DisabilitiesID))) %>% 
  mutate(
    Issue = "Conflicting Disability yes/no",
    Type = "Error"
  ) %>%
  select(all_of(vars_we_want))

rm(detail_conflicting_disabilities, smallDisabilities)

# Extremely Long Stayers --------------------------------------------------

th_stayers <- served_in_date_range %>%
  select(all_of(vars_prep)) %>%
  mutate(Days = as.numeric(difftime(today(), ymd(EntryDate)))) %>%
  filter(is.na(ExitDate) &
           ProjectType == 2)

Top2_TH <- subset(th_stayers, Days > quantile(Days, prob = 1 - 2 / 100))

rrh_stayers <- served_in_date_range %>%
  select(all_of(vars_prep)) %>%
  filter(is.na(ExitDate) &
           ProjectType == 13) %>%
  mutate(Days = as.numeric(difftime(today(), ymd(EntryDate)))) 

Top2_RRH <- subset(rrh_stayers, Days > quantile(Days, prob = 1 - 2 / 100))

es_stayers <- served_in_date_range %>%
  select(all_of(vars_prep)) %>%
  filter(is.na(ExitDate) &
           ProjectType == 1) %>%
  mutate(Days = as.numeric(difftime(today(), ymd(EntryDate)))) 

Top2_ES <- subset(es_stayers, Days > quantile(Days, prob = 1 - 2 / 100))

psh_stayers <- served_in_date_range %>%
  select(all_of(vars_prep)) %>%
  filter(is.na(ExitDate) &
           ProjectType == 3) %>%
  mutate(Days = as.numeric(difftime(today(), ymd(EntryDate)))) 

Top1_PSH <- subset(psh_stayers, Days > quantile(Days, prob = 1 - 1 / 100))

hp_stayers <- served_in_date_range %>%
  select(all_of(vars_prep)) %>%
  filter(is.na(ExitDate) &
           ProjectType == 12) %>%
  mutate(Days = as.numeric(difftime(today(), ymd(EntryDate)))) 

Top5_HP <- subset(hp_stayers, Days > quantile(Days, prob = 1 - 5 / 100))

extremely_long_stayers <- rbind(Top1_PSH,
                                Top2_ES,
                                Top2_RRH,
                                Top2_TH,
                                Top5_HP) %>%
  mutate(Issue = "Extremely Long Stayer",
         Type = "Warning") %>%
  select(all_of(vars_we_want))

rm(list = ls(pattern = "Top*"),
   es_stayers,
   th_stayers,
   psh_stayers,
   rrh_stayers,
   hp_stayers)

# CountyServed

missing_county_served <- served_in_date_range %>%
  filter(is.na(CountyServed)) %>%
  mutate(Issue = "Missing County Served",
         Type = "Error") %>%
  select(all_of(vars_we_want))

# CountyPrior
# check to see if all hh members have to answer this or if just adults or all?
missing_county_prior <- served_in_date_range %>%
  filter(is.na(CountyPrior),
         RelationshipToHoH == 1) %>%
  mutate(Issue = "Missing County of Prior Residence",
         Type = "Error") %>%
  select(all_of(vars_we_want))

# Check Eligibility, Project Type, Residence Prior ------------------------

check_eligibility <- served_in_date_range %>%
  select(
    all_of(vars_prep),
    ProjectID,
    AgeAtEntry,
    RelationshipToHoH,
    LivingSituation,
    LengthOfStay,
    LOSUnderThreshold,
    PreviousStreetESSH
  ) %>%
  filter(
    RelationshipToHoH == 1 &
      AgeAtEntry > 17 &
      ymd(EntryDate) > mdy("10012016") &
      ProjectID != 1859 &
      # "Crisis TH" which should be treated like an es
      (
        (ProjectType %in% c(2, 3, 9, 10, 13) &
           # PTCs that require LH status
           (
             is.na(LivingSituation) |
               (
                 LivingSituation %in% c(4:7, 15, 25:27, 29) & # institution
                   (
                     !LengthOfStay %in% c(2, 3, 10, 11) | # <90 days
                       is.na(LengthOfStay) |
                       PreviousStreetESSH == 0 | # LH prior
                       is.na(PreviousStreetESSH)
                   )
               ) |
               (
                 LivingSituation %in% c(3, 10, 11, 19:23, 28, 31, 35, 36) &
                   # not homeless
                   (
                     !LengthOfStay %in% c(10, 11) |  # <1 week
                       is.na(LengthOfStay) |
                       PreviousStreetESSH == 0 | # LH prior
                       is.na(PreviousStreetESSH)
                   )
               )
           )) |
          (
            ProjectType == 12 &
              !LivingSituation %in% c(3, 10, 11, 19:23, 28, 31, 35, 36)
          ) |
          (ProjectType %in% c(8, 4) & # Safe Haven and Outreach
             LivingSituation != 16) # unsheltered only
      )
  ) 
    
    detail_eligibility <- check_eligibility %>%
      select(
        PersonalID,
        ProjectName,
        ProjectType,
        LivingSituation,
        EntryDate,
        ExitDate,
        LengthOfStay,
        LOSUnderThreshold,
        PreviousStreetESSH
      ) %>%
      mutate(
        ResidencePrior =
          living_situation(LivingSituation),
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
        )
      )
    
    check_eligibility <- check_eligibility %>%
      mutate(Issue = "Check Eligibility", Type = "Warning") %>%
      select(all_of(vars_we_want))
    
    # Missing Destination
    missing_destination <- served_in_date_range %>%
      filter(!is.na(ExitDate) &
               (is.na(Destination) | Destination %in% c(99, 30))) %>%
      mutate(Issue = "Missing Destination",
             Type = "Warning") %>%
      select(all_of(vars_we_want))
    
    dkr_destination <- served_in_date_range %>%
      filter(Destination %in% c(8, 9)) %>%
      mutate(Issue = "Don't Know/Refused Destination",
             Type = "Warning") %>%
      select(all_of(vars_we_want))
    
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
                                       ProjectCounty)
    
    path_missing_los_res_prior <- served_in_date_range %>%
      select(
        all_of(vars_prep),
        ProjectID,
        AgeAtEntry,
        ClientEnrolledInPATH,
        LengthOfStay,
        EEType
      ) %>%
      left_join(smallProject, by = c("ProjectID", "ProjectName")) %>%
      filter(EEType == "PATH" &
               AgeAtEntry > 17 &
               ClientEnrolledInPATH == 1 &
               (is.na(LengthOfStay) | LengthOfStay == 99)) %>%
      mutate(Issue = "Missing Residence Prior Length of Stay (PATH)",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    
    #* Engagement at Exit
    ### adult, PATH-enrolled, Date of Engagement is null -> error
    
    path_no_status_at_exit <- served_in_date_range %>%
      select(
        all_of(vars_prep),
        AgeAtEntry,
        ClientEnrolledInPATH,
        DateOfPATHStatus,
        ReasonNotEnrolled,
        EEType
      ) %>%
      left_join(smallProject, by = "ProjectName") %>%
      filter(EEType == "PATH" &
               !is.na(ExitDate) &
               AgeAtEntry > 17 &
               (
                 is.na(ClientEnrolledInPATH) |
                   is.na(DateOfPATHStatus) |
                   (ClientEnrolledInPATH == 0 &
                      is.na(ReasonNotEnrolled))
               )) %>%
      mutate(Issue = "PATH Status at Exit Missing or Incomplete",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    #* Status Determination at Exit
    ### adult, PATH-Enrolled is not null
    ### Date of Status Determ is null -> error
    path_status_determination <- served_in_date_range %>%
      select(all_of(vars_prep),
             AgeAtEntry,
             ClientEnrolledInPATH,
             DateOfPATHStatus,
             EEType) %>%
      left_join(smallProject, by = "ProjectName") %>%
      filter(
        EEType == "PATH" &
          AgeAtEntry > 17 &
          !is.na(ClientEnrolledInPATH) &
          is.na(DateOfPATHStatus)
      ) %>%
      mutate(Issue = "Missing Date of PATH Status",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    #* PATH Enrolled at Exit
    ### adult and:
    ### PATH Enrolled null or DNC -> error -OR-
    
    path_enrolled_missing <- served_in_date_range %>%
      select(all_of(vars_prep), AgeAtEntry, ClientEnrolledInPATH, EEType) %>%
      left_join(smallProject, by = "ProjectName") %>%
      filter(
        EEType == "PATH" &
          !is.na(ExitDate) &
          AgeAtEntry > 17 &
          (ClientEnrolledInPATH == 99 |
             is.na(ClientEnrolledInPATH))
      ) %>%
      mutate(Issue = "Missing PATH Enrollment at Exit",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    #* Not Enrolled Reason
    ### adult
    ### PATH Enrolled = No
    ### Reason is null -> error
    
    path_reason_missing <- served_in_date_range %>%
      select(
        all_of(vars_prep),
        AgeAtEntry,
        ClientEnrolledInPATH,
        EEType,
        ReasonNotEnrolled,
        ProjectType
      ) %>%
      left_join(smallProject, by = "ProjectName") %>%
      filter(EEType == "PATH" &
               AgeAtEntry > 17 &
               ClientEnrolledInPATH == 0 &
               is.na(ReasonNotEnrolled)) %>%
      mutate(Issue = "Missing Reason Not PATH Enrolled",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    #* Connection with SOAR at Exit
    ### adult
    ### Connection w/ SOAR is null or DNC -> error -OR-
    ### Connection w/ SOAR DKR -> warning
    
    smallIncomeSOAR <- IncomeBenefits %>%
      select(PersonalID,
             EnrollmentID,
             ConnectionWithSOAR,
             DataCollectionStage) %>%
      filter(DataCollectionStage == 3)
    
    path_SOAR_missing_at_exit <- served_in_date_range %>%
      select(all_of(vars_prep),
             EnrollmentID,
             AgeAtEntry,
             ClientEnrolledInPATH,
             EEType) %>%
      left_join(smallProject, by = "ProjectName") %>%
      left_join(smallIncomeSOAR, by = c("PersonalID", "EnrollmentID")) %>%
      filter(EEType == "PATH" &
               AgeAtEntry > 17 &
               DataCollectionStage == 3 &
               is.na(ConnectionWithSOAR)) %>%
      mutate(Issue = "Missing Connection with SOAR at Exit",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    rm(smallIncomeSOAR)
    
    # Missing PATH Contacts
    ## client is adult/hoh and has no contact record in the EE -> error
    ## this is a high priority data quality issue
    
    # Incorrect PATH Contact Date
    ## client is adult/hoh, has a contact record, and the first record in the EE
    ## does not equal the Entry Date ->  error
    
    # Missing PATH Contact End Date
    ## client is adult/hoh, has a contact record, and the End Date is null -> error
    
    # Duplicate EEs -----------------------------------------------------------
    # this could be more nuanced
    duplicate_ees <-
      get_dupes(served_in_date_range, PersonalID, ProjectID, EntryDate) %>%
      mutate(Issue = "Duplicate Entry Exits", Type = "High Priority") %>%
      select(all_of(vars_we_want))
    
    
    # Future Entry Exits ------------------------------------------------------
    # PSHs in the old days before Move In Dates would definitely have been entering
    # their clients prior to their Entry Date since back then the Entry Date was the
    # day they moved in. So they're excused from this prior to Move In Date's existence.
    
    future_ees <- served_in_date_range %>%
      filter(ymd(EntryDate) > ymd_hms(DateCreated) &
               (ProjectType %in% c(1, 2, 4, 8) |
                  (
                    ProjectType %in% c(3, 9) & ymd(EntryDate) >= mdy("10012017")
                  )))  %>%
      mutate(Issue = "Future Entry Date", Type = "Warning") %>%
      select(all_of(vars_we_want))
    
    # Incorrect Entry Exit Type -----------------------------------------------
    # check ART report for exact logic.
    incorrect_ee_type <- served_in_date_range %>%
      filter(
        (
          is.na(GrantType) &
            !grepl("GPD", ProjectName) &
            !grepl("HCHV", ProjectName) &
            !grepl("VET", ProjectName) &
            !grepl("Veterans", ProjectName) &
            ProjectID != 1695 &
            EEType != "HUD"
        ) |
          ((
            GrantType == "SSVF" |
              grepl("GPD", ProjectName) |
              grepl("HCHV", ProjectName) |
              grepl("Veterans", ProjectName) &
              grepl("VET", ProjectName)
          ) &
            EEType != "VA"
          ) |
          (GrantType == "RHY" &
             !grepl("YHDP", ProjectName) &
             EEType != "RHY") |
          (GrantType == "RHY" &
             grepl("YHDP", ProjectName) &
             EEType != "HUD") |
          (GrantType == "PATH" & EEType != "PATH") |
          (ProjectID == 1695 & EEType != "Standard")
      ) %>%
      mutate(Issue = "Incorrect Entry Exit Type",
             Type = "High Priority") %>%
      select(all_of(vars_we_want))
    
    # HoHs Entering PH without SPDATs -----------------------------------------
    
    ees_with_spdats <-
      left_join(served_in_date_range, Scores, by = "PersonalID") %>%
      select(PersonalID,
             EnrollmentID,
             RelationshipToHoH,
             EntryDate,
             ExitAdjust,
             ScoreDate,
             Score) %>%
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
      select(-MaxScoreDate, -MaxScore) %>%
      mutate(ScoreAdjusted = if_else(is.na(Score), 0, Score))
    
    rm(Scores)
    
    entered_ph_without_spdat <-
      anti_join(served_in_date_range, ees_with_spdats, by = "EnrollmentID") %>%
      filter(
        ProjectType %in% c(3, 9, 13) &
          !grepl("SSVF", ProjectName) &
          !grepl("GPD", ProjectName) &
          ymd(EntryDate) > ymd("20190101") &
          # only looking at 1/1/2019 forward
          RelationshipToHoH == 1 &
          (VeteranStatus != 1 |
             is.na(VeteranStatus)) &
          (CurrentlyFleeing != 1 |
             is.na(CurrentlyFleeing))
      ) %>%
      mutate(Issue = "Non-Veteran Non-DV HoHs Entering PH without SPDAT",
             Type = "Warning") %>%
      select(all_of(vars_we_want))
    
    # HoHs in Shelter without a SPDAT -----------------------------------------
    # this is a little different than the ART report; it only flags stayers
    # since users can't do anything about leavers
    lh_without_spdat <- served_in_date_range %>%
      filter(is.na(PHTrack) | PHTrack != "Self Resolve" |
               ymd(ExpectedPHDate) < today()) %>%
      anti_join(ees_with_spdats, by = "EnrollmentID") %>%
      filter(
        ProjectType %in% c(1, 2, 4, 8) &
          VeteranStatus != 1 &
          RelationshipToHoH == 1 &
          ymd(EntryDate) < today() - days(8) &
          is.na(ExitDate) &
          ymd(EntryDate) > ymd("20190101")
      ) %>%
      mutate(Issue = "HoHs in shelter or Transitional Housing for 8+ days without SPDAT",
             Type = "Warning") %>%
      select(all_of(vars_we_want))
    
    spdat_on_non_hoh <- ees_with_spdats %>%
      left_join(
        served_in_date_range,
        by = c(
          "PersonalID",
          "EnrollmentID",
          "RelationshipToHoH",
          "EntryDate",
          "ExitAdjust"
        )
      ) %>%
      filter(RelationshipToHoH != 1) %>%
      mutate(Issue = "SPDAT Created on a Non-Head-of-Household",
             Type = "Warning") %>%
      select(all_of(vars_we_want))
    
    rm(ees_with_spdats)
    
    # Missing Income at Entry -------------------------------------------------
    IncomeBenefits <- IncomeBenefits %>% select(-DateCreated)
    
    missing_income_entry <- served_in_date_range %>%
      left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
      select(
        all_of(vars_prep),
        AgeAtEntry,
        DataCollectionStage,
        TotalMonthlyIncome,
        IncomeFromAnySource
      ) %>%
      filter(DataCollectionStage == 1 &
               (AgeAtEntry > 17 |
                  is.na(AgeAtEntry)) &
               (IncomeFromAnySource == 99 |
                  is.na(IncomeFromAnySource))) %>%
      mutate(Issue = "Income Missing at Entry",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    smallIncome <- IncomeBenefits %>%
      select(
        PersonalID,
        EnrollmentID,
        Earned,
        Unemployment,
        SSI,
        SSDI,
        VADisabilityService,
        VADisabilityNonService,
        PrivateDisability,
        WorkersComp,
        TANF,
        GA,
        SocSecRetirement,
        Pension,
        ChildSupport,
        Alimony,
        OtherIncomeSource,
        DataCollectionStage
      )
    
    smallIncome[is.na(smallIncome)] <- 0
    
    smallIncome <-
      smallIncome %>% full_join(IncomeBenefits[c(
        "PersonalID",
        "EnrollmentID",
        "DataCollectionStage",
        "TotalMonthlyIncome",
        "IncomeFromAnySource"
      )],
      by = c("PersonalID",
             "EnrollmentID",
             "DataCollectionStage"))
    
    income_subs <- served_in_date_range[c("EnrollmentID",
                                          "AgeAtEntry",
                                          vars_prep)] %>%
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
    
    
    conflicting_income_entry <- income_subs %>%
      filter(DataCollectionStage == 1 &
               (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
               ((IncomeFromAnySource == 1 &
                   IncomeCount == 0) |
                  (IncomeFromAnySource == 0 &
                     IncomeCount > 0)
               )) %>%
      mutate(Issue = "Conflicting Income yes/no at Entry",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    # Not calculating Conflicting Income Amounts bc they're calculating the TMI from the
    # subs instead of using the field itself. Understandable but that means I would
    # have to pull the TMI data in through RMisc OR we kill TMI altogether. (We
    # decided to kill TMI altogether.)
    
    # Missing Income at Exit --------------------------------------------------
    
    missing_income_exit <- served_in_date_range %>%
      left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
      select(
        all_of(vars_prep),
        AgeAtEntry,
        DataCollectionStage,
        TotalMonthlyIncome,
        IncomeFromAnySource,
        UserCreating
      ) %>%
      filter(DataCollectionStage == 3 &
               (AgeAtEntry > 17 |
                  is.na(AgeAtEntry)) &
               (IncomeFromAnySource == 99 |
                  is.na(IncomeFromAnySource))) %>%
      mutate(Issue = "Income Missing at Exit",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    conflicting_income_exit <- income_subs %>%
      filter(DataCollectionStage == 3 &
               (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
               ((IncomeFromAnySource == 1 &
                   IncomeCount == 0) |
                  (IncomeFromAnySource == 0 &
                     IncomeCount > 0)
               )) %>%
      mutate(Issue = "Conflicting Income yes/no at Exit",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    rm(income_subs)
    
    # conflictingIncomeYN <- served_in_date_range %>%
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
    #   select(all_of(vars_we_want))
    
    # Overlapping Enrollment/Move In Dates ------------------------------------
    
    # this only pulls the most recent EE in the overlap and I think that's fine but
    # some users won't like being flagged for it if it's someone else's fault
    # but you can't tell whose fault it is from the data so...
    
    staging_overlaps <- served_in_date_range %>%
      select(all_of(vars_prep), ExitAdjust) %>%
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
        Type = "High Priority"
      ) %>%
      filter(!is.na(LiterallyInProject) &
               int_length(LiterallyInProject) > 0) %>%
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
    
    same_day_overlaps <- served_in_date_range %>%
      filter((ProjectType == 13 & MoveInDateAdjust == ExitDate) |
               ProjectType != 13) %>%
      select(all_of(vars_prep), ExitAdjust) %>%
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
        LiterallyInProject = case_when(
          ProjectType %in% c(3, 9) ~ interval(MoveInDateAdjust, ExitAdjust),
          ProjectType %in% c(1, 2, 4, 8, 12) ~ interval(EntryAdjust, ExitAdjust)
        ),
        Issue = "Overlapping Project Stays",
        Type = "High Priority"
      ) %>%
      filter((!is.na(LiterallyInProject) & ProjectType != 13) |
               ProjectType == 13) %>%
      get_dupes(., PersonalID) %>%
      group_by(PersonalID) %>%
      arrange(PersonalID, EntryAdjust) %>%
      mutate(
        PreviousEntryAdjust = lag(EntryAdjust),
        PreviousExitAdjust = lag(ExitAdjust),
        PreviousProject = lag(ProjectName)
      ) %>%
      filter(ExitDate > PreviousEntryAdjust &
               ExitDate < PreviousExitAdjust) %>%
      ungroup() %>%
      select(all_of(vars_we_want), PreviousProject)
    
    rrh_overlaps <- served_in_date_range %>%
      select(all_of(vars_prep), ExitAdjust) %>%
      mutate(
        ExitAdjust = ExitAdjust - days(1),
        # bc a client can exit&enter same day
        InProject = interval(EntryDate, ExitAdjust),
        Issue = "Overlapping Project Stays",
        Type = "High Priority"
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
      select(all_of(vars_we_want), PreviousProject)
    
    psh_overlaps <- served_in_date_range %>%
      select(all_of(vars_prep), ExitAdjust) %>%
      mutate(
        ExitAdjust = ExitAdjust - days(1),
        # bc a client can exit&enter same day
        InProject = interval(EntryDate, ExitAdjust),
        Issue = "Overlapping Project Stays",
        Type = "High Priority"
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
      select(all_of(vars_we_want), PreviousProject)
    
    dq_overlaps <- staging_overlaps %>%
      mutate(
        PreviousStay = interval(PreviousEntryAdjust, PreviousExitAdjust),
        Overlap = int_overlaps(LiterallyInProject, PreviousStay)
      ) %>%
      filter(Overlap == TRUE) %>%
      select(all_of(vars_we_want), PreviousProject)
    
    dq_overlaps <-
      rbind(dq_overlaps, rrh_overlaps, psh_overlaps, same_day_overlaps)
    
    rm(staging_overlaps,
       same_day_overlaps,
       rrh_overlaps,
       psh_overlaps)
    
    unsh_overlaps <- dq_overlaps %>%
      filter(ProjectName == "Unsheltered Clients - OUTREACH") %>%
      left_join(Users, by = "UserCreating") %>%
      select(PersonalID,
             DefaultProvider,
             EntryDate,
             ExitDate,
             PreviousProject)
    
    # Missing Health Ins ------------------------------------------------------
    
    missing_health_insurance_entry <- served_in_date_range %>%
      left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
      select(all_of(vars_prep),
             AgeAtEntry,
             DataCollectionStage,
             InsuranceFromAnySource) %>%
      filter(DataCollectionStage == 1 &
               (InsuranceFromAnySource == 99 |
                  is.na(InsuranceFromAnySource))) %>%
      mutate(Issue = "Health Insurance Missing at Entry",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    missing_health_insurance_exit <- served_in_date_range %>%
      left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
      select(all_of(vars_prep),
             DataCollectionStage,
             InsuranceFromAnySource) %>%
      filter(DataCollectionStage == 3 &
               (InsuranceFromAnySource == 99 |
                  is.na(InsuranceFromAnySource))) %>%
      mutate(Issue = "Health Insurance Missing at Exit",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    health_insurance_subs <- served_in_date_range %>%
      left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
      select(
        all_of(vars_prep),
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
      mutate(
        SourceCount = Medicaid + SCHIP + VAMedicalServices + EmployerProvided +
          COBRA + PrivatePay + StateHealthIns + IndianHealthServices +
          OtherInsurance + Medicare
      )
    
    conflicting_health_insurance_entry <- health_insurance_subs %>%
      filter(DataCollectionStage == 1 &
               ((InsuranceFromAnySource == 1 &
                   SourceCount == 0) |
                  (InsuranceFromAnySource == 0 &
                     SourceCount > 0)
               )) %>%
      mutate(Issue = "Conflicting Health Insurance yes/no at Entry",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    conflicting_health_insurance_exit <- health_insurance_subs %>%
      filter(DataCollectionStage == 3 &
               ((InsuranceFromAnySource == 1 &
                   SourceCount == 0) |
                  (InsuranceFromAnySource == 0 &
                     SourceCount > 0)
               )) %>%
      mutate(Issue = "Conflicting Health Insurance yes/no at Exit",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    rm(health_insurance_subs)
    
    # Missing NCBs at Entry ---------------------------------------------------
    
    ncb_subs <- IncomeBenefits %>%
      select(
        PersonalID,
        EnrollmentID,
        DataCollectionStage,
        SNAP,
        WIC,
        TANFChildCare,
        TANFTransportation,
        OtherTANF,
        OtherBenefitsSource
      )
    
    ncb_subs[is.na(ncb_subs)] <- 0
    
    ncb_subs <- ncb_subs %>%
      full_join(IncomeBenefits[c("PersonalID",
                                 "EnrollmentID",
                                 "DataCollectionStage",
                                 "BenefitsFromAnySource")],
                by = c("PersonalID",
                       "EnrollmentID",
                       "DataCollectionStage"))
    
    ncb_subs <- served_in_date_range %>%
      left_join(ncb_subs, by = c("PersonalID", "EnrollmentID")) %>%
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
        SNAP,
        WIC,
        TANFChildCare,
        TANFTransportation,
        OtherTANF,
        OtherBenefitsSource,
        UserCreating
      ) %>%
      mutate(
        BenefitCount = SNAP + WIC + TANFChildCare + TANFTransportation +
          OtherTANF + OtherBenefitsSource
      ) %>%
      select(PersonalID,
             EnrollmentID,
             DataCollectionStage,
             BenefitsFromAnySource,
             BenefitCount) %>%
      unique()
    
    missing_ncbs_entry <- served_in_date_range %>%
      left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
      select(AgeAtEntry,
             all_of(vars_prep),
             DataCollectionStage,
             BenefitsFromAnySource) %>%
      filter(
        DataCollectionStage == 1 &
          (AgeAtEntry > 17 |
             is.na(AgeAtEntry)) &
          (BenefitsFromAnySource == 99 |
             is.na(BenefitsFromAnySource))
      ) %>%
      mutate(Issue = "Non-cash Benefits Missing at Entry",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    conflicting_ncbs_entry <- served_in_date_range %>%
      left_join(ncb_subs, by = c("PersonalID", "EnrollmentID")) %>%
      select(AgeAtEntry,
             all_of(vars_prep),
             DataCollectionStage,
             BenefitsFromAnySource,
             BenefitCount) %>%
      filter(DataCollectionStage == 1 &
               (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
               ((BenefitsFromAnySource == 1 &
                   BenefitCount == 0) |
                  (BenefitsFromAnySource == 0 &
                     BenefitCount > 0)
               )) %>%
      mutate(Issue = "Conflicting Non-cash Benefits yes/no at Entry",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    # Missing NCBs at Exit ----------------------------------------------------
    missing_ncbs_exit <- served_in_date_range %>%
      left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
      select(AgeAtEntry,
             all_of(vars_prep),
             DataCollectionStage,
             BenefitsFromAnySource) %>%
      filter(
        DataCollectionStage == 3 &
          (AgeAtEntry > 17 |
             is.na(AgeAtEntry)) &
          (BenefitsFromAnySource == 99 |
             is.na(BenefitsFromAnySource))
      ) %>%
      mutate(Issue = "Non-cash Benefits Missing at Exit",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    conflicting_ncbs_exit <- served_in_date_range %>%
      left_join(ncb_subs, by = c("PersonalID", "EnrollmentID")) %>%
      select(
        AgeAtEntry,
        all_of(vars_prep),
        DataCollectionStage,
        BenefitsFromAnySource,
        BenefitCount
      ) %>%
      filter(DataCollectionStage == 3 &
               (AgeAtEntry > 17 | is.na(AgeAtEntry)) &
               ((BenefitsFromAnySource == 1 &
                   BenefitCount == 0) |
                  (BenefitsFromAnySource == 0 &
                     BenefitCount > 0)
               )) %>%
      mutate(Issue = "Conflicting Non-cash Benefits yes/no at Exit",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    rm(ncb_subs)
    
    # SSI/SSDI but no Disability (Q) ------------------------------------------
    smallIncome <- IncomeBenefits %>%
      select(EnrollmentID, PersonalID, SSI, SSDI)
    
    check_disability_ssi <- served_in_date_range %>%
      select(all_of(vars_prep),
             EnrollmentID,
             AgeAtEntry,
             DisablingCondition) %>%
      left_join(smallIncome, by = c("EnrollmentID", "PersonalID")) %>%
      mutate(SSI = if_else(is.na(SSI), 0, SSI),
             SSDI = if_else(is.na(SSDI), 0, SSDI)) %>%
      filter(SSI + SSDI > 0 &
               DisablingCondition == 0 & AgeAtEntry > 17) %>%
      select(-DisablingCondition,-SSI,-SSDI,-AgeAtEntry) %>%
      unique() %>%
      mutate(Issue = "Client with No Disability Receiving SSI/SSDI (could be ok)",
             Type = "Warning") %>%
      select(all_of(vars_we_want))
    
    rm(IncomeBenefits, smallIncome)
    
    # Non HoHs w Svcs or Referrals --------------------------------------------
    # SSVF projects should be showing this as an Error, whereas non-SSVF projects
    # should be showing it as a warning, and only back to Feb of 2018.
    services_on_hh_members <- served_in_date_range %>%
      select(all_of(vars_prep),
             EnrollmentID,
             RelationshipToHoH,
             GrantType) %>%
      filter(
        RelationshipToHoH != 1 &
          ymd(EntryDate) >= mdy("02012019") &
          (GrantType != "SSVF" | is.na(GrantType))
      ) %>%
      semi_join(Services, by = c("PersonalID", "EnrollmentID")) %>%
      mutate(Issue = "Service Transaction on a Non Head of Household",
             Type = "Warning") %>%
      select(all_of(vars_we_want))
    
    services_on_hh_members_ssvf <- served_in_date_range %>%
      select(all_of(vars_prep),
             EnrollmentID,
             RelationshipToHoH,
             GrantType) %>%
      filter(RelationshipToHoH != 1 &
               GrantType == "SSVF") %>%
      semi_join(Services, by = c("PersonalID", "EnrollmentID")) %>%
      mutate(Issue = "Service Transaction on a Non Head of Household (SSVF)",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    rm(Services)
    
    referrals_on_hh_members <- served_in_date_range %>%
      select(all_of(vars_prep),
             RelationshipToHoH,
             EnrollmentID,
             GrantType) %>%
      filter(RelationshipToHoH != 1 &
               (GrantType != "SSVF"  | is.na(GrantType))) %>%
      semi_join(Referrals,
                by = c("PersonalID", "ProjectName" = "ProviderCreating")) %>%
      mutate(Issue = "Referral on a Non Head of Household",
             Type = "Warning") %>%
      select(all_of(vars_we_want))
    
    referrals_on_hh_members_ssvf <- served_in_date_range %>%
      select(all_of(vars_prep),
             RelationshipToHoH,
             EnrollmentID,
             GrantType) %>%
      filter(RelationshipToHoH != 1 &
               GrantType == "SSVF") %>%
      semi_join(Referrals, by = c("PersonalID")) %>%
      mutate(Issue = "Referral on a Non Head of Household (SSVF)",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    # Stray Services (fall outside EE) ----------------------------------------
    # Because a lot of these records are stray Services due to there being no
    # Entry Exit at all, this can't be shown in the same data set as all the other
    # errors. I'm going to have to make this its own thing. :(
    stray_services <- stray_services %>%
      mutate(Issue = "Service Not Attached to an Entry Exit",
             Type = "Warning") %>%
      select(PersonalID, ServiceProvider, ServiceStartDate, Issue, Type)
    
    # AP No Recent Referrals --------------------------------------------------
    co_APs <- Project %>%
      filter(ProjectType == 14) %>%
      select(
        ProjectID,
        OperatingStartDate,
        OperatingEndDate,
        ProjectName,
        ProjectAKA,
        HMISParticipatingProject,
        ProjectCounty
      )
    
    aps_no_referrals <- Referrals %>%
      right_join(co_APs, by = c("ProviderCreating" = "ProjectName")) %>%
      filter(is.na(PersonalID)) %>%
      select(ProviderCreating) %>%
      unique()
    
    aps_with_referrals <- Referrals %>%
      right_join(co_APs, by = c("ProviderCreating" = "ProjectName")) %>%
      filter(!is.na(PersonalID)) %>%
      select(ProviderCreating) %>%
      unique()
    
    data_APs <- data.frame(
      category = c("No Referrals", "Has Created Referrals"),
      count = c(nrow(aps_no_referrals), nrow(aps_with_referrals)),
      providertype = rep("Access Points"),
      total = rep(c(
        nrow(aps_no_referrals) + nrow(aps_with_referrals)
      ))
    )
    
    data_APs <- data_APs %>%
      mutate(percent = count / total,
             prettypercent = percent(count / total))
    
    dq_plot_aps_referrals <-
      ggplot(data_APs, aes(fill = category, x = providertype, y = percent)) +
      geom_bar(position = "fill",
               stat = "identity",
               width = .1) +
      geom_label(
        aes(label = paste(
          data_APs$category,
          "\n",
          data_APs$prettypercent
        )),
        position = position_stack(),
        vjust = 2,
        fill = "white",
        colour = "black",
        fontface = "bold"
      ) +
      scale_fill_manual(values = c("#00952e", "#a11207"), guide = FALSE) +
      theme_void()
    
    
    
    rm(aps_with_referrals, co_APs)
    
    # AP entering project stays -----------------------------------------------
    
    aps_with_ees <- served_in_date_range %>%
      filter(ProjectType == 14) %>%
      mutate(Issue = "Access Point with Entry Exits",
             Type = "High Priority") %>%
      select(all_of(vars_we_want))
    
    # Side Door ---------------------------------------------------------------
    # use Referrals, get logic from ART report- it's pretty lax I think
    
    
    # Old Outstanding Referrals -----------------------------------------------
    # CW says ProviderCreating should work instead of Referred-From Provider
    # Using ProviderCreating instead. Either way, I feel this should go in the
    # Provider Dashboard, not the Data Quality report.
    
    internal_old_outstanding_referrals <- served_in_date_range %>%
      semi_join(Referrals,
                by = c("PersonalID")) %>%
      left_join(Referrals,
                by = c("PersonalID")) %>%
      filter(ProviderCreating == ProjectName,
             ProjectID != 1695) %>%
      select(all_of(vars_prep),
             ProviderCreating,
             ReferralDate,
             ReferralOutcome,
             EnrollmentID) %>%
      filter(is.na(ReferralOutcome) &
               ReferralDate < today() - days(14)) %>%
      mutate(ProjectName = ProviderCreating,
             Issue = "Old Outstanding Referral",
             Type = "Warning") %>%
      select(all_of(vars_we_want))
    
    # ^^this is pulling in neither the Unsheltered NOR referrals from APs
    
    staging_outstanding_referrals <-
      internal_old_outstanding_referrals %>%
      left_join(Project[c("ProjectName", "ProjectID")], by = "ProjectName") %>%
      select(ProjectName, ProjectID, PersonalID) %>%
      group_by(ProjectName, ProjectID) %>%
      summarise(Open_Referrals = n()) %>%
      arrange(desc(Open_Referrals)) %>%
      mutate(Project = paste0(ProjectName, ":", ProjectID))
    
    dq_plot_outstanding_referrals <-
      ggplot(
        head(staging_outstanding_referrals, 20L),
        aes(
          x = reorder(Project, Open_Referrals),
          y = Open_Referrals,
          fill = Open_Referrals
        )
      ) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "",
           y = "Referrals") +
      scale_fill_viridis_c(direction = -1) +
      theme_minimal(base_size = 18)
    
    
    # Unsheltered Incorrect Residence Prior -----------------------------------
    unsheltered_enrollments <- served_in_date_range %>%
      filter(ProjectID == 1695) %>%
      select(
        all_of(vars_prep),
        RelationshipToHoH,
        LivingSituation,
        AgeAtEntry,
        EEType,
        Destination,
        CountyServed,
        ProjectCounty,
        LivingSituation
      )
    
    unsheltered_not_unsheltered <- unsheltered_enrollments %>%
      filter(LivingSituation != 16) %>%
      mutate(Type = "High Priority",
             Issue = "Wrong Provider (Not Unsheltered)") %>%
      select(all_of(vars_we_want))
    
    # Unsheltered Incorrect Provider in CM Record -----------------------------
    
    unsh_incorrect_cmprovider <- CaseManagers %>%
      left_join(unsheltered_enrollments %>%
                  filter(EntryDate > mdy("01012019")),
                by = c("PersonalID")) %>%
      filter(
        EEProvider == "Unsheltered Clients - OUTREACH" &
          CMProvider == "Unsheltered Clients - OUTREACH"
      ) %>%
      mutate(Type = "Error",
             Issue = "Incorrect Provider in Case Manager record") %>%
      select(all_of(vars_we_want))
    
    # Missing End Date on Outreach Contact ------------------------------------
    
    # Unsheltered Missing Outreach Contact Note -------------------------------
    
    # Unsheltered Missing Outreach Contact Record -----------------------------
    
    # Unsheltered Outreach Contact Incorrect Start Date -----------------------
    
    # Unsheltered Currently Unsheltered 30+ Days w No Referral ----------------
    
    long_unsheltered <- unsheltered_enrollments %>%
      filter(is.na(ExitDate) &
               ymd(EntryDate) < today() - days(30))
    
    unsheltered_referred <- Referrals %>%
      filter(ProviderCreating == "Unsheltered Clients - OUTREACH")
    
    unsheltered_long_not_referred <-
      anti_join(long_unsheltered, unsheltered_referred, by = "PersonalID") %>%
      mutate(Type = "Warning",
             Issue = "Unsheltered 30+ Days with no Referral") %>%
      select(all_of(vars_we_want))
    
    rm(long_unsheltered, unsheltered_referred, Referrals)
    # Unsheltered No Case Manager ---------------------------------------------
    
    unsh_missing_cm <- unsheltered_enrollments %>%
      filter(EntryDate > mdy("01012019")) %>%
      anti_join(
        CaseManagers %>%
          filter(EEProvider == "Unsheltered Clients - OUTREACH"),
        by = c("PersonalID")
      ) %>%
      mutate(Type = "Error",
             Issue = "Missing Case Manager record") %>%
      select(all_of(vars_we_want))
    
    # SSVF --------------------------------------------------------------------
    
    ssvf_served_in_date_range <- Enrollment %>%
      select(
        EnrollmentID,
        HouseholdID,
        PersonalID,
        ProjectName,
        ProjectType,
        EntryDate,
        MoveInDateAdjust,
        ExitDate,
        UserCreating,
        RelationshipToHoH,
        PercentAMI,
        LastPermanentStreet,
        LastPermanentCity,
        LastPermanentState,
        LastPermanentZIP,
        AddressDataQuality,
        VAMCStation,
        HPScreeningScore,
        ThresholdScore,
        IraqAfghanistan,
        FemVet
      ) %>%
      right_join(
        served_in_date_range %>%
          filter(GrantType == "SSVF") %>%
          select(PersonalID, EnrollmentID, HouseholdID, ProjectRegion),
        by = c("PersonalID", "EnrollmentID", "HouseholdID")
      ) %>%
      left_join(
        Client %>%
          select(
            PersonalID,
            VeteranStatus,
            YearEnteredService,
            YearSeparated,
            WorldWarII,
            KoreanWar,
            VietnamWar,
            DesertStorm,
            AfghanistanOEF,
            IraqOIF,
            IraqOND,
            OtherTheater,
            MilitaryBranch,
            DischargeStatus
          ),
        by = "PersonalID"
      )
    
    missing_client_veteran_info <- ssvf_served_in_date_range %>%
      filter(VeteranStatus == 1) %>%
      mutate(
        Issue = case_when(
          is.na(YearEnteredService) ~ "Missing Year Entered Service",
          YearEnteredService > year(today()) ~ "Incorrect Year Entered Service",
          is.na(YearSeparated) ~ "Missing Year Separated",
          YearSeparated > year(today()) ~ "Incorrect Year Separated",
          is.na(WorldWarII) | WorldWarII == 99 |
            is.na(KoreanWar) | KoreanWar == 99 |
            is.na(VietnamWar) | VietnamWar == 99 |
            is.na(DesertStorm) | DesertStorm == 99 |
            is.na(AfghanistanOEF) | AfghanistanOEF == 99 |
            is.na(IraqOIF) | IraqOIF == 99 |
            is.na(IraqOND) | IraqOND == 99 |
            is.na(OtherTheater) |
            OtherTheater == 99  ~ "Missing War(s)",
          is.na(MilitaryBranch) ~ "Missing Military Branch",
          is.na(DischargeStatus) ~ "Missing Discharge Status"
        ),
        Type = "Error"
      ) %>%
      filter(!is.na(Issue)) %>%
      select(all_of(vars_we_want))
    
    dkr_client_veteran_info <- ssvf_served_in_date_range %>%
      filter(VeteranStatus == 1) %>%
      mutate(
        Issue = case_when(
          WorldWarII %in% c(8, 9) |
            KoreanWar %in% c(8, 9) |
            VietnamWar %in% c(8, 9) |
            DesertStorm  %in% c(8, 9) |
            AfghanistanOEF %in% c(8, 9) |
            IraqOIF %in% c(8, 9) |
            IraqOND %in% c(8, 9) |
            OtherTheater  %in% c(8, 9)  ~ "Don't Know/Refused War(s)",
          MilitaryBranch %in% c(8, 9) ~ "Missing Military Branch",
          DischargeStatus %in% c(8, 9) ~ "Missing Discharge Status"
        ),
        Type = "Warning"
      ) %>%
      filter(!is.na(Issue)) %>%
      select(all_of(vars_we_want))
    
    ssvf_at_entry <- ssvf_served_in_date_range %>%
      filter(RelationshipToHoH == 1) %>%
      mutate(
        Issue = case_when(
          is.na(PercentAMI) ~ "Missing Percent AMI",
          is.na(VAMCStation) ~ "Missing VAMC Station Number",
          is.na(LastPermanentStreet) |
            is.na(LastPermanentCity) |
            # is.na(LastPermanentState) | # still not fixed in export
            is.na(LastPermanentZIP) ~ "Missing Some or All of Last Permanent Address"
        ),
        
        Type = "Error"
      ) %>%
      filter(!is.na(Issue)) %>%
      select(all_of(vars_we_want))
    
    ssvf_hp_screen <- ssvf_served_in_date_range %>%
      filter(ProjectType == 12 &
               RelationshipToHoH == 1 &
               (is.na(HPScreeningScore) |
                  is.na(ThresholdScore))) %>%
      mutate(Issue = "Missing HP Screening or Threshold Score",
             Type = "Error") %>%
      select(all_of(vars_we_want))
    
    
    # All together now --------------------------------------------------------
    
    dq_main <- rbind(
      aps_with_ees,
      check_disability_ssi,
      check_eligibility,
      conflicting_disabilities,
      conflicting_health_insurance_entry,
      conflicting_health_insurance_exit,
      conflicting_income_entry,
      conflicting_income_exit,
      conflicting_ncbs_entry,
      conflicting_ncbs_exit,
      dkr_client_veteran_info,
      dkr_destination,
      dkr_LoS,
      dkr_months_times_homeless,
      dkr_residence_prior,
      duplicate_ees,
      entered_ph_without_spdat,
      extremely_long_stayers,
      future_ees,
      hh_issues,
      incorrect_ee_type,
      lh_without_spdat,
      missing_approx_date_homeless,
      missing_client_veteran_info,
      missing_county_served,
      missing_county_prior,
      missing_destination,
      missing_disabilities,
      missing_disability_subs,
      missing_health_insurance_entry,
      missing_health_insurance_exit,
      missing_income_entry,
      missing_income_exit,
      missing_LoS,
      missing_long_duration,
      missing_months_times_homeless,
      missing_ncbs_entry,
      conflicting_ncbs_exit,
      missing_previous_street_ESSH,
      missing_residence_prior,
      missing_udes,
      internal_old_outstanding_referrals,
      path_enrolled_missing,
      path_missing_los_res_prior,
      path_no_status_at_exit,
      path_reason_missing,
      path_SOAR_missing_at_exit,
      path_status_determination,
      referrals_on_hh_members,
      referrals_on_hh_members_ssvf,
      services_on_hh_members,
      services_on_hh_members_ssvf,
      spdat_on_non_hoh,
      ssvf_at_entry,
      ssvf_hp_screen
    ) %>%
      filter(!ProjectName %in% c(
        "Diversion from Homeless System",
        "Unsheltered Clients - OUTREACH"
      ))
    
    # UNTIL WELLSKY FIXES THEIR EXPORT: ---------------------------------------
    
    dq_main <- dq_main %>%
      filter(
        !Issue %in% c(
          "Conflicting Disability yes/no",
          "Conflicting Health Insurance yes/no at Entry",
          "Conflicting Health Insurance yes/no at Exit",
          "Conflicting Income yes/no at Entry",
          "Conflicting Income yes/no at Exit",
          "Conflicting Non-cash Benefits yes/no at Entry",
          "Conflicting Non-cash Benefits yes/no at Exit",
          # "Non-cash Benefits Missing at Exit",
          # "Health Insurance Missing at Entry",
          # "Health Insurance Missing at Exit",
          # "Income Missing at Entry",
          # "Income Missing at Exit",
          # "Non-cash Benefits Missing at Entry",
          "Missing Disability Subs",
          "Missing Length of Stay",  # case 873163
          # "Don't Know/Refused Residence Prior",
          # "Don't Know/Refused Months or Times Homeless",
          # "Missing Residence Prior",
          "Missing Months or Times Homeless"
        )
      )
    
    # Unsheltered DQ ----------------------------------------------------------
    
    dq_unsheltered <- rbind(
      check_disability_ssi,
      dkr_destination,
      dkr_months_times_homeless,
      dkr_residence_prior,
      dkr_LoS,
      duplicate_ees,
      future_ees,
      hh_issues,
      incorrect_ee_type,
      lh_without_spdat,
      missing_approx_date_homeless,
      missing_destination,
      missing_county_served,
      missing_LoS,
      missing_months_times_homeless,
      missing_residence_prior,
      missing_udes,
      internal_old_outstanding_referrals,
      referrals_on_hh_members,
      spdat_on_non_hoh,
      unsheltered_not_unsheltered,
      unsh_incorrect_cmprovider,
      unsh_missing_cm,
      unsheltered_long_not_referred
    ) %>%
      filter(ProjectName == "Unsheltered Clients - OUTREACH") %>%
      left_join(Users, by = "UserCreating") %>%
      select(-UserID,-UserName,-ProjectRegion) %>%
      filter(
        UserCounty != "Franklin" &
          !Issue %in% c(
            "Conflicting Disability yes/no",
            "Conflicting Disability yes/no at Exit",
            "Conflicting Health Insurance yes/no at Entry",
            "Conflicting Health Insurance yes/no at Exit",
            "Conflicting Income yes/no at Entry",
            "Conflicting Income yes/no at Exit",
            "Conflicting Non-cash Benefits yes/no at Entry",
            "Conflicting Non-cash Benefits yes/no at Exit",
            "Missing Disability Subs",
            # "Incomplete Living Situation Data",
            "Missing Months or Times Homeless"#,
            # "Don't Know/Refused Residence Prior",
            # "Don't Know/Refused Months or Times Homeless",
            # "Health Insurance Missing at Entry",
            # "Health Insurance Missing at Exit",
            # "Income Missing at Entry",
            # "Income Missing at Exit",
            # "Missing Residence Prior",
            # "Non-cash Benefits Missing at Entry",
            # "Non-cash Benefits Missing at Exit"
          )
      )
    # add in Issue explanations -----------------------------------------------
    
    a <- dq_main %>%
      select(Type, Issue)
    b <- dq_unsheltered %>%
      select(Type, Issue)
    c <- rbind(a, b) %>% unique()
    
    user_help <- c %>%
      arrange(Type, Issue) %>%
      mutate(
        Guidance = case_when(
          Issue == "Extremely Long Stayer" ~
            "This client is showing as an outlier for Length of Stay for this 
          project type in the Balance of State CoC. Please verify that this 
          client is still in your project. If they are, be sure there are no 
          alternative permanent housing solutions for this client. If the client 
          is no longer in your project, please enter their Exit Date as the 
          closest estimation of the day they left your project.",
          Issue == "Access Point with Entry Exits" ~
            "Access Points should only be entering Referrals and Diversion Services
      into the AP provider- not Entry Exits. If a user has done this, the Entry
      Exit should be deleted. Please see the
      <a href=\"http://hmis.cohhio.org/index.php?pg=kb.page&id=151\"
          target=\"_blank\">Coordinated Entry workflow</a>.",
          Issue == "Children Only Household" ~
            "Unless your project serves youth younger than 18 exclusively, every
    household should have at least one adult in it. If you are not sure how
    to correct this, please contact the HMIS team for help.",
          Issue == "Conflicting Disability yes/no" ~
            "If the user answered \"Yes\" to the \"Does the client have a disabling
    condition?\", then there should be a disability subassessment where it
    indicates the disability determination is Yes and the \"If yes,... long
    duration\" question is also Yes. Similarly if the user answered \"No\", the
      client has that type of disability",
          Issue %in% c(
            "Conflicting Health Insurance yes/no at Entry",
            "Conflicting Health Insurance yes/no at Exit"
          ) ~
            "If the user answered \"Yes\" to \"Covered by Health Insurance?\", then
    there should be a Health Insurance subassessment where it indicates which
    type of health insurance the client is receiving. Similarly if the user
    answered \"No\", there should not be any Health Insurance records that say
      the client is receiving that type of Health Insurance.",
          Issue %in% c(
            "Conflicting Income yes/no at Entry",
            "Conflicting Income yes/no at Exit"
          ) ~
            "If the user answered \"Yes\" to \"Income from any source\", then
    there should be an income subassessment where it indicates which
    type of income the client is receiving. Similarly if the user answered
    \"No\", there should not be any income records that say the client is
      receiving that type of income.",
          Issue %in% c(
            "Conflicting Non-cash Benefits yes/no at Entry",
            "Conflicting Non-cash Benefits yes/no at Exit"
          ) ~
            "If the user answered \"Yes\" to \"Non-cash benefits from any source\",
    then there should be a Non-cash benefits subassessment where it indicates
    which type of income the client is receiving. Similarly if the user answered
    \"No\", then there should not be any non-cash records that say the client is
      receiving that type of benefit",
          Issue == "Disabilities: missing Long Duration in subassessment" ~
            "Any Disability subassessment the user answers \"Yes\" to should also
      have the \"If yes, is the disability of long duration...\" answered as \"Yes\".",
          Issue == "Incorrect Date of Birth or Entry Date" ~
            "The HMIS data is indicating the client entered the project PRIOR to
      being born. Correct either the Date of Birth or the Entry Date, whichever
      is incorrect.",
          Issue == "Incorrect Entry Exit Type" ~
            "The user selected the wrong Entry Exit Type. To correct, click the
      Entry pencil and Save & Continue. The Entry Exit Type at the top can then
      be changed. Click \"Update\" to make this change take effect.",
          Issue == "Invalid SSN" ~
            "The Social Security Number does not conform with standards set by the
      Social Security Administration. This includes rules like every SSN is
      exactly 9 digits and cannot have certain number patterns. Correct by
      navigating to the client's record, then clicking the Client Profile tab,
      then click into the Client Record pencil to correct the data.",
          Issue %in% c(
            "Missing Connection with SOAR at Exit",
            "PATH Status at Exit Missing or Incomplete",
            "Health Insurance Missing at Exit",
            "Income Missing at Exit"
          ) ~
            "Please enter the data for this item by clicking into the Exit pencil on
      the given Client ID on the appropriate program stay.",
          Issue == "Missing PATH Enrollment at Exit" ~
            "Please enter the data for this item by clicking into the Entry or Exit
      pencil and creating an Interim. In the assessment, enter the correct PATH
      Enrollment Date and Save.",
          Issue %in% c(
            "Missing Date of Birth Data Quality",
            "Missing Length of Stay",
            "Missing County of Prior Residence",
            "Missing Disabling Condition",
            "Missing Ethnicity",
            "Missing Gender",
            "Missing Months or Times Homeless",
            "Missing Previously From Street, ES, or SH (Length of Time Homeless questions)",
            "Missing Race",
            "Missing Residence Prior",
            "Non-cash Benefits Missing at Entry",
            "Missing Some or All of Last Permanent Address",
            "Missing Year Separated",
            "Missing VAMC Station Number",
            "Missing Residence Prior Length of Stay (PATH)",
            "Missing Percent AMI",
            "Missing War(s)",
            "Missing Year Entered Service",
            "Missing HP Screening or Threshold Score",
            "Health Insurance Missing at Entry",
            "Income Missing at Entry",
            "Non-cash Benefits Missing at Entry"
          ) ~
            "This data element is required to be collected at project Entry. Please
      click into the client's Entry pencil to save this data to HMIS.",
          Issue == "Missing Reason Not PATH Enrolled" ~
            "The user has indicated the household was not enrolled into PATH, but
      no reason was selected.",
          Issue == "Missing County Served" ~
            "County Served must be collected at Entry for all clients. County is
      very important so that the client is prioritized into the correct service
      areas for various housing solutions. This can be corrected through the
      Entry pencil.",
          Issue %in% c(
            "Missing Name Data Quality",
            "Missing SSN",
            "Missing Veteran Status"
          ) ~
            "Please correct by navigating to the client's record, then clicking the
      Client Profile tab, then click into the Client Record pencil to save the
      missing data.",
          Issue == "No Head of Household" ~
            "Please be sure all members of the household are included in the program
      stay, and that each household member's birthdate is correct. If those
      things are both true, or the client is a single, check inside the Entry
      pencil to be sure each household member has \"Relationship to Head of
      Household\" answered and that one of them says Self (head of household).
      Singles are always Self (head of household).",
          Issue == "Too Many Heads of Household" ~
            "Check inside the Entry pencil to be sure each household member has
      \"Relationship to Head of Household\" answered and that only one of
      them says Self (head of household).",
          Issue %in% c(
            "Referral on a Non Head of Household",
            "Referral on a Non Head of Household (SSVF)"
          ) ~
            "Users should not checkbox all the household members when creating a
      Referral. Only the Head of Household needs the Referral. It is recommended
      that users delete any referrals on Non Heads of Household related to this
      project stay so that the receiving agency does not have to deal with them
      and they stop showing in reporting."
          ,
          Issue %in% c(
            "Service Transaction on a Non Head of Household (SSVF)",
            "Service Transaction on a Non Head of Household"
          ) ~
            "Users should not checkbox all the household members when creating a
      Service Transaction. Only the Head of Household needs a Service
      Transaction. Delete any extraneous Service Transactions related to this
      project stay.",
          Issue == "Duplicate Entry Exits" ~
            "Users sometimes create this error when they forget to click into a
      program stay by using the Entry pencil, and instead they click \"Add
      Entry/Exit\" each time. To correct, EDA to the project the Entry/Exit
      belongs to, navigate to the Entry/Exit tab and delete the program stay
      that was accidentally added for each household member.",
          Issue == "Check Eligibility" ~
            "Your Residence Prior data suggests that this project is either serving
      ineligible households, the household was entered into the wrong project,
      or the Residence Prior data at Entry is incorrect. Please check the terms
      of your grant or speak with the CoC team at COHHIO if you are unsure of
      eligibility criteria for your project type.",
          Issue == "Check Veteran Status for Accuracy" ~
            "You have indicated the household exited to a destination that only
      veterans are eligible for, but the head of household appears to be not a
      veteran. Either the Veteran Status is incorrect or the Destination is
      incorrect.",
          Issue == "Client with No Disability Receiving SSI/SSDI (could be ok)" ~
            "If a client is receiving SSI or SSDI for THEIR OWN disability, that
      disability should be indicated in the Disabilities data elements. If
      an adult is receiving SSI or SSDI benefits on behalf a minor child,
      then there is no action needed.",
          Issue %in% c(
            "Don't Know/Refused Ethnicity",
            "Don't Know/Refused or Approx. Date of Birth",
            "Don't Know/Refused Race",
            "Don't Know/Refused Veteran Status",
            "Don't Know/Refused Destination",
            "Don't Know/Refused Months or Times Homeless",
            "Don't Know/Refused Residence Prior",
            "Don't Know/Refused SSN",
            "Don't Know/Refused War(s)",
            "Incomplete or Don't Know/Refused Name"
          ) ~
            "It is widely understood that not every client will be able to or consent
      to answer every question in every assessment. If you do have any of this
      data, but it is just not entered into HMIS yet, please enter it. If you
      can reasonably attempt again to collect this data from the client (like
      if they are still in your project), then please do so. Otherwise, there is
      no action needed.",
          Issue == "Old Outstanding Referral" ~
            "Referrals should be closed in about 2 weeks. Please be sure you are
      following up with any referrals and helping the client to find permanent
      housing. Once a Referral is made, the receiving agency should be saving
      the \"Referral Outcome\" once it is known. If you have Referrals that are
      legitimately still open after 2 weeks because there is a lot of follow
      up going on, no action is needed since the HMIS data is accurate.",
          Issue == "Missing Destination" ~
            "It is widely understood that not every client will complete an exit
      interview, especially for high-volume emergency shelters. A few warnings
      for Missing Destination is no cause for concern, but if there is a large
      number, please contact the CoC team to work out a way to improve client
      engagement.",
          Issue == "SPDAT Created on a Non-Head-of-Household" ~
            "It is very important to be sure that the VI-SPDAT score goes on the
        Head of Household of a given program stay because otherwise that score
      may not pull into any reporting. It is possible a Non Head of Household
      was a Head of Household in a past program stay, and in that situation,
      this should not be corrected unless the Head of Household of your program
      stay is missing their score. To correct this, you would need to completely
      re-enter the score on the correct client's record.",
          Issue == "Non-Veteran Non-DV HoHs Entering PH without SPDAT" ~
            "Every household (besides those fleeing domestic violence and veteran
      households) must have a VI-SPDAT score to aid with prioritization into
      the Permanent Housing (RRH or PSH) project.",
          Issue == "HoHs in shelter or Transitional Housing for 8+ days without SPDAT" ~
            "Any household who has been in shelter, Transitional Housing, or a
      Safe Haven for over 8 days should be assessed with the VI-SPDAT so that
      they can be prioritized for Permanent Housing (RRH or PSH).",
          Issue == "Missing Disability Subs" ~
            "The HMIS data indicates the client has a disability, but there is no
      corresponding Disability to match it.",
          Issue == "Future Entry Date" ~
            "Users should not be entering a client into a project on a date in the
      future. There is no action needed, but going forward, please be sure that
      your data entry workflow is correct according to your project type.",
          Issue == "Wrong Provider (Not Unsheltered)" ~
            "Clients who were incorrectly entered into the Unsheltered
          provider should be exited. Otherwise, correct the data. Please review
          the <a href=\"https://www.youtube.com/watch?v=qdmrqOHXoN0&t=174s\"
          target=\"_blank\">data entry portion of the Unsheltered video training</a>
          for more info.",
          Issue == "Incorrect Provider in Case Manager record" ~
            "When you save a Case Manager record, select the Access Point provider
          that diverted the household in the provider picklist. See Step 4
          in the <a href=\"http://hmis.cohhio.org/admin.php?pg=kb.page&page=168\"
          target=\"_blank\">Diversion workflow</a> for more info.",
          Issue == "Missing Case Manager record" ~
            "Each client entered into the Diversion Provider should have a Case
          Manager record. See Step 4 in the
          <a href=\"http://hmis.cohhio.org/admin.php?pg=kb.page&page=168\"
          target=\"_blank\">Diversion workflow</a> for more info.",
          Issue == "Unsheltered 30+ Days with no Referral" ~
            "Ideally households are being referred for housing from the Unsheltered
          provider within a short period of time. These particular households are
          currently unsheltered and without a referral. If the household has been
          referred but that has not been captured in HMIS, please enter the referral.
          To learn more about when to exit a household from the Unsheltered
          Provider, click
          <a href=\"https://youtu.be/qdmrqOHXoN0?t=721\" target=\"_blank\">here</a>."
        )
      )
    
    dq_main <- dq_main %>%
      left_join(user_help, by = c("Type", "Issue")) %>%
      mutate(Type = factor(Type, levels = c("High Priority",
                                            "Error",
                                            "Warning")))
    
    # Unsheltered DQ ----------------------------------------------------------
    
    dq_unsheltered <- dq_unsheltered %>%
      left_join(user_help, by = c("Type", "Issue")) %>%
      mutate(
        Type = if_else(Issue == "Missing County Served", "High Priority", Type),
        Type = factor(Type, levels = c("High Priority",
                                       "Error",
                                       "Warning"))
      )
    
    rm(Users)
    
    # Controls what is shown in the CoC-wide DQ tab ---------------------------
    
    ReportStart <- "10012018"
    ReportEnd <- format.Date(today(), "%m-%d-%Y")
    
    dq_past_year <- dq_main %>%
      filter(served_between(., ReportStart, ReportEnd)) %>%
      left_join(Project[c("ProjectID", "ProjectName")], by = "ProjectName")
    
    ReportStart <- "01012019"
    ReportEnd <- "12312019"
    
    dq_2019 <- dq_main %>%
      filter(served_between(., ReportStart, ReportEnd)) %>%
      left_join(Project[c("ProjectID", "ProjectName")], by = "ProjectName")
    
    rm(ReportStart, ReportEnd)
    
    projects_current_hmis <- projects_current_hmis %>%
      filter(ProjectID != 1695)
    
    dq_providers <- sort(projects_current_hmis$ProjectName)
    
    # Clean up the house ------------------------------------------------------
    
    rm(
      aps_with_ees,
      CaseManagers,
      check_disability_ssi,
      check_eligibility,
      Client,
      conflicting_disabilities,
      conflicting_health_insurance_entry,
      conflicting_health_insurance_exit,
      conflicting_income_entry,
      conflicting_income_exit,
      conflicting_ncbs_entry,
      conflicting_ncbs_exit,
      dkr_months_times_homeless,
      dkr_residence_prior,
      dkr_destination,
      dkr_LoS,
      dkr_client_veteran_info,
      duplicate_ees,
      Enrollment,
      entered_ph_without_spdat,
      extremely_long_stayers,
      future_ees,
      hh_issues,
      projects_current_hmis,
      incorrect_ee_type,
      # incorrectMoveInDate,
      lh_without_spdat,
      missing_approx_date_homeless,
      missing_client_veteran_info,
      missing_county_prior,
      missing_county_served,
      missing_destination,
      missing_disabilities,
      detail_missing_disabilities,
      missing_disability_subs,
      missing_health_insurance_entry,
      missing_health_insurance_exit,
      missing_income_entry,
      missing_income_exit,
      detail_missing_living_situation,
      missing_long_duration,
      missing_LoS,
      missing_months_times_homeless,
      missing_ncbs_entry,
      missing_ncbs_exit,
      missing_residence_prior,
      missing_udes,
      internal_old_outstanding_referrals,
      path_enrolled_missing,
      path_missing_los_res_prior,
      path_no_status_at_exit,
      path_reason_missing,
      path_SOAR_missing_at_exit,
      path_status_determination,
      Project,
      referrals_on_hh_members,
      referrals_on_hh_members_ssvf,
      served_in_date_range,
      services_on_hh_members,
      services_on_hh_members_ssvf,
      smallProject,
      spdat_on_non_hoh,
      ssvf_at_entry,
      ssvf_hp_screen,
      ssvf_served_in_date_range,
      unsheltered_enrollments,
      unsheltered_not_unsheltered,
      unsh_missing_cm,
      unsh_incorrect_cmprovider,
      unsheltered_long_not_referred,
      update_date,
      user_help,
      vars_we_want
    )
    
    dq_data_errors_plot <- dq_past_year %>%
      filter(
        Type %in% c("Error", "High Priority") &
          !Issue %in% c(
            "No Head of Household",
            "Too Many Heads of Household",
            "Children Only Household"
          )
      ) %>%
      select(PersonalID, ProjectID, ProjectName) %>%
      unique() %>%
      group_by(ProjectName, ProjectID) %>%
      summarise(clientsWithErrors = n()) %>%
      ungroup() %>%
      arrange(desc(clientsWithErrors))
    
    dq_data_errors_plot$hover <-
      with(dq_data_errors_plot,
           paste0(ProjectName, ":", ProjectID))
    
    dq_plot_projects_errors <-
      ggplot(
        head(dq_data_errors_plot, 20L),
        aes(
          x = reorder(hover, clientsWithErrors),
          y = clientsWithErrors,
          fill = clientsWithErrors
        )
      ) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "",
           y = "Clients") +
      scale_fill_viridis_c(direction = -1) +
      theme_minimal(base_size = 18)
    
    dq_data_warnings_plot <- dq_past_year %>%
      filter(Type == "Warning") %>%
      group_by(ProjectName, ProjectID) %>%
      summarise(Warnings = n()) %>%
      ungroup() %>%
      arrange(desc(Warnings))
    
    dq_data_warnings_plot$hover <-
      with(dq_data_warnings_plot,
           paste0(ProjectName, ":", ProjectID))
    
    dq_plot_projects_warnings <-
      ggplot(head(dq_data_warnings_plot, 20L),
             aes(
               x = reorder(hover, Warnings),
               y = Warnings,
               fill = Warnings
             )) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "",
           y = "Clients") +
      scale_fill_viridis_c(direction = -1) +
      theme_minimal(base_size = 18)
    
    dq_data_error_types <- dq_past_year %>%
      filter(Type %in% c("Error", "High Priority")) %>%
      group_by(Issue) %>%
      summarise(Errors = n()) %>%
      ungroup() %>%
      arrange(desc(Errors))
    
    dq_plot_errors <-
      ggplot(head(dq_data_error_types, 10L),
             aes(
               x = reorder(Issue, Errors),
               y = Errors,
               fill = Errors
             )) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "",
           y = "Clients") +
      scale_fill_viridis_c(direction = -1) +
      theme_minimal(base_size = 18)
    
    dq_data_warning_types <- dq_past_year %>%
      filter(Type == "Warning") %>%
      group_by(Issue) %>%
      summarise(Warnings = n()) %>%
      ungroup() %>%
      arrange(desc(Warnings))
    
    dq_plot_warnings <-
      ggplot(head(dq_data_warning_types, 10L),
             aes(
               x = reorder(Issue, Warnings),
               y = Warnings,
               fill = Warnings
             )) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "",
           y = "Clients") +
      scale_fill_viridis_c(direction = -1) +
      theme_minimal(base_size = 18)
    
    dq_data_hh_issues_plot <- dq_past_year %>%
      filter(
        Type %in% c("Error", "High Priority") &
          Issue %in% c(
            "No Head of Household",
            "Too Many Heads of Household",
            "Children Only Household"
          )
      ) %>%
      select(PersonalID, ProjectID, ProjectName) %>%
      unique() %>%
      group_by(ProjectName, ProjectID) %>%
      summarise(Households = n()) %>%
      ungroup() %>%
      arrange(desc(Households))
    
    dq_data_hh_issues_plot$hover <-
      with(dq_data_hh_issues_plot,
           paste0(ProjectName, ":", ProjectID))
    
    dq_plot_hh_errors <-
      ggplot(head(dq_data_hh_issues_plot, 20L),
             aes(
               x = reorder(hover, Households),
               y = Households,
               fill = Households
             )) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "") +
      scale_fill_viridis_c(direction = -1) +
      theme_minimal(base_size = 18)
    
    dq_data_outstanding_referrals_plot <- dq_past_year %>%
      filter(Issue == "Old Outstanding Referral") %>%
      select(PersonalID, ProjectID, ProjectName) %>%
      unique() %>%
      group_by(ProjectName, ProjectID) %>%
      summarise(Households = n()) %>%
      ungroup() %>%
      arrange(desc(Households)) %>%
      mutate(hover = paste(ProjectName, ":", ProjectID))
    
    dq_plot_projects_outstanding_referrals <-
      ggplot(
        head(dq_data_outstanding_referrals_plot, 20L),
        aes(
          x = reorder(hover, Households),
          y = Households,
          fill = Households
        )
      ) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "") +
      scale_fill_viridis_c(direction = -1) +
      theme_minimal(base_size = 18)
    
    dq_data_eligibility_plot <- dq_past_year %>%
      filter(Type == "Warning" &
               Issue %in% c("Check Eligibility")) %>%
      select(PersonalID, ProjectID, ProjectName) %>%
      unique() %>%
      group_by(ProjectName, ProjectID) %>%
      summarise(Households = n()) %>%
      ungroup() %>%
      arrange(desc(Households))
    
    dq_data_eligibility_plot$hover <-
      with(dq_data_eligibility_plot,
           paste0(ProjectName, ":", ProjectID))
    
    dq_plot_eligibility <-
      ggplot(
        head(dq_data_eligibility_plot, 20L),
        aes(
          x = reorder(hover, Households),
          y = Households,
          fill = Households
        )
      ) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "") +
      scale_fill_viridis_c(direction = -1) +
      theme_minimal(base_size = 18)
    
    dq_data_without_spdat_plot <- dq_past_year %>%
      filter(
        Type == "Warning" &
          Issue %in% c(
            "HoHs Entering PH without SPDAT",
            "HoHs in shelter or Transitional Housing for 8+ days without SPDAT"
          )
      ) %>%
      select(PersonalID, ProjectID, ProjectName) %>%
      unique() %>%
      group_by(ProjectName, ProjectID) %>%
      summarise(Households = n()) %>%
      ungroup() %>%
      mutate(ProjectDisplay = paste0(ProjectName, ":", ProjectID)) %>%
      arrange(desc(Households))
    
    dq_plot_hh_no_spdat <-
      ggplot(
        head(dq_data_without_spdat_plot, 20L),
        aes(
          x = reorder(ProjectDisplay, Households),
          y = Households,
          fill = Households
        )
      ) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(x = "") +
      scale_fill_viridis_c(direction = -1) +
      theme_minimal(base_size = 18)
    
    rm(list = ls(pattern = "dq_data_"))
    rm(
      stray_services,
      staging_outstanding_referrals,
      HealthAndDV,
      missing_previous_street_ESSH,
      a,
      b,
      c
    )
    
    save.image("images/Data_Quality.RData")

