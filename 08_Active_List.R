# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
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
library(lubridate)
library(janitor)

load("images/cohorts.RData")
load("images/COHHIOHMIS.RData")

# clients currently entered into a homeless project in our system

co_currently_homeless <- co_clients_served %>%
  filter(is.na(ExitDate) &
           (ProjectType %in% c(4, lh_project_types) |
              (
                ProjectType %in% c(ph_project_types) &
                  is.na(MoveInDateAdjust)
              ))) %>%
  select(
    PersonalID,
    ProjectName,
    ProjectType,
    HouseholdID,
    EnrollmentID,
    RelationshipToHoH,
    VeteranStatus,
    EntryDate,
    AgeAtEntry
  )

active_list <- co_currently_homeless

# Account for Multiple EEs ------------------------------------------------

# bucket the ptc's
ptc_status <- active_list %>%
  mutate(PTCStatus = case_when(
    ProjectType %in% c(lh_project_types, 4) ~ "LH",
    ProjectType %in% c(ph_project_types) ~ "PH"
  )) 

# split out the clients with ph entries
clients_in_ph <- ptc_status %>%
  filter(PTCStatus == "PH") %>%
  select(PersonalID) %>%
  distinct() %>%
  mutate(InPH = 1)

# split out the clients with lh entries
clients_in_lh <- ptc_status %>%
  filter(PTCStatus == "LH") %>%
  select(PersonalID) %>%
  distinct() %>%
  mutate(LH = 1)

# join them back, with one row per client, create PTCStatus variable
client_ptc_status <- clients_in_lh %>%
  full_join(clients_in_ph, by = "PersonalID") %>%
  mutate(
    InPH = if_else(is.na(InPH), 0, InPH),
    LH = if_else(is.na(LH), 0, LH),
    PTCStatus = if_else(
      InPH == 1,
      "Has Entry into RRH or PSH",
      "Currently Has No Entry into RRH or PSH"
    )
  ) %>%
  select(PersonalID, PTCStatus)

# add PTCStatus variable into the main dataframe (but now you have multiple
# rows per client again)
active_list <- active_list %>%
  left_join(client_ptc_status, by = "PersonalID")

# take only the clients with multiple rows and slice out only the current lh
# ee's or if there are multiple lh ee's, take the most recent (thinking the
# most recent is probably the most up to date?)
split_up_dupes <- get_dupes(active_list, PersonalID) %>%
  mutate(PTCGames = if_else(ProjectType %in% c(lh_project_types, 4), 1, 2)) %>%
  group_by(PersonalID) %>%
  arrange(PTCGames, desc(EntryDate)) %>%
  slice(1L) %>%
  select(-dupe_count, -PTCGames) %>%
  ungroup()

# remove the clients in split_up_dupes from the main dataframe so you can
# add them back in now that they've been deduplicated
duplicated_clients <- get_dupes(active_list, PersonalID) %>%
  pull(PersonalID) %>% unique()

filter_out_dupes <- active_list %>%
  filter(!PersonalID %in% duplicated_clients)

# add the deduplicated clients back in with the ones with only one ee
active_list <- rbind(split_up_dupes, filter_out_dupes)

# correcting for bad hh data (while also flagging it) ---------------------

# what household ids exist in the data?
ALL_HHIDs <- active_list %>% select(HouseholdID) %>% unique()

# marking who is a hoh (accounts for singles not marked as hohs in the data)
active_list <- active_list %>%
  mutate(
    RelationshipToHoH = if_else(is.na(RelationshipToHoH), 99, RelationshipToHoH),
    hoh = if_else(str_detect(HouseholdID, fixed("s_")) |
           (str_detect(HouseholdID, fixed("h_")) &
              RelationshipToHoH == 1), 1, 0)) 

# what household ids exist if we only count those with a hoh?
HHIDs_in_current_logic <- active_list %>% 
  filter(hoh == 1) %>%
  select(HouseholdID) %>%
  unique()

# which hh ids did not have a hoh?
HHIDs_with_bad_dq <-
  anti_join(ALL_HHIDs, HHIDs_in_current_logic,
            by = "HouseholdID") 

# what household ids have multiple hohs?
mult_hohs <- active_list %>% 
  group_by(HouseholdID) %>% 
  summarise(hohs = sum(hoh)) %>%
  filter(hohs > 1) %>%
  select(HouseholdID)

# give me ALL household ids with some sort of problem
HHIDs_with_bad_dq <- rbind(HHIDs_with_bad_dq, mult_hohs)

# let's see those same household ids but with all the needed columns
HHIDs_with_bad_dq <-
  left_join(HHIDs_with_bad_dq, active_list, by = "HouseholdID")

rm(ALL_HHIDs, HHIDs_in_current_logic, mult_hohs)

# assigning hoh status to the oldest person in the hh
Adjusted_HoHs <- HHIDs_with_bad_dq %>%
  group_by(HouseholdID) %>%
  arrange(desc(AgeAtEntry)) %>% # picking oldest hh member
  slice(1L) %>% 
  mutate(correctedhoh = 1) %>%
  select(HouseholdID, PersonalID, EnrollmentID, correctedhoh) %>%
  ungroup()

# merging the "corrected" hohs back into the main dataset with a flag, then
# correcting the RelationshipToHoH
hohs <- active_list %>%
  left_join(Adjusted_HoHs,
            by = c("HouseholdID", "PersonalID", "EnrollmentID")) %>%
  mutate(RelationshipToHoH = if_else(correctedhoh == 1, 1, RelationshipToHoH)) %>%
  select(PersonalID, HouseholdID, correctedhoh)
  

active_list <- active_list %>%
  left_join(hohs, by = c("HouseholdID", "PersonalID"))

# COVID-19 ----------------------------------------------------------------

get_res_prior <- Enrollment %>%
  select(PersonalID, EntryDate, ExitDate, LivingSituation) %>%
  group_by(PersonalID) %>%
  arrange(desc(EntryDate)) %>%
  slice(1L)

covid_clients <- covid19 %>%
  mutate(
    COVID19AssessmentDate = ymd(COVID19AssessmentDate),
    ContactWithConfirmedDate = ymd(ContactWithConfirmedDate),
    ContactWithUnderInvestigationDate = ymd(ContactWithUnderInvestigationDate),
    TestDate = ymd(TestDate),
    DateUnderInvestigation = ymd(DateUnderInvestigation)
  ) %>%
  filter(ymd(COVID19AssessmentDate) >= ymd("20200401") &
           ymd(COVID19AssessmentDate) <= today()) %>%
  left_join(get_res_prior, by = "PersonalID") %>%
  mutate(LivingSituationDescr = living_situation(LivingSituation)) %>%
  as_tibble() %>%
  mutate(
    COVID19Priority = case_when(
      (
        Tested == 1 &
          TestResults == "Positive" &
          ymd(TestDate) > today() - days(14) &
          !is.na(TestDate)
      ) |
        # if tested positive in the past 14 days ^^
        (
          UnderInvestigation == 1 &
            ymd(DateUnderInvestigation) > today() - days(14)
        ) |
        (
          ContactWithConfirmedCOVID19Patient == 1 &
            (
              ymd(ContactWithConfirmedDate) >
                today() - days(14) |
                is.na(ContactWithConfirmedDate)
              # contact with definite COVID-19 in the past 14 days ^^
            )
        ) |
        (
          ContactWithUnderCOVID19Investigation == 1 &
            (
              ymd(ContactWithUnderInvestigationDate) >
                today() - days(14) |
                is.na(ContactWithUnderInvestigationDate)
            )
          # contact date with maybe COVID-19 was within the past 14 days ^^
        ) |
        (
          LivingSituation %in% c(7, 25) &
            EntryDate > today() - days(14) &
            EntryDate <= today()
        ) |
        # if the client came from jail or nursing home ^^
        (
          Symptom1BreathingDifficult +
            Symptom1Cough +
            Symptom2Chills +
            Symptom2SoreThroat +
            Symptom2Fever +
            Symptom2Headache +
            Symptom2LostTasteSmell +
            Symptom2MusclePain +
            Symptom2Congestion +
            Symptom2Nausea +
            Symptom2Diarrhea +
            Symptom2Weak) > 0 ~ 1, # "Needs Isolation/Quarantine"
      # if the client has any symptoms at all ^^
      (
        HealthRiskHistoryOfRespiratoryIllness +
          HealthRiskChronicIllness +
          HealthRiskOver65 +
          HealthRiskKidneyDisease +
          HealthRiskImmunocompromised +
          HealthRiskSmoke > 0
      )  ~ 2, # "Has Health Risk(s)",
      # if the client has any risks at all ^^
      TRUE ~ 4 # "No Known Risks or Exposure"
      # everyone else lands here ^
      # in the report, there will be another level: "Not Assessed Recently"
    )
  ) %>%
  select(PersonalID, COVID19Priority)

covid_hhs <- active_list %>%
  left_join(covid_clients, by = "PersonalID") %>%
  mutate(
    COVID19Priority = if_else(
      is.na(COVID19Priority),
      3, # "Not Assessed Recently"
      COVID19Priority
    )
  ) %>%
  group_by(HouseholdID) %>%
  mutate(COVID19Priority_hh = max(COVID19Priority)) %>%
  ungroup() %>%
  select(PersonalID, HouseholdID, COVID19Priority_hh) %>%
  mutate(
    COVID19Priority = case_when(
      COVID19Priority_hh == 1 ~ "Needs Isolation/Quarantine",
      COVID19Priority_hh == 2 ~ "Has Health Risk(s)",
      COVID19Priority_hh == 3 ~ "Not Assessed Recently",
      COVID19Priority_hh == 4 ~ "No Known Risks or Exposure"
    ),
    COVID19Priority = factor(
      COVID19Priority,
      levels = c(
        "Needs Isolation/Quarantine",
        "Has Health Risk(s)",
        "Not Assessed Recently",
        "No Known Risks or Exposure"
      )
    )
  ) %>%
  select(-COVID19Priority_hh)

# adding COVID19Priority to active list
active_list <- active_list %>%
  left_join(covid_hhs, by = c("PersonalID", "HouseholdID"))

# time to collapse from clients to hohs!
hh_size <- active_list %>%
  group_by(HouseholdID) %>%
  summarise(HouseholdSize = n()) %>%
  ungroup() 

active_list <- active_list %>%
  right_join(hh_size, by = "HouseholdID")

# Adding in Disability Status of HH, County, PHTrack ----------------------

# getting whatever data's needed from the Enrollment data frame, creating
# columns that tell us something about each household and some that are about
# each client
disability_data <- active_list %>%
  left_join(
    Enrollment %>%
      select(
        PersonalID,
        HouseholdID,
        DisablingCondition,
        CountyServed,
        PHTrack,
        ExpectedPHDate
      ),
    by = c("PersonalID", "HouseholdID")
  ) %>%
  group_by(HouseholdID) %>%
  mutate(
    CountyServed = if_else(is.na(CountyServed), "MISSING County", CountyServed),
    DisabilityInHH = max(if_else(DisablingCondition == 1, 1, 0)),
    TAY = if_else(max(AgeAtEntry) < 25 & max(AgeAtEntry) >= 16, 1, 0),
    PHTrack = if_else(
      !is.na(PHTrack) &
        !is.na(ExpectedPHDate) &
        ymd(ExpectedPHDate) >= today(), PHTrack, NULL)
  ) %>%
  ungroup() %>%
  select(-AgeAtEntry)

# saving these new columns back to the active list
active_list <- disability_data



# County Guessing ---------------------------------------------------------

# replacing non-Unsheltered-Provider missings with County of the provider
county <- active_list %>%
  left_join(Project %>%
              select(ProjectName, ProjectCounty), by = "ProjectName") %>%
  mutate(
    CountyGuessed = if_else(CountyServed == "MISSING County", 1, 0),
    CountyServed = if_else(
      CountyServed == "MISSING County" &
        ProjectName != "Unsheltered Clients - OUTREACH",
      ProjectCounty,
      CountyServed
    ),
    ProjectCounty = NULL
  )

# replacing missings for the Unsheltered Provider with the County of the
# Default Provider of the person who entered the Enrollment (grrr!)
active_list <- county %>%
  left_join(Enrollment %>%
              select(EnrollmentID, UserCreating), by = "EnrollmentID") %>%
  mutate(
    UserID = gsub(pattern = '[^0-9\\.]', '', UserCreating, perl = TRUE),
    UserCreating = str_remove(UserCreating, "\\(.*\\)"),
    UserID = as.numeric(UserID)
  ) %>%
  left_join(Users %>%
              select(UserID, UserCounty), by = "UserID") %>%
  mutate(CountyServed = if_else(CountyServed == "MISSING County" &
                                  ProjectName == "Unsheltered Clients - OUTREACH",
                                UserCounty,
                                CountyServed)) %>%
  select(-starts_with("User"))

# Indicate if the Household Has No Income ---------------------------------

# getting income-related data and data collection stages. this will balloon
# out the number of rows per client, listing each yes/no update, then, using
# DateCreated, it picks out the most recent answer, keeping only that one
income_data <- active_list %>%
  left_join(
    IncomeBenefits %>%
      select(
        PersonalID,
        EnrollmentID,
        IncomeFromAnySource,
        DateCreated,
        DataCollectionStage
      ),
    by = c("PersonalID", "EnrollmentID")
  ) %>%
  mutate(DateCreated = ymd_hms(DateCreated)) %>%
  group_by(PersonalID, EnrollmentID) %>%
  mutate(
    MaxUpdate = max(DateCreated),
    IncomeFromAnySource = if_else(is.na(IncomeFromAnySource),
                                  99,
                                  IncomeFromAnySource)
  ) %>%
  filter(MaxUpdate == DateCreated) %>%
  ungroup() %>%
  select(PersonalID,
         EnrollmentID,
         IncomeFromAnySource)
  
# adding the column into the active list
active_list <- active_list %>%
  left_join(income_data, by = c("PersonalID", "EnrollmentID")) 
  
# Add in Score ------------------------------------------------------------

# taking the most recent score on the client, but this score cannot be over a
# year old.
scores_staging <- Scores %>%
  filter(ScoreDate > today() - years(1)) %>%
  group_by(PersonalID) %>%
  arrange(desc(ymd(ScoreDate))) %>%
  slice(1L) %>%
  ungroup() %>%
  select(-ScoreDate)

active_list <- active_list %>%
  left_join(scores_staging, by = "PersonalID")

# Add Chronicity ----------------------------------------------------------

# creating a small basic dataframe to work with
smallEnrollment <- Enrollment %>%
  select(EnrollmentID, PersonalID, HouseholdID, LivingSituation, 
         DateToStreetESSH, TimesHomelessPastThreeYears, ExitAdjust,
         MonthsHomelessPastThreeYears)

# getting only the independently-chronic clients. they're chronic right now
# and because of *their own* homeless history
singly_chronic <-
  active_list %>%
  left_join(smallEnrollment,
            by = c("PersonalID",
                   "EnrollmentID",
                   "HouseholdID")) %>%
  mutate(SinglyChronic =
           if_else(((ymd(DateToStreetESSH) + days(365) <= ymd(EntryDate) &
                       !is.na(DateToStreetESSH)) |
                      (
                        MonthsHomelessPastThreeYears %in% c(112, 113) &
                          TimesHomelessPastThreeYears == 4 &
                          !is.na(MonthsHomelessPastThreeYears) &
                          !is.na(TimesHomelessPastThreeYears)
                      )
           ) &
             DisablingCondition == 1 &
             !is.na(DisablingCondition), 1, 0))

# pulling all EEs with the Chronic designation, marking all hh members of anyone
# with a Chronic marker as also Chronic
household_chronic <- singly_chronic %>%
  group_by(HouseholdID) %>%
  mutate(
    ChronicHousehold = sum(SinglyChronic, na.rm = TRUE),
    ChronicStatus = case_when(
      ChronicHousehold > 0 ~ "Chronic",
      ChronicHousehold == 0 ~ "Not Chronic"
    )
  ) %>%
  ungroup() %>%
  select(-ChronicHousehold)

# adds current days in ES or SH projects to days homeless prior to entry and if
# it adds up to 365 or more, it marks the client as AgedIn
agedIntoChronicity <- household_chronic %>%
  mutate(
    DaysHomelessInProject = difftime(ymd(ExitAdjust),
                                     ymd(EntryDate),
                                     units = "days"),
    DaysHomelessBeforeEntry = difftime(ymd(EntryDate),
                                       if_else(
                                         is.na(ymd(DateToStreetESSH)),
                                         ymd(EntryDate),
                                         ymd(DateToStreetESSH)
                                       ),
                                       units = "days"),
    ChronicStatus = if_else(
      ProjectType %in% c(1, 8) &
        ChronicStatus == "Not Chronic" &
        ymd(DateToStreetESSH) + days(365) > ymd(EntryDate) &
        !is.na(DateToStreetESSH) &
        DaysHomelessBeforeEntry + DaysHomelessInProject >= 365,
      "Aged In",
      ChronicStatus
    )
  ) %>%
  select(-DaysHomelessInProject,-DaysHomelessBeforeEntry)

# adds another ChronicStatus of "Nearly Chronic" which catches those hhs with
# almost enough times and months to qualify as Chronic
nearly_chronic <- agedIntoChronicity %>%
  mutate(
    ChronicStatus = if_else(
      ChronicStatus == "Not Chronic" &
        ((
          ymd(DateToStreetESSH) + days(365) <= ymd(EntryDate) &
            !is.na(DateToStreetESSH)
        ) |
          (
            MonthsHomelessPastThreeYears %in% c(110:113) &
              TimesHomelessPastThreeYears%in% c(3, 4) &
              !is.na(MonthsHomelessPastThreeYears) &
              !is.na(TimesHomelessPastThreeYears)
          )
        ) &
        DisablingCondition == 1 &
        !is.na(DisablingCondition),
      "Nearly Chronic",
      ChronicStatus
    )
  )
  
active_list <- active_list %>%
  left_join(
    nearly_chronic %>%
      select("PersonalID",
             "HouseholdID",
             "EnrollmentID",
             "ChronicStatus"),
    by = c("PersonalID", "HouseholdID", "EnrollmentID")
  ) %>%
  mutate(
    ChronicStatus = factor(
      ChronicStatus,
      levels = c(
        "Chronic",
        "Aged In",
        "Nearly Chronic",
        "Not Chronic"
      )
    )
  )

# THIS IS WHERE WE'RE SUMMARISING BY HOUSEHOLD (after all the group_bys)

active_list <- active_list %>%
  mutate(
    HH_DQ_issue = if_else(
      correctedhoh == 1 & !is.na(correctedhoh),
      1,
      0
    ),
    HoH_Adjust = case_when(correctedhoh == 1 ~ 1,
                           is.na(correctedhoh) ~ hoh)
  ) %>%
  filter(HoH_Adjust == 1) %>%
  select(-correctedhoh, -RelationshipToHoH, -hoh, -HoH_Adjust)

# Add Referral Status -----------------------------------------------------

# thinking maybe it makes the most sense to only look at referrals that have 
# been accepted for the purposes of the Active List. Because who cares if
# there's an open referral on a client who needs housing? That doesn't mean
# anything because we haven't really assigned a meaning to that. But an
# accepted referral does supposedly mean something, and it would add context
# to know that a household on this list has been accepted into (if not entered 
# into) another project.

# also thinking the Refer-to provider should be an RRH or PSH? Maybe? Because
# referrals to a homeless project wouldn't mean anything on an Active List,
# right?

Referrals <- Referrals %>%
  left_join(Project %>% 
              select(ProjectName, "ReferToPTC" = ProjectType),
            by = c("Referred-ToProvider" = "ProjectName"))

# isolates hhs with an Accepted Referral into a PSH or RRH project
who_has_referrals <- active_list %>%
  left_join(Referrals %>%
              filter(ReferralDate >= today() - days(14) &
                       ReferralOutcome == "Accepted" &
                       ReferToPTC %in% c(3, 9, 13)),
            by = c("PersonalID")) %>%
      select(PersonalID,
             HouseholdID,
             EnrollmentID,
             "ReferredToProvider" = "Referred-ToProvider",
             ReferralDate)

active_list <- active_list %>%
  left_join(
    who_has_referrals,
    by = c("PersonalID", "HouseholdID", "EnrollmentID")
  )

# Fleeing DV --------------------------------------------------------------


dv <- active_list %>%
  left_join(
    HealthAndDV %>%
      filter(DataCollectionStage == 1) %>%
      select(EnrollmentID,
             PersonalID,
             CurrentlyFleeing,
             WhenOccurred),
    by = c("EnrollmentID", "PersonalID")
  ) %>%
  mutate(
    CurrentlyFleeing = if_else(is.na(CurrentlyFleeing), 99, CurrentlyFleeing),
    WhenOccurred = if_else(is.na(WhenOccurred), 99, WhenOccurred),
    CurrentlyFleeing = case_when(
      CurrentlyFleeing %in% c(0, 99) &
        WhenOccurred %in% c(4, 8, 9, 99) ~ "No",
      CurrentlyFleeing == 1 |
        WhenOccurred %in% c(1:3) ~ "Yes",
      CurrentlyFleeing %in% c(8, 9) ~ "Unknown"
    )
  ) %>%
  select(-WhenOccurred)

active_list <- dv

# Clean the House ---------------------------------------------------------

active_list <- active_list %>%
  mutate(
    VeteranStatus = case_when(
      VeteranStatus == 1 ~ "Yes", 
      VeteranStatus == 0 ~ "No",
      VeteranStatus %in% c(8, 9, 99) ~ "Unknown"
      ),
    DisabilityInHH = case_when(
      DisabilityInHH == 1 ~ "Yes", 
      DisabilityInHH == 0 ~ "No",
      DisabilityInHH %in% c(8, 9, 99) ~ "Unknown"
      ),
    IncomeFromAnySource = case_when(
      IncomeFromAnySource == 1 ~ "Yes",
      IncomeFromAnySource == 0 ~ "No",
      IncomeFromAnySource %in% c(8, 9, 99) ~ "Unknown"
    ),
    TAY = case_when(TAY == 1 ~ "Yes",
                    TAY == 0 ~ "No",
                    is.na(TAY) ~ "Unknown"), 
    ProjectName = if_else(
      ProjectName == "Unsheltered Clients - OUTREACH",
      paste("Unsheltered in",
            CountyServed,
            "County"),
      ProjectName
    ),
    PersonalID = as.character(PersonalID),
    Situation = case_when(
      PTCStatus == "Has Entry into RRH or PSH" ~ PTCStatus,
      PTCStatus == "Currently Has No Entry into RRH or PSH" &
        !is.na(ReferredToProvider) ~
        paste(
          "No current Entry into RRH or PSH but",
          ReferredToProvider,
          "accepted this household's referral on",
          ReferralDate
        ),
      PTCStatus == "Currently Has No Entry into RRH or PSH" &
        is.na(ReferredToProvider) &
        !is.na(PHTrack) ~ paste("Permanent Housing Track:",
                                PHTrack,
                                "by",
                                ExpectedPHDate),
      PTCStatus == "Currently Has No Entry into RRH or PSH" &
        is.na(ReferredToProvider) &
        is.na(PHTrack) ~ 
        "No Entry or accepted Referral into PSH/RRH, and no current Permanent Housing Track"
    )
  ) 

rm(list = ls()[!(ls() %in% c("active_list"))])

save.image("images/Active_List.RData")


