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
library(lubridate)
library(janitor)

load("images/cohorts.RData")
load("images/COHHIOHMIS.RData")

# clients currently entered into a homeless project in our system

co_currently_homeless <- co_clients_served %>%
  filter(is.na(ExitDate) &
           (ProjectType %in% c(1, 2, 4, 8) |
              (
                ProjectType %in% c(3, 9, 13) &
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

# correcting for bad hh data (while also flagging it) ---------------------

ALL_HHIDs <- co_currently_homeless %>% select(HouseholdID) %>% unique()

# marking who is a hoh (accounts for singles not marked as hohs in the data)

clean_hh_data <- co_currently_homeless %>%
  mutate(
    RelationshipToHoH = if_else(is.na(RelationshipToHoH), 99, RelationshipToHoH),
    hoh = if_else(str_detect(HouseholdID, fixed("s_")) |
           (str_detect(HouseholdID, fixed("h_")) &
              RelationshipToHoH == 1), 1, 0)) 

HHIDs_in_current_logic <- clean_hh_data %>% 
  filter(hoh == 1) %>%
  select(HouseholdID) %>%
  unique()

# marking which hhs are not represented in the hohs marked (bc of bad hh data)

HHIDs_with_bad_dq <-
  anti_join(ALL_HHIDs, HHIDs_in_current_logic,
            by = "HouseholdID") %>%
  left_join(clean_hh_data, by = "HouseholdID")

rm(ALL_HHIDs, HHIDs_in_current_logic)

# assigning hoh status to the oldest person in the hh

Adjusted_HoHs <- HHIDs_with_bad_dq %>%
  group_by(HouseholdID) %>%
  arrange(desc(AgeAtEntry)) %>% # picking oldest hh member
  slice(1L) %>% 
  mutate(correctedhoh = 1) %>%
  select(HouseholdID, PersonalID, EnrollmentID, correctedhoh) %>%
  ungroup()

# merging the "corrected" hohs back into the main dataset with a flag

co_active_list <- clean_hh_data %>%
  left_join(Adjusted_HoHs,
            by = c("HouseholdID", "PersonalID", "EnrollmentID")) %>%
  mutate(
    Note = if_else(
      correctedhoh == 1,
      "This household has a Households-related Data Quality issue. PLEASE correct.",
      NULL
    ),
    HoH_Adjust = case_when(correctedhoh == 1 ~ 1,
                           is.na(correctedhoh) ~ hoh)
  ) %>%
  select(-correctedhoh, -hoh)

rm(Adjusted_HoHs, co_currently_homeless, HHIDs_with_bad_dq, clean_hh_data)

# Adding in Disability Status of HH, County, PHTrack ----------------------


disability_data <- co_active_list %>%
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
    HouseholdSize = n(),
    DisablingCondition = if_else(DisablingCondition == 1, 100, DisablingCondition),
    DisabilityInHH = max(DisablingCondition),
    DisablingCondition = if_else(DisablingCondition == 100, 1, DisablingCondition),
    DisabilityInHH = if_else(DisabilityInHH == 100, 1, 0),
    TAY = if_else(max(AgeAtEntry) < 25, 1, 0),
    PHTrack = if_else(ymd(ExpectedPHDate) < today(), "<expired>", PHTrack)
  ) %>%
  ungroup() 

# Indicate if the Household Has No Income ---------------------------------

income_data <- disability_data %>%
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
  mutate(
    DataCollectionStage = case_when(
      DataCollectionStage == 1 ~ "Entry",
      DataCollectionStage == 2 ~ "Update",
      DataCollectionStage == 3 ~ "Exit",
      DataCollectionStage == 5 ~ "Annual"
    )
  )

income_staging_fixed <- income_data %>% 
  filter(DataCollectionStage == "Entry") 

income_staging_variable <- income_data %>%
  filter(DataCollectionStage %in% c("Update", "Annual", "Exit")) %>%
  group_by(EnrollmentID) %>%
  mutate(MaxUpdate = max(ymd_hms(DateCreated))) %>%
  filter(MaxUpdate == DateCreated) %>%
  select(-MaxUpdate) %>%
  distinct() %>%
  ungroup() 

income_staging <-
  rbind(income_staging_fixed, income_staging_variable) %>%
  select(PersonalID,
         EnrollmentID,
         IncomeFromAnySource,
         DataCollectionStage) %>%
  unique() %>%
  mutate(
    DataCollectionStage = case_when(
      DataCollectionStage == "Entry" ~ "Entry",
      DataCollectionStage != "Entry" ~ "After Entry"
    )
  ) %>% 
  group_by(PersonalID, EnrollmentID) %>%
  arrange(DataCollectionStage) %>%
  slice(1L) %>%
  ungroup() %>%
  select(-DataCollectionStage)
  
adding_in_income <- disability_data %>%
  left_join(income_staging, by = c("PersonalID", "EnrollmentID")) 
  
# Add in Score ------------------------------------------------------------

scores_staging <- Scores %>%
  filter(ScoreDate > today() - years(1)) %>%
  group_by(PersonalID) %>%
  arrange(desc(ymd(ScoreDate))) %>%
  slice(1L) %>%
  ungroup() %>%
  select(-ScoreDate)

add_scores <- adding_in_income %>%
  left_join(scores_staging, by = "PersonalID")

# Add Chronicity ----------------------------------------------------------

# creating a small dataframe of only independently chronic clients

smallEnrollment <- Enrollment %>%
  select(EnrollmentID, PersonalID, HouseholdID, LivingSituation, 
         DateToStreetESSH, TimesHomelessPastThreeYears, ExitAdjust,
         MonthsHomelessPastThreeYears, DisablingCondition)

singly_chronic <- 
  co_active_list %>%
  left_join(smallEnrollment, by = c("PersonalID",
                                    "EnrollmentID",
                                    "HouseholdID")) %>%
  mutate(SinglyChronic = if_else(((ymd(DateToStreetESSH) + years(1) <= ymd(EntryDate)) |
                                     (
                                       MonthsHomelessPastThreeYears %in% c(112, 113) &
                                         TimesHomelessPastThreeYears == 4
                                     )) &
                                    DisablingCondition == 1, 1, 0))

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

# adds days in ES or SH projects to days homeless prior to entry and if it adds
# up to 365 or more, it marks the client as ConsecutiveChronic

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
        ymd(DateToStreetESSH) + years(1) > ymd(EntryDate) &
        DaysHomelessBeforeEntry + DaysHomelessInProject >= 365,
      "Aged In",
      ChronicStatus
    )
  ) %>%
  select(-DaysHomelessInProject,-DaysHomelessBeforeEntry)

nearly_chronic <- agedIntoChronicity %>%
  mutate(
    ChronicStatus = if_else(
      ChronicStatus == "Not Chronic" &
        DisablingCondition == 1 &
        ((ymd(DateToStreetESSH) + months(10) <= ymd(EntryDate)) |
           (
             MonthsHomelessPastThreeYears %in% c(110:113) &
               TimesHomelessPastThreeYears %in% c(3, 4)
           )
        ),
      "Nearly Chronic",
      ChronicStatus
    )
  )
  
add_chronicity <- add_scores %>%
  left_join(
    nearly_chronic %>%
      select("PersonalID",
             "HouseholdID",
             "EnrollmentID",
             "ChronicStatus"),
    by = c("PersonalID", "HouseholdID", "EnrollmentID")
  )

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
  left_join(Project %>% select(ProjectName, "ReferToPTC" = ProjectType),
            by = c("Referred-ToProvider" = "ProjectName"))

who_has_referrals <- add_chronicity %>%
  left_join(Referrals %>%
              filter(ReferralDate >= today() - days(14) &
                       ReferralOutcome == "Accepted" &
                       ReferToPTC %in% c(3, 9, 13)),
            by = c("PersonalID"))

add_referrals <- add_chronicity %>%
  left_join(
    who_has_referrals %>%
      select(PersonalID,
             HouseholdID,
             EnrollmentID,
             "ReferredToProvider" = "Referred-ToProvider",
             ReferralDate),
    by = c("PersonalID", "HouseholdID", "EnrollmentID")
  )


# Add COVID-19 Status -----------------------------------------------------


# Account for Multiple EEs ------------------------------------------------

ptc_status <- add_referrals %>%
  mutate(PTCStatus = case_when(
    ProjectType %in% c(1, 2, 4, 8) ~ "LH",
    ProjectType %in% c(3, 9, 13) ~ "PH"
  )) 

clients_in_ph <- ptc_status %>%
  group_by(PersonalID) %>%
  arrange(desc(PTCStatus)) %>%
  slice(1L) %>%
  filter(PTCStatus == "PH") %>%
  ungroup() %>%
  mutate(InPH = 1) %>%
  select(PersonalID, InPH)

clients_in_lh <- ptc_status %>%
  group_by(PersonalID) %>%
  arrange(PTCStatus) %>%
  slice(1L) %>%
  filter(PTCStatus == "LH") %>%
  ungroup() %>%
  mutate(LH = 1) %>%
  select(PersonalID, LH)

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

add_ptc_status <- add_referrals %>%
    left_join(client_ptc_status, by = "PersonalID")

split_up_dupes <- get_dupes(add_ptc_status, PersonalID) %>%
  mutate(PTCGames = if_else(ProjectType %in% c(1, 2, 4, 8), 1, 2)) %>%
  group_by(PersonalID) %>%
  arrange(PTCGames, desc(EntryDate)) %>%
  slice(1L)

duplicated_clients <- get_dupes(add_ptc_status, PersonalID) %>%
  pull(PersonalID) %>% unique()

filter_out_dupes <- add_ptc_status %>%
  filter(!PersonalID %in% duplicated_clients)

deduplicated_active_list <- rbind(split_up_dupes, filter_out_dupes)
  
  

# join for Active List ----------------------------------------------------





