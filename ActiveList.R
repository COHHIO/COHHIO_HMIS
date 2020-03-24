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
  filter(
    ((ymd(DateToStreetESSH) + years(1) <= ymd(EntryDate)) |
       (
         MonthsHomelessPastThreeYears %in% c(112, 113) &
           TimesHomelessPastThreeYears == 4
       )) &
      DisablingCondition == 1
  ) %>%
  mutate(ChronicAtEntry = 1)

# pulling all EEs with the Chronic designation, marking all hh members of anyone
# with a Chronic marker as also Chronic

household_chronic <-
  full_join(
    smallEnrollment,
    singly_chronic %>%
      select(EnrollmentID, PersonalID, HouseholdID, ChronicAtEntry),
    by = c("EnrollmentID",
           "PersonalID",
           "HouseholdID")
  ) %>%
  group_by(HouseholdID) %>%
  mutate(
    ChronicHousehold = sum(ChronicAtEntry, na.rm = TRUE),
    ChronicStatus = case_when(
      ChronicHousehold > 0 ~ "Chronic",
      ChronicHousehold == 0 ~ "Not Chronic"
    )
  ) %>%
  ungroup() %>%
  select(-ChronicHousehold)

# adds days in ES or SH projects to days homeless prior to entry and if it adds
# up to 365 or more, it marks the client as ConsecutiveChronic

agedIntoChronicity <- co_active_list %>%
  left_join(smallEnrollment,
            by = c("PersonalID",
                   "EnrollmentID",
                   "HouseholdID")) %>%
  filter(ProjectType %in% c(1, 8) &
           ymd(DateToStreetESSH) + years(1) > ymd(EntryDate)) %>%
  mutate(
    DaysHomelessInProject = difftime(ymd(ExitAdjust),
                                     ymd(EntryDate),
                                     units = "days"),
    DaysBetweenHomeless = difftime(ymd(EntryDate),
                                   if_else(
                                     is.na(ymd(DateToStreetESSH)),
                                     ymd(EntryDate),
                                     ymd(DateToStreetESSH)
                                   ),
                                   units = "days"),
    ConsecutiveChronic = DaysBetweenHomeless + DaysHomelessInProject >= 365,
    ChronicStatus = "Aged In"
  ) %>%
  filter(ConsecutiveChronic == TRUE) %>%
  select(-DaysHomelessInProject,-DaysBetweenHomeless,-ConsecutiveChronic)

nearly_chronic <- allChronicAtEntry %>%
  filter(ChronicStatus == "Not Chronic" &
           ((ymd(DateToStreetESSH) + months(10) <= ymd(EntryDate)) |
              (
                MonthsHomelessPastThreeYears %in% c(110:113) &
                  TimesHomelessPastThreeYears %in% c(3, 4)
              )) &
           DisablingCondition == 1
  ) %>%
  mutate(ChronicStatus = "Nearly Chronic")
  
add_chronicity <- add_scores 

# Add Referral Status -----------------------------------------------------


# Add COVID-19 Status -----------------------------------------------------


# join for Active List ----------------------------------------------------





