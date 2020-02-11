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

# clients currently homeless in our system

co_currently_homeless <- co_clients_served %>%
  filter(ProjectType %in% c(1, 2, 4, 8),
         is.na(ExitDate)) %>%
  select(PersonalID, ProjectName, ProjectType, HouseholdID, RelationshipToHoH,
         VeteranStatus, EntryDate, AgeAtEntry)



# correcting for bad hh data (while also flagging it) ---------------------

ALL_HHIDs <- co_currently_homeless %>% select(HouseholdID) %>% unique()

# marking who is a hoh

clean_hh_data <- co_currently_homeless %>%
  mutate(
    RelationshipToHoH = if_else(is.na(RelationshipToHoH), 99, RelationshipToHoH),
    hoh = if_else(str_detect(HouseholdID, fixed("s_")) |
           (str_detect(HouseholdID, fixed("h_")) &
              RelationshipToHoH == 1), 1, 0)) 

HHIDs_in_current_logic <- clean_hh_data %>% filter(hoh == 1) %>%
  select(HouseholdID) %>% unique()

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
  select(HouseholdID, PersonalID, correctedhoh) %>%
  ungroup()

# merging the "corrected" hohs back into the main dataset with a flag

Active_List <- clean_hh_data %>%
  left_join(Adjusted_HoHs,
            by = c("HouseholdID", "PersonalID")) %>%
  mutate(
    Note = if_else(
      correctedhoh == 1,
      "This household has a Households-related Data Quality issue. PLEASE correct.",
      NULL
    ),
    HoH_Adjust = case_when(correctedhoh == 1 ~ 1,
                           is.na(correctedhoh) ~ hoh)
  ) %>%
  select(-correctedhoh, - hoh)

rm(Adjusted_HoHs, co_currently_homeless, HHIDs_with_bad_dq)


# Adding in Disability Status of Household --------------------------------


# Indicate if the Household Has No Income ---------------------------------


# Add in Score ------------------------------------------------------------


# Add in Household Size ---------------------------------------------------


# Add in PH Track and Expected PH Date ------------------------------------


# Add in County -----------------------------------------------------------


# Add in TAY status -------------------------------------------------------




