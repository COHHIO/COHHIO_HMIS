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
library(TSstudio)

load("images/cohorts.RData")
load("images/COHHIOHMIS.RData")

unsheltered_hhs <- co_clients_served %>%
  filter(ProjectName == "Unsheltered Clients - OUTREACH" &
           served_between(., "10012019", "04152020")) %>%
  select(HouseholdID, EntryDate, ExitAdjust) %>%
  left_join(Enrollment %>% 
              select(HouseholdID, CountyServed, UserCreating),
            by = "HouseholdID") %>%
  unique()

county_guesses <- unsheltered_hhs %>%
  left_join(Users, by = "UserCreating")

df <- county_guesses %>%
  mutate(County = if_else(is.na(CountyServed),
                          UserCounty,
                          CountyServed),
         EntryDate = paste0(month(EntryDate), "-", year(EntryDate))) %>%
  select(HouseholdID, EntryDate, County) %>%
  group_by(EntryDate, County) %>%
  summarise(Entries = n()) %>%
  mutate(Entries = as.numeric(Entries)) %>%
  ungroup()

try_df <- df %>%
  pivot_wider(names_from = County, values_from = Entries)

try_df[is.na(try_df)] <- 0


