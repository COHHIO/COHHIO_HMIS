# COHHIO_HMIS
# Copyright (C) 2021  Coalition on Homelessness and Housing in Ohio (COHHIO)
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
library(here)

load(here("images/Data_Quality.RData"))
load(here("images/COHHIOHMIS.RData"))

provider <- "Licking - LCCH - RROhio - RRH"

# this is an error that can be corrected at the hh level
households <- dq_main %>%
  filter(
    ProjectName == provider &
      str_starts(Issue, "WellSky") &
      served_between(., "10012018", "09302020")
  ) %>%
  group_by(HouseholdID) %>%
  slice_head %>%
  ungroup()

# missing interims are worse
priority_1 <- households %>%
  filter(str_ends(Issue, "Missing Interim")) %>%
  mutate(Priority = 1)

# this past year is more important than the year before
priority_2 <- households %>%
  filter(str_ends(Issue, "Interim Date") &
           served_between(., "10012019", "09302020")) %>%
  mutate(Priority = 2)

priority_3 <- households %>%
  anti_join(priority_1, by = "HouseholdID") %>%
  anti_join(priority_2, by = "HouseholdID") %>%
  mutate(Priority = 3)

striated_dataset <- rbind(priority_1, priority_2, priority_3) %>%
  select(Priority, PersonalID, EntryDate, MoveInDateAdjust)

write_csv(striated_dataset, "random_data/RROhioInterimDQ.csv")


