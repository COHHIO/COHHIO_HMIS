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
library(readxl)

art_data <- read_xlsx(here("random_data/OrganizationsBitFocus.xlsx"), 
                      sheet = 4) %>%
  filter(!is.na(ProjectID)) %>% # dropping deleted providers
  mutate(MinEntry = as.Date(MinEntry, origin = "1899-12-30"),
         MaxExit = as.Date(MaxExit, origin = "1899-12-30"),
         OpenEEs = replace_na(OpenEEs, 0),
         operating = if_else(operating == "Yes", 1, 0),
         participating = if_else(participating == "Yes", 1, 0))

# the warnings are ok
org_level <- art_data %>%
  group_by(Org) %>%
  summarise(minEntry = min(ymd(MinEntry), na.rm = TRUE),
            maxExit = max(ymd(MaxExit), na.rm = TRUE),
            openEEs = sum(OpenEEs),
            maxOperating = max(operating),
            maxParticipating = max(participating))


# Separating Out Orgs that are coming over --------------------------------

# orgs that have providers with data in them that's newer than 2014-05-01 or
#   some project under the org is HMIS participating
# definitely coming over
orgs_data_7yrs <- org_level %>%
  filter(ymd(maxExit) > ymd("20140501")) %>%
  select(Org) %>%
  unique()

# some provider in the org is HMIS participating
# definitely coming over
orgs_data_7yrs_participating <- orgs_data_7yrs %>%
  left_join(org_level, by = "Org") %>%
  filter(maxParticipating == 1)

# no providers in the org are HMIS participating
# maybe coming over
orgs_data_7yrs_not_participating <- orgs_data_7yrs %>%
  left_join(org_level, by = "Org") %>%
  filter(maxParticipating == 0)


orgs_data_old <- org_level %>%
  filter(ymd(maxExit) <= ymd("20140501")) %>%
  select(Org) %>%
  unique() 

VASH_only_orgs <- art_data %>%
  mutate(VASH = if_else(str_detect(Project, "VASH"), 1, 0)) %>%
  group_by(Org) %>%
  summarise(total = n(),
            vash = sum(VASH)) %>%
  ungroup() %>%
  filter(total - vash == 0) %>%
  select(Org)

non_participating_orgs <- art_data %>%
  anti_join(orgs_data_7yrs, by = "Org") %>%
  filter(str_starts(Project, "zz", negate = TRUE) &
           participating == "No") %>%
  # select(Org) %>%
  unique()

org_same_as_project <- art_data %>%
  filter(Org == Project)
