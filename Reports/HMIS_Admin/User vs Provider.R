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
library(janitor)
library(here)
library(readxl)

users_eda_groups <- read_xlsx(here("data/RMisc2.xlsx"),
                              sheet = 15) %>%
  select(UserID, UserName, EDAGroupName) %>%
  mutate(EDAGroupName = str_remove(EDAGroupName, "\\(.*\\)")) 

eda_groups_providers <- read_xlsx(here("data/RMisc2.xlsx"),
                                  sheet = 16) %>%
  select(ProjectID, EDAGroup) %>%
  mutate(EDAGroup = str_remove(EDAGroup, "\\(.*\\)"))

orgs <- read_xlsx(here("data/RMisc2.xlsx"),
                  sheet = 3) %>%
  mutate(OrganizationID = str_extract(OrganizationName, "\\(.*\\)"),
         OrganizationID = str_remove(OrganizationID, "[(]"),
         OrganizationID = str_remove(OrganizationID, "[)]"),
         OrganizationID = as.double(OrganizationID),
         OrganizationName = str_remove(OrganizationName, "\\(.*\\)")) %>%
  select(ProjectID, ProjectName, OrganizationID, OrganizationName)

providers_users <- users_eda_groups %>%
  left_join(eda_groups_providers, by = c("EDAGroupName" = "EDAGroup")) %>%
  filter(!is.na(ProjectID) &
           ProjectID != 1695) %>%
  unique() %>%
  select(UserID, UserName, ProjectID) %>%
  left_join(orgs, by = "ProjectID") %>%
  mutate(OrgAdjust = case_when(OrganizationID == 1 ~ ProjectID,
                               TRUE ~ OrganizationID)) %>%
  filter(!OrgAdjust %in% c(2361, 1027, 1032) &
           !is.na(OrgAdjust)) %>%
  select(UserID, UserName, OrgAdjust) %>%
  left_join(orgs[c("ProjectID", "ProjectName")] %>%
              unique(), by = c("OrgAdjust" = "ProjectID")) %>%
  unique()

user_count <- providers_users %>%
  count(OrgAdjust)

users_in_more_than_1_org <- providers_users %>%
  get_dupes(UserID, UserName)

mahoning_projects <-
  c(696:697, 1327:1328, 1330:1331, 1392, 1638:1641, 1704, 1738, 2103, 2105,
    2110, 2322:2336, 2338:2360, 2362:2385, 2437)
