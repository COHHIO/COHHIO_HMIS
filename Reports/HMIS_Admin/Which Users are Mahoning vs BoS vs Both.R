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

load(here("images/cohorts.RData"))


users_eda_groups <- read_xlsx(here("data/RMisc2.xlsx"), 
                              sheet = 15) %>%
  select(UserID, UserEmail, EDAGroupName)

eda_groups_providers <- read_xlsx(here("data/RMisc2.xlsx"),
                                  sheet = 16) %>%
  select(ProjectID, EDAGroup)

providers_users <- users_eda_groups %>%
  left_join(eda_groups_providers, by = c("EDAGroupName" = "EDAGroup")) %>%
  filter(!is.na(ProjectID)) %>%
  mutate(coc = case_when(
    ProjectID %in% c(mahoning_projects) ~ "mahoning",
    TRUE ~ "bos"
  )) 

both <- providers_users %>%
  count(UserID, coc) %>%
  get_dupes(UserID) %>%
  select(UserID) %>% unique() %>%
  left_join(providers_users, by = "UserID") %>%
  select(UserEmail) %>% unique()

mahoning_only <- providers_users %>%
  filter(coc == "mahoning") %>%
  anti_join(both, by = "UserID") %>%
  select(UserID) %>%
  unique() %>%
  left_join(providers_users, by = "UserID") %>%
  select(UserEmail) %>% unique()


  
  
    



