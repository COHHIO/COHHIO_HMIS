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

ClientIDs <- 
  read_csv("data/DupeClientIDs.csv", 
           col_types = "cn") %>%
  distinct() %>%
  filter(!str_starts(Unique, "aa"))

dupes <- get_dupes(ClientIDs, Unique)

Rm_Clients <- co_clients_served %>%
  select(PersonalID) %>%
  distinct() %>%
  left_join(ClientIDs, by = c("PersonalID" = "ClientID"))

dupes_that_matter <-
  semi_join(dupes, Rm_Clients,
            by = "Unique")



