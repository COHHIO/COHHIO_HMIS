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

# this gives me the PIT Counts for each project in HMIS.
# I copy it to the R Project called HIC in the raw_data folder
# This only needs to happen once a year.
# Change the PIT date as necessary

library(tidyverse)
library(here)

PITDate <- "01212020"

load(here("images/COHHIOHMIS.RData"))

PIT2020 <- Enrollment %>%
  filter(served_between(., PITDate, PITDate)) %>%
  group_by(ProjectID) %>%
  summarise(PITCount = n()) %>%
  ungroup()

write_csv(PIT2020, here("Reports/PIT2020.csv"))
