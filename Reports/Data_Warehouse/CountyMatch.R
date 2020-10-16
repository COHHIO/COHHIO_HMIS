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
library(here)

load(here("images/COHHIOHMIS.RData"))

county <- Enrollment %>%
  left_join(Project %>%
              select(ProjectName, ProjectCounty), by = "ProjectName") %>%
  filter(ProjectName != "Unsheltered Clients - OUTREACH") %>%
  mutate(
    CountyGuessed = if_else(is.na(CountyServed), 1, 0),
    County = if_else(CountyGuessed == 1, 
                     ProjectCounty, 
                     CountyServed)
  ) %>%
  select(EnrollmentID, County)

write_csv(county, here("Reports/Data_Warehouse/bos-county.csv"))



