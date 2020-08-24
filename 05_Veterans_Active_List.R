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

load("images/COHHIOHMIS.RData")

# Projects ----------------------------------------------------------------

vet_projects <- Project %>%
  filter(ProjectType %in% c(1, 2, 3, 4, 8, 9, 13) &
           operating_between(., format(today() - days(90), "%m%d%Y"),
                               format(today(), "%m%d%Y")))

# Get all veterans and associated hh members ------------------------------

responsible_providers <- ServiceAreas %>%
  select(County, SSVFServiceArea) 

bos_counties <- ServiceAreas %>%
  filter(CoC == "OH-507 Balance of State") %>%
  pull(County)

vet_ees <- Enrollment %>%
  left_join(Client %>% select(PersonalID, VeteranStatus), by = "PersonalID") %>%
  mutate(VeteranStatus = if_else(VeteranStatus == 1, 1, 0)) %>%
  group_by(HouseholdID) %>%
  summarise(VetCount = sum(VeteranStatus)) %>%
  ungroup() %>%
  filter(VetCount > 0) %>%
  left_join(Enrollment, by = "HouseholdID") %>%
  left_join(Client %>% select(PersonalID, VeteranStatus), by = "PersonalID") %>%
  filter((CountyServed %in% c(bos_counties) | is.na(CountyServed)) &
           !ProjectID %in% c(1282)) %>%
  select(1, 3:15, 17, 51, 67, 73, 75:90)

# Currently in PSH/RRH ----------------------------------------------------

# RRH PSH stays with no Exit but a valid Move-In Date

currently_housed_in_psh_rrh <- vet_ees %>%
  filter(stayed_between(., start = format(today(), "%m%d%Y"), 
                        end = format(today(), "%m%d%Y")) &
           ProjectType %in% c(3, 9, 13)) %>%
  pull(HouseholdID)

# Active List -------------------------------------------------------------

# stayers & people who exited in the past 90 days to a temp destination

active_list <- vet_ees %>%
  filter(!HouseholdID %in% c(currently_housed_in_psh_rrh) &
           (is.na(ExitDate) |
              (!Destination %in% c(3, 10, 11, 19:23, 28, 31, 33, 34, 36) &
                 ymd(ExitDate) >= today() - days(90))))

# Currently Homeless Vets -------------------------------------------------


# Entered in Past 90 Days -------------------------------------------------


# Declined  ---------------------------------------------------------------


# Data Quality ------------------------------------------------------------


# Current Long Term -------------------------------------------------------


# Active Long Term --------------------------------------------------------


# Current Chronic ---------------------------------------------------------


# Active Chronic ----------------------------------------------------------


# New GPD -----------------------------------------------------------------


# Offers on Current -------------------------------------------------------


# Offers on Active --------------------------------------------------------


# Exited to PH ------------------------------------------------------------


# New and Exited to PH ----------------------------------------------------





