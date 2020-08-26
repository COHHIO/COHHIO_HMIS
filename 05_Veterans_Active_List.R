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
load("images/cohorts.RData")

# Projects ----------------------------------------------------------------

vet_projects <- Project %>%
  filter(ProjectType %in% c(lh_at_entry_project_types) &
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
           ProjectType %in% c(lh_project_types)) %>%
  pull(PersonalID)

# Active List -------------------------------------------------------------

# stayers & people who exited in the past 90 days to a temp destination

active_list <- vet_ees %>%
  filter(!PersonalID %in% c(currently_housed_in_psh_rrh) &
           (is.na(ExitDate) |
              (!Destination %in% c(perm_destinations) &
                 ymd(ExitDate) >= today() - days(90))))

# Currently Homeless Vets -------------------------------------------------

# same as Active List except it only includes stayers and leaves out households 
# that have exited to a temporary destination. Not sure we'll need this actually
# because we can just make it a widget on the report, to exclude those.

# Entered in Past 90 Days -------------------------------------------------

entered_past_90 <- vet_ees %>%
  filter(entered_between(., format(today() - days(90), "%m%d%Y"),
                         format(today(), "%m%d%Y")))

# Declined  ---------------------------------------------------------------

declined <- vet_ees %>%
  left_join(VeteranCE, by = c("PersonalID", "EnrollmentID")) %>%
  filter(MostRecentOfferStatus == "Declined" &
           ymd(MostRecentOfferDate) >= today() - days(90))

# Data Quality ------------------------------------------------------------

# this is just an intersection of currently homeless vets and currently housed
# in rrh and psh that would indicate a data quality issue, but these would 
# already be on the Data Quality report as an Overlap, so why do we need this
# here? We could flag any households with overlaps in the report.

# Long Term -------------------------------------------------------

# thinking of moving the code I already wrote for this in the Active List
# up to cohorts.R so I can get this easily from there instead of having to
# copy that code to here

# Chronic ---------------------------------------------------------

# thinking of moving the code I already wrote for this in the Active List
# up to cohorts.R so I can get this easily from there instead of having to
# copy that code to here

# New GPD -----------------------------------------------------------------

new_gpd <- entered_past_90 %>%
  filter(ProjectID %in% c(GPD_project_ids))

# Offers ------------------------------------------------------------------

# checking to be sure I'm not using "Most Recent Offer ..." data anywhere
# since I should be able to just use the subs in Rm/Rme and eliminate those
# redundant data elements once this is all done.

# Exited to PH ------------------------------------------------------------


# New and Exited to PH ----------------------------------------------------





