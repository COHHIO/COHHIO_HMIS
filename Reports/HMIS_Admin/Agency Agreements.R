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

# prep

library(tidyverse)
library(lubridate)
library(here)

load(here("images/COHHIOHMIS.RData"))

# agencies with HMIS-participating projects under them

agencies <- Project %>%
  select(
    ProjectID,
    OrganizationID,
    OperatingStartDate,
    OperatingEndDate,
    HMISParticipatingProject
  ) %>%
  filter(HMISParticipatingProject == 1 &
           !is.na(OperatingStartDate) &
           is.na(OperatingEndDate)) %>%
  left_join(Organization, by = "OrganizationID") %>%
  select(OrganizationID, OrganizationName) %>%
  unique()

# agency agreements currently in effect

raw_agreements <- read_csv(here("Reports/HMIS_Admin/agency_agreements.csv"))

current_agreements <- raw_agreements %>%
  mutate(DateSubmitted = NULL,
         AgreementStartDate = mdy(AgreementStartDate),
         AgreementEndDate = mdy(AgreementEndDate)) %>%
  filter(!is.na(AgreementStartDate) &
           (ymd(AgreementEndDate) >= today() |
           is.na(AgreementEndDate)))

# agencies' current agency agreements

all_agencies <- agencies %>%
  left_join(current_agreements, by = "OrganizationID") %>%
  filter(OrganizationID != 1) %>%
  arrange(OrganizationID) %>%
  select(1, "OrganizationName" = 2, 4, 5)

# write it out

write_csv(all_agencies, here("Reports/HMIS_Admin/AgencyAgreementsToAW.csv"))

