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

load("images/COHHIOHMIS.RData")

participating_providers <- Project %>%
  filter(HMISParticipatingProject == 1)

participating_orgs <- participating_providers %>%
  group_by(OrganizationID, OrganizationName) %>%
  summarise(ParticipatingOrgs = n())

orgs <- Organization %>%
  select(OrganizationID, OrganizationName)


