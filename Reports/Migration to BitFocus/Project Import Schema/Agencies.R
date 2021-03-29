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
library(HMIS)
library(here)

load(here("images/COHHIOHMIS.RData"))

# I think the granularity of this dataset is each address associated with an
# Organization, which makes it very Project-esque. But whatever, we can do that.

agency_from_export <- Organization %>%
  select("id" = OrganizationID,
         "name" = OrganizationName,
         "victim_service_provider" = VictimServicesProvider,
         "added_date" = DateCreated,
         "last_updated" = DateUpdated,
         )

agency_ids <- Project %>%
  select(ProjectID, OrganizationID) %>%
  left_join(ProjectCoC[c("ProjectID", "ProjectCoCID")], by = "ProjectID") %>%
  left_join(Organization[c("OrganizationID", "OrganizationName")], by = "OrganizationID")




