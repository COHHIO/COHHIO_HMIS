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
library(readxl)

# get necessary data ------------------------------------------------------

Project <- 
  read_csv(here("data_to_Clarity/Project.csv"),
           col_types = "nnccDDnnnnnnnnTTcTn") 

Organization <- 
  read_csv(here("data_to_Clarity/Organization.csv"),
           col_types = "ncncTTnTn") 

ProjectCoC <- 
  read_csv(here("data_to_Clarity/ProjectCoC.csv"),
           col_types = "nncnccccnnTTcTn")

Addresses <- read_xlsx(
    here("data_to_Clarity/RMisc2.xlsx"),
    sheet = 3,
    col_types = c("numeric", replicate(16, "text"))
  ) %>% 
  mutate(OrganizationName = str_remove(OrganizationName, "\\(.*\\)"))


# Adding in any data that can come from the HUD CSV Export ----------------

# The granularity of this dataset is the Organization, 1 site:1 Organization

agency_from_export <- Organization %>%
  select("id" = OrganizationID,
         "name" = OrganizationName,
         "default_site_name" = OrganizationName,
         "victim_service_provider" = VictimServicesProvider,
         "added_date" = DateCreated,
         "last_updated" = DateUpdated
         ) %>%
  filter(!is.na(name) & id != 1) # both records filtered out from this are not needed. GD

# Getting Addresses of Organizations from RMisc ---------------------------

incl_addresses <- agency_from_export %>%
  left_join(Addresses[
    c(
      "ProjectID",
      "Address1",
      "Address2",
      "City",
      "State",
      "ProjectCounty",
      "ZIP",
      "Lat",
      "Long"
    )
  ], by = c("id" = "ProjectID")) %>%
  mutate(
    geolocations.address = paste(Address1, Address2),
    geolocations.city = City,
    geolocations.state = State,
    counties.id = ProjectCounty, # will need to be converted to a number from BF
    geolocations.zipcode = substr(ZIP, 1, 5),
    geolocations.geocode = paste(Lat, Long),
    site.name = name
  ) %>%
  select(-Address1, -Address2, -City, -State, -ProjectCounty, -ZIP, -Lat, -Long)

# Check for missing addresses of Organizations ----------------------------

missing_addresses <- incl_addresses %>%
  filter(is.na(geolocations.city))

write_csv(missing_addresses, here("random_data/missing_org_addresses.csv"))

# Adding in CoC at the Org level (which is weird, some straddle CoCs) -----

# This is currently written to just take a random CoC value where there
# are two CoC's associated with an Organization

coc <- ProjectCoC %>%
  left_join(Project[c("ProjectID", "OrganizationID")], by = "ProjectID") %>%
  select(OrganizationID, CoCCode) %>%
  unique() %>%
  select("id" = OrganizationID, "coc" = CoCCode) %>%
  filter(coc %in% c("OH-504", "OH-507"))

incl_coc <- incl_addresses %>%
  left_join(coc, by = "id") %>%
  group_by(id) %>%
  slice_max(coc)

# Adding in other columns -------------------------------------------------

Agencies <- incl_coc %>%
  mutate(
    status = 1,
    navigation_profiles.id = 0,
    screens.name = 0,
    home_screen = 1,
    ref_looker_report_open_units = 0,
    all_client_forms_enabled = 1,
    department = 0,
    clients = 2,
    release_of_information = 1,
    ref_coordinated_entry = 0,
    ref_looker_report = 0,
    send_referral_notifications = 1
  ) %>%
  relocate(victim_service_provider, .after = ref_looker_report_open_units) %>%
  relocate(c(added_date, last_updated), .after = all_client_forms_enabled)


# Writing it out to csv ---------------------------------------------------

write_csv(Agencies, here("random_data/Agencies.csv"))
