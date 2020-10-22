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
library(readxl)
library(here)


# Demo Settings -----------------------------------------------------------

raw_demo_assessment_locations <-
  read_xlsx(here(
    "random_data/DemoCheckAssessmentAssignmentsandVisibility.xlsx"
  ),
  sheet = 1) %>%
  select(1, "displaytype" = 3, 4, 5)

raw_demo_ee_types_assessments <-
  read_xlsx(here(
    "random_data/DemoCheckAssessmentAssignmentsandVisibility.xlsx"
  ),
  sheet = 2)

raw_demo_assessments_wo_global_visibility <-
  read_xlsx(here(
    "random_data/DemoCheckAssessmentAssignmentsandVisibility.xlsx"
  ),
  sheet = 4)

raw_demo_providers_wo_global_visibility <-
  read_xlsx(here(
    "random_data/DemoCheckAssessmentAssignmentsandVisibility.xlsx"
  ),
  sheet = 5)

raw_demo_flags <-
  read_xlsx(here("random_data/DemoFlagsandHUDStandards.xlsx"),
            sheet = 1)

raw_demo_sp_module <-
  read_xlsx(here("random_data/DemoProviderAdminModules.xlsx"),
            sheet = 1)

raw_demo_services_module <-
  read_xlsx(here("random_data/DemoProviderAdminModules.xlsx"),
            sheet = 2)

raw_demo_client_point_module <-
  read_xlsx(here("random_data/DemoProviderAdminModules.xlsx"),
            sheet = 3)

raw_demo_dupes <- read_xlsx(here("random_data/0212demo.xlsx"),
                            sheet = 1)

# BoS Settings ------------------------------------------------------------

raw_BoS_assessment_locations <-
  read_xlsx(here(
    "random_data/BoSCheckAssessmentAssignmentsandVisibility.xlsx"
  ),
  sheet = 1) %>%
  select(1, "displaytype" = 3, 4, 5)

raw_BoS_ee_types_assessments <-
  read_xlsx(here(
    "random_data/BoSCheckAssessmentAssignmentsandVisibility.xlsx"
  ),
  sheet = 2)

raw_BoS_assessments_wo_global_visibility <-
  read_xlsx(here(
    "random_data/BoSCheckAssessmentAssignmentsandVisibility.xlsx"
  ),
  sheet = 4)

raw_BoS_providers_wo_global_visibility <-
  read_xlsx(here(
    "random_data/BoSCheckAssessmentAssignmentsandVisibility.xlsx"
  ),
  sheet = 5)

raw_BoS_flags <-
  read_xlsx(here("random_data/BoSFlagsandHUDStandards.xlsx"),
            sheet = 1)

raw_BoS_sp_module <-
  read_xlsx(here("random_data/BoSProviderAdminModules.xlsx"),
            sheet = 1)

raw_BoS_services_module <-
  read_xlsx(here("random_data/BoSProviderAdminModules.xlsx"),
            sheet = 2)

raw_BoS_client_point_module <-
  read_xlsx(here("random_data/BoSProviderAdminModules.xlsx"),
            sheet = 3)

raw_BoS_dupes <- 
  read_xlsx(here("random_data/0212BoS.xlsx"),
            sheet = 1)

raw_YO_dupes <- 
  read_xlsx(here("random_data/0212YO.xlsx"),
            sheet = 1) %>%
  select("Client Unique ID" = 1, "Client Uid" = 2, 6:9)

# Comparisons -------------------------------------------------------------

yo_data <- raw_YO_dupes %>%
  select(`First Name`, `Last Name`) %>%
  unique()

bos_data <- raw_BoS_dupes %>%
  select(`First Name`, `Last Name`) %>%
  unique()

demo_data <- raw_demo_dupes %>%
  select(`First Name`, `Last Name`) %>%
  unique()

not_on_demo <- setdiff(bos_data, demo_data)
not_on_bos <- setdiff(demo_data, bos_data)
not_on_yo <- setdiff(demo_data, yo_data)

new_dupes <- setdiff(not_on_bos, yo_data) %>% view()



