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
library(rlang)
library(lubridate)


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

new_dupes <- setdiff(not_on_bos, yo_data)


# RMisc comparisons -------------------------------------------------------

rmisc_1_demo <- read_xlsx(here("random_data/RMisc2.xlsx"), sheet = 1)
rmisc_1_bos <- read_xlsx(here("data/RMisc2.xlsx"), sheet = 1)
rmisc_2_demo <- read_xlsx(here("random_data/RMisc2.xlsx"), sheet = 2)
rmisc_2_bos <- read_xlsx(here("data/RMisc2.xlsx"), sheet = 2)
rmisc_3_demo <- read_xlsx(here("random_data/RMisc2.xlsx"), sheet = 3)
rmisc_3_bos <- read_xlsx(here("data/RMisc2.xlsx"), sheet = 3)
rmisc_4_demo <- read_xlsx(here("random_data/RMisc2.xlsx"), sheet = 4)
rmisc_4_bos <- read_xlsx(here("data/RMisc2.xlsx"), sheet = 4)
rmisc_5_demo <- read_xlsx(here("random_data/RMisc2.xlsx"), sheet = 5)
rmisc_5_bos <- read_xlsx(here("data/RMisc2.xlsx"), sheet = 5)
rmisc_6_demo <- read_xlsx(here("random_data/RMisc2.xlsx"), sheet = 6)
rmisc_6_bos <- read_xlsx(here("data/RMisc2.xlsx"), sheet = 6)
rmisc_7_demo <- read_xlsx(here("random_data/RMisc2.xlsx"), sheet = 7)
rmisc_7_bos <- read_xlsx(here("data/RMisc2.xlsx"), sheet = 7)
rmisc_8_demo <- read_xlsx(here("random_data/RMisc2.xlsx"), sheet = 8)
rmisc_8_bos <- read_xlsx(here("data/RMisc2.xlsx"), sheet = 8)
rmisc_9_demo <- read_xlsx(here("random_data/RMisc2.xlsx"), sheet = 9)
rmisc_9_bos <- read_xlsx(here("data/RMisc2.xlsx"), sheet = 9)
rmisc_10_demo <- read_xlsx(here("random_data/RMisc2.xlsx"), sheet = 10)
rmisc_10_bos <- read_xlsx(here("data/RMisc2.xlsx"), sheet = 10)
rmisc_11_demo <- read_xlsx(here("random_data/RMisc2.xlsx"), sheet = 11)
rmisc_11_bos <- read_xlsx(here("data/RMisc2.xlsx"), sheet = 11)
rmisc_12_demo <- read_xlsx(here("random_data/RMisc2.xlsx"), sheet = 12)
rmisc_12_bos <- read_xlsx(here("data/RMisc2.xlsx"), sheet = 12)
rmisc_13_demo <- read_xlsx(here("random_data/RMisc2.xlsx"), sheet = 13)
rmisc_13_bos <- read_xlsx(here("data/RMisc2.xlsx"), sheet = 13)
rmisc_14_demo <- read_xlsx(here("random_data/RMisc2.xlsx"), sheet = 14)
rmisc_14_bos <- read_xlsx(here("data/RMisc2.xlsx"), sheet = 14)
rmisc_15_demo <- read_xlsx(here("random_data/RMisc2.xlsx"), sheet = 15)
rmisc_15_bos <- read_xlsx(here("data/RMisc2.xlsx"), sheet = 15)
rmisc_16_demo <- read_xlsx(here("random_data/RMisc2.xlsx"), sheet = 16)
rmisc_16_bos <- read_xlsx(here("data/RMisc2.xlsx"), sheet = 16)
rmisc_17_demo <- read_xlsx(here("random_data/RMisc2.xlsx"), sheet = 17)
rmisc_17_bos <- read_xlsx(here("data/RMisc2.xlsx"), sheet = 17)
rmisc_18_demo <- read_xlsx(here("random_data/RMisc2.xlsx"), sheet = 18)
rmisc_18_bos <- read_xlsx(here("data/RMisc2.xlsx"), sheet = 18)

# sheet 1- can't really do anything bc EnrollmentIDs aren't equivalent

# sheet 2- extra records on demo are Mahoning users (ok)

setdiff(rmisc_2_demo, rmisc_2_bos) %>% view()

# sheet 3- extra records on demo are Mahoning providers (ok)

setdiff(rmisc_3_demo, rmisc_3_bos) %>% view()

# sheet 4- can't really do anything bc PersonalIDs aren't equivalent




# Merge EEs report compare ------------------------------------------------

ees_bos <- read_xlsx(here("sampledata/Merge_ EEs_bos.xlsx")) %>% 
  # select(-EEProvider) %>%
  mutate(EntryDate = as.Date(EntryDate, origin = "1899-12-30"),
         ExitDate = as.Date(ExitDate, origin = "1899-12-30"),
         EEDateAdded = as.Date(EEDateAdded, origin = "1899-12-30"),
         EEDateUpdated = as.Date(EEDateUpdated, origin = "1899-12-30"))
  
ees_demo <- read_xlsx(here("sampledata/Merge_ EEs_demo.xlsx")) %>% 
  # select(-EEProvider) %>%
  mutate(EntryDate = as.Date(EntryDate, origin = "1899-12-30"),
         ExitDate = as.Date(ExitDate, origin = "1899-12-30"),
         EEDateAdded = as.Date(EEDateAdded, origin = "1899-12-30"),
         EEDateUpdated = as.Date(EEDateUpdated, origin = "1899-12-30"))

ees_yo <- read_xlsx(here("sampledata/Merge_ EEs_yo.xlsx")) %>% 
  # select(-EEProvider) %>%
  mutate(EntryDate = as.Date(EntryDate, origin = "1899-12-30"),
         ExitDate = as.Date(ExitDate, origin = "1899-12-30"),
         EEDateAdded = as.Date(EEDateAdded, origin = "1899-12-30"),
         EEDateUpdated = as.Date(EEDateUpdated, origin = "1899-12-30"))

ee_bos_and_yo <- rbind(ees_bos, ees_yo) %>% unique()

not_on_demo <- setdiff(ees_demo$ClientUID, ee_bos_and_yo$ClientUID)

missing_ees_from_yo <- ees_yo %>% filter(ClientUID %in% c(not_on_demo) &
                                           EEInactive == "No" &
                                           str_starts(EEProvider, "zz", 
                                                      negate = TRUE))

# Merge Svcs report compare -----------------------------------------------

svcs_bos <- read_xlsx(here("random_data/Merge_Services_bos.xlsx")) %>% 
  mutate(Start = floor_date(as.Date(Start, origin = "1899-12-30"), unit = "day"),
         End = floor_date(as.Date(End, origin = "1899-12-30"), unit = "day"),
         Provider = str_remove(Provider, "\\(.*\\)"),
         DateAdded = floor_date(as.Date(DateAdded, origin = "1899-12-30"), unit = "day"),
         ReferDate = floor_date(as.Date(ReferDate, origin = "1899-12-30"), unit = "day"))

svcs_demo <- read_xlsx(here("random_data/Merge_Services_demo.xlsx")) %>% 
  mutate(Start = floor_date(as.Date(Start, origin = "1899-12-30"), unit = "day"),
         End = floor_date(as.Date(End, origin = "1899-12-30"), unit = "day"),
         Provider = str_remove(Provider, "\\(.*\\)"),
         DateAdded = floor_date(as.Date(DateAdded, origin = "1899-12-30"), unit = "day"),
         ReferDate = floor_date(as.Date(ReferDate, origin = "1899-12-30"), unit = "day"))


svcs_yo <- read_xlsx(here("random_data/Merge_Services_yo.xlsx")) %>% 
  mutate(Start = floor_date(as.Date(Start, origin = "1899-12-30"), unit = "day"),
         End = floor_date(as.Date(End, origin = "1899-12-30"), unit = "day"),
         Provider = str_remove(Provider, "\\(.*\\)"),
         DateAdded = floor_date(as.Date(DateAdded, origin = "1899-12-30"), unit = "day"),
         ReferDate = floor_date(as.Date(ReferDate, origin = "1899-12-30"), unit = "day"))


svcs_bos_and_yo <- rbind(svcs_bos, svcs_yo) %>% unique()

not_on_demo <- svcs_bos %>% anti_join(svcs_demo) %>%
  filter(str_starts(Provider, "zz", negate = TRUE),
         ymd(DateAdded) <= mdy("10302020"))

svcs_bos %>% filter(PersonalID == 215225 & ServiceCode =="FT-1000") ==
svcs_demo %>% filter(PersonalID == 215225 & ServiceCode == "FT-1000")
