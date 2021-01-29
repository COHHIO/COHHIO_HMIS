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

load("images/COHHIOHMIS.RData")
load("images/cohorts.RData")


# Pinpointing where Vaccines are Wanted -----------------------------------

# add a way to more closely pinpoint where these clients are
distribution <- covid19 %>%
  filter(ConsentToVaccine == "Yes (HUD)") %>%
  select(PersonalID, CountyServed)

# Connecting Clients to their 2nd Doses -----------------------------------

dose_counts <- doses %>%
  group_by(PersonalID) %>%
  summarise(DoseCount = n()) %>%
  ungroup()

one_dose <- dose_counts %>%
  filter(DoseCount == 1) %>%
  left_join(doses, by = "PersonalID") %>%
  left_join(co_clients_served, by = "PersonalID") %>%
  mutate(
    NextDoseNeededDate = case_when(
      COVID19VaccineManufacturer == "Moderna" ~ ymd(COVID19DoseDate) + days(28),
      COVID19VaccineManufacturer == "Pfizer" ~ ymd(COVID19DoseDate) + days(21)
    ),
    CurrentLocation = case_when(
      is.na(EntryDate) ~ if_else(
        is.na(VaccineContactInfo),
        "No contact info and not currently enrolled in any project.",
        VaccineContactInfo
      ),
      (((ProjectType %in% ph_project_types &
         today() >= ymd(MoveInDateAdjust)) | 
        (ProjectType %in% lh_project_types &
           today() >= ymd(EntryDate))) &
      ymd(ExitDate) > today()) |
        is.na(ExitDate) ~ paste(
          "Currently in",
          ProjectName,
          "Contact Info:",
          VaccineContactInfo
        ),
      ExitDate <= today() ~ paste(
        "Exited",
        ProjectName,
        "on",
        ExitDate,
        "to",
        living_situation(Destination),
        "Contact info:",
        VaccineContactInfo
      )
    )
  ) %>%
  select(PersonalID,
         COVID19VaccineManufacturer,
         AgeAtEntry,
         VeteranStatus,
         NextDoseNeededDate,
         CurrentLocation)



# Data Quality Issues -----------------------------------------------------

multiple_doses <- dose_counts %>%
  filter(DoseCount > 1) %>%
  left_join(co_clients_served, by = "PersonalID")







