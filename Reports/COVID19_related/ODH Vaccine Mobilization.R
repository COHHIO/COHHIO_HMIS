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
library(janitor)
library(gt)

load(here("images/COHHIOHMIS.RData"))
load(here("images/cohorts.RData"))

complete <- doses %>%
  get_dupes(PersonalID) %>%
  select(PersonalID) %>% 
  unique() %>%
  filter(!PersonalID %in% c(199596, 244414, 276684, 278912)) %>% # bad data
  mutate(AlreadyVaccinated = "Yes")

current <- Enrollment %>%
  filter(AgeAtEntry >= 16 & 
           ProjectType %in% c(lh_project_types) &
           is.na(ExitDate)) %>%
  select(CountyServed, PersonalID, ProjectName) %>%
  left_join(covid19[c("PersonalID", "ConsentToVaccine", "VaccineConcerns")], 
            by = "PersonalID") %>%
  left_join(complete, by = "PersonalID") %>%
  mutate(AlreadyVaccinated = if_else(is.na(AlreadyVaccinated), 
                                     "Not acc. to HMIS", 
                                     AlreadyVaccinated))

total_sheltered_by_county <- current %>%
  count(CountyServed) %>%
  rename("TotalSheltered" = n) %>%
  arrange(desc(TotalSheltered))

total_sheltered_by_project <- current %>%
  count(ProjectName) %>%
  rename("TotalSheltered" = n) %>%
  arrange(desc(TotalSheltered))

would_consent <- current %>%
  mutate(
    ConsentToVaccine = if_else(is.na(ConsentToVaccine), 
                               "Data not collected (HUD)", 
                               ConsentToVaccine),
    WouldConsent = case_when(
      AlreadyVaccinated == "Yes" ~ "Already fully vaccinated",
      ConsentToVaccine == "Yes (HUD)" ~ "Answered Yes to Consent question",
      !ConsentToVaccine %in% c("Yes (HUD)", "No (HUD)") ~ "Consent Unknown",
      ConsentToVaccine == "No (HUD)" ~ "Answered No to Consent question")) 

would_consent_by_county <- would_consent %>%
  count(CountyServed, WouldConsent) %>%
  pivot_wider(names_from = WouldConsent, values_from = n, values_fill = 0)

would_consent_by_project <- would_consent %>%
  count(ProjectName, WouldConsent) %>%
  pivot_wider(names_from = WouldConsent, values_from = n, values_fill = 0)

total_by_county <- total_sheltered_by_county %>%
  left_join(would_consent_by_county, by = "CountyServed")

total_by_project <- total_sheltered_by_project %>%
  left_join(would_consent_by_project, by = "ProjectName")

write_xlsx(
  x = list("By County" = total_by_county,
           "By Project" = total_by_project),
  "random_data/current_counts_vaccine.xlsx"
)
