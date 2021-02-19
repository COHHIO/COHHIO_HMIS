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
library(wordcloud)
library(RColorBrewer)
library(tm)

if (!exists("Enrollment")) load("images/COHHIOHMIS.RData")
if (!exists("tay")) {
  load("images/cohorts.RData")
  rlang::env_binding_lock(environment(), ls())
}

# Pinpointing where Vaccines are Wanted -----------------------------------

# add a way to more closely pinpoint where these clients are
vaccine_distribution <- covid19 %>%
  filter(ConsentToVaccine == "Yes (HUD)") %>%
  select(PersonalID, CountyServed)

# Connecting Clients to their 2nd Doses -----------------------------------

dose_counts <- doses %>%
  count(PersonalID) %>%
  dplyr::rename("DoseCount" = n)

most_recent_entries <- co_clients_served %>%
  group_by(PersonalID) %>%
  slice_max(EntryDate) %>%
  slice_max(EnrollmentID)

vaccine_needs_second_dose <- dose_counts %>%
  filter(DoseCount == 1) %>%
  left_join(doses, by = "PersonalID") %>%
  left_join(most_recent_entries, by = "PersonalID") %>%
  mutate(
    NextDoseNeededDate = case_when(
      COVID19VaccineManufacturer == "Moderna" ~ 
        ymd(COVID19DoseDate) + days(28),
      COVID19VaccineManufacturer == "Pfizer" ~ 
        ymd(COVID19DoseDate) + days(21),
      str_starts(COVID19VaccineManufacturer, "Client doesn't know") == TRUE ~ 
        ymd(COVID19DoseDate) + days(28)
    ),
    CurrentLocation = case_when(
      is.na(EntryDate) ~ if_else(
        is.na(VaccineContactInfo),
        "No contact info and not currently enrolled in any project.",
        VaccineContactInfo
      ),
      (((
        ProjectType %in% ph_project_types &
          today() >= ymd(MoveInDateAdjust)
      ) |
        (ProjectType %in% lh_project_types &
           today() >= ymd(EntryDate))
      ) &
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
  select(
    PersonalID,
    COVID19VaccineManufacturer,
    AgeAtEntry,
    VeteranStatus,
    NextDoseNeededDate,
    CurrentLocation
  )

# Concerns ----------------------------------------------------------------

concerns <- covid19 %>%
  select(PersonalID, ConsentToVaccine, VaccineConcerns) %>%
  filter(ConsentToVaccine != "Yes (HUD)" & !is.na(VaccineConcerns))

text <- concerns$VaccineConcerns

text <- tolower(text)

text <- str_replace(text, "side affects", "side effects")

text <-
  removeWords(
    text,
    c(
      "am",
      "is",
      "are",
      "was",
      "been",
      "did",
      "want",
      "will",
      "would",
      "doesnt",
      "dont",
      "have",
      "has",
      "hasn't",
      "hasnt",
      "does",
      "doesn't",
      "don't",
      "the",
      "not",
      "and",
      "vaccine",
      "vaccines",
      "about",
      "into",
      "for",
      "its",
      "it's",
      "that"
    )
  )

cloud <- Corpus(VectorSource(text))

cloud <- tm_map(cloud, content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(stripWhitespace)

vaccine_concerns_cloud <- wordcloud(
  cloud,
  colors = brewer.pal(8, "Dark2"),
  random.order = FALSE,
  random.color = FALSE,
  scale = c(4, .2),
  fixed.asp = TRUE
)

rm(list = ls()[!(ls() %in% c(
  "vaccine_concerns_cloud",
  "vaccine_needs_second_dose",
  "vaccine_distribution"
))])

save(list = ls(),
     file = "images/COVID_vaccine.RData",
     compress = FALSE)
