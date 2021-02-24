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

library(ggplot2) 
library(tidyverse)
library(lubridate)
# library(wordcloud)
# library(leaflet)
# library(RColorBrewer)
library(plotly)
# library(tm)
# library(urbnmapr)
library(choroplethr)
library(choroplethrMaps)

if (!exists("Enrollment"))
  load("images/COHHIOHMIS.RData")
if (!exists("tay")) {
  load("images/cohorts.RData")
  rlang::env_binding_lock(environment(), ls())
}

most_recent_entries <- co_clients_served %>%
  left_join(Enrollment[c("EnrollmentID", "CountyServed")], by = "EnrollmentID") %>%
  group_by(PersonalID) %>%
  slice_max(EntryDate) %>%
  slice_max(EnrollmentID)

data(county.map)

counties <- county.map %>% filter(STATE == 39) %>% select(NAME, region)

# Pinpointing where Vaccines are Wanted -----------------------------------

vaccine_distribution_county <- covid19 %>%
  filter(ConsentToVaccine == "Yes (HUD)") %>%
  rename("NAME" = CountyServed) %>%
  count(NAME) %>%
  right_join(counties, by = "NAME") %>%
  mutate(n = replace_na(n, 0)) %>%
  select(region, "value" = n) %>% 
  unique()

county_choropleth(vaccine_distribution_county,
                  state_zoom = "ohio",
                  num_colors = 1,
                  title = "Would Consent to COVID-19 Vaccine",
                  legend = "# of Adults and Children")


vaccine_distribution_provider <- covid19 %>%
  filter(ConsentToVaccine == "Yes (HUD)") %>%
  select(PersonalID, CountyServed) %>%
  left_join(most_recent_entries[c("PersonalID",
                                  "EntryDate",
                                  "ExitDate",
                                  "ProjectName",
                                  "Destination")], by = "PersonalID") %>%
  mutate(
    CurrentLocation = case_when(
      is.na(EntryDate) ~ "Not currently enrolled in any homeless dedicated project.",
      today() >= ymd(EntryDate) &
        (ymd(ExitDate) > today()) |
        is.na(ExitDate) ~ paste(
          "Currently in",
          ProjectName),
      ymd(ExitDate) <= today() ~ paste(
        "Exited",
        ProjectName,
        "on",
        ExitDate,
        "to",
        living_situation(Destination))
    )
  ) %>%
  count(CurrentLocation)



# Connecting Clients to their 2nd Doses -----------------------------------

dose_counts <- doses %>%
  count(PersonalID) %>%
  dplyr::rename("DoseCount" = n)

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
      today() >= ymd(EntryDate) &
        (ymd(ExitDate) > today()) | is.na(ExitDate) ~ 
        paste(
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
    ),
    DaysUntilNextDose = difftime(ymd(NextDoseNeededDate), today()),
    VeteranStatus = case_when(
      VeteranStatus == 0 ~ "No",
      VeteranStatus == 1 ~ "Yes",
      TRUE ~ "Unknown"
    ),
    AgeAtEntry = case_when(
      AgeAtEntry < 12 ~ "0-11",
      AgeAtEntry < 16 ~ "12-15",
      AgeAtEntry < 25 ~ "16-24",
      AgeAtEntry < 65 ~ "25-64",
      AgeAtEntry < 75 ~ "65-74",
      AgeAtEntry < 85 ~ "75-84",
      AgeAtEntry < 120 ~ "85+",
      TRUE ~ "Unknown"
    ),
    HowSoon = case_when(
      DaysUntilNextDose < 0 ~ "Overdue",
      DaysUntilNextDose < 4 ~ "3 days",
      DaysUntilNextDose < 8 ~ "7 days",
      DaysUntilNextDose < 29 ~ "Next week"
    )
  ) %>%
  select(
    PersonalID,
    HouseholdID,
    CountyServed,
    COVID19VaccineManufacturer,
    AgeAtEntry,
    VeteranStatus,
    NextDoseNeededDate,
    HowSoon,
    CurrentLocation
  )

 # Concerns ----------------------------------------------------------------

# concerns <- covid19 %>%
#   select(PersonalID, ConsentToVaccine, VaccineConcerns) %>%
#   filter(ConsentToVaccine != "Yes (HUD)" & !is.na(VaccineConcerns))
# 
# text <- concerns$VaccineConcerns
# 
# text <- tolower(text)
# 
# text <- str_replace(text, "side affects", "side effects")
# 
# text <-
#   removeWords(
#     text,
#     c(
#       "am",
#       "is",
#       "are",
#       "was",
#       "been",
#       "did",
#       "want",
#       "will",
#       "would",
#       "doesnt",
#       "dont",
#       "have",
#       "has",
#       "hasn't",
#       "hasnt",
#       "does",
#       "doesn't",
#       "don't",
#       "the",
#       "not",
#       "and",
#       "vaccine",
#       "vaccines",
#       "about",
#       "into",
#       "for",
#       "its",
#       "it's",
#       "that"
#     )
#   )
# 
# cloud <- Corpus(VectorSource(text))
# 
# cloud <- tm_map(cloud, content_transformer(tolower)) %>%
#   tm_map(removeNumbers) %>%
#   tm_map(removePunctuation) %>%
#   tm_map(stripWhitespace)
# 
# vaccine_concerns_cloud <- wordcloud(
#   cloud,
#   colors = brewer.pal(8, "Dark2"),
#   random.order = FALSE,
#   random.color = FALSE,
#   scale = c(4, .2),
#   fixed.asp = TRUE
# )

rm(list = ls()[!(
  ls() %in% c(
    "vaccine_concerns_cloud",
    "vaccine_needs_second_dose",
    "vaccine_distribution_provider"
  )
)])

save(list = ls(),
     file = "images/COVID_vaccine.RData",
     compress = FALSE)
