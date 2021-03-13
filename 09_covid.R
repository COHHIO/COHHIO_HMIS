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

# library(ggplot2) 
library(tidyverse)
library(lubridate)
library(HMIS)
library(here)
library(sf)
library(urbnmapr)

if (!exists("Enrollment"))
  load("images/COHHIOHMIS.RData")
if(!exists("dq_main"))
  load("images/Data_Quality.RData")
if (!exists("tay")) {
  load("images/cohorts.RData")
  rlang::env_binding_lock(environment(), ls())
}

most_recent_entries <- co_clients_served %>%
  left_join(Enrollment[c("EnrollmentID", "CountyServed")], by = "EnrollmentID") %>%
  group_by(PersonalID) %>%
  slice_max(EntryDate) %>%
  slice_max(EnrollmentID)

counties <- get_urbn_map("counties", sf = TRUE)

counties <- st_transform(counties, "+init=epsg:3857")

counties <- counties %>%
  mutate(county_name = str_remove(county_name, " County"))

data(county.map)

oh_counties <- county.map %>% filter(STATE == 39) %>% select(NAME, region)

# Pinpointing where Vaccines are Wanted -----------------------------------

vaccine_distribution_county <- covid19 %>%
  filter(ConsentToVaccine == "Yes (HUD)") %>%
  rename("county_name" = CountyServed) %>%
  count(county_name) %>%
  right_join(counties, by = "county_name") %>%
  mutate(n = replace_na(n, 0)) %>%
  select(county_fips, "value" = n) %>% 
  unique()

consent_yn <- covid19 %>%
  filter(!is.na(ConsentToVaccine)) %>%
  count(ConsentToVaccine)

# 
# oh_507 <- readOGR(dsn = here("Ohio/OH_507"),
#                    layer="OH_507",
#                    verbose = FALSE)

# ohio_counties <- readOGR(dsn = here("Ohio/Counties"),
#                          layer = "REFER_COUNTY")


  
household_data <- left_join(counties, countydata, by = "county_fips") %>%
  filter(state_fips == 39) %>%
  left_join(vaccine_distribution_county %>%
            # mutate(region = as.character(region)) %>%
            select(county_fips, value), by = "county_fips") 


household_data %>%
  ggplot() +
  scale_fill_viridis_c(super = ScaleContinuous, direction = -1) +
  geom_sf(aes(fill = value)) +
  geom_sf_text(aes(label = str_remove(county_name, " County")), 
               check_overlap = TRUE,
               size = 3) +
  labs(fill = "Would Consent") +
  theme_void()

# Connecting Clients to their 2nd Doses -----------------------------------

vaccine_needs_second_dose <- dose_counts %>%
  filter(Doses == 1) %>%
  left_join(doses, by = "PersonalID") %>%
  left_join(most_recent_entries, by = "PersonalID") %>%
  filter(COVID19VaccineManufacturer != "Johnson & Johnson") %>%
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
    DaysUntilNextDose = ymd(NextDoseNeededDate) - today(),
    VeteranStatus = case_when(
      VeteranStatus == 0 ~ "No",
      VeteranStatus == 1 ~ "Yes",
      TRUE ~ "Unknown"
    ),
    AgeAtEntry = case_when(
      AgeAtEntry < 12 ~ "0-11",
      AgeAtEntry < 16 ~ "12-15",
      AgeAtEntry < 25 ~ "16-24",
      AgeAtEntry < 65 ~ "25-59",
      AgeAtEntry < 75 ~ "60-74",
      AgeAtEntry < 85 ~ "75-84",
      AgeAtEntry < 120 ~ "85+",
      TRUE ~ "Unknown"
    ),
    HowSoon = case_when(
      DaysUntilNextDose < 0 ~ "Overdue",
      DaysUntilNextDose > 7 ~ "Next Week",
      DaysUntilNextDose > 3 ~ "7 days",
      DaysUntilNextDose >= 0 ~ "3 days"
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
    DaysUntilNextDose,
    CurrentLocation
  )

 # Concerns ----------------------------------------------------------------
# 
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
#   scale = c(3, .2)
# )


# Who needs followup? -----------------------------------------------------


served_since_02052021 <- co_clients_served %>%
  filter(served_between(., hc_bos_start_vaccine_data, meta_HUDCSV_Export_End)) %>%
  count(ProjectName) %>%
  rename("totalserved" = n)

exited <- missing_vaccine_exited %>%
  count(ProjectName) %>%
  rename("missingexited" = n)

current <- missing_vaccine_current %>%
  count(ProjectName) %>%
  rename("missingcurrent" = n)

all <- full_join(served_since_02052021, current, by = "ProjectName") %>%
  full_join(exited, by = "ProjectName") %>%
  mutate(missingcurrent = replace_na(missingcurrent, 0),
         missingexited = replace_na(missingexited, 0),
         allmissing = missingcurrent + missingexited,
         percentmissing = allmissing/totalserved) 

write_csv(all, "random_data/percentmissing.csv")

# to update this, I'm saving this to DB > HMIS > Covid-19 Data Analysis for EM

# cleanup -----------------------------------------------------------------

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
