library(tidyverse)
library(lubridate)
library(here)

load(here("images/COHHIOHMIS.RData"))

x <- Enrollment %>%
  select(PersonalID,
         EntryDate,
         ExitDate,
         CountyServed,
         ProjectID,
         ProjectType) %>%
  filter(CountyServed == "Delaware" &
           EntryDate < mdy("01012020") &
           (is.na(ExitDate) | ExitDate >= mdy("01012019")) &
           ProjectType != 12) %>%
  select(PersonalID) %>%
  unique()

nrow(x)

y <- Enrollment %>%
  select(PersonalID,
         HouseholdID,
         EntryDate,
         ExitDate,
         CountyServed,
         ProjectID,
         ProjectType) %>%
  filter(
    CountyServed == "Delaware" &
      EntryDate < mdy("01012020") &
      (is.na(ExitDate) | ExitDate >= mdy("01012019")) &
      ProjectType != 12
  ) %>%
  select(HouseholdID) %>%
  unique()

nrow(y)

un <- Enrollment %>%
  select(HouseholdID,
         PersonalID,
         EntryDate,
         ExitDate,
         CountyServed,
         ProjectID,
         ProjectType) %>%
  filter(CountyServed == "Delaware" &
           EntryDate < mdy("01012020") &
           (is.na(ExitDate) | ExitDate >= mdy("01012019")) &
           ProjectType == 4) %>%
  select(PersonalID) %>%
  unique()

nrow(un)

outcomes_unsheltered <-
  Enrollment %>%
  select(HouseholdID,
         PersonalID,
         EntryDate,
         ExitDate,
         CountyServed,
         ProjectType,
         Destination) %>%
  filter(
    CountyServed == "Delaware" &
      EntryDate < mdy("01012019") &
      ExitDate >= mdy("01012018") &
      ProjectType == 4
  ) %>%
  mutate(
    DestinationGroup = case_when(
      Destination %in% c(1, 2, 12, 13, 14, 16, 18, 27) ~ "Temporary",
      Destination %in% c(3, 10, 11, 19:23, 28, 31) ~ "Permanent",
      Destination %in% c(4:7, 15, 25:27, 29) ~ "Institutional",
      Destination %in% c(8, 9, 17, 24, 30, 99) ~ "Other",
      is.na(Destination) ~ "Still in Program"
    )
  ) %>%
  select(PersonalID, DestinationGroup) %>%
  unique() %>%
  group_by(DestinationGroup) %>%
  summarise(n())

children <- Enrollment %>%
  select(HouseholdID,
         PersonalID,
         EntryDate,
         ExitDate,
         CountyServed,
         ProjectType,
         AgeAtEntry) %>%
  filter(
    CountyServed == "Delaware" &
      EntryDate < mdy("01012019") &
      ExitDate >= mdy("01012018") &
      ProjectType == 4
  ) %>%
  mutate(agerange = case_when(
    AgeAtEntry < 2 ~ "infant",
    between(AgeAtEntry, 2, 5) ~ "preschooler",
    between(AgeAtEntry, 6, 17) ~ "school age",
    between(AgeAtEntry, 18, 60) ~ "adult",
    AgeAtEntry > 60 ~ "very wise!"
  )) %>%
  select(PersonalID, agerange) %>%
  unique() %>%
  group_by(agerange) %>%
  summarise(n())


