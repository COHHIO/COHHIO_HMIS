# MIT License

# Copyright (c) 2020 gwenbeebe

library(tidyverse)
library(lubridate)
library(dplyr)
library(janitor)
library(scales)

load("images/cohorts.RData")

ReportEnd <- mdy("12312020")

##  set up definitions and initial dataframe, adjust as needed

return_project_types <- c(1, 2, 3, 4, 8, 9, 10, 13) # pg 17 SPM specs

df_for_returns <- co_clients_served %>%
  filter(ProjectType %in% return_project_types &
           ProjectID != 1695) %>%
  select(HouseholdID,
         PersonalID,
         EnrollmentID,
         ProjectID,
         EntryDate,
         ExitAdjust,
         ExitDate,
         ProjectType,
         Destination)

# Universe to Compare -----------------------------------------------------

##  get permanent exit counts for project types listed in SPMs logic
## grabbing first permanent-destination exit in the past 2 years

scan_forward_from <- df_for_returns %>%
  filter(Destination %in% perm_destinations &
           ymd(ExitAdjust) >= ReportEnd - years(3) &
           ymd(ExitAdjust) <= ReportEnd - years(2) &
           !ProjectID %in% c(mahoning_projects)) %>%
  mutate(
    orderby = case_when(
      ProjectType == 1 ~ 1,
      ProjectType == 2 ~ 2,
      ProjectType == 4 ~ 3,
      ProjectType == 8 ~ 4,
      ProjectType == 3 ~ 5,
      ProjectType == 9 ~ 6,
      ProjectType == 10 ~ 7,
      ProjectType == 13 ~ 8
    ) # in case someone exits to perm housing on the same date
  ) %>% 
  group_by(PersonalID) %>%
  arrange(ymd(ExitAdjust), orderby) %>%
  slice(n = 1) %>% 
  ungroup() %>%
  select("ScanAheadHHID" = HouseholdID,
         PersonalID,
         "ScanAheadEEID" = EnrollmentID,
         "ScanAheadProjectID" = ProjectID,
         "ScanAheadEntry" = EntryDate,
         "ScanAheadExitAdjust" = ExitAdjust,
         "ScanAheadExit" = ExitDate,
         "ScanAheadPTC" = ProjectType)

# column b in SPM specs, page 17

column_b <- scan_forward_from %>%
  group_by(ScanAheadPTC) %>%
  summarise(ExitedToPerm = n()) %>%
  select("ProjectType" = ScanAheadPTC, ExitedToPerm) %>%
  adorn_totals()

# Recurrence --------------------------------------------------------------

every_recurrence <- df_for_returns %>%
  left_join(scan_forward_from, by = "PersonalID") %>%
  select(starts_with("ScanAhead"), everything()) %>%
  filter(!is.na(ScanAheadHHID) &
           difftime(ymd(EntryDate), ymd(ScanAheadExitAdjust), units = "days") >= 14 &
           difftime(ymd(EntryDate), ymd(ScanAheadExitAdjust), units = "days") <= 730) %>%
  mutate(Gap = as.numeric(difftime(ymd(EntryDate), ymd(ScanAheadExitAdjust), units = "days")),
         RecurrenceBucket = case_when(
           between(Gap, 14, 90) ~ "SixMo",
           between(Gap, 91, 365) ~ "OneYr",
           between(Gap, 366, 730) ~ "TwoYr"
         ))
  
# the specs say a client can only be counted in the 6mo, 1yr, or 2yr column
# so I'm taking the first recurrence

counted_recurrences <- every_recurrence %>%
  group_by(PersonalID) %>%
  arrange(Gap) %>%
  slice(n = 1) %>%
  ungroup()

# Trying to Code to Specs -------------------------------------------------

to_specs <- counted_recurrences %>%
  group_by(ScanAheadPTC, RecurrenceBucket) %>%
  summarise(Recurrences = n()) %>%
  ungroup()  %>%
  mutate(ProjectType = as.character(ScanAheadPTC)) %>%
  select(-ScanAheadPTC) %>%
  left_join(column_b, by = "ProjectType")  %>%
  pivot_wider(names_from = RecurrenceBucket,
              values_from = Recurrences,
              values_fill = 0) %>%
  adorn_totals() %>%
  mutate(
    SixMoRecurrence = percent(SixMo / ExitedToPerm),
    OneYrRecurrence = percent(OneYr / ExitedToPerm),
    TwoYrRecurrence = percent(TwoYr / ExitedToPerm),
    TotalReturning = SixMo + OneYr + TwoYr,
    TotalRecurrence = percent(TotalReturning / ExitedToPerm),
    ProjectType = case_when(
      ProjectType == 9 ~ "PH - Housing Only",
      ProjectType %in% c(1:8, 13) ~ project_type(ProjectType),
      TRUE ~ "All Project Types"
    )
  ) %>% 
  select(ProjectType,
         ExitedToPerm,
         SixMo,
         SixMoRecurrence,
         OneYr,
         OneYrRecurrence,
         TwoYr,
         TwoYrRecurrence,
         TotalReturning,
         TotalRecurrence)


# Flagging Recurrers ------------------------------------------------------

recurring_clients <- counted_recurrences %>%
  select(PersonalID,
         "HouseholdID" = ScanAheadHHID,
         "RecurrenceProjectType" = ProjectType,
         RecurrenceBucket)

co_clients_served_test <- co_clients_served %>%
  left_join(recurring_clients, by = c("PersonalID", "HouseholdID"))



