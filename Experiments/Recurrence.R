# MIT License

# Copyright (c) 2020 gwenbeebe

library(tidyverse)
library(lubridate)
library(dplyr)

load("images/cohorts.RData")

ReportEnd <- mdy("09302020")

##  set up definitions and initial dataframe, adjust as needed
all_program_types <- c(1:4, 6:14)   

return_project_types <- c(2, 3, 9, 10, 1, 4, 13, 8, 14)

housing_program_types <- c(2, 3, 9, 10, 13)

ph_program_types <- c(3, 9, 10, 13)

df_for_returns <- co_clients_served %>%
  filter(ProjectType %in% return_project_types) %>%
  select(PersonalID, EnrollmentID, EntryDate, ExitDate, ProjectType, Destination) %>%
  mutate(two_weeks_after_exit = if_else(!is.na(ExitDate), ExitDate + ddays(14), NULL))


##  find all exits from TH or PH programs
housing_exits <- df_for_returns %>%
  filter(ProjectType %in% housing_program_types,
         !is.na(ExitDate)) %>%
  setNames(paste("Exited_PHTH", colnames(df_for_returns), sep = "_"))

##  find all entries to PH programs
ph_enrollments <- df_for_returns %>%
  filter(ProjectType %in% ph_program_types) %>%
  setNames(paste("Entered_PH", colnames(df_for_returns), sep = "_")) %>%
  select(-Entered_PH_two_weeks_after_exit)

##  identify all PH enrollments within 14 days of a TH or PH exit 
## (wouldn't we also want to get exits from ESs and SHs?)
excluded_PH_entries <- ph_enrollments %>%
  left_join(housing_exits, by = c("Entered_PH_PersonalID" = "Exited_PHTH_PersonalID")) %>%
  filter(Entered_PH_EntryDate >= Exited_PHTH_ExitDate &
           Entered_PH_EntryDate <= Exited_PHTH_two_weeks_after_exit) %>%
  select(Entered_PH_EnrollmentID) %>%
  distinct()

##  remove enrollments identified above from enrollments used to flag returns
returning_entries <- df_for_returns %>%
  anti_join(excluded_PH_entries, by = c("EnrollmentID" = "Entered_PH_EnrollmentID")) %>%
  setNames(paste("Returning_Entries", colnames(df_for_returns), sep = "_"))

##  get all enrollments with permanent exits
permanent_exits <- df_for_returns %>%
  filter(Destination %in% c(perm_destinations)) %>%
  setNames(paste("Perm_Exits", colnames(df_for_returns), sep = "_")) %>%
  mutate(two_years_after_exit = Perm_Exits_ExitDate + dyears(2))

## create flag for all enrollments with a qualifying returning entry
return_flags <- permanent_exits %>%
  left_join(returning_entries, by = c("Perm_Exits_PersonalID" = "Returning_Entries_PersonalID")) %>%
  group_by(Perm_Exits_EnrollmentID) %>%
  mutate(return_flag = 
           if_else(
             (Returning_Entries_ProjectType %in% housing_program_types &
                Returning_Entries_EntryDate >= Perm_Exits_two_weeks_after_exit &
                Returning_Entries_EntryDate <= two_years_after_exit) |
               (!Returning_Entries_ProjectType %in% housing_program_types &
                  Returning_Entries_EntryDate >= Perm_Exits_ExitDate &
                  Returning_Entries_EntryDate <= two_years_after_exit),
             1, 0
           ),
         return_flag = if_else(is.na(max(return_flag)), 0, max(return_flag))) %>%
  ungroup() %>%
  select(Perm_Exits_EnrollmentID, return_flag) %>%
  distinct()

rm(list = ls()[!(ls() %in% c("return_flags"))])
save.image("images/return_flags.RData")

