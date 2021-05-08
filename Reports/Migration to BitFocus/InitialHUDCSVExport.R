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

library(here)
library(lubridate)
library(readxl)
library(HMIS)
library(tidyverse)

source(here("00_dates.R"))

# Get Pretty Names --------------------------------------------------------

project_names <- read_xlsx(
  here("data_to_Clarity/RMisc2.xlsx"),
  sheet = 3,
  range = cell_cols("A:D"))

# Overwrite Project Names -------------------------------------------------

Project <- 
  read_csv(here("data_to_Clarity/Project.csv"),
           col_types = "nnccDDnnnnnnnnTTcTn") 

Project_improved <- 
  Project %>%
  left_join(project_names, by = "ProjectID") %>%
  dplyr::select(ProjectID,
         OrganizationID,
         "ProjectName" = ProjectName.y,
         "ProjectCommonName" = ProjectAKA,
         OperatingStartDate:ExportID)

write_csv(Project_improved, here("data_to_Clarity/Project.csv"), 
          append = FALSE,
          na = "")


# Overwrite Organization Names --------------------------------------------

Organization <- 
  read_csv(here("data_to_Clarity/Organization.csv"),
           col_types = "ncncTTnTn")
  
Organization_improved <- 
  Organization %>%
  left_join(project_names, by = c("OrganizationID" = "ProjectID")) %>%
  dplyr::select(OrganizationID,
                "OrganizationName" = ProjectName,
                VictimServicesProvider:ExportID)

write_csv(Organization_improved, here("data_to_Clarity/Organization.csv"), 
          append = FALSE,
          na = "")

# Move-In Date Madness ----------------------------------------------------

Enrollment <-
  read_csv(here("data_to_Clarity/Enrollment.csv"),
           col_types =
             "nnnDcnnnlnDnnnDDDnnnncccnnDnnnncnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnTTnTn")

Exit <-
  read_csv(here("data_to_Clarity/Exit.csv"),
           col_types = "nnnDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTnTn")

small_exit <- Exit %>% dplyr::select(EnrollmentID, 
                              ExitDate)

EntryExits <- left_join(Enrollment, small_exit, by = "EnrollmentID") %>%
  mutate(ExitAdjust = if_else(is.na(ExitDate) |
                                ExitDate > today(),
                              today(), ExitDate))

small_project <- Project %>%
  dplyr::select(ProjectID, ProjectType, ProjectName) 

HHMoveIn <- EntryExits %>%
  left_join(small_project, by = "ProjectID") %>%
  filter(ProjectType %in% c(3, 9, 13)) %>%
  mutate(
    AssumedMoveIn = if_else(
      ymd(EntryDate) < hc_psh_started_collecting_move_in_date &
        ProjectType %in% c(3, 9),
      1,
      0
    ),
    ValidMoveIn = case_when(
      AssumedMoveIn == 1 ~ EntryDate,
      AssumedMoveIn == 0 &
        ProjectType %in% c(3, 9) &
        ymd(EntryDate) <= ymd(MoveInDate) &
        ymd(ExitAdjust) > ymd(MoveInDate) ~ MoveInDate,
      # the Move-In Dates must fall between the Entry and ExitAdjust to be
      # considered valid and for PSH the hmid cannot = ExitDate
      ymd(MoveInDate) <= ymd(ExitAdjust) &
        ymd(MoveInDate) >= ymd(EntryDate) &
        ProjectType == 13 ~ MoveInDate
    )
  ) %>% 
  filter(!is.na(ValidMoveIn)) %>%
  group_by(HouseholdID) %>%
  mutate(HHMoveIn = min(ValidMoveIn)) %>%
  ungroup() %>%
  dplyr::select(HouseholdID, HHMoveIn) %>%
  unique()

HHEntry <- EntryExits %>%
  left_join(small_project, by = "ProjectID") %>%
  group_by(HouseholdID) %>%
  mutate(FirstEntry = min(EntryDate)) %>%
  ungroup() %>%
  dplyr::select(HouseholdID, "HHEntry" = FirstEntry) %>%
  unique() %>%
  left_join(HHMoveIn, by = "HouseholdID")

Enrollment_improved <- EntryExits %>%
  left_join(HHEntry, by = "HouseholdID") %>%
  mutate(
    MoveInDateAdjust = if_else(!is.na(HHMoveIn) &
                                 ymd(HHMoveIn) <= ymd(ExitAdjust),
                               if_else(ymd(EntryDate) <= ymd(HHMoveIn),
                                       HHMoveIn, EntryDate),
                               NA_real_)
  ) %>%
  dplyr::select(-ExitDate, -ExitAdjust, -HHEntry, -HHMoveIn, -MoveInDate) %>%
  rename("MoveInDate" = MoveInDateAdjust) %>%
  relocate(MoveInDate, .after = DateOfEngagement)

write_csv(Enrollment_improved, 
          here("data_to_Clarity/Enrollment.csv"), 
          append = FALSE,
          na = "")

# Unquoting the other files -----------------------------------------------

remove_quotes <- function(file) {
  cat(file, sep = "\n")
  x <- read_csv(here(paste0("data_to_Clarity/", file, ".csv")),
                col_types = cols()) 

  write_csv(x,
            here(paste0("data_to_Clarity/", file, ".csv")),
            append = FALSE,
            na = "")
}

data.frame(
  file = c(
    "Affiliation",
    "Assessment",
    "AssessmentQuestions",
    "AssessmentResults",
    "Client",
    "CurrentLivingSituation",
    "Disabilities",
    "EmploymentEducation",
    "Enrollment",
    "EnrollmentCoC",
    "Event",
    "Exit",
    "Export",
    "Funder",
    "HealthAndDV",
    "IncomeBenefits",
    "Inventory",
    "ProjectCoC",
    "Services",
    "User"
  )
) %>%
  purrr::pwalk(remove_quotes)





