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

write_csv(Project_improved, here("data_to_Clarity/Project.csv"), append = FALSE)


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




