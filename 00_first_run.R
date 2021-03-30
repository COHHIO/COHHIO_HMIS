#COHHIO_HMIS
#Copyright (C) 2021  Coalition on Homelessness and Housing in Ohio (COHHIO)

#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU Affero General Public License as published
#by the Free Software Foundation, either version 3 of the License, or
#any later version.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#GNU Affero General Public License for more details at 
#<https://www.gnu.org/licenses/>.

# Install packages for COHHIO_HMIS
if (!require(pacman))
  install.packages("pacman")

pacman::p_load(char = c(
  "cli",
  "feather",
  "tidyverse",
  "lubridate",
  "readxl",
  "rlang",
  "scales",
  "janitor",
  "devtools",
  "",
  "sf",
  "choroplethrMaps",
  "plotly"
))

remotes::install_github("UrbanInstitute/urbnmapr",
                        dependencies = "Imports",
                        upgrade = "always")

# remotes::install_github("jimhester/archive",
#                         dependencies = "Imports",
#                         upgrade = "always")

# Install packages for Rminor
if (!require(pacman))
  install.packages("pacman")
pacman::p_load(
  char = c(
    "tidyverse",
    "lubridate",
    "scales",
    "shinydashboard",
    "shiny",
    "shinyWidgets",
    "plotly",
    "zoo",
    "DT"
  )
)

# Install packages for Rminor_elevated
if (!require(pacman))
  install.packages("pacman")
pacman::p_load(
  char = c(
    "tidyverse",
    "lubridate",
    "scales",
    "shinydashboard",
    "shiny",
    "shinyWidgets",
    "plotly",
    "zoo",
    "DT",
    "writexl",
    "viridis",
    "glue"
  )
)

# Install HMIS package
devtools::install_github("COHHIO/HMIS", 
                         dependencies = "Imports", 
                         upgrade = "always")

# Create needed directories

if(!dir.exists("data")) dir.create("data")
if(!dir.exists("images")) dir.create("images")
if(!dir.exists("random_data")) dir.create("random_data")
# if(!dir.exists("SPM_data_BoS")) dir.create("SPM_data_BoS")
# if(!dir.exists("SPM_data_YO")) dir.create("SPM_data_YO")
# if(!dir.exists("SPM_data_YO/Current")) dir.create("SPM_data_YO/Current")
# if(!dir.exists("SPM_data_YO/Prior")) dir.create("SPM_data_YO/Prior")
# if(!dir.exists("SPM_data_BoS/Current")) dir.create("SPM_data_BoS/Current")
# if(!dir.exists("SPM_data_BoS/Prior")) dir.create("SPM_data_BoS/Prior")

