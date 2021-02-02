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
  "devtools"
))
remotes::install_github("jimhester/archive", dependencies = "Imports", upgrade = "always")

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
    "viridis"
  )
)

# Install HMIS package
devtools::install_github("COHHIO/HMIS", , dependencies = "Imports", upgrade = "always")



# Create needed directories

if(!dir.exists("data")) dir.create("data")
if(!dir.exists("images")) dir.create("images")
