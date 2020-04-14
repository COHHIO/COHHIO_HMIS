# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
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
library(janitor)

load("images/COHHIOHMIS.RData")

if(file.exists("data/phtrackclients.zip")) {
  unzip(zipfile = "./data/phtrackclients.zip", exdir = "./data")
  
  file.rename(paste0("data/", list.files("./data", pattern = "(report_)")),
              "data/phtrackclients.csv")
  
  file.remove("data/phtrackclients.zip")
}

ap_entered <- read_csv("data/phtrackclients.csv", col_types = "ncc?") %>%
  mutate(
    ExpectedPHDate = mdy(ExpectedPHDate)
  )

ap_entered_prioritize <- ap_entered %>%
  filter(ymd(ExpectedPHDate) >= today())

ap_entered_no_ee <- ap_entered_prioritize %>%
  anti_join(Enrollment, by = "PersonalID")

ap_entered_no_ee$ProivderCreating %>% unique()

# ProviderCreating is 1. mispelled and 2. could just indicate who first
# created that client, which really means nothing. 

# It would be more meaningful to know who entered the PH Track