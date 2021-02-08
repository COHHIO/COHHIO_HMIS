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

library(tidyverse)
library(highcharter)
library(here)

load(here("images/Data_Quality.RData"))

# this is so cute and could be added to a region's DQ page

errors <- dq_main %>%
  filter(ProjectName == "Licking - LCCH - RROhio - RRH") %>%
  count(Type)

hchart(errors, type = "item", hcaes(name = Type, y = n)) 



