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

library(tidycensus)
library(dplyr)

census2010 <- load_variables(2010, "sf1", cache = TRUE)

ohio <- get_decennial(geography = "county", 
                      year = 2010, 
                      state = 39,
                      variables = c("P005003", "P005004", "P005005", "P005006",
                                    "P005007", "P005008", "P005009", "P005011",
                                    "P005012", "P005013", "P005014", "P005015",
                                    "P005016", "P005017"))


