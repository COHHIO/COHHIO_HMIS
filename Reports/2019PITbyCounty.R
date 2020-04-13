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
library(here)
library(usmap)

data <- read_csv(here("Reports/Individuals2019data.csv"))

data <- data %>%
  select(-County) %>%
  rename("County" = 5)

# County, FIPS, Unsheltered Single Count, Sheltered Single Count

unsheltered <- data %>%
  filter(`Project Type` == "Unsheltered") %>%
  group_by(County) %>%
  count(name = "UnshelteredSingles")

sheltered <- data %>%
  filter(`Project Type` %in% c("Transitional Housing",
                               "Emergency Shelter",
                               "Safe Haven")) %>%
  group_by(County) %>%
  count(name = "ShelteredSingles")

none <- data %>%
  filter(`Project Type` == "None Counted") %>%
  group_by(County) %>%
  count(name = "NoneCounted")

all <- sheltered %>%
  full_join(unsheltered, by = "County") %>%
  full_join(none, by = "County") %>%
  mutate(FIPS = fips("Ohio", county = County)) %>%
  select(-NoneCounted) %>%
  arrange(County)

all[is.na(all)] <- 0

write_csv(all, here("Reports/SinglesPointInTime2019byCountyOH507.csv"))
