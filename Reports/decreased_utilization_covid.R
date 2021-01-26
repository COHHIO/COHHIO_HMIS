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

source("01_Bed_Unit_Utilization.R")
library(janitor)

shelters <- utilization_bed %>% filter(ProjectType == 1) %>%
  rename_with(~ paste0("bernie", .x), ends_with(c('2019'))) %>%
  rename_with(~ paste0("aoc", .x), ends_with(c('2020'))) 

pre <- shelters %>%
  select(
    ProjectID,
    ProjectName,
    starts_with("bernie"),
    aoc01012020,
    aoc02012020,
    aoc03012020
  ) %>%
  mutate(pre = mean(bernie01012019:aoc03012020, na.rm = TRUE))

post <- shelters %>%
  select(
    ProjectID,
    ProjectName,
    starts_with("aoc"),
    -aoc01012020,
    -aoc02012020,
    -aoc03012020
  )




