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
library(lubridate)
library(janitor)
library(HMIS)

start <- ymd("20190101")
end <- ymd("20191231")

if (!exists("Enrollment")) load("images/COHHIOHMIS.RData")

clients <- Enrollment %>%
  filter(served_between(., start, end)) %>%
  mutate(
    lh_start = EntryDate,
    lh_end = case_when(
      ProjectType %in% c(1, 2, 4, 8) ~ ExitAdjust,
      ProjectType %in% c(3, 13) &
        !is.na(MoveInDateAdjust) ~ MoveInDateAdjust
    )
  ) %>%
  filter(
    CountyServed %in% c("Mahoning", "Trumbull", "Columbiana") &
      ymd(lh_end) >= start &
      !is.na(lh_end)
  ) %>%
  select(CountyServed,
         PersonalID) %>% unique() %>%
  group_by(CountyServed) %>%
  count()

