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
library(HMIS)
library(tidymetrics)
library(here)

load(here("images/cohorts.RData"))
load(here("images/COHHIOHMIS.RData"))

entries <- co_hohs_entered %>%
  select(HouseholdID, "date" = EntryDate) %>%
  cross_by_periods() %>%
  summarise(
    Entries = n()
  )

ggplot(entries, aes(date, Entries, color = period)) +
  geom_line() +
  theme_minimal()

entries_ptc <- co_hohs_entered %>%
  rename("date" = EntryDate) %>%
  filter(ymd(date) > today() - years(1)) %>%
  cross_by_periods(periods = "week") %>%
  ungroup() %>%
  cross_by_dimensions(ProjectType, date) %>%
  summarise(Entries = n()) %>%
  mutate(ProjectType = project_type(ProjectType)) %>%
  filter(ProjectType != "All" & date != "All")

ggplot(entries_ptc,
       mapping = aes(
         x = date,
         y = Entries,
         group = ProjectType,
         fill = ProjectType
       )) +
  # geom_area(alpha = 0.6,
  #           size = .5,
  #           colour = "white") +
  scale_color_viridis_d() +
  geom_line(aes(color = ProjectType)) +
  geom_point(aes(color = ProjectType)) +
  geom_smooth() +
  scale_x_discrete(limits = date, breaks = date[seq(1, length(date), by = 2)]) +
  ggtitle("Project Entries Across Time") +
  theme(axis.text.x = element_text(angle = 33)) +
  theme_minimal()

