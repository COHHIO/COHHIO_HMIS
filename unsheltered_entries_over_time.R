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
library(TSstudio)
library(plotly)

load("images/cohorts.RData")
load("images/COHHIOHMIS.RData")

unsheltered_hhs <- co_clients_served %>%
  filter(ProjectName == "Unsheltered Clients - OUTREACH" &
           entered_between(., "10012019", "04152020")) %>%
  select(HouseholdID, EntryDate, ExitAdjust) %>%
  left_join(Enrollment %>% 
              select(HouseholdID, CountyServed, UserCreating),
            by = "HouseholdID") %>%
  unique()

county_guesses <- unsheltered_hhs %>%
  left_join(Users, by = "UserCreating")

counties <- regions$County

df <- county_guesses %>%
  mutate(County = if_else(is.na(CountyServed),
                          UserCounty,
                          CountyServed),
         EntryDate = paste0(month(EntryDate), "-", year(EntryDate))) %>%
  select(HouseholdID, EntryDate, County) %>%
  group_by(EntryDate, County) %>%
  summarise(Entries = n()) %>%
  ungroup() %>%
  mutate(Entries = as.numeric(Entries),
         EntryDate = factor(
           EntryDate,
           levels = c(
             "10-2019",
             "11-2019",
             "12-2019",
             "1-2020",
             "2-2020",
             "3-2020",
             "4-2020"
           )
         ), 
         County = factor(County, levels = c(counties))) %>%
  pivot_wider(names_from = County, values_from = Entries)

df[is.na(df)] <- 0

df <- df %>%
  pivot_longer(cols = !all_of("EntryDate"), 
               names_to = "County", 
               values_to = "Entries")

ggplot(df, aes(x = EntryDate, y = Entries, group = County, color = County)) +
  geom_line()

plot_ly(df %>% 
          arrange(EntryDate, County) %>%
          group_by(County), 
        x = ~EntryDate, 
        y = ~Entries, 
        type = 'scatter', 
        mode = 'lines', 
        color = ~County)
