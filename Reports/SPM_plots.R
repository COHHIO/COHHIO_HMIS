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

library(ggplot2)
library(lubridate)
library(plotly)

source("07_SPMs.R")

rm(spm_4_1_empl_stayers,
   spm_4_2_income_non_empl_stayers,
   spm_4_3_income_total_stayers,
   spm_4_4_income_empl_leavers,
   spm_4_5_income_non_empl_leavers,
   spm_4_6_income_total_leavers,
   spm_5_1_first_timers_lh,
   spm_5_2_first_timers_all)

metric_1a_med <- spm_1a_loth_ees[2,] %>% 
  select(Metric1a, "PriorMedian" = Prior_MedLoT, "CurrentMedian" = MedLoT) %>%
   pivot_longer(cols = c("PriorMedian", "CurrentMedian")) %>% 
  mutate(Year = case_when(
    name == "PriorMedian" ~ paste(format.Date(spm_prior_start_date, "%b %Y"), "to",
                            format.Date(spm_prior_end_date - days(1), "%b %Y")),
    name == "CurrentMedian" ~ paste(format.Date(spm_current_start_date, "%b %Y"), "to",
                              format.Date(spm_current_end_date - days(1), "%b %Y"))
  )) 

metric_1a_avg <- spm_1a_loth_ees[2,] %>% 
  select(Metric1a, "PriorAverage" = Prior_AvgLoT, "CurrentAverage" = AvgLoT) %>%
  pivot_longer(cols = c("PriorAverage", "CurrentAverage")) %>% 
  mutate(Year = case_when(
    name == "PriorAverage" ~ paste(format.Date(spm_prior_start_date, "%b %Y"), "to",
                                  format.Date(spm_prior_end_date - days(1), "%b %Y")),
    name == "CurrentAverage" ~ paste(format.Date(spm_current_start_date, "%b %Y"), "to",
                                    format.Date(spm_current_end_date - days(1), "%b %Y"))
  )) 

metric_3 <- spm_3_homeless_count %>%
  filter(Type == "Unduplicated Total Sheltered Homeless Persons") %>%
  pivot_longer(cols = c("Prior", "Current")) %>%
  mutate(Year = case_when(
    name == "Prior" ~ paste(format.Date(spm_prior_start_date, "%b %Y"), "to",
                            format.Date(spm_prior_end_date - days(1), "%b %Y")),
    name == "Current" ~ paste(format.Date(spm_current_start_date, "%b %Y"), "to",
                              format.Date(spm_current_end_date - days(1), "%b %Y"))
  )) %>%
  select("Measure" = Type, "LiterallyHomeless" = value, Year)

a <- ggplot(metric_3, aes(Year, LiterallyHomeless)) +
  geom_line(group = 1) + geom_point() + 
  ylim(0, max(metric_3$LiterallyHomeless) + 1000) +
  labs(title =
         "Metric 3: Homeless Clients Served in Shelter or Transitional Housing") +
  xlab("") +
  ylab("Homeless Clients in HMIS") +
  theme_bw() 

ggplotly(a)



