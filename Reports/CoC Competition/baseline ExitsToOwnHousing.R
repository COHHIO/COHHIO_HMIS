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
library(here)
library(patchwork)

load(here("images/ProjectEvaluation.RData"))


# Review Exits to Own Housing --------------------------------------------


TH <- ggplot(summary_pe_own_housing %>%
               filter(ProjectType == 2),
             aes(y = OwnHousingPoints,
                 fill = ProjectType)) +
  geom_bar(width = .5, show.legend = FALSE) +
  theme_minimal() +
  labs(title = "5 Possible Points",
       y = "Points",
       x = "TH Projects")  + 
  scale_x_continuous(expand = c(0, 0), limits = c(0,NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(-.5, 5.5))

RRH <- ggplot(summary_pe_own_housing %>%
                filter(ProjectType == 13),
              aes(y = OwnHousingPoints,
                  fill = ProjectType)) +
  geom_bar(width = .5, show.legend = FALSE) +
  theme_minimal() +
  labs(title = "5 Possible Points",
       y = NULL,
       x = "RRH Projects") + 
  scale_x_continuous(expand = c(0, 0), limits = c(0,NA)) + 
  scale_y_continuous(expand = c(0, 0), limits = c(-.5, 5.5))

TH + RRH 

