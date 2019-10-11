# COHHIO_HMIS
# Copyright (C) 2019  Coalition on Homelessness and Housing in Ohio (COHHIO)
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details at 
#<https://www.gnu.org/licenses/>.

library(tidyverse)
library(lubridate)

load("images/Data_Quality.RData")

ARTReport <- read_csv("data/OverlapsReport.csv") %>%
  mutate(Provider = str_remove(Provider, "\\(.*\\)"))

ART_IDs <- as.data.frame(ARTReport$PersonalID)

R_IDs <- as.data.frame(overlaps$PersonalID)

ART_IDs <- `colnames<-`(ART_IDs, "PersonalID")

R_IDs <- `colnames<-`(R_IDs, "PersonalID")

Extra_In_R <- anti_join(R_IDs, ART_IDs, by = "PersonalID")

Extra_In_ART <- anti_join(ART_IDs, R_IDs, by = "PersonalID")

Extra_In_R <-
  left_join(Extra_In_R, servedInDateRange, by = "PersonalID") %>%
  select(PersonalID,
         EntryDate,
         MoveInDate,
         MoveInDateAdjust,
         ExitDate)

Extra_In_ART <-
  left_join(Extra_In_ART, servedInDateRange, by = "PersonalID") %>%
  select(PersonalID,
         EntryDate,
         MoveInDate,
         MoveInDateAdjust,
         ExitDate)
