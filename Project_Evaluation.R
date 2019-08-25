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

load("images/COHHIOHMIS.RData")

# The specs for this report is here: 
#https://cohhio.org/wp-content/uploads/2019/03/2019-CoC-Competition-Plan-and-Timeline-FINAL-merged-3.29.19.pdf

# Staging -----------------------------------------------------------------
# filter to only CoC-funded projects
reporting_year <- 2019

coc_funded <- Funder %>%
  filter(Funder %in% c(1:7, 43) &
           ymd(StartDate) <= mdy(paste("1231", reporting_year)) &
           (is.na(EndDate) |
              ymd(EndDate) >= mdy(paste("0101", reporting_year)))) %>%
  select(ProjectID, Funder)

served_in_date_range <-  Enrollment %>%
  right_join(coc_funded, by = "ProjectID")  %>%
  filter(served_between(., paste(
    "0101", reporting_year
  ),
  paste(
    "1231", reporting_year
  ))) %>%
  left_join(Client, by = "PersonalID") %>%
  select(
    PersonalID,
    ProjectType,
    VeteranStatus,
    EnrollmentID,
    ProjectID,
    EntryDate,
    HouseholdID,
    RelationshipToHoH,
    LivingSituation,
    LengthOfStay,
    LOSUnderThreshold,
    PreviousStreetESSH,
    DateToStreetESSH,
    TimesHomelessPastThreeYears,
    AgeAtEntry,
    MonthsHomelessPastThreeYears,
    DisablingCondition,
    MoveInDate,
    MoveInDateAdjust,
    ExitDate,
    Destination,
    ExitAdjust
  )

entered_in_date_range <-  Enrollment %>%
  right_join(coc_funded, by = "ProjectID")  %>%
  filter(entered_between(., paste(
    "0101", reporting_year
  ),
  paste(
    "1231", reporting_year
  ))) %>%
  left_join(Client, by = "PersonalID") %>%
  select(
    PersonalID,
    ProjectType,
    VeteranStatus,
    EnrollmentID,
    ProjectID,
    EntryDate,
    HouseholdID,
    RelationshipToHoH,
    LivingSituation,
    LengthOfStay,
    LOSUnderThreshold,
    PreviousStreetESSH,
    DateToStreetESSH,
    TimesHomelessPastThreeYears,
    AgeAtEntry,
    MonthsHomelessPastThreeYears,
    DisablingCondition,
    MoveInDate,
    MoveInDateAdjust,
    ExitDate,
    Destination,
    ExitAdjust
  )

# Housing Stability: Exits to PH ------------------------------------------
# PSH (includes stayers tho), TH, SH, RRH

# Housing Stability: Moved into Own Housing -------------------------------
# TH, SH, RRH

# Housing Stability: 6 mo Recurrence --------------------------------------
# PSH, TH, SH, RRH

# Housing Stability: 6-24 mo Recurrence -----------------------------------
# PSH, TH, SH, RRH

# Accessing Mainstream Resources: NCBs or Health Insurance ----------------
# PSH, TH, SH, RRH

# Accessing Mainstream Resources: Increase Total Income -------------------
# PSH, TH, SH, RRH

# Housing Stability: Length of Time Homeless ------------------------------
# TH, SH, RRH

# Community Need: Average Bed/Unit Utilization ----------------------------
# PSH, TH, SH, RRH (it's true! not sure why)

# Community Need: Res Prior = Streets or ESSH -----------------------------
# PSH, TH, SH (Street only), RRH

# Community Need: Entries with No Income ----------------------------------
# PSH, TH, SH, RRH

# Community Need: Homeless History Index ----------------------------------
# PSH, TH, SH, RRH

# Community Need: Long Term Homeless Households ---------------------------
# PSH

# HMIS Data Quality -------------------------------------------------------
# PSH, TH, SH, RRH



