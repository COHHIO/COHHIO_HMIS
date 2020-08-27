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

load("images/cohorts.RData")
load("images/COHHIOHMIS.RData")

# Add Chronicity ----------------------------------------------------------

# creating a small basic dataframe to work with
smallEnrollment <- Enrollment %>%
  select(EnrollmentID, PersonalID, HouseholdID, LivingSituation, 
         DateToStreetESSH, TimesHomelessPastThreeYears, ExitAdjust,
         MonthsHomelessPastThreeYears)

# getting only the independently-chronic clients. they're chronic right now
# and because of *their own* homeless history
singly_chronic <-
  active_list %>%
  left_join(smallEnrollment,
            by = c("PersonalID",
                   "EnrollmentID",
                   "HouseholdID")) %>%
  mutate(SinglyChronic =
           if_else(((ymd(DateToStreetESSH) + days(365) <= ymd(EntryDate) &
                       !is.na(DateToStreetESSH)) |
                      (
                        MonthsHomelessPastThreeYears %in% c(112, 113) &
                          TimesHomelessPastThreeYears == 4 &
                          !is.na(MonthsHomelessPastThreeYears) &
                          !is.na(TimesHomelessPastThreeYears)
                      )
           ) &
             DisablingCondition == 1 &
             !is.na(DisablingCondition), 1, 0))

# pulling all EEs with the Chronic designation, marking all hh members of anyone
# with a Chronic marker as also Chronic
household_chronic <- singly_chronic %>%
  group_by(HouseholdID) %>%
  mutate(
    ChronicHousehold = sum(SinglyChronic, na.rm = TRUE),
    ChronicStatus = case_when(
      ChronicHousehold > 0 ~ "Chronic",
      ChronicHousehold == 0 ~ "Not Chronic"
    )
  ) %>%
  ungroup() %>%
  select(-ChronicHousehold)

# adds current days in ES or SH projects to days homeless prior to entry and if
# it adds up to 365 or more, it marks the client as AgedIn
agedIntoChronicity <- household_chronic %>%
  mutate(
    DaysHomelessInProject = difftime(ymd(ExitAdjust),
                                     ymd(EntryDate),
                                     units = "days"),
    DaysHomelessBeforeEntry = difftime(ymd(EntryDate),
                                       if_else(
                                         is.na(ymd(DateToStreetESSH)),
                                         ymd(EntryDate),
                                         ymd(DateToStreetESSH)
                                       ),
                                       units = "days"),
    ChronicStatus = if_else(
      ProjectType %in% c(1, 8) &
        ChronicStatus == "Not Chronic" &
        ymd(DateToStreetESSH) + days(365) > ymd(EntryDate) &
        !is.na(DateToStreetESSH) &
        DaysHomelessBeforeEntry + DaysHomelessInProject >= 365,
      "Aged In",
      ChronicStatus
    )
  ) %>%
  select(-DaysHomelessInProject,-DaysHomelessBeforeEntry)

# adds another ChronicStatus of "Nearly Chronic" which catches those hhs with
# almost enough times and months to qualify as Chronic
nearly_chronic <- agedIntoChronicity %>%
  mutate(
    ChronicStatus = if_else(
      ChronicStatus == "Not Chronic" &
        ((
          ymd(DateToStreetESSH) + days(365) <= ymd(EntryDate) &
            !is.na(DateToStreetESSH)
        ) |
          (
            MonthsHomelessPastThreeYears %in% c(110:113) &
              TimesHomelessPastThreeYears%in% c(3, 4) &
              !is.na(MonthsHomelessPastThreeYears) &
              !is.na(TimesHomelessPastThreeYears)
          )
        ) &
        DisablingCondition == 1 &
        !is.na(DisablingCondition),
      "Nearly Chronic",
      ChronicStatus
    )
  )

active_list <- active_list %>%
  left_join(
    nearly_chronic %>%
      select("PersonalID",
             "HouseholdID",
             "EnrollmentID",
             "ChronicStatus"),
    by = c("PersonalID", "HouseholdID", "EnrollmentID")
  )

# THIS IS WHERE WE'RE SUMMARISING BY HOUSEHOLD (after all the group_bys)

active_list <- active_list %>%
  mutate(
    HH_DQ_issue = if_else(
      correctedhoh == 1 & !is.na(correctedhoh),
      1,
      0
    ),
    HoH_Adjust = case_when(correctedhoh == 1 ~ 1,
                           is.na(correctedhoh) ~ hoh)
  ) %>%
  filter(HoH_Adjust == 1) %>%
  select(-correctedhoh, -RelationshipToHoH, -hoh, -HoH_Adjust)



