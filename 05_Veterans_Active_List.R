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

load("images/COHHIOHMIS.RData")
load("images/cohorts.RData")

# Get all veterans and associated hh members ------------------------------

responsible_providers <- ServiceAreas %>%
  select(County, SSVFServiceArea) 

bos_counties <- ServiceAreas %>%
  filter(CoC == "OH-507 Balance of State") %>%
  pull(County)

vet_ees <- Enrollment %>%
  filter(ProjectType %in% c(lh_at_entry_project_types)) %>%
  left_join(Client %>% select(PersonalID, VeteranStatus), by = "PersonalID") %>%
  mutate(VeteranStatus = if_else(VeteranStatus == 1, 1, 0)) %>%
  group_by(HouseholdID) %>%
  summarise(VetCount = sum(VeteranStatus)) %>%
  ungroup() %>%
  filter(VetCount > 0) %>%
  left_join(Enrollment, by = "HouseholdID") %>%
  left_join(Client %>% select(PersonalID, VeteranStatus), by = "PersonalID") %>%
  filter((CountyServed %in% c(bos_counties) | is.na(CountyServed)) &
           !ProjectID %in% c(1282)) %>%
  select(1, 3:15, 17, 51, 67, 73, 75:90)

# Currently in PSH/RRH ----------------------------------------------------

# RRH PSH stays with no Exit but a valid Move-In Date

currently_housed_in_psh_rrh <- vet_ees %>%
  filter(stayed_between(., start = format(today(), "%m%d%Y"), 
                        end = format(today(), "%m%d%Y")) &
           ProjectType %in% c(ph_project_types)) %>%
  pull(PersonalID)

# Active List -------------------------------------------------------------

# stayers & people who exited in the past 90 days to a temp destination

veteran_active_list <- vet_ees %>%
  filter(!PersonalID %in% c(currently_housed_in_psh_rrh) &
           (is.na(ExitDate) |
              (!Destination %in% c(perm_destinations) &
                 ymd(ExitDate) >= today() - days(90))))

# Currently Homeless Vets -------------------------------------------------

# same as Active List except it only includes stayers and leaves out households 
# that have exited to a temporary destination. Not sure we'll need this actually
# because we can just make it a widget on the report, to exclude those.

# Entered in Past 90 Days -------------------------------------------------

entered_past_90 <- vet_ees %>%
  filter(entered_between(., format(today() - days(90), "%m%d%Y"),
                         format(today(), "%m%d%Y")))

# Declined  ---------------------------------------------------------------

most_recent_offer <- Offers %>%
  group_by(PersonalID) %>%
  slice_max(ymd(OfferDate))

declined <- vet_ees %>%
  left_join(most_recent_offer, by = "PersonalID") %>%
  filter(OfferAccepted == "No" &
           ymd(OfferDate) >= today() - days(14))

# Data Quality ------------------------------------------------------------

# this is just an intersection of currently homeless vets and currently housed
# in rrh and psh that would indicate a data quality issue, but these would 
# already be on the Data Quality report as an Overlap, so why do we need this
# here? We could flag any households with overlaps in the report.

# Long Term -------------------------------------------------------

# thinking of moving the code I already wrote for this in the Active List
# up to cohorts.R so I can get this easily from there instead of having to
# copy that code to here

# Chronic ---------------------------------------------------------

# thinking of moving the code I already wrote for this in the Active List
# up to cohorts.R so I can get this easily from there instead of having to
# copy that code to here

# actually maybe not because the chronic code in the active_list.R looks at
# an entire household's chronic status and then marks otherwise-non-chronic
# clients as chronic if they're in a household, but this report only looks at
# veterans. BUT maybe it shouldn't. Like it would make more sense to calculate
# chronicity the same from one report to the other and take into account a 
# veteran's household's chronic status as well.

# ON THE OTHER HAND, it's very specific to the way the Active List is written
# because that script is untangling household data quality issues first and THEN
# calculating it, but I'm not planning to untangle household dq issues in this
# report. Maybe I should untangle household dq issues in cohorts too. AAaaa


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

# New GPD -----------------------------------------------------------------

new_gpd <- entered_past_90 %>%
  filter(ProjectID %in% c(GPD_project_ids))

# Offers ------------------------------------------------------------------

# checking to be sure I'm not using "Most Recent Offer ..." data anywhere
# since I should be able to just use the subs in Rm/Rme and eliminate those
# redundant data elements once this is all done.

# Exited to PH ------------------------------------------------------------


# New and Exited to PH ----------------------------------------------------





