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
library(HMIS)

if (!exists("Enrollment")) load("images/COHHIOHMIS.RData")
if (!exists("tay")) {
  load("images/cohorts.RData")
  rlang::env_binding_lock(environment(), ls())
}

# Get all veterans and associated hh members ------------------------------

responsible_providers <- ServiceAreas %>%
  select(County, SSVFServiceArea) 

vet_ees <- co_clients_served %>%
  filter(ProjectType %in% c(lh_at_entry_project_types)) %>%
  mutate(VeteranStatus = if_else(VeteranStatus == 1, 1, 0)) %>%
  group_by(HouseholdID) %>% # pulling in all Veterans & non-veteran hh members
  summarise(VetCount = sum(VeteranStatus)) %>%
  ungroup() %>%
  filter(VetCount > 0) %>%
  left_join(Enrollment, by = "HouseholdID") %>%
  left_join(co_clients_served[c("PersonalID", "VeteranStatus")], by = "PersonalID") %>%
  left_join(Project[c("ProjectID", "ProjectCounty")], by = "ProjectID") %>%
  left_join(VeteranCE, 
            by = c("PersonalID", "EnrollmentID", "ExpectedPHDate", "PHTrack")) %>%

  mutate(
    County = if_else(is.na(CountyServed), ProjectCounty, CountyServed)
  ) %>%
  filter((County %in% c(bos_counties) | 
            County == "Mahoning") &
           !ProjectID %in% c(1282)) %>% # i don't remember why i'm excluding this? 
  select(
    HouseholdID,
    EnrollmentID,
    PersonalID,
    HOMESID,
    ProjectID,
    ProjectType,
    ProjectName,
    ProjectCounty,
    DateVeteranIdentified,
    EntryDate,
    EntryAdjust,
    HHEntry,
    HHMoveIn,
    MoveInDateAdjust,
    ExitDate,
    ExitAdjust,
    RelationshipToHoH,
    LivingSituation,
    ListStatus,
    VAEligible,
    SSVFIneligible,
    LengthOfStay,
    LOSUnderThreshold,
    PreviousStreetESSH,
    DateToStreetESSH,
    TimesHomelessPastThreeYears,
    MonthsHomelessPastThreeYears,
    DisablingCondition,
    AnnualPercentAMI,
    VAMCStation,
    UserCreating,
    County,
    PHTrack,
    ExpectedPHDate,
    Destination,
    OtherDestination,
    ClientLocation,
    AgeAtEntry,
    VeteranStatus
  )

# Currently in PSH/RRH ----------------------------------------------------

# RRH PSH stays with no Exit but a valid Move-In Date

currently_housed_in_psh_rrh <- vet_ees %>%
  filter(stayed_between(., start = format(today(), "%m%d%Y"), 
                        end = format(today(), "%m%d%Y")) &
           ProjectType %in% c(ph_project_types) &
           VeteranStatus == 1) %>%
  pull(PersonalID)

# Declined  ---------------------------------------------------------------

most_recent_offer <- Offers %>%
  filter(!is.na(AcceptDeclineDate) &
           !is.na(OfferAccepted) &
           !is.na(PHTypeOffered)) %>%
  group_by(PersonalID) %>%
  slice_max(ymd(OfferDate)) %>% # same date
  slice_max(OfferAccepted) %>% # both rejected/acceptedd
  slice(1) %>% # pick 1 doesn't matter if those ^ are the same
  ungroup() %>%
  unique()

declined <- vet_ees %>%
  left_join(most_recent_offer, by = "PersonalID") %>%
  filter(OfferAccepted == "No" &
           ymd(OfferDate) >= today() - days(14) &
           VeteranStatus == 1) %>% 
  unique()

# Notes -------------------------------------------------------------------

small_CLS <- Contacts %>%
  filter(RecordType == "CLS") %>%
  mutate(Notes = str_remove_all(Notes, "<"),
         Notes = str_remove_all(Notes, ">")) %>% # in case there's html in the notes
  unite("Notes", ContactDate, Notes, sep = ": ") %>%
  select(PersonalID, Notes) %>%
  group_by(PersonalID) %>%
  arrange(desc(Notes)) %>%
  summarise(Notes = list(Notes)) %>%
  ungroup() %>%
  mutate(
    Notes = as.character(Notes),
    Notes = if_else(str_starts(Notes, "c"),
                    str_replace_all(Notes, "\", \"", "<br>"),
                    Notes),
    Notes = gsub("c\\(\"", "", Notes),
    Notes = gsub("\"\\)", "", Notes)
  )

# Active List -------------------------------------------------------------

# stayers & people who exited in the past 90 days to a temp destination

hh_size <- vet_ees %>%
  select(HouseholdID, PersonalID) %>%
  unique() %>%
  count(HouseholdID)

veteran_active_list <- vet_ees %>%
  filter(!PersonalID %in% c(currently_housed_in_psh_rrh) &
           VeteranStatus == 1 &
           (is.na(ExitDate) |
              (
                !Destination %in% c(perm_destinations) &
                  ymd(ExitDate) >= today() - days(90)
              ))) %>%
  left_join(hh_size, by = "HouseholdID") %>%
  rename("HouseholdSize" = n) %>%
  left_join(most_recent_offer, by = "PersonalID") %>%
  left_join(small_CLS, by = "PersonalID") %>%
  mutate(
    ActiveDate = case_when(
      is.na(DateVeteranIdentified) ~ EntryDate,
      ymd(DateVeteranIdentified) < ymd(EntryDate) ~ DateVeteranIdentified,
      TRUE ~ EntryDate
    ),
    ActiveDateDisplay = paste0(ActiveDate,
                               "<br>(",
                               difftime(today(), ymd(ActiveDate)),
                               " days)"),
    TimeInProject = if_else(
      is.na(ExitDate),
      paste("Since", format(ymd(EntryDate), "%m-%d-%Y")),
      paste(
        format(ymd(EntryDate), "%m-%d-%Y"),
        "to",
        format(ymd(ExitDate), "%m-%d-%Y")
      )
    ),
    DaysActive = difftime(today(), ymd(ActiveDate)),
    Eligibility =
      if_else(
        is.na(VAEligible) & is.na(SSVFIneligible),
        "Unknown",
        paste(
          "VA Eligibility:",
          VAEligible,
          "<br><br>SSVF Eligibility:",
          SSVFIneligible
        )
      ),
    ActiveDate = format(ActiveDate, "%m-%d-%Y"),
    MostRecentOffer = if_else(
      is.na(AcceptDeclineDate),
      "None",
      paste(
        "Offer of",
        PHTypeOffered,
        "on",
        format(OfferDate, "%m-%d-%Y"),
        "was",
        if_else(OfferAccepted == "Yes", "accepted", "declined"),
        "on",
        format(AcceptDeclineDate, "%m-%d-%Y")
      )
    ),
    HousingPlan =
      if_else(
        is.na(PHTrack) & is.na(ExpectedPHDate),
        paste("No Housing Track<br><br>Notes:",
              Notes),
        paste(PHTrack,
        "by",
        if_else(
          is.na(ExpectedPHDate),
          "unknown date",
          format(ExpectedPHDate, "%m-%d-%Y")
        ),
      "<br><br>Notes:<br>",
      Notes)
      )
  ) %>%
  left_join(responsible_providers, by = "County") %>%
  unique()

# Currently Homeless Vets -------------------------------------------------

# same as Active List except it only includes stayers and leaves out households 
# that have exited to a temporary destination. Not sure we'll need this actually
# because we can just make it a widget on the report, to exclude those.

# Entered in Past 90 Days -------------------------------------------------

entered_past_90 <- vet_ees %>%
  filter(entered_between(., format(today() - days(90), "%m%d%Y"),
                         format(today(), "%m%d%Y")))


# Veterans Missing Veteran Assessment -------------------------------------



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

# I think it will be best to move the chronic code to cohorts, and the Returns
# code can go there too.

# New GPD -----------------------------------------------------------------

new_gpd <- entered_past_90 %>%
  filter(ProjectID %in% c(GPD_project_ids))

# Offers ------------------------------------------------------------------

# checking to be sure I'm not using "Most Recent Offer ..." data anywhere
# since I should be able to just use the subs in Rm/Rme and eliminate those
# redundant data elements once this is all done.

# Exited to PH ------------------------------------------------------------


# New and Exited to PH ----------------------------------------------------



# Save it out -------------------------------------------------------------

# WARNING save.image does not save the environment properly, save must be used.
save(list = ls(), file = "images/Vet_Active_List.RData", compress = FALSE)


