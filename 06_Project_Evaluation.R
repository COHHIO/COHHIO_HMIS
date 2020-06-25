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
library(scales)

load("images/20200619COHHIOHMIS.RData")
rm(Affiliation, CaseManagers, Disabilities, EmploymentEducation, EnrollmentCoC, 
   Export, HealthAndDV, Inventory, Offers, ProjectCoC, Referrals, 
   regions, Scores, Services, stray_services, Users, VeteranCE)

load("images/20200619cohorts.RData")
rm(FileActualStart, FileStart, FileEnd, update_date, summary)

load("images/20200619DQ.RData")

# Points function ---------------------------------------------------------

pe_score <- function(structure, value) {
  case_when(
    structure == "24_30_11" & value >= .3 ~ 11,
    structure == "24_30_11" & value >= .27 & value < .3 ~ 8,
    structure == "24_30_11" & value >= .24 & value < .27 ~ 5,
    structure == "24_30_11" & value < .24 ~ 0,
    structure == "75_85_12" & value >= .85 ~ 12,
    structure == "75_85_12" & value >= .8 & value < .85 ~ 9,
    structure == "75_85_12" & value >= .75 & value < .8 ~ 5,
    structure == "75_85_12" & value < .75 ~ 0,
    structure == "80_90_12" & value >= .9 ~ 12,
    structure == "80_90_12" & value >= .85 & value < .9 ~ 9,
    structure == "80_90_12" & value >= .8 & value < .85 ~ 5,
    structure == "80_90_12" & value < .8 ~ 0,
    structure == "0_730_10" & value <= 730 ~ 10,
    structure == "0_730_10" & value > 730 ~ 0,
    structure == "75_85_10" & value >= .85 ~ 10,
    structure == "75_85_10" & value >= .8 & value < .85 ~ 7.5,
    structure == "75_85_10" & value >= .75 & value < .8 ~ 5,
    structure == "75_85_10" & value < .75 ~ 0,
    structure == "20_90_10" & value >= .9 ~ 10,
    structure == "20_90_10" & value >= .75 & value < .9 ~ 8,
    structure == "20_90_10" & value >= .5 & value < .75 ~ 6,
    structure == "20_90_10" & value >= .3 & value < .5 ~ 4,
    structure == "20_90_10" & value >= .2 & value < .3 ~ 2,
    structure == "20_90_10" & value < .2 ~ 0,
    structure == "2_6_10" & value <= .02 ~ 10,
    structure == "2_6_10" & value <= .04 & value > .02 ~ 7.5,
    structure == "2_6_10" & value <= .06 & value > .04 ~ 5,
    structure == "2_6_10" & value > .06 ~ 0,
    structure == "5_9_10" & value <= .05 ~ 10,
    structure == "5_9_10" & value <= .08 & value > .05 ~ 7.5,
    structure == "5_9_10" & value <= .09 & value > .08 ~ 5,
    structure == "5_9_10" & value > .09 ~ 0,
    structure == "75_83_10" & value >= .83 ~ 10,
    structure == "75_83_10" & value >= .79 & value < .83 ~ 7.5,
    structure == "75_83_10" & value >= .75 & value < .79 ~ 5,
    structure == "75_83_10" & value < .75 ~ 0,
    structure == "72_80_5" & value >= .8 ~ 5,
    structure == "72_80_5" & value >= .76 & value < .8 ~ 3,
    structure == "72_80_5" & value >= .72 & value < .76 ~ 2,
    structure == "72_80_5" & value < .72 ~ 0,
    structure == "7_12_10" & value <= .07 ~ 10,
    structure == "7_12_10" & value <= .09 & value > .07 ~ 7.5,
    structure == "7_12_10" & value <= .12 & value > .09 ~ 5,
    structure == "7_12_10" & value > .12 ~ 0,
    structure == "12_17_10" & value <= .12 ~ 10,
    structure == "12_17_10" & value > 12 & value <= .14 ~ 7.5,
    structure == "12_17_10" & value > .14 & value <= 17 ~ 5,
    structure == "12_17_10" & value > .17 ~ 0,
    structure == "22_28_10" & value >= .28 ~ 10,
    structure == "22_28_10" & value >= .26 & value < .28 ~ 7.5,
    structure == "22_28_10" & value >= .22 & value < .26 ~ 5,
    structure == "22_28_10" & value < .22 ~ 0,
    structure == "0_7_10_PSH" & value >= 6 & value <= 7 ~ 10,
    structure == "0_7_10_PSH" & value >= 5 & value < 6 ~ 9,
    structure == "0_7_10_PSH" & value >= 3 & value < 5 ~ 8,
    structure == "0_7_10_PSH" & value >= 2 & value < 3 ~ 5,
    structure == "0_7_10_PSH" & value >= 1 & value < 2 ~ 2,
    structure == "0_7_10_PSH" & value < 1 ~ 0,
    structure == "200_280_10" & value <= 200 ~ 10,
    structure == "200_280_10" & value <= 240 & value > 200 ~ 7.5,
    structure == "200_280_10" & value <= 280 & value > 240 ~ 5,
    structure == "200_280_10" & value > 280 ~ 0,
    structure == "67_75_10" & value >= .75 ~ 10,
    structure == "67_75_10" & value >= .71 & value < .75 ~ 7.5,
    structure == "67_75_10" & value >= .67 & value < .71 ~ 5,
    structure == "67_75_10" & value < .67 ~ 0,
    structure == "0_7_10" & value >= 4 & value <= 7 ~ 10,
    structure == "0_7_10" & value >= 3 & value < 4 ~ 8,
    structure == "0_7_10" & value >= 2 & value < 3 ~ 7,
    structure == "0_7_10" & value >= 1 & value < 2 ~ 5,
    structure == "0_7_10" & value < 1 ~ 0,
    structure == "15_19_10" & value <= .15 ~ 10,
    structure == "15_19_10" & value <= .17 & value > .15 ~ 7.5,
    structure == "15_19_10" & value <= .19 & value > .17 ~ 5,
    structure == "15_19_10" & value > .19 ~ 0,
    structure == "20_24_10" & value <= .2 ~ 10,
    structure == "20_24_10" & value <= .22 & value > .2 ~ 7.5,
    structure == "20_24_10" & value <= .24 & value > .22 ~ 5,
    structure == "20_24_10" & value > .24 ~ 0,
    structure == "16_20_10" & value >= .2 ~ 10,
    structure == "16_20_10" & value < .2 & value >= .18 ~ 7.5,
    structure == "16_20_10" & value < .18 & value >= .16 ~ 5,
    structure == "16_20_10" & value < .16 ~ 0,
    structure == "260_340_10" & value <= 260 ~ 10,
    structure == "260_340_10" & value > 260 & value <= 300 ~ 7.5,
    structure == "260_340_10" & value > 300 & value <= 340 ~ 5,
    structure == "260_340_10" & value > 340 ~ 0,
    structure == "0_100_10" & value == 1 ~ 10,
    structure == "0_100_10" & value < 1 ~ 0,
    structure == "14_18_10" & value >=  .18 ~ 10,
    structure == "14_18_10" & value < .18 & value >= .16 ~ 7.5,
    structure == "14_18_10" & value < .16 & value >= .14 ~ 5,
    structure == "14_18_10" & value < .14 ~ 0,
    structure == "150_210_10" & value <= 150 ~ 10,
    structure == "150_210_10" & value <= 170 & value > 150 ~ 7.5,
    structure == "150_210_10" & value <= 210 & value > 170 ~ 5,
    structure == "150_210_10" & value > 210 ~ 0,
    structure == "80_90_10" & value >= .9 ~ 10,
    structure == "80_90_10" & value >= .85 & value < .9 ~ 7.5,
    structure == "80_90_10" & value >= .8 & value < .85 ~ 5,
    structure == "80_90_10" & value < .8 ~ 0,
    structure == "34_40_10" & value >= .4 ~ 10,
    structure == "34_40_10" & value >= .37 & value < .4 ~ 7.5,
    structure == "34_40_10" & value >= .34 & value < .37 ~ 5,
    structure == "34_40_10" & value < .34 ~ 0,
    structure == "24_30_10" & value >= .3 ~ 10,
    structure == "24_30_10" & value >= .27 & value < .3 ~ 7.5,
    structure == "24_30_10" & value >= .24 & value < .27 ~ 5,
    structure == "24_30_10" & value < .24 ~ 0,
    structure == "90_100_5" & value == 1 ~ 5,
    structure == "90_100_5" & value >= .9 & value < 1 ~ 2,
    structure == "90_100_5" & value < .9 ~ 0
  )
}

# The specs for this report is here: 
#https://cohhio.org/wp-content/uploads/2019/03/2019-CoC-Competition-Plan-and-Timeline-FINAL-merged-3.29.19.pdf

ReportYear <- "2019"
ReportStart <- format.Date(mdy(paste0("0101", ReportYear)), "%m-%d-%Y")
ReportEnd <- format.Date(mdy(paste0("1231", ReportYear)), "%m-%d-%Y")

# Staging -----------------------------------------------------------------

pe_coc_funded <- Funder %>%
  filter(
    ProjectID %in% c(718, 721, 1323, 1354, 1774) |# consolidated in 2019
      (
        Funder %in% c(1:7) &
          ymd(StartDate) <= mdy(ReportEnd) &
          (is.na(EndDate) |
             ymd(EndDate) >= mdy(ReportEnd)) &
          ProjectID != 2069
      )
  ) %>%
  select(ProjectID, Funder, StartDate, EndDate) %>%
  left_join(Project[c("ProjectID",
                      "ProjectName",
                      "ProjectType",
                      "HMISParticipatingProject")], by = "ProjectID") %>%
  filter(HMISParticipatingProject == 1) %>%
  select(ProjectType,
         ProjectName,
         ProjectID)

# consolidated projects

consolidations <- pe_coc_funded %>%
  filter(ProjectID %in% c(718, 719, 721, 
                          1353, 1354, 
                          746, 747, 
                          1774, 15,
                          737, 738, 739,
                          548, 763, 764, 774,
                          1323, 208,
                          1566, 1579)) %>%
  mutate(
    AltProjectID = case_when(
      ProjectID %in% c(718, 719, 721) ~ 3000,
      ProjectID %in% c(1353, 1354) ~ 3001,
      ProjectID %in% c(746, 747) ~ 3002,
      ProjectID %in% c(1774, 15) ~ 3003,
      ProjectID %in% c(737, 738, 739) ~ 3004,
      ProjectID %in% c(548, 763, 764, 774) ~ 3005,
      ProjectID %in% c(1323, 208) ~ 3006,
      ProjectID %in% c(1566, 1579) ~ 3007
    ),
    AltProjectName = case_when(
      ProjectID %in% c(718, 719, 721) ~ "Butler County PSH Combined",
      ProjectID %in% c(1353, 1354) ~ "Springfield SPC 1 Combined",
      ProjectID %in% c(746, 747) ~ "Jefferson County SPC Combined",
      ProjectID %in% c(1774, 15) ~ "GLCAP PSH Combined",
      ProjectID %in% c(737, 738, 739) ~ "Lake SPC Combined",
      ProjectID %in% c(548, 763, 764, 774) ~ "Trumbull SPC Vouchers Combined",
      ProjectID %in% c(1323, 208) ~ "Warren SPC Combined",
      ProjectID %in% c(1566, 1579) ~ "One Eighty PSH Plus Care Combined"
    )
  ) %>%
  select(ProjectID, ProjectName, AltProjectID, AltProjectName)

keepers <- c(746, 15, 1353, 719, 737, 774, 208, 1566) 
retired <- c(747, 1774, 1354, 718, 721, 738, 739, 548, 763, 764, 1323, 1579)

# I messed up the way the cocscoring data was coming in so I'm fixing that here
# this should not be needed for 2021

Project <- Project %>%
  mutate(
    CostPerExit = NA,
    DateReceivedPPDocs = NA,
    HousingFirstScore = NA,
    ChronicPrioritizationScore = NA,
    OnTrackSpendingScoring = NA,
    UnspentFundsScoring = NA,
    CostPerExit = NA
  ) 

# filter to only CoC-funded projects (leaving out the SSO)

 pe_coc_funded <- pe_coc_funded %>%
  left_join(consolidations, by = c("ProjectID", "ProjectName")) %>%
  mutate(
    AltProjectID = if_else(is.na(AltProjectID), ProjectID, AltProjectID),
    AltProjectName = if_else(is.na(AltProjectName), ProjectName, AltProjectName)
  )

vars_we_want <- c(
  "PersonalID",
  "ProjectType",
  "AltProjectID",
  "VeteranStatus",
  "EnrollmentID",
  "AltProjectName",
  "EntryDate",
  "HouseholdID",
  "RelationshipToHoH",
  "LivingSituation",
  "LengthOfStay",
  "LOSUnderThreshold",
  "PreviousStreetESSH",
  "DateToStreetESSH",
  "TimesHomelessPastThreeYears",
  "AgeAtEntry",
  "MonthsHomelessPastThreeYears",
  "DisablingCondition",
  "MoveInDate",
  "MoveInDateAdjust",
  "ExitDate",
  "Destination",
  "EntryAdjust",
  "ExitAdjust"
)

vars_to_the_apps <- c(
  "ProjectType",
  "AltProjectName",
  "PersonalID",
  "EnrollmentID",
  "HouseholdID",
  "EntryDate",
  "MoveInDateAdjust",
  "ExitDate",
  "MeetsObjective"
)


# Project Evaluation cohorts ----------------------------------------------

# clients served during date range

pe_clients_served <-  co_clients_served %>%
  filter(served_between(., ReportStart, ReportEnd)) %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  inner_join(pe_coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(
    Enrollment %>%
      select(-UserID,-DateCreated,-DateUpdated,-DateDeleted,-ExportID),
    by = c(
      "PersonalID",
      "EnrollmentID",
      "ProjectID",
      "ProjectType",
      "ProjectName"
    )
  ) %>%
  select(all_of(vars_we_want)) %>%
  arrange(PersonalID, AltProjectID, desc(EntryDate)) %>%
  distinct(PersonalID, AltProjectName, .keep_all = TRUE) # no dupes w/in a project
# several measures will use this

# Checking for deceased hohs for points adjustments

hoh_exits_to_deceased <- pe_clients_served %>%
  filter(Destination == 24 &
           RelationshipToHoH == 1) %>%
  group_by(AltProjectID) %>%
  summarise(HoHDeaths = n()) %>%
  ungroup() %>%
  right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID")

hoh_exits_to_deceased[is.na(hoh_exits_to_deceased)] <- 0

# Adults who entered during date range

pe_adults_entered <-  co_adults_served %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  inner_join(pe_coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(
    Enrollment %>%
      select(-UserID, -DateCreated, -DateUpdated, -DateDeleted, -ExportID),
    by = c(
      "PersonalID",
      "EnrollmentID",
      "ProjectID",
      "ProjectType",
      "ProjectName"
    )
  ) %>%
  group_by(HouseholdID) %>%
  mutate(HHEntryDate = min(EntryDate)) %>%
  ungroup() %>%
  filter(entered_between(., ReportStart, ReportEnd) & 
           EntryDate == HHEntryDate) %>%
  select(all_of(vars_we_want)) %>%
  arrange(PersonalID, AltProjectID, desc(EntryDate))

# this one counts each entry 

## for vispdat measure

pe_hohs_entered <-  co_hohs_entered %>%
  filter(entered_between(., ReportStart, ReportEnd)) %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  inner_join(pe_coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(
    Enrollment %>%
      select(-UserID,-DateCreated,-DateUpdated,-DateDeleted,-ExportID),
    by = c(
      "PersonalID",
      "EnrollmentID",
      "ProjectID",
      "ProjectType",
      "ProjectName"
    )
  ) %>%
  select(all_of(vars_we_want)) %>%
  arrange(PersonalID, AltProjectID, desc(EntryDate))

# for ncb logic
# Adults who moved in and exited during date range

pe_adults_moved_in_leavers <-  co_adults_moved_in_leavers %>%
  filter(
    stayed_between(., ReportStart, ReportEnd) &
      exited_between(., ReportStart, ReportEnd)
  ) %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  inner_join(pe_coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(
    Enrollment %>%
      select(-UserID,-DateCreated,-DateUpdated,-DateDeleted,-ExportID),
    by = c(
      "PersonalID",
      "EnrollmentID",
      "ProjectID",
      "ProjectType",
      "ProjectName"
    )
  ) %>%
  select(all_of(vars_we_want)) %>%
  arrange(PersonalID, AltProjectID, desc(EntryDate)) %>%
  distinct(PersonalID, AltProjectName, .keep_all = TRUE) # no dupes w/in a project

# increase income
#Adults who moved in and were served during date range

pe_adults_moved_in <-  co_adults_moved_in %>%
  filter(stayed_between(., ReportStart, ReportEnd)) %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  inner_join(pe_coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(
    Enrollment %>%
      select(-UserID,-DateCreated,-DateUpdated,-DateDeleted,-ExportID),
    by = c(
      "PersonalID",
      "EnrollmentID",
      "ProjectID",
      "ProjectType",
      "ProjectName"
    )
  ) %>%
  select(all_of(vars_we_want)) %>%
  arrange(PersonalID, AltProjectID, desc(EntryDate)) %>%
  distinct(PersonalID, AltProjectName, .keep_all = TRUE) # no dupes w/in a project	

# health insurance
# Clients who moved in and exited during date range

pe_clients_moved_in_leavers <-  co_clients_moved_in_leavers %>%
  filter(stayed_between(., ReportStart, ReportEnd) &
           exited_between(., ReportStart, ReportEnd)) %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  inner_join(pe_coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(
    Enrollment %>%
      select(-UserID,-DateCreated,-DateUpdated,-DateDeleted,-ExportID),
    by = c(
      "PersonalID",
      "EnrollmentID",
      "ProjectID",
      "ProjectType",
      "ProjectName"
    )
  ) %>%
  select(all_of(vars_we_want)) %>%
  arrange(PersonalID, AltProjectID, desc(EntryDate)) %>%
  distinct(PersonalID, AltProjectName, .keep_all = TRUE) # no dupes w/in a project

# exits to PH, but needs an added filter of only mover-inners
# Heads of Household who were served during date range

pe_hohs_served <- co_hohs_served %>%
  filter(served_between(., ReportStart, ReportEnd)) %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  inner_join(pe_coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(
    Enrollment %>%
      select(-UserID,-DateCreated,-DateUpdated,-DateDeleted,-ExportID),
    by = c(
      "PersonalID",
      "EnrollmentID",
      "ProjectID",
      "ProjectType",
      "ProjectName"
    )
  ) %>%
  select(all_of(vars_we_want)) %>%
  arrange(PersonalID, AltProjectID, desc(EntryDate)) %>%
  distinct(PersonalID, AltProjectName, .keep_all = TRUE) # no dupes w/in a project	

pe_hohs_served_leavers <- pe_hohs_served %>%
  filter(!is.na(ExitDate))

# own housing and LoS
# Heads of Household who moved in and exited during date range

pe_hohs_moved_in_leavers <-  co_hohs_moved_in_leavers %>%
  filter(stayed_between(., ReportStart, ReportEnd) &
           exited_between(., ReportStart, ReportEnd)) %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  inner_join(pe_coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(
    Enrollment %>%
      select(-UserID,-DateCreated,-DateUpdated,-DateDeleted,-ExportID),
    by = c(
      "PersonalID",
      "EnrollmentID",
      "ProjectID",
      "ProjectType",
      "ProjectName"
    )
  ) %>%
  select(all_of(vars_we_want)) %>%
  arrange(PersonalID, AltProjectID, desc(EntryDate)) %>%
  distinct(PersonalID, AltProjectName, .keep_all = TRUE) # no dupes w/in a project

# Create Validation Summary -----------------------------------------------

summary_pe_hohs_moved_in_leavers <- pe_hohs_moved_in_leavers %>%
  group_by(AltProjectID) %>%
  summarise(HoHsMovedInLeavers = n()) %>%
  ungroup() %>%
  right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID") %>%
  mutate(HoHsMovedInLeavers = if_else(is.na(HoHsMovedInLeavers),
                                      as.integer(0),
                                      HoHsMovedInLeavers))

summary_pe_adults_moved_in_leavers <- pe_adults_moved_in_leavers %>%
  group_by(AltProjectID) %>%
  summarise(AdultMovedInLeavers = n()) %>%
  right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID") %>%
  mutate(AdultMovedInLeavers = if_else(is.na(AdultMovedInLeavers),
                                       as.integer(0),
                                       AdultMovedInLeavers))

summary_pe_adults_moved_in <- pe_adults_moved_in %>%
  group_by(AltProjectID) %>%
  summarise(AdultsMovedIn = n()) %>%
  ungroup() %>%
  right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID") %>%
  mutate(AdultsMovedIn = if_else(is.na(AdultsMovedIn),
                                 as.integer(0),
                                 AdultsMovedIn))

summary_pe_clients_moved_in_leavers <- pe_clients_moved_in_leavers %>%
  group_by(AltProjectID) %>%
  summarise(ClientsMovedInLeavers = n()) %>%
  ungroup() %>%
  right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID") %>%
  mutate(ClientsMovedInLeavers = if_else(is.na(ClientsMovedInLeavers),
                                         as.integer(0),
                                         ClientsMovedInLeavers))

summary_pe_hohs_served <- pe_hohs_served %>%
  group_by(AltProjectID) %>%
  summarise(HoHsServed = n()) %>%
  ungroup() %>%
  right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID") %>%
  mutate(HoHsServed = if_else(is.na(HoHsServed),
                              as.integer(0),
                              HoHsServed))

summary_pe_hohs_served_leavers <- pe_hohs_served_leavers %>%
  group_by(AltProjectID) %>%
  summarise(HoHsServedLeavers = n()) %>%
  ungroup() %>%
  right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID") %>%
  mutate(HoHsServedLeavers = if_else(is.na(HoHsServedLeavers),
                              as.integer(0),
                              HoHsServedLeavers))

summary_pe_clients_served <- pe_clients_served %>%
  group_by(AltProjectID) %>%
  summarise(ClientsServed = n()) %>%
  ungroup() %>%
  right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID") %>%
  mutate(ClientsServed = if_else(is.na(ClientsServed),
                                 as.integer(0),
                                 ClientsServed))

summary_pe_adults_entered <- pe_adults_entered %>%
  group_by(AltProjectID) %>%
  summarise(AdultsEntered = n()) %>%
  ungroup() %>%
  right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID") %>%
  mutate(AdultsEntered = if_else(is.na(AdultsEntered),
                                 as.integer(0),
                                 AdultsEntered))

summary_pe_hohs_entered <- pe_hohs_entered %>%
  group_by(AltProjectID) %>%
  summarise(HoHsEntered = n()) %>%
  ungroup() %>%
  right_join(pe_coc_funded["AltProjectID"] %>% unique(), by = "AltProjectID") %>%
  mutate(HoHsEntered = if_else(is.na(HoHsEntered),
                               as.integer(0),
                               HoHsEntered))

pe_validation_summary <- summary_pe_adults_entered %>%
  full_join(summary_pe_adults_moved_in, by = "AltProjectID") %>%
  full_join(summary_pe_hohs_served_leavers, by = "AltProjectID") %>%
  full_join(summary_pe_adults_moved_in_leavers, by = "AltProjectID") %>%
  full_join(summary_pe_clients_served, by = "AltProjectID") %>%
  full_join(summary_pe_clients_moved_in_leavers, by = "AltProjectID") %>%
  full_join(summary_pe_hohs_moved_in_leavers, by = "AltProjectID") %>%
  full_join(summary_pe_hohs_served, by = "AltProjectID") %>%
  full_join(summary_pe_hohs_entered, by = "AltProjectID") %>%
  left_join(pe_coc_funded %>% 
              select(AltProjectID, ProjectType, AltProjectName) %>% 
              unique(), by = c("AltProjectID")) %>%
  left_join(hoh_exits_to_deceased, by = "AltProjectID") %>%
  select(
    ProjectType,
    AltProjectID,
    AltProjectName,
    ClientsServed,
    HoHsEntered,
    HoHsServed,
    HoHsServedLeavers,
    HoHDeaths,
    AdultsMovedIn,
    AdultsEntered,
    ClientsMovedInLeavers,
    AdultMovedInLeavers,
    HoHsMovedInLeavers
  )

rm(list = ls(pattern = "summary_"))

# Finalizing DQ Flags -----------------------------------------------------
dq_flags_staging <- dq_2019 %>%
  right_join(pe_coc_funded, by = c("ProjectType", "ProjectID", "ProjectName")) %>%
  mutate(
    GeneralFlag =
      if_else(
        Issue %in% c(
          "Duplicate Entry Exits",
          # "Incorrect Entry Exit Type", (should have been included in 2020 but wasn't)
          "Children Only Household",
          "No Head of Household",
          "Too Many Heads of Household"
        ),
        1,
        0
      ),
    BenefitsFlag =
      if_else(
        Issue %in% c(
          "Non-cash Benefits Missing at Entry",
          "Conflicting Non-cash Benefits yes/no at Entry"
        ),
        1,
        0
      ),
    IncomeFlag =
      if_else(
        Issue %in% c("Income Missing at Entry",
                     "Conflicting Income yes/no at Entry"),
        1,
        0
      ),
    LoTHFlag =
      if_else(
        Issue %in% c("Missing Residence Prior",
                     "Missing Months or Times Homeless"),
        1,
        0
      )
  ) %>% 
  select(AltProjectName, 
         PersonalID, 
         HouseholdID,
         GeneralFlag, 
         BenefitsFlag, 
         IncomeFlag, 
         LoTHFlag) %>%
  filter(
    GeneralFlag + BenefitsFlag + IncomeFlag + LoTHFlag > 0
  ) %>% 
  group_by(AltProjectName) %>%
  summarise(GeneralFlagTotal = sum(GeneralFlag),
            BenefitsFlagTotal = sum(BenefitsFlag),
            IncomeFlagTotal = sum(IncomeFlag),
            LoTHFlagTotal = sum(LoTHFlag))

data_quality_flags_detail <- pe_validation_summary %>%
  left_join(dq_flags_staging, by = "AltProjectName") %>%
  mutate(General_DQ = if_else(GeneralFlagTotal/ClientsServed >= .02, 1, 0),
         Benefits_DQ = if_else(BenefitsFlagTotal/AdultsEntered >= .02, 1, 0),
         Income_DQ = if_else(IncomeFlagTotal/AdultsEntered >= .02, 1, 0),
         LoTH_DQ = if_else(LoTHFlagTotal/HoHsServed >= .02, 1, 0))

data_quality_flags_detail[is.na(data_quality_flags_detail)] <- 0

data_quality_flags <- data_quality_flags_detail %>%
  select(AltProjectName, General_DQ, Benefits_DQ, Income_DQ, LoTH_DQ)

# CoC Scoring -------------------------------------------------------------
docs_due <- mdy("04012020")

lower_th <- 6000
upper_th <- 10000

lower_psh_sh <- 8000
upper_psh_sh <- 12000

lower_rrh <- 5000
upper_rrh <- 9000



summary_pe_coc_scoring <- pe_coc_funded %>%
  left_join(Project, by = c("ProjectType", "ProjectName", "ProjectID")) %>%
  select(
    ProjectType,
    ProjectID,
    AltProjectID,
    AltProjectName,
    CostPerExit,
    DateReceivedPPDocs,
    HousingFirstScore,
    ChronicPrioritizationScore,
    OnTrackSpendingScoring,
    UnspentFundsScoring
  ) %>%
  filter(!ProjectID %in% retired) %>%
  mutate(
    CostPerExitScore = case_when(
      (ProjectType == 2 &
         CostPerExit <= lower_th) |
        (ProjectType %in% c(3, 8) &
           CostPerExit <= lower_psh_sh) |
        (ProjectType == 13 &
           CostPerExit <= lower_rrh) ~ 0, # would be 5 but covid19
      (ProjectType == 2 &
         CostPerExit > lower_th &
         CostPerExit <= upper_th) |
        (
          ProjectType %in% c(3, 8) &
            CostPerExit > lower_psh_sh &
            CostPerExit <= upper_psh_sh
        ) |
        (
          ProjectType == 13 &
            CostPerExit > lower_rrh &
            CostPerExit <= upper_rrh
        ) ~ 0, # would be 2 but covid19
      (ProjectType == 2 &
         CostPerExit > upper_th) |
        (ProjectType %in% c(3, 8) &
           CostPerExit > upper_psh_sh) |
        (ProjectType == 13 &
           CostPerExit > upper_rrh) ~ 0 # <- naturally zero!
    ),
    CostPerExitPossible = 0, # would be 5, but virus
    CostPerExitMath = "NOT SCORED in 2020 due to COVID-19.",
    OnTrackSpendingPossible = 0, # would be 5, but virus
    OnTrackSpendingMath = "NOT SCORED in 2020 due to COVID-19.",
    UnspentFundsPossible = 0, # would be 5, but virus
    UnspentFundsMath = "NOT SCORED in 2020 due to COVID-19.",
    HousingFirstPossible = 0, # would be 15, but virus
    HousingFirstDQ = case_when(
      ymd(DateReceivedPPDocs) <= ymd(docs_due) &
        is.na(HousingFirstScore) ~ 3,
      is.na(DateReceivedPPDocs) &
        is.na(HousingFirstScore) ~ 2,
      is.na(DateReceivedPPDocs) &
        !is.na(HousingFirstScore) ~ 4,
      ymd(DateReceivedPPDocs) > ymd(docs_due) ~ 5
    ),
    HousingFirstScore = 0, 
    #   case_when(
    #   ymd(DateReceivedPPDocs) <= ymd(docs_due) &
    #     !is.na(HousingFirstScore) ~ HousingFirstScore,
    #   is.na(DateReceivedPPDocs) &
    #     is.na(HousingFirstScore) ~ -10L,
    #   ymd(DateReceivedPPDocs) > ymd(docs_due) ~ -10L
    # ),  # commented out due to covid19
    HousingFirstMath = "NOT SCORED in 2020 due to COVID-19.",
    ChronicPrioritizationDQ = case_when(
      ymd(DateReceivedPPDocs) <= ymd(docs_due) &
        is.na(ChronicPrioritizationScore) ~ 3,
      is.na(DateReceivedPPDocs) &
        is.na(ChronicPrioritizationScore) ~ 2,
      is.na(DateReceivedPPDocs) &
        !is.na(ChronicPrioritizationScore) ~ 4,
      ymd(DateReceivedPPDocs) > ymd(docs_due) ~ 5
    ),
    ChronicPrioritizationPossible = if_else(ProjectType == 3, 0, NULL), 
    # true would be 10, but virus
    ChronicPrioritizationScore = if_else(ProjectType == 3, 0, NULL), 
    #   case_when(
    #   ymd(DateReceivedPPDocs) <= ymd(docs_due) &
    #     ProjectType == 3 &
    #     !is.na(ChronicPrioritizationScore) ~ ChronicPrioritizationScore,
    #   is.na(DateReceivedPPDocs) &
    #     ProjectType == 3 &
    #     is.na(ChronicPrioritizationScore) ~ -5L,
    #   ymd(DateReceivedPPDocs) > ymd(docs_due) &
    #     ProjectType == 3 ~ -5L
    # ), # commented out due to covid19
    ChronicPrioritizationMath = "NOT SCORED in 2020 due to COVID-19."
  ) 

# 2 = Documents not yet received
# 3 = Docs received, not yet scored
# 4 = CoC Error
# 5 = Docs received past the due date

# Housing Stability: Exits to PH ------------------------------------------
# PSH (includes stayers tho), TH, SH, RRH

pe_exits_to_ph <- pe_hohs_served %>%
  left_join(data_quality_flags, by = "AltProjectName") %>%
  mutate(
    DestinationGroup = case_when(
      Destination %in% c(1, 2, 12, 13, 14, 16, 18, 27) ~ "Temporary",
      Destination %in% c(3, 10:11, 19:23, 28, 31, 33:34, 36) ~ "Permanent",
      Destination %in% c(4:7, 15, 25:27, 29) ~ "Institutional",
      Destination %in% c(8, 9, 17, 30, 99) ~ "Other",
      Destination == 24 ~ "Deceased",
      is.na(Destination) ~ "Still in Program"
    ),
    ExitsToPHDQ = case_when(
      General_DQ == 1 ~ 1,
      TRUE ~ 0
    ),
    MeetsObjective =
      case_when(
        ProjectType %in% c(3, 9) &
          DestinationGroup %in% c("Permanent", "Still in Program") ~ 1,
        ProjectType %in% c(3, 9) &
          (!DestinationGroup %in% c("Permanent", "Still in Program")) ~ 0,
        ProjectType %in% c(2, 8, 13) &
          DestinationGroup == "Permanent" ~ 1,
        ProjectType %in% c(2, 8, 13) &
          (DestinationGroup != "Permanent") ~ 0
      )
  ) %>%
  filter((ProjectType %in% c(2, 8, 13) & !is.na(ExitDate)) |
           ProjectType == 3) %>% # filtering out non-PSH stayers
  select(all_of(vars_to_the_apps), ExitsToPHDQ, Destination, DestinationGroup)

summary_pe_exits_to_ph <- pe_exits_to_ph %>%
  group_by(ProjectType, AltProjectName, ExitsToPHDQ) %>%
  summarise(ExitsToPH = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "AltProjectName")) %>%
  mutate(
    ExitsToPHCohort = "HoHsServedLeavers",
    HoHsServedLeavers = HoHsServedLeavers - HoHDeaths,
    HoHsServed = HoHsServed - HoHDeaths,
    ExitsToPH = if_else(is.na(ExitsToPH), 0, ExitsToPH),
    Structure = case_when(
      ProjectType == 3 ~ "80_90_12",
      ProjectType %in% c(2, 13) ~ "75_83_10",
      ProjectType == 8 ~ "67_75_10"
    ),
    ExitsToPHPercent = if_else(
      ProjectType == 3,
      ExitsToPH / HoHsServed,
      ExitsToPH / HoHsServedLeavers
    ),
    ExitsToPHMath = case_when(
      ProjectType == 3 & HoHsServed != 0 ~
        paste(
          ExitsToPH,
          "exits to permanent housing or retention in PSH /",
          HoHsServed,
          "heads of household =",
          percent(ExitsToPHPercent, accuracy = 1)
        ),
      ProjectType != 3 & HoHsServedLeavers != 0 ~
        paste(
          ExitsToPH,
          "exits to permanent housing /",
          HoHsServedLeavers,
          "heads of household leavers =",
          percent(ExitsToPHPercent, accuracy = 1)
        )
    ), 
    ExitsToPHPoints = if_else(
      (ProjectType == 3 &
         HoHsServed == 0) |
        (ProjectType != 3 &
           HoHsServedLeavers == 0),
      if_else(ProjectType == 3, 12, 10),
      pe_score(Structure, ExitsToPHPercent)
    ),
    ExitsToPHPossible = if_else(ProjectType == 3, 12, 10),
    ExitsToPHPoints = if_else(
      ExitsToPHDQ == 0 | is.na(ExitsToPHDQ),
      ExitsToPHPoints,
      0
    )
  ) %>%
  select(
    ProjectType,
    AltProjectName,
    ExitsToPH,
    ExitsToPHMath,
    ExitsToPHPercent,
    ExitsToPHPoints,
    ExitsToPHPossible,
    ExitsToPHDQ,
    ExitsToPHCohort
  )

# Housing Stability: Moved into Own Housing -------------------------------
# TH, SH, RRH

pe_own_housing <- pe_hohs_moved_in_leavers %>%
  left_join(data_quality_flags, by = "AltProjectName") %>%
  filter(ProjectType != 3) %>%
  mutate(
    MeetsObjective = case_when(
      Destination %in% c(3, 10:11, 19:21, 28, 31, 33:34) ~ 1,
      !Destination %in% c(3, 10:11, 19:21, 28, 31, 33:34) ~ 0
    ),
    OwnHousingDQ = case_when(
      General_DQ == 1 ~ 1,
      TRUE ~ 0
    ),
    DestinationGroup = case_when(
      Destination %in% c(1, 2, 12, 13, 14, 16, 18, 27) ~ "Temporary",
      Destination %in% c(3, 10:11, 19:21, 28, 31, 33:34) ~ "Household's Own Housing",
      Destination %in% c(22:23) ~ "Shared Housing",
      Destination %in% c(4:7, 15, 25:27, 29) ~ "Institutional",
      Destination %in% c(8, 9, 17, 30, 99, 32) ~ "Other",
      Destination == 24 ~ "Deceased",
      is.na(Destination) ~ "Still in Program"
    )
  ) %>% 
  select(all_of(vars_to_the_apps), OwnHousingDQ, Destination, DestinationGroup)

summary_pe_own_housing <- pe_own_housing %>%
  group_by(ProjectType, AltProjectName, OwnHousingDQ) %>%
  summarise(OwnHousing = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "AltProjectName")) %>%
  mutate(
    HoHsMovedInLeavers = HoHsMovedInLeavers - HoHDeaths,
    OwnHousing = if_else(is.na(OwnHousing), 0, OwnHousing),
    Structure = if_else(ProjectType != 3, "72_80_5", NULL),
    OwnHousingPercent = if_else(ProjectType != 3,
                                OwnHousing / HoHsMovedInLeavers,
                                NULL),
    OwnHousingMath = case_when(
      HoHsMovedInLeavers == 0 &
        ProjectType != 3 ~
        "All points granted because this project had 0 Heads of Household Leavers who Moved into Housing",
      ProjectType == 3 &
        (HoHsMovedInLeavers == 0 | HoHsMovedInLeavers != 0) ~ "",
      HoHsMovedInLeavers != 0 & ProjectType != 3 ~ paste(
        OwnHousing,
        "exited to their own permanent housing /",
        HoHsMovedInLeavers,
        "heads of household leavers who moved into housing =",
        percent(OwnHousingPercent, accuracy = 1)
      )
    ), 
    OwnHousingPoints = if_else(
      HoHsMovedInLeavers == 0 & ProjectType != 3,
      10,
      pe_score(Structure, OwnHousingPercent)
    ),
    OwnHousingPoints = if_else(is.nan(OwnHousingPercent) &
                                 ProjectType != 3, 5, OwnHousingPoints),
    OwnHousingPoints = case_when(OwnHousingDQ == 1 ~ 0, 
                                 is.na(OwnHousingDQ) |
                                   OwnHousingDQ == 0 ~ OwnHousingPoints),
    OwnHousingPoints = if_else(is.na(OwnHousingPoints), 0, OwnHousingPoints),
    OwnHousingPossible = if_else(ProjectType != 3, 5, NULL),
    OwnHousingCohort = "HoHsMovedInLeavers"
  ) %>%
  select(ProjectType,
         AltProjectName,
         OwnHousingCohort,
         OwnHousing,
         OwnHousingMath,
         OwnHousingPercent,
         OwnHousingPoints,
         OwnHousingPossible,
         OwnHousingDQ)

# Accessing Mainstream Resources: Benefits -----------------------------------
# PSH, TH, SH, RRH

pe_benefits_at_exit <- pe_adults_moved_in_leavers %>%
  left_join(data_quality_flags, by = "AltProjectName") %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    PersonalID,
    AltProjectName,
    EnrollmentID,
    ProjectType,
    HouseholdID,
    RelationshipToHoH,
    VeteranStatus,
    EntryDate,
    MoveInDateAdjust,
    AgeAtEntry,
    ExitDate,
    ExitAdjust,
    BenefitsFromAnySource,
    InsuranceFromAnySource,
    DataCollectionStage,
    General_DQ,
    Benefits_DQ
  ) %>%
  filter(DataCollectionStage == 3) %>%
  mutate(
    MeetsObjective =
      case_when(
        (BenefitsFromAnySource == 1 |
           InsuranceFromAnySource == 1) ~ 1,
        (
          BenefitsFromAnySource != 1 |
            is.na(BenefitsFromAnySource) &
            (InsuranceFromAnySource != 1 |
               is.na(InsuranceFromAnySource)) ~ 0
        )
      ),
    BenefitsAtExitDQ = if_else(General_DQ == 1 |
                                 Benefits_DQ == 1, 1, 0)
  ) %>% 
  select(
    all_of(vars_to_the_apps),
    BenefitsAtExitDQ,
    BenefitsFromAnySource,
    InsuranceFromAnySource
  )

summary_pe_benefits_at_exit <- pe_benefits_at_exit %>%
  group_by(ProjectType, AltProjectName, BenefitsAtExitDQ) %>%
  summarise(BenefitsAtExit = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "AltProjectName")) %>%
  mutate(
    BenefitsAtExit = if_else(is.na(BenefitsAtExit), 0, BenefitsAtExit),
    Structure = case_when(
      ProjectType == 8 ~ "67_75_10",
      ProjectType == 3 ~ "75_85_12", 
      TRUE ~ "75_85_10"),
    BenefitsAtExitPercent = BenefitsAtExit / AdultMovedInLeavers,
    BenefitsAtExitMath = if_else(
      AdultMovedInLeavers == 0,
      "All points granted because this project had no adult leavers who moved into the project's housing",
      paste(
        BenefitsAtExit,
        "exited with benefits or health insurance /",
        AdultMovedInLeavers,
        "adult leavers who moved into the project's housing =",
        percent(BenefitsAtExitPercent, accuracy = 1)
      )
    ), 
    BenefitsAtExitDQ = if_else(is.na(BenefitsAtExitDQ), 0, BenefitsAtExitDQ),
    BenefitsAtExitPoints = if_else(AdultMovedInLeavers == 0,
                               if_else(ProjectType == 3, 12, 10),
                               pe_score(Structure, BenefitsAtExit)),
    BenefitsAtExitPossible = if_else(ProjectType == 3, 12, 10),
    BenefitsAtExitPoints = case_when(
      BenefitsAtExitDQ == 1 ~ 0,
      is.na(BenefitsAtExitDQ) |
        BenefitsAtExitDQ == 0 ~ BenefitsAtExitPoints
    ), 
    BenefitsAtExitCohort = "AdultMovedInLeavers"
  ) %>%
  select(
    ProjectType,
    AltProjectName,
    BenefitsAtExitCohort,
    BenefitsAtExit,
    BenefitsAtExitMath,
    BenefitsAtExitPercent,
    BenefitsAtExitPoints,
    BenefitsAtExitPossible,
    BenefitsAtExitDQ
  ) 

# Accessing Mainstream Resources: Increase Total Income -------------------
# PSH, TH, SH, RRH

income_staging2 <-  pe_adults_moved_in %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(PersonalID,
         EnrollmentID,
         EntryDate,
         ExitDate,
         TotalMonthlyIncome,
         DateCreated,
         DataCollectionStage) %>%
  mutate(
    DataCollectionStage = case_when(
      DataCollectionStage == 1 ~ "Entry",
      DataCollectionStage == 2 ~ "Update",
      DataCollectionStage == 3 ~ "Exit",
      DataCollectionStage == 5 ~ "Annual"
    )
  )
  
income_staging_fixed <- income_staging2 %>% 
  filter(DataCollectionStage == "Entry") 

income_staging_variable <- income_staging2 %>%
  filter(DataCollectionStage %in% c("Update", "Annual", "Exit")) %>%
  group_by(EnrollmentID) %>%
  mutate(MaxUpdate = max(ymd_hms(DateCreated))) %>%
  filter(MaxUpdate == DateCreated) %>%
  select(-MaxUpdate) %>%
  distinct() %>%
  ungroup() 

income_staging <- rbind(income_staging_fixed, income_staging_variable) %>%
  select(PersonalID, EnrollmentID, TotalMonthlyIncome, DataCollectionStage) %>%
  unique()

pe_increase_income <- income_staging %>%
  pivot_wider(names_from = DataCollectionStage,
              values_from = TotalMonthlyIncome) %>%
  left_join(pe_adults_moved_in, by = c("PersonalID", "EnrollmentID")) %>%
  left_join(data_quality_flags, by = "AltProjectName") %>%
  mutate(
    MostRecentIncome = case_when(
      !is.na(Exit) ~ Exit,
      !is.na(Update) ~ Update,
      !is.na(Annual) ~ Annual
    ),
    IncomeAtEntry = if_else(is.na(Entry), 0, Entry),
    IncomeMostRecent = if_else(is.na(MostRecentIncome), 
                               IncomeAtEntry, 
                               MostRecentIncome),
    MeetsObjective = case_when(
      IncomeMostRecent > IncomeAtEntry ~ 1,
      IncomeMostRecent <= IncomeAtEntry ~ 0),
    IncreasedIncomeDQ = if_else(General_DQ == 1 |
                                  Income_DQ == 1, 1, 0)
  ) %>%  
  select(
    all_of(vars_to_the_apps),
    IncreasedIncomeDQ,
    IncomeAtEntry,
    IncomeMostRecent
  )

rm(list = ls(pattern = "income_staging"))

summary_pe_increase_income <- pe_increase_income %>%
  group_by(ProjectType, AltProjectName, IncreasedIncomeDQ) %>%
  summarise(IncreasedIncome = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "AltProjectName")) %>%
  mutate(
    IncreasedIncome = if_else(is.na(IncreasedIncome), 0, IncreasedIncome),
    Structure = case_when(
      ProjectType == 3 ~ "24_30_11",
      ProjectType == 2 ~ "22_28_10",
      ProjectType == 8 ~ "16_20_10",
      ProjectType == 13 ~ "14_18_10"
    ),
    IncreasedIncomePercent = IncreasedIncome / AdultsMovedIn,
    IncreasedIncomeMath = if_else(
      AdultsMovedIn != 0,
      paste(
        IncreasedIncome,
        "increased income during their stay /",
        AdultsMovedIn,
        "adults who moved into the project's housing =",
        percent(IncreasedIncomePercent, accuracy = 1)
      ),
      "All points granted because 0 adults moved into the project's housing"
    ), 
    IncreasedIncomePoints = case_when(
      IncreasedIncomeDQ == 1 ~ 0,
      AdultsMovedIn > 0 ~ pe_score(Structure, IncreasedIncomePercent),
      ProjectType == 3 &
      AdultsMovedIn == 0 &
        (IncreasedIncomeDQ == 0 | is.na(IncreasedIncomeDQ)) ~ 11,
      ProjectType != 3 &
        AdultsMovedIn == 0 &
        (IncreasedIncomeDQ == 0 | is.na(IncreasedIncomeDQ)) ~ 10
    ), 
    IncreasedIncomePossible = if_else(ProjectType == 3, 11, 10),
    IncreasedIncomeCohort = "AdultsMovedIn"
  ) %>%
  select(
    ProjectType,
    AltProjectName,
    IncreasedIncome,
    IncreasedIncomeCohort,
    IncreasedIncomeMath,
    IncreasedIncomePercent,
    IncreasedIncomePoints,
    IncreasedIncomePossible,
    IncreasedIncomeDQ
  ) 

# Housing Stability: Length of Time Homeless ------------------------------
# TH, SH, RRH

pe_length_of_stay <- pe_clients_moved_in_leavers %>%
  left_join(data_quality_flags, by = "AltProjectName") %>%
  mutate(DaysInProject = difftime(ymd(ExitAdjust), ymd(EntryDate))) %>%
  select(ProjectType,
         AltProjectName,
         General_DQ,
         EntryDate,
         EntryAdjust,
         MoveInDateAdjust,
         ExitDate,
         DaysInProject,
         PersonalID,
         EnrollmentID,
         HouseholdID)

summary_pe_length_of_stay <- pe_length_of_stay %>%
  group_by(ProjectType, AltProjectName, General_DQ) %>%
  summarise(
    AverageDays = as.numeric(mean(DaysInProject))
  ) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "AltProjectName")) %>%
  mutate(
    Structure = case_when(
      ProjectType == 2 ~ "200_280_10",
      ProjectType == 8 ~ "260_340_10",
      ProjectType == 13 ~ "0_730_10"
    ),
    AverageLoSPoints = case_when(
      ClientsMovedInLeavers == 0 &
        ProjectType != 3 ~ 10,
      TRUE ~ pe_score(Structure, AverageDays)
    ),
    AverageLoSMath = if_else(
      ClientsMovedInLeavers == 0,
      "All points granted because this project had 0 leavers who moved into the project's housing",
      paste(as.integer(AverageDays), "average days")
    ), 
    AverageLoSPossible = if_else(ProjectType %in% c(2, 8, 13), 10, NULL),
    AverageLoSDQ = case_when(
      General_DQ == 1 & ProjectType %in% c(2, 8, 13) ~ 1,
      General_DQ == 0 & ProjectType %in% c(2, 8, 13) ~ 0),
    AverageLoSPoints = case_when(
      AverageLoSDQ == 1 ~ 0, 
      AverageLoSDQ == 0 | is.na(AverageLoSDQ) ~ AverageLoSPoints),
    AverageLoSPoints = if_else(is.na(AverageLoSPoints), 0, AverageLoSPoints),
    AverageLoSCohort = "ClientsMovedInLeavers"
  ) %>%
  select(ProjectType, AltProjectName, AverageLoSMath, AverageLoSCohort, 
         AverageLoSPoints, AverageLoSPossible, AverageLoSDQ)

# Community Need: Res Prior = Streets or ESSH -----------------------------
# PSH, TH, SH (Street only), RRH

pe_res_prior <- pe_adults_entered %>%
  left_join(data_quality_flags, by = "AltProjectName") %>%
  filter(ProjectType %in% c(2, 3, 13, 8)) %>%
  mutate(LHResPriorDQ = if_else(General_DQ == 1, 1, 0),
         MeetsObjective = if_else(
           (ProjectType %in% c(2, 3, 13) &
              LivingSituation %in% c(1, 16, 18)) |
             (ProjectType == 8 &
                LivingSituation == 16),
           1,
           0
         )) %>% 
  select(all_of(vars_to_the_apps), LivingSituation, LHResPriorDQ)

summary_pe_res_prior <- pe_res_prior %>%
  group_by(ProjectType, AltProjectName, LHResPriorDQ) %>%
  summarise(LHResPrior = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary,
             by = c("ProjectType",
                    "AltProjectName")) %>%
  mutate(
    LHResPrior = if_else(is.na(LHResPrior), 0, LHResPrior),
    Structure = case_when(
      ProjectType %in% c(3, 13) ~ "75_85_10",
      ProjectType == 2 ~ "67_75_10",
      ProjectType == 8 ~ "0_100_10"
    ),
    LHResPriorPercent = LHResPrior / AdultsEntered,
    LHResPriorMath = if_else(
      AdultsEntered == 0,
      "All points granted because this project has 0 adults who entered the project",
      paste(
        LHResPrior,
        "coming from shelter or streets (unsheltered) /",
        AdultsEntered,
        "adults who entered the project during the reporting period =",
        percent(LHResPriorPercent, accuracy = 1)
      )
    ), 
    LHResPriorDQ = if_else(is.na(LHResPriorDQ), 0, LHResPriorDQ),
    LHResPriorPoints = if_else(AdultsEntered == 0,
                               10,
                               pe_score(Structure, LHResPriorPercent)),
    LHResPriorPoints = case_when(
      LHResPriorDQ == 1 ~ 0, 
      LHResPriorDQ == 0 | is.na(LHResPriorDQ) ~ LHResPriorPoints),
    LHResPriorPossible = 10,
    LHResPriorCohort = "AdultsEntered"
  ) %>%
  select(
    ProjectType,
    AltProjectName,
    LHResPrior,
    LHResPriorCohort,
    LHResPriorMath,
    LHResPriorPercent,
    LHResPriorPoints,
    LHResPriorPossible,
    LHResPriorDQ
  ) 

# Community Need: Entries with No Income ----------------------------------
# PSH, TH, SH, RRH

pe_entries_no_income <- pe_adults_entered %>%
  left_join(data_quality_flags, by = "AltProjectName") %>%
  filter(ProjectType %in% c(2, 3, 13, 8)) %>%
  left_join(IncomeBenefits %>%
              select(EnrollmentID, 
                     InformationDate,
                     IncomeFromAnySource) %>%
              unique(), 
            by = c("EnrollmentID", "EntryDate" = "InformationDate")) %>%
  mutate(
    IncomeFromAnySource = if_else(is.na(IncomeFromAnySource),
                                  99,
                                  IncomeFromAnySource),
    MeetsObjective = if_else(IncomeFromAnySource == 0, 1, 0),
    NoIncomeAtEntryDQ = if_else(General_DQ == 1 |
                                  Income_DQ == 1, 1, 0)
  ) %>% 
  select(all_of(vars_to_the_apps), IncomeFromAnySource, NoIncomeAtEntryDQ)

summary_pe_entries_no_income <- pe_entries_no_income %>%
  group_by(ProjectType, AltProjectName, NoIncomeAtEntryDQ) %>%
  summarise(NoIncomeAtEntry = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "AltProjectName")) %>%
  mutate(
    NoIncomeAtEntry = if_else(is.na(NoIncomeAtEntry),
                              0,
                              NoIncomeAtEntry),
    Structure = if_else(ProjectType != 2, "34_40_10", "24_30_10"),
    NoIncomeAtEntryDQ = if_else(is.na(NoIncomeAtEntryDQ), 0, NoIncomeAtEntryDQ),
    NoIncomeAtEntryPercent = NoIncomeAtEntry / AdultsEntered,
    NoIncomeAtEntryMath = if_else(
      AdultsEntered == 0,
      "All points granted because 0 adults entered this project during the reporting period",
      paste(
        NoIncomeAtEntry,
        "had no income at entry /",
        AdultsEntered,
        "adults who entered the project during the reporting period =",
        percent(NoIncomeAtEntryPercent, accuracy = 1)
      )
    ), 
    NoIncomeAtEntryPoints = if_else(AdultsEntered == 0, 10,
                     pe_score(Structure, NoIncomeAtEntryPercent)),
    NoIncomeAtEntryPossible = 10,
    NoIncomeAtEntryPoints = case_when(
      NoIncomeAtEntryDQ == 1 ~ 0,
      NoIncomeAtEntryDQ == 0 |
        is.na(NoIncomeAtEntryDQ) ~ NoIncomeAtEntryPoints
    ),
    NoIncomeAtEntryCohort = "AdultsEntered"
  ) %>%
  select(
    ProjectType,
    AltProjectName,
    NoIncomeAtEntry,
    NoIncomeAtEntryCohort,
    NoIncomeAtEntryMath,
    NoIncomeAtEntryPercent,
    NoIncomeAtEntryPoints,
    NoIncomeAtEntryPossible,
    NoIncomeAtEntryDQ
  )

# Community Need: Homeless History Index ----------------------------------
# PSH, TH, SH, RRH

pe_homeless_history_index <- pe_adults_entered %>%
  select(
    ProjectType,
    AltProjectName,
    PersonalID,
    EnrollmentID,
    HouseholdID,
    AgeAtEntry,
    VeteranStatus,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    DateToStreetESSH,
    TimesHomelessPastThreeYears,
    MonthsHomelessPastThreeYears
  ) %>%
  left_join(data_quality_flags, by = "AltProjectName") %>%
  mutate(
    DaysHomelessAtEntry = if_else(
      ymd(EntryDate) >= ymd(DateToStreetESSH),
      difftime(EntryDate,
               DateToStreetESSH,
               units = "days"),
      NULL
    ),
    HHI = case_when(
      DaysHomelessAtEntry > 364 |
        (
          MonthsHomelessPastThreeYears %in% c(112, 113) &
            TimesHomelessPastThreeYears == 4
        )  ~ 7,
      DaysHomelessAtEntry <= 364 &
        ((
          MonthsHomelessPastThreeYears %in% c(112, 113) &
            TimesHomelessPastThreeYears %in% c(1, 2, 3)
        ) |
          (
            MonthsHomelessPastThreeYears %in% c(109, 110, 111) &
              TimesHomelessPastThreeYears == 4
          )
        ) ~ 6,
      DaysHomelessAtEntry <= 364 &
        ((
          MonthsHomelessPastThreeYears %in% c(112, 113) &
            (
              TimesHomelessPastThreeYears %in% c(8, 9, 99) |
                is.na(TimesHomelessPastThreeYears)
            )
        ) |
          (
            MonthsHomelessPastThreeYears %in% c(109, 110, 111) &
              TimesHomelessPastThreeYears %in% c(1, 2, 3)
          )
        ) ~ 5,
      DaysHomelessAtEntry <= 364 &
        ((
          MonthsHomelessPastThreeYears %in% c(105, 106, 107, 108) &
            TimesHomelessPastThreeYears %in% c(2, 3, 4)
        ) |
          (
            MonthsHomelessPastThreeYears %in% c(109, 110, 111) &
              (
                TimesHomelessPastThreeYears %in% c(8, 9, 99) |
                  is.na(TimesHomelessPastThreeYears)
              )
          )
        ) ~ 4,
      DaysHomelessAtEntry <= 364 &
        ((
          MonthsHomelessPastThreeYears %in% c(102, 103, 104) &
            TimesHomelessPastThreeYears == 4
        ) |
          (
            MonthsHomelessPastThreeYears %in% c(105, 106, 107, 108) &
              (
                TimesHomelessPastThreeYears %in% c(8, 9, 99, 1) |
                  is.na(TimesHomelessPastThreeYears)
              )
          )
        ) ~ 3,
      DaysHomelessAtEntry <= 364 &
        (((
          is.na(TimesHomelessPastThreeYears) |
            MonthsHomelessPastThreeYears %in% c(8, 9, 99)
        ) &
          TimesHomelessPastThreeYears == 4
        ) |
          (
            MonthsHomelessPastThreeYears == 101 &
              TimesHomelessPastThreeYears %in% c(2, 3, 4)
          ) |
          (
            MonthsHomelessPastThreeYears %in% c(102, 103, 104) &
              (
                TimesHomelessPastThreeYears %in% c(1, 2, 3, 8, 9, 99) |
                  is.na(TimesHomelessPastThreeYears)
              )
          )
        ) ~ 2,
      DaysHomelessAtEntry <= 364 &
        ((
          MonthsHomelessPastThreeYears == 101 &
            (
              is.na(TimesHomelessPastThreeYears) |
                TimesHomelessPastThreeYears %in% c(1, 8, 9, 99)
            )
        ) |
          ((
            is.na(MonthsHomelessPastThreeYears) |
              MonthsHomelessPastThreeYears %in% c(8, 9, 99)
          ) &
            TimesHomelessPastThreeYears %in% c(1, 2, 3)
          )
        ) ~ 1,
      DaysHomelessAtEntry <= 364 &
        ((
          is.na(MonthsHomelessPastThreeYears) |
            MonthsHomelessPastThreeYears %in% c(8, 9, 99)
        ) &
          (
            TimesHomelessPastThreeYears %in% c(8, 9, 99) |
              is.na(TimesHomelessPastThreeYears)
          )
        ) ~ 0,
      TRUE ~ 0
    )
  )

summary_pe_homeless_history_index <- pe_homeless_history_index %>%
  group_by(ProjectType, AltProjectName, General_DQ) %>%
  summarise(MedHHI = median(HHI)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "AltProjectName")) %>%
  mutate(
    Structure = if_else(ProjectType != 3, "0_7_10", "0_7_10_PSH"),
    MedianHHIMath = if_else(
      AdultsEntered == 0,
      "All points granted since 0 adults entered this project during the reporting period",
      paste("Median Homeless History Index = ", MedHHI)
    ), 
    MedianHHIPoints = if_else(AdultsEntered == 0, 10,
                           pe_score(Structure, MedHHI)),
    MedianHHIPossible = 10,
    MedianHHIDQ = if_else(General_DQ == 1, 1, 0),
    MedianHHIDQ = if_else(is.na(MedianHHIDQ), 0, MedianHHIDQ),
    MedianHHIPoints = case_when(MedianHHIDQ == 1 ~ 0, 
                                MedianHHIDQ == 0 | is.na(MedianHHIDQ) ~ MedianHHIPoints),
    MedianHHICohort = "AdultsEntered"
  ) %>%
  select(ProjectType,
         AltProjectName,
         MedHHI,
         MedianHHIMath,
         MedianHHICohort,
         MedianHHIPoints,
         MedianHHIPossible,
         MedianHHIDQ)

# HMIS Data Quality -------------------------------------------------------
# PSH, TH, SH, RRH

pe_dq <- dq_2019 %>%
  filter(Type %in% c("Error", "High Priority") & 
           ProjectType %in% c(2, 3, 13, 8)) %>%
  inner_join(pe_coc_funded, by = c("ProjectName", "ProjectID", "ProjectType"))

summary_pe_dq <- pe_dq %>% 
  group_by(AltProjectName, ProjectType) %>%
  count() %>%
  ungroup()

summary_pe_dq <- pe_validation_summary %>%
  select(AltProjectName, ProjectType, ClientsServed) %>%
  left_join(summary_pe_dq, by = c("ProjectType", "AltProjectName"))

summary_pe_dq[is.na(summary_pe_dq)] <- 0

summary_pe_dq <- summary_pe_dq %>%
  mutate(DQPercent = n / ClientsServed,
         DQMath = paste(n,
                      "errors /",
                      ClientsServed,
                      "clients served =",
                      percent(DQPercent, accuracy = 1)), 
         DQPoints = case_when(
           n == 0 ~ 5,
           DQPercent > 0 & DQPercent <= .02 ~ 4,
           DQPercent > .02 & DQPercent <= .05 ~ 3,
           DQPercent > .05 & DQPercent <= .08 ~ 2,
           DQPercent > .08 & DQPercent <= .1 ~ 1,
           DQPercent > .1 ~ 0
           ),
         DQPossible = 5,
         DQCohort = "ClientsServed"
         ) %>%
  select(AltProjectName, ProjectType, "DQIssues" = n, DQCohort, DQPercent, 
         DQPoints, DQMath, DQPossible)

# Community Need: Long Term Homeless Households ---------------------------
# PSH
# Decided in Feb meeting that we're going to use Adults Entered for this one

pe_long_term_homeless <- pe_adults_entered %>%
  # left_join(pe_coc_funded %>%
  #             select("AltProjectName", "ProjectType", "AltProjectID") %>%
  #             unique(), 
  #           by = c("ProjectType", "AltProjectID", "AltProjectName")) %>%
  left_join(data_quality_flags, by = "AltProjectName") %>%
  mutate(
    CurrentHomelessDuration = difftime(ymd(EntryDate), ymd(DateToStreetESSH),
                                       units = "days"),
    MeetsObjective = if_else((
      CurrentHomelessDuration >= 365 &
        !is.na(CurrentHomelessDuration)
    ) |
      (
        TimesHomelessPastThreeYears == 4 &
          MonthsHomelessPastThreeYears %in% c(112, 113) &
          !is.na(TimesHomelessPastThreeYears) &
          !is.na(MonthsHomelessPastThreeYears)
      ),
    1,
    0
    ),
    LTHomelessDQ = if_else(ProjectType == 3 & General_DQ == 1, 1, 0)
  ) %>%
  select(all_of(vars_to_the_apps), DateToStreetESSH, 
         CurrentHomelessDuration, MonthsHomelessPastThreeYears, 
         TimesHomelessPastThreeYears, LTHomelessDQ)

summary_pe_long_term_homeless <- pe_long_term_homeless %>%
  group_by(ProjectType, AltProjectName, LTHomelessDQ) %>%
  summarise(LongTermHomeless = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "AltProjectName")) %>%
  mutate(
    LongTermHomeless = if_else(is.na(LongTermHomeless),
                              0,     
                              LongTermHomeless),
    Structure = if_else(ProjectType == 3, "20_90_10", NULL),
    LongTermHomelessPercent = if_else(AdultsEntered > 0,
                                      LongTermHomeless / AdultsEntered,
                                      NULL),    
    LongTermHomelessMath = if_else(
      AdultsEntered == 0,
      "All points granted because 0 adults entered this project during the reporting period",
      paste(
        LongTermHomeless,
        "considered to be long-term homeless /",
        AdultsEntered,
        "adults entered the project during the reporting period =",
        percent(LongTermHomelessPercent, accuracy = 1)
      )
    ), 
    LongTermHomelessPoints = if_else(AdultsEntered == 0 &
                                       ProjectType == 3, 10,
                     pe_score(Structure, LongTermHomelessPercent)), 
    LongTermHomelessPoints = case_when(LTHomelessDQ == 0 |
                                         is.na(LTHomelessDQ) ~ LongTermHomelessPoints,
                                       LTHomelessDQ == 1 ~ 0), 
    LongTermHomelessPoints = if_else(is.na(LongTermHomelessPoints), 0, 
                                     LongTermHomelessPoints),
    LongTermHomelessPossible = if_else(ProjectType == 3, 10, NULL),
    LongTermhomelessCohort = "AdultsEntered"
  ) %>%
  select(
    ProjectType,
    AltProjectName,
    LongTermHomeless,
    LongTermHomelessPercent,
    LongTermHomelessPoints,
    LongTermHomelessMath,
    LongTermhomelessCohort,
    LongTermHomelessPossible,
    LTHomelessDQ
  ) 

# VISPDATs at Entry into PH -----------------------------------------------

pe_scored_at_ph_entry <- pe_hohs_entered %>%
  right_join(pe_coc_funded %>% 
               select(ProjectType, AltProjectID, AltProjectName) %>%
               unique(), 
             by = c("AltProjectName", "ProjectType", "AltProjectID")) %>%
  left_join(data_quality_flags, by = c("AltProjectName")) %>%
  left_join(
    dq_2019 %>%
      filter(Issue == "Non-DV HoHs Entering PH or TH without SPDAT") %>%
      select("PersonalID", "HouseholdID", "Issue"),
    by = c("PersonalID", "HouseholdID")
  ) %>%
  filter(ProjectType != 8) %>%
  mutate(
    MeetsObjective = case_when(
      !is.na(PersonalID) & is.na(Issue) & ProjectType %in% c(2, 3, 13) ~ 1, 
      !is.na(PersonalID) & !is.na(Issue) & ProjectType %in% c(2, 3, 13) ~ 0,
      is.na(PersonalID) & is.na(Issue) & ProjectType %in% c(2, 3, 13) ~ 1),
    ScoredAtEntryDQ = case_when(
      ProjectType %in% c(2, 3, 13) & General_DQ == 1 ~ 1, 
      ProjectType %in% c(2, 3, 13) & General_DQ == 0 ~ 0)
  ) %>%
  select(all_of(vars_to_the_apps), ScoredAtEntryDQ)

summary_pe_scored_at_ph_entry <- pe_scored_at_ph_entry %>%
  group_by(ProjectType, AltProjectName, ScoredAtEntryDQ) %>%
  summarise(ScoredAtEntry = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "AltProjectName")) %>%
  mutate(
    ScoredAtEntry = if_else(is.na(ScoredAtEntry),
                            0,
                            ScoredAtEntry), 
    Structure = if_else(ProjectType %in% c(2, 3, 13), "90_100_5", NULL),
    ScoredAtEntryPercent = if_else(HoHsEntered > 0,
                                   ScoredAtEntry / HoHsEntered,
                                   NULL),    
    ScoredAtEntryMath = if_else(
      HoHsEntered == 0,
      "All points granted because 0 households entered the project during the reporting period",
      paste(
        ScoredAtEntry,
        "had a VI-SPDAT score at entry /",
        HoHsEntered,
        "heads of household who entered the project during the reporting period =",
        percent(ScoredAtEntryPercent, accuracy = 1)
      )
    ), 
    ScoredAtEntryPoints = case_when(
      HoHsEntered == 0 &
        ProjectType %in% c(2, 3, 13) ~ 5,
      HoHsEntered > 0 &
        ProjectType %in% c(2, 3, 13) ~ pe_score(Structure, ScoredAtEntryPercent)),
    ScoredAtEntryPoints = case_when(
      ScoredAtEntryDQ == 0 ~ ScoredAtEntryPoints,
      ScoredAtEntryDQ == 1 ~ 0,
      is.na(ScoredAtEntryDQ) ~ ScoredAtEntryPoints),
    ScoredAtEntryPoints = if_else(is.na(ScoredAtEntryPoints), 
                                  0, 
                                  ScoredAtEntryPoints),
    ScoredAtEntryPossible = if_else(ProjectType %in% c(2, 3, 13), 5, NULL),
    ScoredAtEntryCohort = "HoHsEntered"
  ) %>%
  select(
    ProjectType,
    AltProjectName,
    ScoredAtEntry,
    ScoredAtEntryMath,
    ScoredAtEntryPercent,
    ScoredAtEntryPoints,
    ScoredAtEntryCohort,
    ScoredAtEntryPossible,
    ScoredAtEntryDQ
  ) 

# Final Scoring -----------------------------------------------------------

summary_pe_final_scoring <-
  pe_coc_funded[c("ProjectType", "AltProjectName")] %>%
  unique() %>%
  left_join(summary_pe_dq, by = c("ProjectType", "AltProjectName")) %>%
  left_join(summary_pe_entries_no_income,
            by = c("ProjectType", "AltProjectName")) %>%
  left_join(summary_pe_exits_to_ph,
            by = c("ProjectType", "AltProjectName")) %>%
  left_join(summary_pe_scored_at_ph_entry,
            by = c("ProjectType", "AltProjectName")) %>%
  left_join(summary_pe_homeless_history_index,
            by = c("ProjectType", "AltProjectName")) %>%
  left_join(summary_pe_increase_income,
            by = c("ProjectType", "AltProjectName")) %>%
  left_join(summary_pe_length_of_stay,
            by = c("ProjectType", "AltProjectName")) %>%
  left_join(summary_pe_long_term_homeless,
            by = c("ProjectType", "AltProjectName")) %>%
  left_join(summary_pe_benefits_at_exit,
            by = c("ProjectType", "AltProjectName")) %>%
  left_join(summary_pe_own_housing,
            by = c("ProjectType", "AltProjectName")) %>%
  left_join(summary_pe_res_prior,
            by = c("ProjectType", "AltProjectName")) %>%
  left_join(summary_pe_coc_scoring, by = c("ProjectType", "AltProjectName"))

pe_final_scores <- summary_pe_final_scoring

pe_final_scores$HousingFirstScore[is.na(pe_final_scores$HousingFirstScore)] <- 0
pe_final_scores$ChronicPrioritizationScore[is.na(pe_final_scores$ChronicPrioritizationScore)] <- 0
pe_final_scores$OnTrackSpendingScoring[is.na(pe_final_scores$OnTrackSpendingScoring)] <- 0
pe_final_scores$UnspentFundsScoring[is.na(pe_final_scores$UnspentFundsScoring)] <- 0
pe_final_scores$CostPerExitScore[is.na(pe_final_scores$CostPerExitScore)] <- 0

pe_final_scores <- pe_final_scores %>%
  mutate(
    TotalScore = DQPoints +
      NoIncomeAtEntryPoints +
      ExitsToPHPoints +
      ScoredAtEntryPoints +
      MedianHHIPoints +
      IncreasedIncomePoints +
      AverageLoSPoints +
      LongTermHomelessPoints +
      BenefitsAtExitPoints +
      OwnHousingPoints +
      LHResPriorPoints +
      HousingFirstScore +
      ChronicPrioritizationScore +
      OnTrackSpendingScoring +
      UnspentFundsScoring +
      CostPerExitScore
  ) %>%
  select(ProjectType, 
         AltProjectName, 
         ends_with("Points"),
         ends_with("Score"),
         ends_with("Scoring"),
         TotalScore)

project_and_orgs <- Project %>%
  select(ProjectID, ProjectName, OrganizationID)  %>%
  left_join(Organization %>%
              select(OrganizationID, OrganizationName), by = "OrganizationID")

project_and_alt_project <- project_and_orgs %>%
  left_join(consolidations, by = c("ProjectID", "ProjectName"))

final_scores <- pe_final_scores %>%
  select(AltProjectName, TotalScore) %>%
  left_join(project_and_alt_project, by = c("AltProjectName" = "ProjectName")) %>%
  select(OrganizationName, AltProjectName, TotalScore) %>%
  mutate(OrganizationName = case_when(
    AltProjectName == "Butler County PSH Combined" ~ "Butler County Board of Commissioners",
    AltProjectName == "GLCAP PSH Combined" ~ "Great Lakes Community Action Partnership",
    AltProjectName == "Jefferson County SPC Combined" ~ "Coleman Professional Services",
    AltProjectName == "Lake SPC Combined" ~ "Lake County ADAMHS Board",
    AltProjectName == "Springfield SPC 1 Combined" ~ "City of Springfield Ohio",
    AltProjectName == "Trumbull SPC Vouchers Combined" ~ "Trumbull County Mental Health and Recovery Board",
    AltProjectName == "Warren SPC Combined" ~ "Warren Metropolitan Housing Authority", 
    AltProjectName == "One Eighty PSH Plus Care Combined" ~ "OneEighty Inc.",
    TRUE ~ OrganizationName
  )) %>%
  arrange(desc(TotalScore))

# Clean the House ---------------------------------------------------------

rm(list = ls()[!(ls() %in% c(
  'pe_adults_entered',
  'pe_adults_moved_in_leavers',
  'pe_benefits_at_exit',
  'pe_clients_served',
  'pe_coc_funded',
  'pe_coc_scoring',
  'pe_dq_by_provider',
  'pe_entries_no_income',
  'pe_exits_to_ph',
  'pe_final_scores',
  'pe_homeless_history_index',
  'pe_increase_income',
  'pe_length_of_stay',
  'pe_long_term_homeless',
  'pe_own_housing',
  'pe_res_prior',
  'pe_scored_at_ph_entry',
  'pe_validation_summary',
  'summary_pe_benefits_at_exit',
  'summary_pe_dq_by_provider',
  'summary_pe_entries_no_income',
  'summary_pe_exits_to_ph',
  'summary_pe_homeless_history_index',
  'summary_pe_increase_income',
  'summary_pe_length_of_stay',
  'summary_pe_long_term_homeless',
  'summary_pe_own_housing',
  'summary_pe_res_prior',
  'summary_pe_scored_at_ph_entry',
  'summary_pe_utilization',
  'summary_pe_final_scoring',
  'ReportStart',
  'ReportEnd',
  'living_situation',
  'final_scores'
))])

# next_thing_due <- tribble(
#   ~ DueDate, ~ Event,
#   "6/12/2020", "All HMIS Data in the Project Evaluation report corrected and finalized",
#   "6/19/2020", "COHHIO takes final snapshot of Project Evaluation data for final scoring",
#   "6/26/2020", "COHHIO releases preliminary CoC project ranking (renewals only)",
#   "7/8/2020", "Recipients submit appeals of project evaluation results and ranking to ohioboscoc@cohhio.org.",
#   "7/24/2020", "Ohio BoSCoC Steering Committee will communicate decisions about all received appeals",
#   "7/31/2020", "Final CoC project ranking released"
# ) %>%
#   mutate(
#     DueDate = mdy(DueDate),
#     ShowStart = lag(ymd(DueDate), n = 1L, order_by = DueDate),
#     ShowStart = if_else(is.na(ShowStart), today(), ShowStart + days(1)),
#     ShowEnd = ymd(DueDate),
#     DateRange = interval(ShowStart, ShowEnd)
#   ) %>%
#   filter(today() %within% DateRange) %>%
#   select(Event, DueDate)

zero_divisors <- pe_validation_summary %>%
  filter(ClientsServed == 0 |
           HoHsEntered == 0 |
           HoHsServed == 0 |
           HoHsServedLeavers == 0 |
           AdultsMovedIn == 0 |
           AdultsEntered == 0 |
           ClientsMovedInLeavers == 0 |
           AdultMovedInLeavers == 0 |
           HoHsMovedInLeavers == 0) %>%
  select(-HoHDeaths)

write_csv(zero_divisors, "Reports/zero_divisors.csv")

write_csv(final_scores %>%
            select(OrganizationName,
                   AltProjectName,
                   TotalScore), "Reports/pe_final.csv")

save.image("images/20200619ProjectEvaluation.RData") 

## EXPERIMENTAL -----

# Housing Stability: 6 mo Recurrence --------------------------------------
# PSH, TH, SH, RRH

# library(funneljoin)
# 
# perm_destinations <- c(3, 10:11, 19:23, 26, 28, 31, 33:34)
# 
# leavers_psh_to_ph <- co_clients_served %>%
#   filter(Destination %in% perm_destinations &
#            ProjectType == 3)
# 
# leavers_rrh_to_ph <- co_clients_served %>%
#   filter(Destination %in% perm_destinations &
#            ProjectType == 13)
# 
# leavers_th_to_ph <- co_clients_served %>%
#   filter(Destination %in% perm_destinations &
#            ProjectType == 2)
# 
# leavers_es_to_ph <- co_clients_served %>%
#   filter(Destination %in% perm_destinations &
#            ProjectType == 1)
# 
# leavers_sso_to_ph <- co_clients_served %>%
#   filter(Destination %in% perm_destinations &
#            ProjectType == 6)
# 
# leavers_sh_to_ph <- co_clients_served %>%
#   filter(Destination %in% perm_destinations &
#            ProjectType == 8)
# 
# returners <- co_clients_served
# 


# Housing Stability: 6-24 mo Recurrence -----------------------------------
# PSH, TH, SH, RRH




