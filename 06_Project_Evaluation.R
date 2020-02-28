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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details at 
# <https://www.gnu.org/licenses/>.

library(tidyverse)
library(lubridate)

load("images/COHHIOHMIS.RData")
rm(Affiliation, CaseManagers, Disabilities, EmploymentEducation, EnrollmentCoC, 
   Export, HealthAndDV, Inventory, Offers, Organization, ProjectCoC, Referrals, 
   regions, Scores, Services, stray_services, Users, VeteranCE)

load("images/cohorts.RData")
rm(FileActualStart, FileStart, FileEnd, update_date, summary)

load("images/Data_Quality.RData")

# Points function ---------------------------------------------------------

pe_score <- function(structure, value) {
  case_when(
    structure == "75_85_10" & value >= .85 ~ 10,
    structure == "75_85_10" & value >= .8 & value < .85 ~ 7.5,
    structure == "75_85_10" & value >= .75 & value < .8 ~ 5,
    structure == "75_85_10" & value < .75 ~ 0,
    structure == "20_90_5" & value >= .9 ~ 5,
    structure == "20_90_5" & value >= .75 & value < .9 ~ 4,
    structure == "20_90_5" & value >= .5 & value < .75 ~ 3,
    structure == "20_90_5" & value >= .3 & value < .5 ~ 2,
    structure == "20_90_5" & value >= .2 & value < .3 ~ 1,
    structure == "20_90_5" & value < .2 ~ 0,
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

# filter to only CoC-funded projects (leaving out the SSO)

coc_funded <- Funder %>%
  filter(Funder %in% c(1:7) &
           ymd(StartDate) <= mdy(ReportEnd) &
           (is.na(EndDate) |
              ymd(EndDate) >= mdy(ReportStart))) %>%
  select(ProjectID, Funder)

pe_coc_funded <- Funder %>%
  filter(Funder %in% c(1:7) &
           ymd(StartDate) <= mdy(ReportEnd) &
           (is.na(EndDate) |
              ymd(EndDate) >= mdy(ReportEnd))) %>%
  select(ProjectID, Funder, StartDate, EndDate) %>%
  left_join(Project[c("ProjectID", 
                      "ProjectName", 
                      "ProjectType", 
                      "HMISParticipatingProject")], by = "ProjectID") %>%
  filter(HMISParticipatingProject == 1) %>%
  select(ProjectType,
         ProjectName,
         ProjectID,
         Funder,
         StartDate,
         EndDate)

vars_we_want <- c(
  "PersonalID",
  "ProjectType",
  "ProjectID",
  "VeteranStatus",
  "EnrollmentID",
  "ProjectName",
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
  "ProjectName",
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
  arrange(PersonalID, ProjectID, desc(EntryDate)) %>%
  distinct(PersonalID, ProjectName, .keep_all = TRUE) # no dupes w/in a project
# several measures will use this

# Checking for deceased hohs for points adjustments

hoh_exits_to_deceased <- pe_clients_served %>%
  filter(Destination == 24 &
           RelationshipToHoH == 1) %>%
  group_by(ProjectID) %>%
  summarise(HoHDeaths = n()) %>%
  ungroup() %>%
  right_join(pe_coc_funded["ProjectID"], by = "ProjectID")

hoh_exits_to_deceased[is.na(hoh_exits_to_deceased)] <- 0

# Adults who entered during date range

pe_adults_entered <-  co_adults_entered %>%
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
  arrange(PersonalID, ProjectID, desc(EntryDate))

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
  arrange(PersonalID, ProjectID, desc(EntryDate))

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
  arrange(PersonalID, ProjectID, desc(EntryDate)) %>%
  distinct(PersonalID, ProjectName, .keep_all = TRUE) # no dupes w/in a project

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
  arrange(PersonalID, ProjectID, desc(EntryDate)) %>%
  distinct(PersonalID, ProjectName, .keep_all = TRUE) # no dupes w/in a project	

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
  arrange(PersonalID, ProjectID, desc(EntryDate)) %>%
  distinct(PersonalID, ProjectName, .keep_all = TRUE) # no dupes w/in a project

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
  arrange(PersonalID, ProjectID, desc(EntryDate)) %>%
  distinct(PersonalID, ProjectName, .keep_all = TRUE) # no dupes w/in a project	

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
  arrange(PersonalID, ProjectID, desc(EntryDate)) %>%
  distinct(PersonalID, ProjectName, .keep_all = TRUE) # no dupes w/in a project

# Create Validation Summary -----------------------------------------------

summary_pe_hohs_moved_in_leavers <- pe_hohs_moved_in_leavers %>%
  group_by(ProjectID) %>%
  summarise(HoHsMovedInLeavers = n()) %>%
  ungroup() %>%
  right_join(pe_coc_funded["ProjectID"], by = "ProjectID") %>%
  mutate(HoHsMovedInLeavers = if_else(is.na(HoHsMovedInLeavers),
                                      as.integer(0),
                                      HoHsMovedInLeavers))

summary_pe_adults_moved_in_leavers <- pe_adults_moved_in_leavers %>%
  group_by(ProjectID) %>%
  summarise(AdultMovedInLeavers = n()) %>%
  right_join(pe_coc_funded["ProjectID"], by = "ProjectID") %>%
  mutate(AdultMovedInLeavers = if_else(is.na(AdultMovedInLeavers),
                                       as.integer(0),
                                       AdultMovedInLeavers))

summary_pe_adults_moved_in <- pe_adults_moved_in %>%
  group_by(ProjectID) %>%
  summarise(AdultsMovedIn = n()) %>%
  ungroup() %>%
  right_join(pe_coc_funded["ProjectID"], by = "ProjectID") %>%
  mutate(AdultsMovedIn = if_else(is.na(AdultsMovedIn),
                                 as.integer(0),
                                 AdultsMovedIn))

summary_pe_clients_moved_in_leavers <- pe_clients_moved_in_leavers %>%
  group_by(ProjectID) %>%
  summarise(ClientsMovedInLeavers = n()) %>%
  ungroup() %>%
  right_join(pe_coc_funded["ProjectID"], by = "ProjectID") %>%
  mutate(ClientsMovedInLeavers = if_else(is.na(ClientsMovedInLeavers),
                                         as.integer(0),
                                         ClientsMovedInLeavers))

summary_pe_hohs_served <- pe_hohs_served %>%
  group_by(ProjectID) %>%
  summarise(HoHsServed = n()) %>%
  ungroup() %>%
  right_join(pe_coc_funded["ProjectID"], by = "ProjectID") %>%
  mutate(HoHsServed = if_else(is.na(HoHsServed),
                              as.integer(0),
                              HoHsServed))

summary_pe_hohs_served_leavers <- pe_hohs_served %>%
  group_by(ProjectID) %>%
  summarise(HoHsServedLeavers = n()) %>%
  ungroup() %>%
  right_join(pe_coc_funded["ProjectID"], by = "ProjectID") %>%
  mutate(HoHsServedLeavers = if_else(is.na(HoHsServedLeavers),
                              as.integer(0),
                              HoHsServedLeavers))

summary_pe_clients_served <- pe_clients_served %>%
  group_by(ProjectID) %>%
  summarise(ClientsServed = n()) %>%
  ungroup() %>%
  right_join(pe_coc_funded["ProjectID"], by = "ProjectID") %>%
  mutate(ClientsServed = if_else(is.na(ClientsServed),
                                 as.integer(0),
                                 ClientsServed))

summary_pe_adults_entered <- pe_adults_entered %>%
  group_by(ProjectID) %>%
  summarise(AdultsEntered = n()) %>%
  ungroup() %>%
  right_join(pe_coc_funded["ProjectID"], by = "ProjectID") %>%
  mutate(AdultsEntered = if_else(is.na(AdultsEntered),
                                 as.integer(0),
                                 AdultsEntered))

summary_pe_hohs_entered <- pe_hohs_entered %>%
  group_by(ProjectID) %>%
  summarise(HoHsEntered = n()) %>%
  ungroup() %>%
  right_join(pe_coc_funded["ProjectID"], by = "ProjectID") %>%
  mutate(HoHsEntered = if_else(is.na(HoHsEntered),
                               as.integer(0),
                               HoHsEntered))

pe_validation_summary <- summary_pe_adults_entered %>%
  full_join(summary_pe_adults_moved_in, by = "ProjectID") %>%
  full_join(summary_pe_hohs_served_leavers, by = "ProjectID") %>%
  full_join(summary_pe_adults_moved_in_leavers, by = "ProjectID") %>%
  full_join(summary_pe_clients_served, by = "ProjectID") %>%
  full_join(summary_pe_clients_moved_in_leavers, by = "ProjectID") %>%
  full_join(summary_pe_hohs_moved_in_leavers, by = "ProjectID") %>%
  full_join(summary_pe_hohs_served, by = "ProjectID") %>%
  full_join(summary_pe_hohs_entered, by = "ProjectID") %>%
  left_join(pe_coc_funded, by = "ProjectID") %>%
  left_join(hoh_exits_to_deceased, by = "ProjectID") %>%
  select(
    ProjectType,
    ProjectID,
    ProjectName,
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

## THIS pe_validation_summary NEEDS TO BE **THOROUGHLY** TESTED!!!!
## Ask if you need to subtract the deaths from all the Leaver totals and
# remove Destination 24 from the "Other" Destination Group.


# Finalizing DQ Flags -----------------------------------------------------
dq_flags_staging <- dq_2019 %>%
  right_join(pe_coc_funded, by = c("ProjectType", "ProjectID", "ProjectName")) %>%
  mutate(
    GeneralFlag =
      if_else(
        Issue %in% c(
          "Duplicate Entry Exits",
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
  select(ProjectName, 
         PersonalID, 
         HouseholdID,
         GeneralFlag, 
         BenefitsFlag, 
         IncomeFlag, 
         LoTHFlag) %>%
  filter(
    GeneralFlag + BenefitsFlag + IncomeFlag + LoTHFlag > 0
  ) %>% 
  group_by(ProjectName) %>%
  summarise(GeneralFlagTotal = sum(GeneralFlag),
            BenefitsFlagTotal = sum(BenefitsFlag),
            IncomeFlagTotal = sum(IncomeFlag),
            LoTHFlagTotal = sum(LoTHFlag))


# Considering adding a DQ flag for when subs don't match the yes/no but:
# 1. Rme has not had sub dq data in it all this time
# 2. The yes/no is actually more likely to be correct than the subs anyway

data_quality_flags_detail <- pe_validation_summary %>%
  left_join(dq_flags_staging, by = "ProjectName") %>%
  mutate(General_DQ = if_else(GeneralFlagTotal/ClientsServed >= .02, 1, 0),
         Benefits_DQ = if_else(BenefitsFlagTotal/AdultsEntered >= .02, 1, 0),
         Income_DQ = if_else(IncomeFlagTotal/AdultsEntered >= .02, 1, 0),
         Destination_DQ = if_else(DestinationFlagTotal/ClientsServed >= .02, 1, 0))

data_quality_flags_detail[is.na(data_quality_flags_detail)] <- 0

data_quality_flags <- data_quality_flags_detail %>%
  select(ProjectName, General_DQ, Benefits_DQ, Income_DQ, Destination_DQ)

# CoC Scoring -------------------------------------------------------------

summary_pe_coc_scoring <- pe_coc_funded %>%
  left_join(Project, by = c("ProjectType", "ProjectName")) %>%
  select(
    ProjectType,
    ProjectName,
    CostPerExit,
    CostPerExitScore,
    DateReceivedPPDocs,
    HousingFirstScore,
    ChronicPrioritizationScore,
    OnTrackSpendingScoring,
    UnspentFundsScoring
  ) %>%
  mutate(
    CostPerExitPossible = 5,
    HousingFirstPossible = 5,
    ChronicPrioritizationPossible = 5,
    OnTrackSpendingPossible = 5,
    UnspentFundsPossible = 5
  )

# Housing Stability: Exits to PH ------------------------------------------
# PSH (includes stayers tho), TH, SH, RRH
# DQ: General & Destination

pe_exits_to_ph <- pe_hohs_served %>%
  left_join(data_quality_flags, by = "ProjectName") %>%
  mutate(
    DestinationGroup = case_when(
      Destination %in% c(1, 2, 12, 13, 14, 16, 18, 27) ~ "Temporary",
      Destination %in% c(3, 10:11, 19:23, 28, 31, 33:34, 36) ~ "Permanent",
      Destination %in% c(4:7, 15, 25:27, 29) ~ "Institutional",
      Destination %in% c(8, 9, 17, 30, 99) ~ "Other",
      Destination == 24 ~ "Deceased (not counted as negative or positive",
      is.na(Destination) ~ "Still in Program"
    ),
    ExitsToPHDQ = case_when(
      General_DQ == 1 | Destination_DQ == 1 ~ 1,
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
           ProjectType %in% c(3, 9)) %>% # filtering out non-PSH stayers
  select(all_of(vars_to_the_apps), ExitsToPHDQ, Destination, DestinationGroup)

summary_pe_exits_to_ph <- pe_exits_to_ph %>%
  group_by(ProjectType, ProjectName, ExitsToPHDQ) %>%
  summarise(ExitsToPH = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "ProjectName")) %>%
  mutate(
    HoHsServedLeavers = HoHsServedLeavers - HoHDeaths,
    ExitsToPH = if_else(is.na(ExitsToPH), 0, ExitsToPH),
    Structure = case_when(
      ProjectType == 3 ~ "80_90_10",
      ProjectType %in% c(2, 13) ~ "75_83_10",
      ProjectType == 8 ~ "67_75_10"
    ),
    ExitsToPHPercent = if_else(
      ProjectType == 3,
      ExitsToPH / HoHsServed,
      ExitsToPH / HoHsServedLeavers
    ),
    ExitsToPHPoints = if_else(
      (ProjectType == 3 &
         HoHsServed == 0) |
        (ProjectType != 3 &
           HoHsServedLeavers == 0),
      10,
      pe_score(Structure, ExitsToPHPercent)
    ),
    ExitsToPHPossible = 10,
    ExitsToPHPoints = if_else(
      ExitsToPHDQ == 0 | is.na(ExitsToPHDQ),
      ExitsToPHPoints,
      0
    ),
    ExitsToTPHCohort = "HoHsServedLeavers"
  ) %>%
  select(
    ProjectType,
    ProjectName,
    ExitsToPH,
    ExitsToPHPercent,
    ExitsToPHPoints,
    ExitsToPHPossible,
    ExitsToPHDQ,
    ExitsToTPHCohort
  )

# Housing Stability: Moved into Own Housing -------------------------------
# TH, SH, RRH

pe_own_housing <- pe_hohs_moved_in_leavers %>%
  left_join(data_quality_flags, by = "ProjectName") %>%
  filter(ProjectType != 3) %>%
  mutate(
    MeetsObjective = case_when(
      Destination %in% c(3, 10:11, 19:21, 28, 31, 33:34) ~ 1,
      !Destination %in% c(3, 10:11, 19:21, 28, 31, 33:34) ~ 0
    ),
    OwnHousingDQ = case_when(
      General_DQ == 1 |
        Destination_DQ == 1 ~ 1,
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
  group_by(ProjectType, ProjectName, OwnHousingDQ) %>%
  summarise(OwnHousing = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "ProjectName")) %>%
  mutate(
    HoHsMovedInLeavers = HoHsMovedInLeavers - HoHDeaths,
    OwnHousing = if_else(is.na(OwnHousing), 0, OwnHousing),
    Structure = if_else(ProjectType != 3, "72_80_5", NULL),
    OwnHousingPercent = if_else(ProjectType != 3,
                                OwnHousing / HoHsMovedInLeavers,
                                NULL),
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
    OwnHousingPossible = if_else(ProjectType != 3, 5, NULL),
    OwnHousingCohort = "HoHsMovedInLeavers"
  ) %>%
  select(ProjectType,
         ProjectName,
         OwnHousingCohort,
         OwnHousing,
         OwnHousingPercent,
         OwnHousingPoints,
         OwnHousingPossible,
         OwnHousingDQ)

# Accessing Mainstream Resources: Benefits -----------------------------------
# PSH, TH, SH, RRH

pe_benefits_at_exit <- pe_adults_moved_in_leavers %>%
  left_join(data_quality_flags, by = "ProjectName") %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    PersonalID,
    ProjectName,
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
  group_by(ProjectType, ProjectName, BenefitsAtExitDQ) %>%
  summarise(BenefitsAtExit = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "ProjectName")) %>%
  mutate(
    BenefitsAtExit = if_else(is.na(BenefitsAtExit), 0, BenefitsAtExit),
    Structure = if_else(ProjectType != 8, "75_85_10", "67_75_10"),
    BenefitsAtExitPercent = BenefitsAtExit / AdultMovedInLeavers,
    BenefitsAtExitPoints = if_else(AdultMovedInLeavers == 0,
                               10,
                               pe_score(Structure, BenefitsAtExit)),
    BenefitsAtExitPossible = 10,
    BenefitsAtExitPoints = case_when(
      BenefitsAtExitDQ == 1 ~ 0,
      is.na(BenefitsAtExitDQ) |
        BenefitsAtExitDQ == 0 ~ BenefitsAtExitPoints
    ), 
    BenefitsAtExitCohort = "AdultMovedInLeavers"
  ) %>%
  select(
    ProjectType,
    ProjectName,
    BenefitsAtExitCohort,
    BenefitsAtExit,
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
  left_join(data_quality_flags, by = "ProjectName") %>%
  mutate(
    MostRecentIncome = case_when(
      !is.na(Exit) ~ Exit,
      !is.na(Update) ~ Update,
      !is.na(Annual) ~ Annual
    ),
    IncomeAtEntry = if_else(is.na(Entry), 0, Entry),
    IncomeMostRecent = if_else(is.na(MostRecentIncome), Entry, MostRecentIncome),
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
  group_by(ProjectType, ProjectName, IncreasedIncomeDQ) %>%
  summarise(IncreasedIncome = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "ProjectName")) %>%
  mutate(
    IncreasedIncome = if_else(is.na(IncreasedIncome), 0, IncreasedIncome),
    Structure = case_when(
      ProjectType == 3 ~ "24_30_10",
      ProjectType == 2 ~ "22_28_10",
      ProjectType == 8 ~ "16_20_10",
      ProjectType == 13 ~ "14_18_10"
    ),
    IncreasedIncomePercent = IncreasedIncome / AdultsMovedIn,
    IncreasedIncomePoints = case_when(
      AdultsMovedIn > 0 ~ pe_score(Structure, IncreasedIncomePercent),
      AdultsMovedIn == 0 & (IncreasedIncomeDQ == 0 | is.na(IncreasedIncomeDQ)) ~ 10,
      IncreasedIncomeDQ == 1 ~ 0),
    IncreasedIncomePossible = 10,
    IncreasedIncomeCohort = "AdultsMovedIn"
  ) %>%
  select(
    ProjectType,
    ProjectName,
    IncreasedIncome,
    IncreasedIncomeCohort,
    IncreasedIncomePercent,
    IncreasedIncomePoints,
    IncreasedIncomePossible,
    IncreasedIncomeDQ
  ) 

# Housing Stability: Length of Time Homeless ------------------------------
# TH, SH, RRH

pe_length_of_stay <- pe_clients_moved_in_leavers %>%
  left_join(data_quality_flags, by = "ProjectName") %>%
  mutate(DaysInProject = difftime(ymd(ExitAdjust), ymd(EntryDate))) %>%
  select(ProjectType,
         ProjectName,
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
  group_by(ProjectType, ProjectName, General_DQ) %>%
  summarise(
    AverageDays = as.numeric(mean(DaysInProject))
  ) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "ProjectName")) %>%
  mutate(
    Structure = case_when(
      ProjectType == 2 ~ "200_280_10",
      ProjectType == 8 ~ "260_340_10",
      ProjectType == 13 ~ "150_210_10"
    ),
    AverageLoSPoints = case_when(
      ClientsMovedInLeavers == 0 &
        ProjectType != 3 ~ 10,
      TRUE ~ pe_score(Structure, AverageDays)
    ),
    AverageLoSPossible = if_else(ProjectType %in% c(2, 8, 13), 10, NULL),
    AverageLoSDQ = case_when(
      General_DQ == 1 & ProjectType %in% c(2, 8 ,13) ~ 1,
      General_DQ == 0 & ProjectType %in% c(2, 8 ,13) ~ 0),
    AverageLoSPoints = case_when(
      AverageLoSDQ == 1 ~ 0, 
      AverageLoSDQ == 0 | is.na(AverageLoSDQ) ~ AverageLoSPoints),
    AverageLoSCohort = "ClientsMovedInLeavers"
  ) %>%
  select(ProjectType, ProjectName, AverageDays, AverageLoSCohort, 
         AverageLoSPoints, AverageLoSPossible, AverageLoSDQ)

# Community Need: Res Prior = Streets or ESSH -----------------------------
# PSH, TH, SH (Street only), RRH

pe_res_prior <- pe_adults_entered %>%
  left_join(data_quality_flags, by = "ProjectName") %>%
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
  group_by(ProjectType, ProjectName, LHResPriorDQ) %>%
  summarise(LHResPrior = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary,
             by = c("ProjectType",
                    "ProjectName")) %>%
  mutate(
    LHResPrior = if_else(is.na(LHResPrior), 0, LHResPrior),
    Structure = case_when(
      ProjectType %in% c(3, 13) ~ "75_85_10",
      ProjectType == 2 ~ "67_75_10",
      ProjectType == 8 ~ "0_100_10"
    ),
    LHResPriorPercent = LHResPrior / AdultsEntered,
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
    ProjectName,
    LHResPrior,
    LHResPriorCohort,
    LHResPriorPercent,
    LHResPriorPoints,
    LHResPriorPossible,
    LHResPriorDQ
  ) 

# Community Need: Entries with No Income ----------------------------------
# PSH, TH, SH, RRH

pe_entries_no_income <- pe_adults_entered %>%
  left_join(data_quality_flags, by = "ProjectName") %>%
  filter(ProjectType %in% c(2, 3, 13, 8)) %>%
  left_join(IncomeBenefits %>%
              select(EnrollmentID, 
                     IncomeFromAnySource) %>%
              unique(), 
            by = c("EnrollmentID")) %>%
  mutate(MeetsObjective = if_else(IncomeFromAnySource == 0, 1, 0),
         NoIncomeAtEntryDQ = if_else(General_DQ == 1|
                                     Income_DQ == 1, 1, 0)) %>%
  select(all_of(vars_to_the_apps), NoIncomeAtEntryDQ)

summary_pe_entries_no_income <- pe_entries_no_income %>%
  group_by(ProjectType, ProjectName, NoIncomeAtEntryDQ) %>%
  summarise(NoIncomeAtEntry = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "ProjectName")) %>%
  mutate(
    NoIncomeAtEntry = if_else(is.na(NoIncomeAtEntry),
                              0,
                              NoIncomeAtEntry),
    Structure = if_else(ProjectType != 2, "34_40_10", "24_30_10"),
    NoIncomeAtEntryPercent = NoIncomeAtEntry / AdultsEntered,
    NoIncomeAtEntryPoints = if_else(AdultsEntered == 0, 10,
                     pe_score(Structure, NoIncomeAtEntryPercent)),
    NoIncomeAtEntryPossible = if_else(ProjectType != 2, 10, NULL),
    NoIncomeAtEntryPoints = case_when(NoIncomeAtEntryDQ == 1 ~ 0,
                                      NoIncomeAtEntryDQ == 0 | is.na(NoIncomeAtEntryDQ) ~ NoIncomeAtEntryPoints),
    NoIncomeAtEntryCohort = "AdultsEntered"
  ) %>%
  select(
    ProjectType,
    ProjectName,
    NoIncomeAtEntry,
    NoIncomeAtEntryCohort,
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
    ProjectName,
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
  left_join(data_quality_flags, by = "ProjectName") %>%
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
  group_by(ProjectType, ProjectName, General_DQ) %>%
  summarise(MedHHI = median(HHI)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "ProjectName")) %>%
  mutate(
    Structure = if_else(ProjectType != 3, "0_7_10", "0_7_10_PSH"),
    MedianHHIPoints = if_else(AdultsEntered == 0, 10,
                           pe_score(Structure, MedHHI)),
    MedianHHIPossible = 10,
    MedianHHIDQ = if_else(General_DQ == 1, 1, 0),
    MedianHHIPoints = case_when(MedianHHIDQ == 1 ~ 0, 
                                MedianHHIDQ == 0 | is.na(MedianHHIDQ) ~ MedianHHIPoints),
    MedianHHICohort = "AdultsEntered"
  ) %>%
  select(ProjectType,
         ProjectName,
         MedHHI,
         MedianHHICohort,
         MedianHHIPoints,
         MedianHHIPossible,
         MedianHHIDQ)

# HMIS Data Quality -------------------------------------------------------
# PSH, TH, SH, RRH

pe_dq <- dq_2019 %>%
  filter(Type %in% c("Error", "High Priority") & 
           ProjectType %in% c(2, 3, 13, 8))

summary_pe_dq <- pe_dq %>% 
  group_by(ProjectName, ProjectType) %>%
  summarise(Issues = n()) %>%
  ungroup()

summary_pe_dq <- pe_validation_summary %>%
  select(ProjectName, ProjectType, ClientsServed) %>%
  left_join(summary_pe_dq, by = c("ProjectType", "ProjectName"))

summary_pe_dq[is.na(summary_pe_dq)] <- 0

summary_pe_dq <- summary_pe_dq %>%
  mutate(DQPercent = Issues / ClientsServed,
         DQPoints = case_when(
           Issues == 0 ~ 5,
           DQPercent > 0 & DQPercent <= .02 ~ 4,
           DQPercent > .02 & DQPercent <= .05 ~ 3,
           DQPercent > .05 & DQPercent <= .08 ~ 2,
           DQPercent > .08 & DQPercent <= .1 ~ 1,
           DQPercent > .1 ~ 0
           ),
         DQPossible = 5,
         DQCohort = "ClientsServed"
         ) %>%
  select(ProjectName, ProjectType, "DQIssues" = Issues, DQCohort, DQPercent, 
         DQPoints, DQPossible)

# Community Need: Long Term Homeless Households ---------------------------
# PSH
# Decided in Feb meeting that we're going to use Adults Entered for this one

pe_long_term_homeless <- pe_adults_entered %>%
  right_join(pe_coc_funded, by = c("ProjectName", "ProjectType", "ProjectID")) %>%
  left_join(data_quality_flags, by = "ProjectName") %>%
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
  group_by(ProjectType, ProjectName, LTHomelessDQ) %>%
  summarise(LongTermHomeless = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "ProjectName")) %>%
  mutate(
    LongTermHomeless = if_else(is.na(LongTermHomeless),
                              0,     
                              LongTermHomeless),
    Structure = if_else(ProjectType == 3, "20_90_5", NULL),
    LongTermHomelessPercent = if_else(AdultsEntered > 0,
                                      LongTermHomeless / AdultsEntered,
                                      NULL),    
    LongTermHomelessPoints = if_else(AdultsEntered == 0 &
                                       ProjectType == 3, 5,
                     pe_score(Structure, LongTermHomelessPercent)), 
    LongTermHomelessPoints = case_when(LTHomelessDQ == 0 |
                                         is.na(LTHomelessDQ) ~ LongTermHomelessPoints,
                                       LTHomelessDQ == 1 ~ 0), 
    LongTermHomelessPossible = if_else(ProjectType == 3, 5, NULL),
    LongTermhomelessCohort = "AdultsEntered"
  ) %>%
  select(
    ProjectType,
    ProjectName,
    LongTermHomeless,
    LongTermHomelessPercent,
    LongTermHomelessPoints,
    LongTermhomelessCohort,
    LongTermHomelessPossible,
    LTHomelessDQ
  ) 

# VISPDATs at Entry into PH -----------------------------------------------

pe_scored_at_ph_entry <- pe_hohs_entered %>%
  right_join(pe_coc_funded, by = c("ProjectName", "ProjectType", "ProjectID")) %>%
  left_join(data_quality_flags, by = c("ProjectName")) %>%
  left_join(
    dq_2019 %>%
      filter(Issue == "Non-Veteran Non-DV HoHs Entering PH without SPDAT") %>%
      select("ProjectName", "PersonalID", "HouseholdID", "Issue"),
    by = c("ProjectName", "PersonalID", "HouseholdID")
  ) %>%
  mutate(
    MeetsObjective = case_when(
      !is.na(PersonalID) & is.na(Issue) & ProjectType %in% c(3, 13) ~ 1, 
      !is.na(PersonalID) & !is.na(Issue) & ProjectType %in% c(3, 13) ~ 0),
    ScoredAtEntryDQ = case_when(
      !is.na(PersonalID) & ProjectType %in% c(3, 13) & General_DQ == 1 ~ 1, 
      !is.na(PersonalID) & ProjectType %in% c(3, 13) & General_DQ == 0 ~ 0)
  ) %>%
  select(all_of(vars_to_the_apps), ScoredAtEntryDQ)

summary_pe_scored_at_ph_entry <- pe_scored_at_ph_entry %>%
  group_by(ProjectType, ProjectName, ScoredAtEntryDQ) %>%
  summarise(ScoredAtEntry = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "ProjectName")) %>%
  mutate(
    ScoredAtEntry = if_else(is.na(ScoredAtEntry),
                            0,
                            ScoredAtEntry), 
    Structure = if_else(ProjectType %in% c(3, 13), "90_100_5", NULL),
    ScoredAtEntryPercent = if_else(HoHsEntered > 0,
                                   ScoredAtEntry / HoHsEntered,
                                   NULL),    
    ScoredAtEntryPoints = case_when(
      HoHsEntered == 0 &
        ProjectType %in% c(3, 13) ~ 5,
      HoHsEntered > 0 &
        ProjectType %in% c(3, 13) ~ pe_score(Structure, ScoredAtEntryPercent)),
    ScoredAtEntryPoints = case_when(
      ScoredAtEntryDQ == 0 ~ ScoredAtEntryPoints,
      ScoredAtEntryDQ == 1 ~ 0,
      is.na(ScoredAtEntryDQ) ~ ScoredAtEntryPoints),
    ScoredAtEntryPossible = if_else(ProjectType %in% c(3, 13), 5, NULL),
    ScoredAtEntryCohort = "HoHsEntered"
  ) %>%
  select(
    ProjectType,
    ProjectName,
    ScoredAtEntry,
    ScoredAtEntryPercent,
    ScoredAtEntryPoints,
    ScoredAtEntryCohort,
    ScoredAtEntryPossible,
    ScoredAtEntryDQ
  ) 

# Final Scoring -----------------------------------------------------------

summary_pe_final_scoring <-
  pe_coc_funded[c("ProjectType", "ProjectName")] %>%
  left_join(summary_pe_dq, by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_entries_no_income,
            by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_exits_to_ph,
            by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_scored_at_ph_entry,
            by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_homeless_history_index,
            by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_increase_income,
            by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_length_of_stay,
            by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_long_term_homeless,
            by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_benefits_at_exit,
            by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_own_housing,
            by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_res_prior,
            by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_coc_scoring, by = c("ProjectType", "ProjectName"))

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
  'living_situation'
))])

next_thing_due <- tribble(
  ~ DueDate, ~ Event,
  "1/11/2020", "2019 CoC Competition Training",
  "3/14/2020", "Ohio BoS CoC Grant Inventory Worksheet finalized by COHHIO",
  "3/15/2020", "COHHIO makes Project Evaluation 2020 Report available in Rme",
  "3/22/2020", "COHHIO makes Project Evaluation Estimated Scores available in Rm",
  "4/11/2020", "All HMIS Data in the Project Evaluation report finalized",
  "4/1/2020", "All Policies and Procedures documents submitted by Recipients",
  "4/15/2020", "COHHIO saves out Final Project Evaluation Report data",
  "4/8/2020", "Project Conversion and New CoC Project Proposals due to ODSA/COHHIO",
  "4/19/2020", "Written project Proposal Feedback Provided to Project Conversion 
  and New CoC Project Applicants",
  "5/10/2020", "COHHIO releases Project Evaluation results and preliminary CoC project 
  ranking (renewals only)",
  "5/17/2020", "Appeals Submission Due Date"
) %>%
  mutate(
    DueDate = mdy(DueDate),
    ShowStart = lag(ymd(DueDate), n = 1L, order_by = DueDate),
    ShowStart = if_else(is.na(ShowStart), today(), ShowStart),
    ShowEnd = ymd(DueDate),
    DateRange = interval(ShowStart, ShowEnd)
  ) %>%
  filter(today() %within% DateRange) %>%
  select(Event, DueDate)

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

save.image("images/ProjectEvaluation.RData")

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




