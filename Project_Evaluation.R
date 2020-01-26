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
rm(Affiliation, CaseManagers, Disabilities, EmploymentEducation, EnrollmentCoC, 
   Export, HealthAndDV, Inventory, Offers, Organization, ProjectCoC, Referrals, 
   regions, Scores, Services, stray_services, Users, VeteranCE)

load("images/cohorts.RData")
rm(FileActualStart, FileStart, FileEnd, stop, update_date, summary)
# The specs for this report is here: 
#https://cohhio.org/wp-content/uploads/2019/03/2019-CoC-Competition-Plan-and-Timeline-FINAL-merged-3.29.19.pdf

ReportYear <- "2019"
ReportStart <- format.Date(mdy(paste0("0101", ReportYear)), "%m-%d-%Y")
ReportEnd <- format.Date(mdy(paste0("1231", ReportYear)), "%m-%d-%Y")

# Staging -----------------------------------------------------------------

# filter to only CoC-funded projects

coc_funded <- Funder %>%
  filter(Funder %in% c(1:7, 43) &
           ymd(StartDate) <= mdy(ReportEnd) &
           (is.na(EndDate) |
              ymd(EndDate) >= mdy(ReportStart))) %>%
  select(ProjectID, Funder)

pe_coc_funded <- Funder %>%
  filter(Funder %in% c(1:7, 43) &
           ymd(StartDate) <= mdy(ReportEnd) &
           (is.na(EndDate) |
              ymd(EndDate) >= mdy(ReportStart))) %>%
  select(ProjectID, Funder, StartDate, EndDate) %>%
  left_join(Project[c("ProjectID", 
                      "ProjectName", 
                      "ProjectType", 
                      "HMISParticipatingProject")], by = "ProjectID") %>%
  filter(HMISParticipatingProject == 1) %>%
  select(ProjectType, ProjectName, ProjectID, Funder, StartDate, EndDate)

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
  semi_join(coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(Enrollment, by = c("PersonalID", "EnrollmentID", "ProjectID")) %>%
  select(vars_we_want) %>%
  arrange(PersonalID, ProjectID, desc(EntryDate)) %>%
  distinct(PersonalID, ProjectName, .keep_all = TRUE) # no dupes w/in a project


# several measures will use this
# Adults who entered during date range

pe_adults_entered <-  co_adults_entered %>%
  filter(entered_between(., ReportStart, ReportEnd)) %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  semi_join(coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(Enrollment, by = c("PersonalID", "EnrollmentID", "ProjectID")) %>%
  select(vars_we_want) %>%
  arrange(PersonalID, ProjectID, desc(EntryDate))

# for ncb logic
# Adults who moved in and exited during date range

pe_adults_moved_in_leavers <-  co_adults_moved_in_leavers %>%
  filter(
    stayed_between(., ReportStart, ReportEnd) &
      exited_between(., ReportStart, ReportEnd)
  ) %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  semi_join(coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(Enrollment, by = c("PersonalID", "EnrollmentID", "ProjectID")) %>%
  select(vars_we_want) %>%
  arrange(PersonalID, ProjectID, desc(EntryDate)) %>%
  distinct(PersonalID, ProjectName, .keep_all = TRUE) # no dupes w/in a project

# increase income
#Adults who moved in and were served during date range

pe_adults_moved_in <-  co_adults_moved_in %>%
  filter(stayed_between(., ReportStart, ReportEnd)) %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  semi_join(coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(Enrollment, by = c("PersonalID", "EnrollmentID", "ProjectID")) %>%
  select(vars_we_want) %>%
  arrange(PersonalID, ProjectID, desc(EntryDate)) %>%
  distinct(PersonalID, ProjectName, .keep_all = TRUE) # no dupes w/in a project	

# health insurance
# Clients who moved in and exited during date range

pe_clients_moved_in_leavers <-  co_clients_moved_in_leavers %>%
  filter(stayed_between(., ReportStart, ReportEnd) &
           exited_between(., ReportStart, ReportEnd)) %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  semi_join(coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(Enrollment, by = c("PersonalID", "EnrollmentID", "ProjectID")) %>%
  select(vars_we_want) %>%
  arrange(PersonalID, ProjectID, desc(EntryDate)) %>%
  distinct(PersonalID, ProjectName, .keep_all = TRUE) # no dupes w/in a project

# exits to PH, but needs an added filter of only mover-inners
# Heads of Household who were served during date range

pe_hohs_served <- co_hohs_served %>%
  filter(served_between(., ReportStart, ReportEnd)) %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  semi_join(coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(Enrollment, by = c("PersonalID", "EnrollmentID", "ProjectID")) %>%
  select(vars_we_want) %>%
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
  semi_join(coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(Enrollment, by = c("PersonalID", "EnrollmentID", "ProjectID")) %>%
  select(vars_we_want) %>%
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
  right_join(pe_coc_funded, by = "ProjectID") %>%
  group_by(ProjectID) %>%
  summarise(ClientsServed = n()) %>%
  ungroup()

summary_pe_adults_entered <- pe_adults_entered %>%
  group_by(ProjectID) %>%
  summarise(AdultsEntered = n()) %>%
  ungroup() %>%
  right_join(pe_coc_funded["ProjectID"], by = "ProjectID") %>%
  mutate(AdultsEntered = if_else(is.na(AdultsEntered),
                                 as.integer(0),
                                 AdultsEntered))

pe_validation_summary <- summary_pe_adults_entered %>%
  full_join(summary_pe_adults_moved_in, by = "ProjectID") %>%
  full_join(summary_pe_hohs_served_leavers, by = "ProjectID") %>%
  full_join(summary_pe_adults_moved_in_leavers, by = "ProjectID") %>%
  full_join(summary_pe_clients_served, by = "ProjectID") %>%
  full_join(summary_pe_clients_moved_in_leavers, by = "ProjectID") %>%
  full_join(summary_pe_hohs_moved_in_leavers, by = "ProjectID") %>%
  full_join(summary_pe_hohs_served, by = "ProjectID") %>%
  left_join(pe_coc_funded, by = "ProjectID") %>%
  select(
    ProjectType,
    ProjectID,
    ProjectName,
    ClientsServed,
    HoHsServed,
    HoHsServedLeavers,
    AdultsMovedIn,
    AdultsEntered,
    ClientsMovedInLeavers,
    AdultMovedInLeavers,
    HoHsMovedInLeavers
  )

rm(list = ls(pattern = "summary_"))

## THIS pe_validation_summary NEEDS TO BE **THOROUGHLY** TESTED!!!!

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
  )

# Housing Stability: Exits to PH ------------------------------------------
# PSH (includes stayers tho), TH, SH, RRH

pe_exits_to_ph <- pe_hohs_served %>%
  mutate(
    DestinationGroup = case_when(
      Destination %in% c(1, 2, 12, 13, 14, 16, 18, 27) ~ "Temporary",
      Destination %in% c(3, 10:11, 19:23, 28, 31, 34, 36) ~ "Permanent",
      Destination %in% c(4:7, 15, 25:27, 29) ~ "Institutional",
      Destination %in% c(8, 9, 17, 24, 30, 99) ~ "Other",
      is.na(Destination) ~ "Still in Program"
    ),
    MeetsObjective =
      case_when(ProjectType %in% c(3, 9) &
                  DestinationGroup %in% c("Permanent", "Still in Program") ~ 1,
                ProjectType %in% c(3, 9) &
                  !DestinationGroup %in% c("Permanent", "Still in Program") ~ 0,
                ProjectType %in% c(2, 8, 13) &
                  DestinationGroup == "Permanent" ~ 1,
                ProjectType %in% c(2, 8, 13) &
                  DestinationGroup != "Permanent" ~ 0)
    
  ) %>%
  filter((ProjectType %in% c(2, 8, 13) & !is.na(ExitDate)) |
           ProjectType %in% c(3, 9)) %>% # filtering out non-PSH stayers
  select(vars_to_the_apps, Destination, DestinationGroup)

summary_pe_exits_to_ph <- pe_exits_to_ph %>%
  group_by(ProjectType, ProjectName) %>%
  summarise(ExitsToPH = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "ProjectName")) %>%
  mutate(
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
    ExitsToPHPoints = if_else((ProjectType == 3 &
                        HoHsServed == 0) |
                       (ProjectType != 3 &
                          HoHsServedLeavers) == 0,
                     10,
                     pe_score(Structure, ExitsToPHPercent)
    )
  ) %>%
  select(ProjectType, ProjectName, ExitsToPH, ExitsToPHPercent, ExitsToPHPoints)

# TESTING RESULTS: No percents over 100%, No NAs for Points except the SSO

# Housing Stability: Moved into Own Housing -------------------------------
# TH, SH, RRH

pe_own_housing <- pe_hohs_moved_in_leavers %>%
  filter(ProjectType != 3) %>%
  mutate(
    MeetsObjective = case_when(
      Destination %in% c(3, 10:11, 19:21, 28, 31) ~ 1,
      !Destination %in% c(3, 10:11, 19:21, 28, 31) ~ 0
    ),
    DestinationGroup = case_when(
      Destination %in% c(1, 2, 12, 13, 14, 16, 18, 27) ~ "Temporary",
      Destination %in% c(3, 10:11, 19:21, 28, 31, 33, 34) ~ "Household's Own Housing",
      Destination %in% c(22:23) ~ "Shared Housing",
      Destination %in% c(4:7, 15, 25:27, 29) ~ "Institutional",
      Destination %in% c(8, 9, 17, 24, 30, 99, 32) ~ "Other",
      is.na(Destination) ~ "Still in Program"
    )
  ) %>% 
  select(vars_to_the_apps, Destination, DestinationGroup)

summary_pe_own_housing <- pe_own_housing %>%
  group_by(ProjectType, ProjectName) %>%
  summarise(OwnHousing = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "ProjectName")) %>%
  mutate(
    OwnHousing = if_else(is.na(OwnHousing), 0, OwnHousing),
    Structure = if_else(ProjectType != 3, "72_80_5", NULL),
    OwnHousingPercent = if_else(ProjectType != 3,
                                OwnHousing / HoHsMovedInLeavers,
                                NULL),
    OwnHousingPoints = if_else(HoHsMovedInLeavers == 0 & ProjectType != 3,
                     10,
                     pe_score(Structure, OwnHousingPercent))
  ) %>%
  select(ProjectType, ProjectName, OwnHousing, OwnHousingPercent, OwnHousingPoints)

# TEST RESULTS: No percents over 100%, everyone who should has a score

# Accessing Mainstream Resources: NCBs ------------------------------------
# PSH, TH, SH, RRH

pe_non_cash_at_exit <- pe_adults_moved_in_leavers %>%
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
    DataCollectionStage
  ) %>%
  group_by(
    PersonalID,
    ProjectType,
    VeteranStatus,
    EnrollmentID,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    AgeAtEntry,
    HouseholdID,
    RelationshipToHoH,
    ExitDate,
    ExitAdjust
  ) %>%
  summarise(MostRecentNCB = BenefitsFromAnySource[max(DataCollectionStage)]) %>%
  mutate(MeetsObjective =
           case_when(MostRecentNCB == 1 ~ 1,
                     MostRecentNCB != 1 |
                       is.na(MostRecentNCB) ~ 0)) %>%
  ungroup() %>%
  select(vars_to_the_apps, MostRecentNCB)

summary_pe_non_cash_at_exit <- pe_non_cash_at_exit %>%
  group_by(ProjectType, ProjectName) %>%
  summarise(NCBsAtExit = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "ProjectName")) %>%
  mutate(
    NCBsAtExit = if_else(is.na(NCBsAtExit), 0, NCBsAtExit),
    Structure = "undecided",
    NCBsAtExitPercent = NCBsAtExit / AdultMovedInLeavers,
    NCBsAtExitPoints = "undecided" # pe_score(Structure, NCBsAtExit)
  ) %>%
  select(ProjectType, ProjectName, NCBsAtExit, NCBsAtExitPercent, NCBsAtExitPoints)

# TEST RESULTS: No percents over 100%, no score structure assigned, waiting on 
# committee

# Accessing Mainstream Resources: Health Insurance ------------------------
# PSH, TH, SH, RRH

pe_health_ins_at_exit <- pe_clients_moved_in_leavers %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    PersonalID,
    ProjectType,
    VeteranStatus,
    EnrollmentID,
    ProjectName,
    EntryDate,
    MoveInDateAdjust,
    AgeAtEntry,
    HouseholdID,
    RelationshipToHoH,
    ExitDate,
    ExitAdjust,
    InsuranceFromAnySource,
    DataCollectionStage
  ) %>%
  group_by(
    PersonalID,
    ProjectType,
    VeteranStatus,
    EnrollmentID,
    MoveInDateAdjust,
    AgeAtEntry,    
    ProjectName,
    EntryDate,
    HouseholdID,
    RelationshipToHoH,
    ExitDate,
    ExitAdjust
  ) %>%
  summarise(MostRecentHI = InsuranceFromAnySource[max(DataCollectionStage)]) %>%
  mutate(
    MostRecentHI = if_else(is.na(MostRecentHI) |
                             MostRecentHI == 0, 0, 1),
    MeetsObjective = case_when(MostRecentHI == 1 ~ 1,
                               MostRecentHI != 1 ~ 0)
  ) %>%
  ungroup() %>%
  select(vars_to_the_apps, MostRecentHI)

summary_pe_health_ins_at_exit <- pe_health_ins_at_exit %>%
  group_by(ProjectType, ProjectName) %>%
  summarise(HIatExit = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "ProjectName")) %>%
  mutate(
    HIatExit = if_else(is.na(HIatExit), 0, HIatExit),
    Structure = if_else(ProjectType != 8, "75_85_10", "67_75_10"),
    HIatExitPercent = HIatExit / ClientsMovedInLeavers,
    HIatExitPoints = if_else(ClientsMovedInLeavers == 0,
                     10,
                     pe_score(Structure, HIatExitPercent))
  ) %>%
  select(ProjectType, ProjectName, HIatExit, HIatExitPercent, HIatExitPoints)

# Accessing Mainstream Resources: Increase Total Income -------------------
# PSH, TH, SH, RRH

# one problem is there can be multiple updates and annuals, trying to figure
# out the best way to get the most recent income

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
  mutate(
    MostRecentIncome = case_when(
      !is.na(Exit) ~ Exit,!is.na(Update) ~ Update,
      !is.na(Annual) ~ Annual
    ),
    Exit = NULL,
    Update = NULL,
    Annual = NULL,
    Entry = if_else(is.na(Entry), 0, Entry),
    MostRecentIncome = if_else(is.na(MostRecentIncome), Entry, MostRecentIncome),
    MeetsObjective = case_when(MostRecentIncome > Entry ~ 1,
                               MostRecentIncome <= Entry ~ 0)
  ) %>%
  left_join(pe_adults_moved_in, by = c("PersonalID", "EnrollmentID")) %>%
  select(
    vars_to_the_apps,
    "IncomeAtEntry" = Entry,
    "IncomeMostRecent" = MostRecentIncome
  )

rm(list = ls(pattern = "income_staging"))

summary_pe_increase_income <- pe_increase_income %>%
  group_by(ProjectType, ProjectName) %>%
  summarise(IncreasedIncome = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "ProjectName")) %>%
  mutate(
    IncreasedIncome = if_else(is.na(IncreasedIncome), 0, IncreasedIncome),
    Structure = case_when(
      ProjectType == 2 ~ "24_30_10",
      ProjectType == 3 ~ "22_28_10",
      ProjectType == 8 ~ "16_20_10",
      ProjectType == 13 ~ "14_18_10"
    ),
    IncreasedIncomePercent = IncreasedIncome / AdultsMovedIn,
    IncreasedIncomePoints = if_else(AdultsMovedIn == 0,
                     10,
                     pe_score(Structure, IncreasedIncomePercent))
  ) %>%
  select(ProjectType, ProjectName, IncreasedIncome, IncreasedIncomePercent,
         IncreasedIncomePoints)

#TEST RESULTS: Nothing over 100%, all projects have legit points

# Housing Stability: Length of Time Homeless ------------------------------
# TH, SH, RRH

pe_length_of_stay <- pe_hohs_moved_in_leavers %>%
  mutate(DaysInProject = difftime(ymd(ExitAdjust), ymd(EntryDate))) %>%
  select(ProjectType,
         ProjectName,
         EntryDate,
         EntryAdjust,
         MoveInDateAdjust,
         ExitDate,
         DaysInProject,
         PersonalID,
         EnrollmentID,
         HouseholdID,
         AgeAtEntry,
         VeteranStatus)

summary_pe_length_of_stay <- pe_length_of_stay %>%
  group_by(ProjectType, ProjectName) %>%
  summarise(
    AverageDays = as.numeric(mean(DaysInProject)),
    MedianDays = as.numeric(median(DaysInProject))
  ) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "ProjectName")) %>%
  mutate(
    Structure = case_when(
      ProjectType == 2 ~ "200_280_10",
      ProjectType == 8 ~ "260_340_10",
      ProjectType == 13 ~ "150_210_10"
    ),
    AverageLoSPoints = if_else(HoHsMovedInLeavers == 0 & ProjectType != 3,
                            10,
                            pe_score(Structure, AverageDays)),
    MedianLoSPoints = if_else(HoHsMovedInLeavers == 0 & ProjectType != 3,
                           10,
                           pe_score(Structure, MedianDays))
  ) %>%
  select(ProjectType, ProjectName, AverageDays, MedianDays, AverageLoSPoints, 
         MedianLoSPoints)

# TEST RESULTS: Min and Max days look ok, everyone has points who should

# Community Need: Average Bed/Unit Utilization ----------------------------
# PSH, TH, SH, RRH (it's true! requesting that it be removed from scoring)

source("01_Bed_Unit_Utilization.R")
# getting what we need from the Utilization script
utilization_unit_2019 <- PE_utilization_unit %>%
  ungroup() %>%
  select(ProjectType, ProjectName, AvgUnitUtilization)

utilization_bed_2019 <- PE_utilization_bed %>%
  ungroup() %>%
  select(ProjectType, ProjectName, AvgBedUtilization)

summary_pe_utilization <- pe_coc_funded %>%
  left_join(utilization_bed_2019, by = c("ProjectName", "ProjectType")) %>%
  left_join(utilization_unit_2019, by = c("ProjectName", "ProjectType")) %>%
  select(ProjectType, ProjectName, AvgBedUtilization, AvgUnitUtilization) %>%
  mutate(Structure = "80_90_10",
         BedPoints = pe_score(Structure, AvgBedUtilization),
         UnitPoints = pe_score(Structure, AvgUnitUtilization)) %>%
  select(ProjectType, ProjectName, AvgBedUtilization, AvgUnitUtilization, 
         BedPoints, UnitPoints)

# TEST RESULTS: There are outliers that should be followed up with
# TEST RESULTS: RRH might should have points, but the logic does not 
# currently include them. Can write them in if that's the decision.

# Community Need: Res Prior = Streets or ESSH -----------------------------
# PSH, TH, SH (Street only), RRH

pe_res_prior <- pe_adults_entered %>%
  filter(ProjectType %in% c(2, 3, 13, 8)) %>%
  mutate(MeetsObjective = if_else(
    (ProjectType %in% c(2, 3, 13) &
       LivingSituation %in% c(1, 16, 18)) |
      (ProjectType == 8 &
         LivingSituation == 16),
    1, 
    0
  )) %>%
  select(vars_to_the_apps, LivingSituation) %>%
  filter(!is.na(PersonalID))

summary_pe_res_prior <- pe_res_prior %>%
  group_by(ProjectType, ProjectName) %>%
  summarise(LHResPrior = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "ProjectName")) %>%
  mutate(
    LHResPrior = if_else(is.na(LHResPrior),
                         0,
                         LHResPrior),
    Structure = case_when(
      ProjectType %in% c(3, 13) ~ "75_85_10",
      ProjectType == 2 ~ "67_75_10",
      ProjectType == 8 ~ "0_100_10"
    ),
    LHResPriorPercent = LHResPrior / AdultsEntered,
    LHResPriorPoints = if_else(AdultsEntered == 0,
                     10,
                     pe_score(Structure, LHResPriorPercent))
  ) %>%
  select(ProjectType, ProjectName, LHResPrior, LHResPriorPercent, LHResPriorPoints)

# TEST RESULTS: Nothing over 100%, all projects have points that should

# Community Need: Entries with No Income ----------------------------------
# PSH, TH, SH, RRH

pe_entries_no_income <- pe_adults_entered %>%
  filter(ProjectType %in% c(2, 3, 13, 8)) %>%
  select(EnrollmentID, HouseholdID) %>%
  left_join(pe_increase_income, by = c("EnrollmentID", "HouseholdID")) %>%
  select(
    PersonalID,
    ProjectType,
    ProjectName,
    EnrollmentID,
    HouseholdID,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    IncomeAtEntry
  ) %>%
  mutate(MeetsObjective = if_else(IncomeAtEntry == 0, 1, 0)) %>%
  filter(!is.na(PersonalID))

summary_pe_entries_no_income <- pe_entries_no_income %>%
  group_by(ProjectType, ProjectName) %>%
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
                     pe_score(Structure, NoIncomeAtEntryPercent))
  ) %>%
  select(ProjectType, ProjectName, NoIncomeAtEntry, NoIncomeAtEntryPercent,
         NoIncomeAtEntryPoints)

# TEST RESULTS: nothing over 100%, everyone has points who should

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
  group_by(ProjectType, ProjectName) %>%
  summarise(AvgHHI = mean(HHI),
            MedHHI = median(HHI)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "ProjectName")) %>%
  mutate(
    Structure = if_else(ProjectType != 3, "0_7_10", "0_7_10_PSH"),
    AverageHHIPoints = if_else(AdultsEntered == 0, 10,
                            pe_score(Structure, AvgHHI)),
    MedianHHIPoints = if_else(AdultsEntered == 0, 10,
                           pe_score(Structure, MedHHI))
  ) %>%
  select(ProjectType, ProjectName, AvgHHI, MedHHI, AverageHHIPoints,
         MedianHHIPoints)

# TEST RESULTS: HHIs are as expected, everyone has points

# HMIS Data Quality -------------------------------------------------------
# PSH, TH, SH, RRH

load("images/Data_Quality.RData")

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
         ) %>%
  select(ProjectName, ProjectType, "DQIssues" = Issues, DQPercent, DQPoints)

# TEST RESULTS: no percents over 100%, everyone has points

# Community Need: Long Term Homeless Households ---------------------------
# PSH
# PLEASE NOTE THE SPECS SAY HOHS ENTERED BUT IN 2019 WE GOT PUSHBACK ON THIS
# BECAUSE SOMETIMES THE HOH IS NOT THE ONE WITH THE HOMELESS HISTORY

pe_long_term_homeless <- pe_adults_entered %>%
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
    )
  ) %>%
  select(vars_to_the_apps, DateToStreetESSH, CurrentHomelessDuration,
         MonthsHomelessPastThreeYears, TimesHomelessPastThreeYears)

summary_pe_long_term_homeless <- pe_long_term_homeless %>%
  group_by(ProjectType, ProjectName) %>%
  summarise(LongTermHomeless = sum(MeetsObjective)) %>%
  ungroup() %>%
  right_join(pe_validation_summary, by = c("ProjectType", "ProjectName")) %>%
  mutate(
    LongTermHomeless = if_else(is.na(LongTermHomeless),
                              0,
                              LongTermHomeless),
    Structure = if_else(ProjectType == 3, "20_90_5", NULL),
    LongTermHomelessPercent = LongTermHomeless / AdultsEntered,
    LongTermHomelessPoints = if_else(AdultsEntered == 0, 5,
                     pe_score(Structure, LongTermHomelessPercent))
  ) %>%
  select(ProjectType, ProjectName, LongTermHomeless, LongTermHomelessPercent,
         LongTermHomelessPoints)
  
# TEST RESULTS: No percents over 100%, all PSH's are getting legit points


# Final Scoring -----------------------------------------------------------

summary_pe_final_scoring <- pe_coc_funded[c("ProjectType", "ProjectName")] %>%
  left_join(summary_pe_dq, by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_entries_no_income, by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_exits_to_ph, by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_health_ins_at_exit, by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_homeless_history_index, by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_increase_income, by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_length_of_stay, by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_long_term_homeless, by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_non_cash_at_exit, by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_own_housing, by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_res_prior, by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_utilization, by = c("ProjectType", "ProjectName")) %>%
  left_join(summary_pe_coc_scoring, by = c("ProjectType", "ProjectName"))

# Clean the House ---------------------------------------------------------
# 
# rm(list = ls()[!(ls() %in% c(
#   'pe_coc_funded',
#   'pe_coc_scoring',
#   'pe_dq_by_provider',
#   'pe_entries_no_income',
#   'pe_exits_to_ph',
#   'pe_health_ins_at_exit',
#   'pe_homeless_history_index',
#   'pe_increase_income',
#   'pe_length_of_stay',
#   'pe_long_term_homeless',
#   'pe_non_cash_at_exit',
#   'pe_own_housing',
#   'pe_res_prior',
#   'pe_validation_summary',
#   'summary_pe_dq_by_provider',
#   'summary_pe_entries_no_income',
#   'summary_pe_exits_to_ph',
#   'summary_pe_health_ins_at_exit',
#   'summary_pe_homeless_history_index',
#   'summary_pe_increase_income',
#   'summary_pe_length_of_stay',
#   'summary_pe_long_term_homeless',
#   'summary_pe_non_cash_at_exit',
#   'summary_pe_own_housing',
#   'summary_pe_res_prior',
#   'summary_pe_utilization',
#   'summary_pe_final_scoring',
#   'ReportStart', 
#   'ReportEnd',
#   'living_situation'
# ))])

save.image("images/ProjectEvaluation.RData")

## EXPERIMENTAL -----

# Housing Stability: 6 mo Recurrence --------------------------------------
# PSH, TH, SH, RRH

library(funneljoin)

leavers_psh_to_ph <- co_clients_served %>%
  filter(Destination %in% c(3, 10:11, 19:23, 26, 28, 31, 33:34) &
           ProjectType == 3)

leavers_rrh_to_ph <- co_clients_served %>%
  filter(Destination %in% c(3, 10:11, 19:23, 26, 28, 31, 33:34) &
           ProjectType == 13)

leavers_th_to_ph <- co_clients_served %>%
  filter(Destination %in% c(3, 10:11, 19:23, 26, 28, 31, 33:34) &
           ProjectType == 2)

leavers_es_to_ph <- co_clients_served %>%
  filter(Destination %in% c(3, 10:11, 19:23, 26, 28, 31, 33:34) &
           ProjectType == 1)

leavers_sso_to_ph <- co_clients_served %>%
  filter(Destination %in% c(3, 10:11, 19:23, 26, 28, 31, 33:34) &
           ProjectType == 6)

leavers_sh_to_ph <- co_clients_served %>%
  filter(Destination %in% c(3, 10:11, 19:23, 26, 28, 31, 33:34) &
           ProjectType == 8)

returners <- co_clients_served



# Housing Stability: 6-24 mo Recurrence -----------------------------------
# PSH, TH, SH, RRH




