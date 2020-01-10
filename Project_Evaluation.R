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

coc_funded <- Funder %>%
  filter(Funder %in% c(1:7, 43) &
           ymd(StartDate) <= mdy(FileEnd) &
           (is.na(EndDate) |
              ymd(EndDate) >= mdy(FileStart))) %>%
  select(ProjectID, Funder)

pe_coc_funded <- Funder %>%
  filter(Funder %in% c(1:7, 43) &
           ymd(StartDate) <= mdy(FileEnd) &
           (is.na(EndDate) |
              ymd(EndDate) >= mdy(FileStart))) %>%
  select(ProjectID, Funder, StartDate, EndDate) %>%
  left_join(Project[c("ProjectID", "ProjectName", "ProjectType")], by = "ProjectID") %>%
  select(ProjectType, ProjectName, Funder, StartDate, EndDate)

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
  "AgeAtEntry",
  "VeteranStatus",
  "EntryDate",
  "MoveInDateAdjust",
  "ExitDate",
  "MeetsObjective"
)

load("images/cohorts.RData")

# for data quality checking
# clients served during date range

pe_clients_served <-  co_clients_served %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  semi_join(coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(Enrollment, by = c("PersonalID", "EnrollmentID", "ProjectID")) %>%
  select(vars_we_want)

# several measures will use this
# Adults who entered during date range

pe_adults_entered <-  co_adults_entered %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  semi_join(coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(Enrollment, by = c("PersonalID", "EnrollmentID", "ProjectID")) %>%
  select(vars_we_want)

# for ncb logic
# Adults who moved in and exited during date range

pe_adults_moved_in_leavers <-  co_adults_moved_in_leavers %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  semi_join(coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(Enrollment, by = c("PersonalID", "EnrollmentID", "ProjectID")) %>%
  select(vars_we_want)	

# increase income
#Adults who moved in and were served during date range

pe_adults_moved_in <-  co_adults_moved_in %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  semi_join(coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(Enrollment, by = c("PersonalID", "EnrollmentID", "ProjectID")) %>%
  select(vars_we_want)	

# health insurance
# Clients who moved in and exited during date range

pe_clients_moved_in_leavers <-  co_clients_moved_in_leavers %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  semi_join(coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(Enrollment, by = c("PersonalID", "EnrollmentID", "ProjectID")) %>%
  select(vars_we_want)	

# exits to PH, but needs an added filter of only mover-inners
# Heads of Household who were served during date range

pe_hohs_served <-  co_hohs_served %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  semi_join(coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(Enrollment, by = c("PersonalID", "EnrollmentID", "ProjectID")) %>%
  select(vars_we_want)	

# own housing and LoS
# Heads of Household who moved in and exited during date range

pe_hohs_moved_in_leavers <-  co_hohs_moved_in_leavers %>%
  select("PersonalID", "ProjectID", "EnrollmentID") %>%
  semi_join(coc_funded, by = "ProjectID") %>%
  left_join(Client, by = "PersonalID") %>%
  left_join(Enrollment, by = c("PersonalID", "EnrollmentID", "ProjectID")) %>%
  select(vars_we_want)


# CoC Scoring -------------------------------------------------------------

pe_scoring <- pe_coc_funded %>%
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
      Destination %in% c(3, 10:11, 19:23, 28, 31) ~ "Permanent",
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

# Housing Stability: Moved into Own Housing -------------------------------
# TH, SH, RRH

pe_own_housing <- pe_hohs_moved_in_leavers %>%
  mutate(MeetsObjective = case_when(
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
  )) %>%
  select(vars_to_the_apps, Destination, DestinationGroup)

# Housing Stability: 6 mo Recurrence --------------------------------------
# PSH, TH, SH, RRH

# Housing Stability: 6-24 mo Recurrence -----------------------------------
# PSH, TH, SH, RRH

# Accessing Mainstream Resources: NCBs ------------------------------------
# PSH, TH, SH, RRH

pe_non_cash_at_exit <- pe_adults_moved_in_leavers %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(PersonalID,
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
         DataCollectionStage) %>%
  group_by(PersonalID,
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
           ExitAdjust) %>%
  summarise(MostRecentNCB = BenefitsFromAnySource[max(DataCollectionStage)]) %>%
  mutate(MeetsObjective =
           case_when(MostRecentNCB == 1 ~ 1,
                     MostRecentNCB != 1 |
                       is.na(MostRecentNCB) ~ 0)) %>%
  ungroup() %>%
  select(vars_to_the_apps, MostRecentNCB)

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

# Housing Stability: Length of Time Homeless ------------------------------
# TH, SH, RRH

pe_length_of_stay <- co_hohs_moved_in_leavers %>%
  select(ProjectType,
         ProjectName,
         PersonalID,
         EnrollmentID,
         HouseholdID,
         AgeAtEntry,
         VeteranStatus,
         EntryDate,
         MoveInDateAdjust,
         ExitDate)

# ALL OF THIS WILL NEED TO MOVE TO THE APPS SINCE WE NEED TO BE ABLE TO FILTER
# ON DATES ---
# TotalLeavers <- co_hohs_moved_in_leavers %>%
#   group_by(ProjectName) %>%
#   summarise(Leavers = n())
# 
# length_of_stay_summary <- co_hohs_moved_in_leavers %>%
#   mutate(DaysInProject = difftime(ymd(ExitAdjust), ymd(EntryDate))) %>%
#   group_by(ProjectName, ProjectType) %>%
#   summarise(
#     AverageDays = as.numeric(mean(DaysInProject)),
#     MedianDays = as.numeric(median(DaysInProject))
#   ) %>%
#   left_join(TotalLeavers, by = "ProjectName") 
# ---

# Community Need: Average Bed/Unit Utilization ----------------------------
# PSH, TH, SH, RRH (it's true! not sure why)

# THIS WILL NEED TO BE DONE IN THE APPS IN ORDER TO FILTER ON DATES
# # setting the FilePeriod to the date range of the Project Evaluation
# FilePeriod <- interval(mdy(ReportStart), mdy(ReportEnd))
# # rerunning the Utilization script with that new date interval
# source("01_Bed_Unit_Utilization.R")
# # getting what we need from the Utilization script
# utilization_unit_2019 <- utilization_unit %>%
#   ungroup() %>%
#   select(ProjectType, ProjectName, "AvgUnitUtilization" = FilePeriod)
# 
# utilization_bed_2019 <- utilization_bed %>%
#   ungroup() %>%
#   select(ProjectType, ProjectName, "AvgBedUtilization" = FilePeriod)
# # setting the FilePeriod back to what it was before
# FilePeriod <- interval(mdy(FileStart), mdy(FileEnd))
# # re-re-running the script so the image file is like it was
# source("01_Bed_Unit_Utilization.R")

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
    AgeAtEntry,
    VeteranStatus,
    EntryDate,
    MoveInDateAdjust,
    ExitDate,
    IncomeAtEntry
  ) %>%
  mutate(MeetsObjective = if_else(IncomeAtEntry == 0, 1, 0)) %>%
  filter(!is.na(PersonalID))

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

# THIS MUST MOVE TO THE APPS SINCE WE'LL NEED TO FILTER BY DATES
# hhi_summary <- hhi_detail %>%
#   group_by(ProjectType, ProjectName) %>%
#   summarise(AverageHHI = mean(HHI))

# Community Need: Long Term Homeless Households ---------------------------
# PSH

# HMIS Data Quality -------------------------------------------------------
# PSH, TH, SH, RRH

load("images/Data_Quality.RData")

dq_items_being_checked <- dq_2019 %>%
  filter(Type %in% c("Error", "High Priority") & 
           ProjectType %in% c(2, 3, 13, 8)) %>% 
  select(Issue) %>% 
  unique() 

dq_staging <- dq_2019 %>%
  filter(Type %in% c("Error", "High Priority") & 
           ProjectType %in% c(2, 3, 13, 8)) %>% 
  group_by(ProjectName) %>%
  summarise(Issues = n()) %>%
  ungroup()

pe_dq_by_provider <- pe_clients_served %>%
  select(ProjectName) %>%
  unique() %>%
  left_join(summary, by = "ProjectName") %>%
  select(ProjectName, clients_served) %>%
  left_join(dq_staging, by = "ProjectName")

pe_dq_by_provider[is.na(pe_dq_by_provider)] <- 0

pe_dq_by_provider <- pe_dq_by_provider %>%
  mutate(Percent = Issues / clients_served,
         Points = case_when(
           Issues == 0 ~ 5,
           Percent > 0 & Percent <= .02 ~ 4,
           Percent > .02 & Percent <= .05 ~ 3,
           Percent > .05 & Percent <= .08 ~ 2,
           Percent > .08 & Percent <= .1 ~ 1,
           Percent > .1 ~ 0
           ),
         ) %>%
  select(ProjectName, "ClientsServed" = clients_served, Issues, Percent, Points)

rm(list = ls()[!(
  ls() %in% c(
    'pe_dq_by_provider',
    'pe_entries_no_income',
    'pe_exits_to_ph',
    'pe_health_ins_at_exit',
    'pe_homeless_history_index',
    'pe_increase_income',
    'pe_non_cash_at_exit',
    'pe_own_housing',
    'pe_res_prior',
    'pe_coc_funded'
  )
)])

# Points Data -------------------------------------------------------------

score_structure_80_90_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  .9, 1, 10,
  .85, .9, 7.5,
  .8, .85, 5,
  0, .8, 0
) %>%
  mutate(Structure = "80_90_10")

score_structure_75_85_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  .85, 1, 10,
  .8, .85, 7.5,
  .75, .8, 5,
  0, .75, 0
) %>%
  mutate(Structure = "75_85_10")

score_structure_20_90_5 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  .9, 1, 5,
  .75, .9, 4,
  .5, .75, 3,
  .3, .5, 2,
  .2, .3, 1,
  0, .2, 0
) %>%
  mutate(Structure = "20_90_5")

score_structure_2_6_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  0, .02, 10,
  .02, .04, 7.5,
  .04, .06, 5,
  .06, 1, 0
) %>%
  mutate(Structure = "2_6_10")

score_structure_5_9_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  0, .05, 10,
  .05, .08, 7.5,
  .08, .09, 5,
  .09, 1, 0
) %>%
  mutate(Structure = "5_9_10")

score_structure_24_30_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  .3, 1, 10,
  .27, .3, 7.5,
  .24, .27, 5,
  0, .24, 0
) %>%
  mutate(Structure = "24_30_10")

score_structure_34_40_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  .4, 1, 10,
  .37, .4, 7.5,
  .34, .37, 5,
  0, .34, 0
) %>%
  mutate(Structure = "34_40_10")

score_structure_0_7_10_PSH <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  6, 7, 10,
  5, 5, 9,
  3, 4, 8,
  2, 2, 5,
  1, 1, 2, 
  0, 0, 0
) %>%
  mutate(Structure = "0_7_10_PSH")

score_structure_75_83_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  .83, 1, 10,
  .79, .83, 7.5,
  .75, .79, 5,
  0, .75, 0
) %>%
  mutate(Structure = "75_83_10")

score_structure_72_80_5 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  .8, 1, 5,
  .76, .8, 3,
  .72, .76, 2,
  0, .72, 0
) %>%
  mutate(Structure = "72_80_5")

score_structure_7_12_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  0, .07, 10,
  .07, .09, 7.5,
  .09, .12, 5,
  .12, 1, 0
) %>%
  mutate(Structure = "7_12_10")

score_structure_12_17_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  0, .12, 10,
  .12, .14, 7.5,
  .14, .17, 5,
  .17, 1, 0
) %>%
  mutate(Structure = "12_17_10")

score_structure_22_28_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  .28, 1, 10,
  .26, .28, 7.5,
  .22, .26, 5,
  0, .22, 0
) %>%
  mutate(Structure = "22_28_10")

score_structure_200_280_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  0, 200, 10,
  200, 240, 7.5,
  240, 280, 5,
  280, 9999, 0
) %>%
  mutate(Structure = "200_280_10")

score_structure_67_75_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  .75, 1, 10,
  .71, .75, 7.5,
  .67, .71, 5,
  0, .67, 0
) %>%
  mutate(Structure = "67_75_10")

score_structure_0_7_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  4, 7, 10,
  3, 3, 8,
  2, 2, 7,
  1, 1, 5, 
  0, 0, 0
) %>%
  mutate(Structure = "0_7_10")

score_structure_15_19_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  0, .15, 10,
  .15, .17, 7.5,
  .17, .19, 5,
  .19, 1, 0
) %>%
  mutate(Structure = "15_19_10")

score_structure_20_24_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  0, .2, 10,
  .2, .22, 7.5,
  .22, .24, 5,
  .24, 1, 0
) %>%
  mutate(Structure = "20_24_10")

score_structure_16_20_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  .2, 1, 10,
  .18, .2, 7.5,
  .16, .18, 5,
  0, .16, 0
) %>%
  mutate(Structure = "16_20_10")

score_structure_260_340_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  0, 260, 10,
  260, 300, 7.5,
  300, 340, 5,
  340, 9999, 0
) %>%
  mutate(Structure = "260_340_10")

score_structure_0_100_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  1, 1, 10,
  0, 1, 0
) %>%
  mutate(Structure = "0_100_10")

score_structure_14_18_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  .18, 1, 10,
  .16, .18, 7.5,
  .14, .16, 5,
  0, .14, 0
) %>%
  mutate(Structure = "14_18_10")

score_structure_150_210_10 <- tribble(
  ~GoalMin, ~GoalMax, ~Points,
  0, 150, 10,
  150, 170, 7.5,
  170, 210, 5,
  210, 9999, 0
) %>%
  mutate(Structure = "150_210_10")

pe_score_structure <- mget(ls(pattern="score_structure_")) %>%
  bind_rows()

rm(list = ls(pattern = "score_structure_"))

pe_entries_no_income <- pe_entries_no_income %>%
  mutate(Structure = if_else(ProjectType != 2, "34_40_10", "24_30_10"))

pe_exits_to_ph <- pe_exits_to_ph %>%
  mutate(Structure = case_when(
    ProjectType == 3 ~ "80_90_10",
    ProjectType %in% c(2, 13) ~ "75_83_10",
    ProjectType == 8 ~ "67_75_10"
  ))

pe_health_ins_at_exit <- pe_health_ins_at_exit %>%
  mutate(Structure = if_else(ProjectType != 8, "75_85_10", "67_75_10"))

pe_homeless_history_index <- pe_homeless_history_index %>%
  mutate(Structure = if_else(ProjectType != 3, "0_7_10", "0_7_10_PSH"))

pe_increase_income <- pe_increase_income %>%
  mutate(Structure = case_when(
    ProjectType == 2 ~ "24_30_10",
    ProjectType == 3 ~ "22_28_10",
    ProjectType == 8 ~ "16_20_10",
    ProjectType == 13 ~ "14_18_10"
  ))

save.image("images/ProjectEvaluation.RData")


