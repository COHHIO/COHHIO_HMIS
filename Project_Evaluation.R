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
reporting_year <- 2019

ReportStart <- paste0("0101", reporting_year)
ReportEnd <- paste0("1231", reporting_year)

# filter to only CoC-funded projects

coc_funded <- Funder %>%
  filter(Funder %in% c(1:7, 43) &
           ymd(StartDate) <= mdy(ReportEnd) &
           (is.na(EndDate) |
              ymd(EndDate) >= mdy(ReportStart))) %>%
  select(ProjectID, Funder)

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
    Destination %in% c(3, 10:11, 19:21, 28, 31) ~ "Household's Own Housing",
    Destination %in% c(21:22) ~ "Shared Housing",
    Destination %in% c(4:7, 15, 25:27, 29) ~ "Institutional",
    Destination %in% c(8, 9, 17, 24, 30, 99) ~ "Other",
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
  select(PersonalID, EnrollmentID, TotalMonthlyIncome, DataCollectionStage) 

pe_increase_income <- income_staging %>%
  pivot_wider(names_from = DataCollectionStage,
              values_from = TotalMonthlyIncome) %>%
  mutate(
    MostRecentIncome = case_when(
      !is.na(Exit) ~ Exit,!is.na(Update) ~ Update,!is.na(Annual) ~ Annual
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

# setting the FilePeriod to the date range of the Project Evaluation
FilePeriod <- interval(mdy(ReportStart), mdy(ReportEnd))
# rerunning the Utilization script with that new date interval
source("01_Bed_Unit_Utilization.R")
# getting what we need from the Utilization script
utilization_unit_2019 <- utilization_unit %>%
  ungroup() %>%
  select(ProjectType, ProjectName, "AvgUnitUtilization" = FilePeriod)

utilization_bed_2019 <- utilization_bed %>%
  ungroup() %>%
  select(ProjectType, ProjectName, "AvgBedUtilization" = FilePeriod)
# setting the FilePeriod back to what it was before
FilePeriod <- interval(mdy(FileStart), mdy(FileEnd))
# re-re-running the script so the image file is like it was
source("01_Bed_Unit_Utilization.R")

# Community Need: Res Prior = Streets or ESSH -----------------------------
# PSH, TH, SH (Street only), RRH

# Community Need: Entries with No Income ----------------------------------
# PSH, TH, SH, RRH

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

