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

# several measures will use this
# Adults who entered during date range

co_adults_all_entered <-  Enrollment %>%
  right_join(coc_funded, by = "ProjectID")  %>%
  filter(entered_between(., ReportStart, ReportEnd) &
         AgeAtEntry > 17) %>%
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)

# for ncb logic
# Adults who moved in and exited during date range

co_adults_movein_leavers <-  Enrollment %>%
  right_join(coc_funded, by = "ProjectID")  %>%
  filter(exited_between(., ReportStart, ReportEnd) &
           stayed_between(., ReportStart, ReportEnd) &
         AgeAtEntry > 17) %>%
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)	

# increase income
#Adults who moved in and were served during date range

co_adults_movein_all <-  Enrollment %>%
  right_join(coc_funded, by = "ProjectID")  %>%
  filter(stayed_between(., ReportStart, ReportEnd) &
         AgeAtEntry > 17) %>%
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)	

# health insurance
# Clients who moved in and exited during date range

co_client_movein_leavers <-  Enrollment %>%
  right_join(coc_funded, by = "ProjectID")  %>%
  filter(exited_between(., ReportStart, ReportEnd) &
           stayed_between(., ReportStart, ReportEnd)) %>%
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)	

# exits to PH, but needs an added filter of only mover-inners
# Heads of Household who were served during date range

co_hohs_all <-  Enrollment %>%
  right_join(coc_funded, by = "ProjectID")  %>%
  filter(served_between(., ReportStart, ReportEnd) &
         RelationshipToHoH == 1) %>%
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)	

# own housing and LoS
# Heads of Household who moved in and exited during date range

co_hohs_movein_leavers <-  Enrollment %>%
  right_join(coc_funded, by = "ProjectID")  %>%
  filter(
    stayed_between(., ReportStart, ReportEnd) &
      exited_between(., ReportStart, ReportEnd) &
      RelationshipToHoH == 1
  ) %>% 
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)	


# Housing Stability: Exits to PH ------------------------------------------
# PSH (includes stayers tho), TH, SH, RRH

exits_to_ph <- co_adults_movein_all %>%
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
  select(ProjectType, ProjectName, PersonalID, EntryDate, MoveInDate, ExitDate, Destination,
         DestinationGroup, MeetsObjective)

# Housing Stability: Moved into Own Housing -------------------------------
# TH, SH, RRH

own_housing <- co_hohs_movein_leavers %>%
  mutate(MeetsObjective = case_when(
    Destination %in% c(3, 10:11, 19:21, 28, 31) ~ 1,
    !Destination %in% c(3, 10:11, 19:21, 28, 31) ~ 0
  ))

# Housing Stability: 6 mo Recurrence --------------------------------------
# PSH, TH, SH, RRH

# Housing Stability: 6-24 mo Recurrence -----------------------------------
# PSH, TH, SH, RRH

# Accessing Mainstream Resources: NCBs ------------------------------------
# PSH, TH, SH, RRH

non_cash_at_exit <- co_adults_movein_leavers %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(vars_we_want, BenefitsFromAnySource, DataCollectionStage) %>%
  group_by(PersonalID,
           ProjectType,
           VeteranStatus,
           EnrollmentID,
           ProjectName,
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
           ExitAdjust) %>%
  summarise(MostRecentNCB = BenefitsFromAnySource[max(DataCollectionStage)]) %>%
  mutate(MeetsObjective =
           case_when(MostRecentNCB == 1 ~ 1,
                     MostRecentNCB != 1 |
                       is.na(MostRecentNCB) ~ 0))

# Accessing Mainstream Resources: Health Insurance ------------------------
# PSH, TH, SH, RRH

health_ins_at_exit <- co_client_movein_leavers %>%
  left_join(IncomeBenefits, by = c("PersonalID", "EnrollmentID")) %>%
  select(vars_we_want, InsuranceFromAnySource, DataCollectionStage) %>%
  group_by(PersonalID,
           ProjectType,
           VeteranStatus,
           EnrollmentID,
           ProjectName,
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
           ExitAdjust) %>%
  summarise(MostRecentHI = InsuranceFromAnySource[max(DataCollectionStage)]) %>%
  mutate(MeetsObjective = case_when(MostRecentHI == 1 ~ 1,
                                    MostRecentHI != 1 | is.na(MostRecentHI) ~ 0))

# Accessing Mainstream Resources: Increase Total Income -------------------
# PSH, TH, SH, RRH

# tried to use spread() for this but no dice. :(
income_staging <- IncomeBenefits %>%
  select(EnrollmentID, TotalMonthlyIncome, DataCollectionStage) %>%
  mutate(
    IncomeAtEntry = if_else(DataCollectionStage == 1, TotalMonthlyIncome, NULL),
    IncomeAtExit = if_else(DataCollectionStage == 3, TotalMonthlyIncome, NULL),
    IncomeAtUpdate = if_else(DataCollectionStage == 2, TotalMonthlyIncome, NULL),
    IncomeAtAnnual = if_else(DataCollectionStage == 5, TotalMonthlyIncome, NULL)
  ) %>%
  select(-TotalMonthlyIncome, -DataCollectionStage) %>%
  group_by(EnrollmentID) %>%
  summarise(IncomeAtEntry = max(IncomeAtEntry, na.rm = TRUE), 
            IncomeAtExit = max(IncomeAtExit, na.rm = TRUE),
            IncomeAtUpdate = max(IncomeAtUpdate, na.rm = TRUE),
            IncomeAtAnnual = max(IncomeAtAnnual, na.rm = TRUE))

  
increase_income <- co_adults_movein_all %>%
  left_join(income_staging, by = "EnrollmentID") %>%
  select(vars_we_want, 
         IncomeAtEntry, 
         IncomeAtExit, 
         IncomeAtUpdate, 
         IncomeAtAnnual) %>%
  mutate(
    MeetsObjective = if_else(
      IncomeAtEntry < IncomeAtExit |
        IncomeAtEntry < IncomeAtUpdate |
        IncomeAtEntry < IncomeAtAnnual, 
      1,
      0
    )
  )

# Housing Stability: Length of Time Homeless ------------------------------
# TH, SH, RRH

TotalLeavers <- co_hohs_movein_leavers %>%
  group_by(ProjectName) %>%
  summarise(Leavers = n())

length_of_stay_summary <- co_hohs_movein_leavers %>%
  mutate(DaysInProject = difftime(ymd(ExitAdjust), ymd(EntryDate))) %>%
  group_by(ProjectName, ProjectType) %>%
  summarise(
    AverageDays = as.numeric(mean(DaysInProject)),
    MedianDays = as.numeric(median(DaysInProject))
  ) %>%
  left_join(TotalLeavers, by = "ProjectName") 

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

hhi_detail <- co_adults_all_entered %>%
  select(
    PersonalID,
    ProjectName,
    ProjectType,
    EntryDate,
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

hhi_summary <- hhi_detail %>%
  group_by(ProjectType, ProjectName) %>%
  summarise(AverageHHI = mean(HHI))

# Community Need: Long Term Homeless Households ---------------------------
# PSH

# HMIS Data Quality -------------------------------------------------------
# PSH, TH, SH, RRH

load("images/Data_Quality.RData")

dq_items_being_checked <- dq_2019 %>%
  filter(Type %in% c("Error", "High Priority") & ProjectType %in% c(2, 3, 13, 8)) %>% 
  select(Issue) %>% 
  unique() 

