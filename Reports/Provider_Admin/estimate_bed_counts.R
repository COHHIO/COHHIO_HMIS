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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.


library(tidyverse)
library(lubridate)
library(scales)
library(HMIS)
library(here)

load(here("images/COHHIOHMIS.RData"))
load(here("images/cohorts.RData"))

# despite the fact we're pulling in usually more than 2 years of data, the 
# utilization reporting will only go back 2 years. (decision based on lack of
# a need to go back further and time to code all that.)
FileEnd <- format.Date(today(), "%m-%d-%Y")
FileStart <- format.Date(mdy("10012019"), "%m-%d-%Y")
FilePeriod <- interval(mdy(FileStart), mdy(FileEnd))

# Creating Beds table -----------------------------------------------------

small_project <- Project %>%
  filter((ProjectType == 13 |
            str_detect(ProjectName, "Overflow")) &
           operating_between(Project, FileStart, FileEnd) &
           HMISParticipatingProject == 1) %>%
  select(ProjectID,
         ProjectName,
         ProjectType)

small_inventory <- Inventory  %>%
  filter((
    ymd(InventoryStartDate) <= mdy(FileEnd) &
      (
        ymd(InventoryEndDate) >= mdy(FileStart) |
          is.na(InventoryEndDate)
      )
  ) &
    CoCCode == "OH-507") %>%
  select(
    ProjectID,
    HouseholdType,
    UnitInventory,
    BedInventory,
    InventoryStartDate,
    InventoryEndDate,
    Availability
  )

overflow_and_rrh_beds <- inner_join(small_project, small_inventory, by = "ProjectID")

change_availability_to_overflow <- overflow_and_rrh_beds %>%
  filter(Availability != 3 & ProjectType == 1) %>%
  select(ProjectID, ProjectName) %>%
  unique()

write_csv(change_availability_to_overflow, here("random_data/change_availability_to_overflow.csv"))

# Creating Utilizers table ------------------------------------------------

small_enrollment <- Enrollment %>% 
  select(PersonalID,
         EnrollmentID,
         ProjectID,
         EntryDate,
         EntryAdjust,
         MoveInDateAdjust,
         ExitDate,
         ExitAdjust,
         HouseholdID,
         RelationshipToHoH,
         MoveInDate) %>%
  filter(served_between(., FileStart, FileEnd))

Utilizers <- semi_join(small_enrollment, overflow_and_rrh_beds, by = "ProjectID") 

Utilizers <- left_join(Utilizers, small_project, by = "ProjectID") %>%
  select(
    PersonalID,
    EnrollmentID,
    ProjectID,
    ProjectName,
    ProjectType,
    HouseholdID,
    RelationshipToHoH,
    EntryDate,
    EntryAdjust,
    MoveInDate,
    MoveInDateAdjust,
    ExitDate,
    ExitAdjust
  )

# Cleaning up the house ---------------------------------------------------

rm(Affiliation, Client, Disabilities, EmploymentEducation, EnrollmentCoC, Exit, 
   Export, Funder, HealthAndDV, IncomeBenefits, Organization, 
   ProjectCoC, Scores, Services, small_enrollment, small_inventory, small_project, 
   Users, Offers, VeteranCE, CaseManagers, Referrals, HUD_specs, stray_services)

# Client Utilization of Beds ----------------------------------------------

# filtering out any fake training providers
utilizers_clients <- Utilizers %>%
  mutate(StayWindow = interval(ymd(EntryAdjust), ymd(ExitAdjust))) %>%
  filter(
    int_overlaps(StayWindow, FilePeriod) &
      !ProjectID %in% c(1775, 1695, fake_projects))

# filtering Beds object to exclude any providers that served 0 hhs in date range

Beds <- overflow_and_rrh_beds %>%
  right_join(utilizers_clients %>%
               select(ProjectID) %>%
               unique(), by = "ProjectID")

# function for adding bed nights per ee

bed_nights_per_ee <- function(table, interval) {
  # if the ee date range and a given interval (in my reporting, a month) overlap,
  if_else(int_overlaps(table$StayWindow, interval),
          # then return the difference between
          as.numeric(difftime(
            # if the exit date precedes the end of the interval, then the exit 
            # date, otherwise the end of the interval 
            if_else(
              ymd(table$ExitAdjust) <=  int_end(interval),
              as.POSIXct(table$ExitAdjust),
              int_end(interval) + days(1)
            ),
            # if the entry date is after the start of the interval, then the 
            # entry date, otherwise the beginning of the interval
            if_else(
              ymd(table$EntryAdjust) >= int_start(interval),
              as.POSIXct(table$EntryAdjust),
              int_start(interval)
            ),
            # give it to me in days
            units = "days"
          )), NULL
  )
}

# adding in month columns with utilization numbers

utilizers_clients <- utilizers_clients %>%
  mutate(
    FilePeriod = bed_nights_per_ee(utilizers_clients, FilePeriod),
    HHType = if_else(str_detect(HouseholdID, fixed("s_")), "IND", "FAM")
  ) %>%
  select(ProjectName, ProjectID, ProjectType, PersonalID, EnrollmentID, 
         EntryDate, MoveInDate, ExitDate, FilePeriod, HHType)

utilizers_clients <- as.data.frame(utilizers_clients)

# making granularity by provider instead of by enrollment id
BedNights <- utilizers_clients %>%
  group_by(ProjectName, ProjectID, ProjectType, HHType) %>%
  summarise(BedNights = sum(FilePeriod, na.rm = TRUE)) %>%
  ungroup()

# per night estimate

EstimatedBeds <- BedNights %>%
  mutate(ApproxBeds = BedNights /
                              as.numeric(difftime(
                                mdy(FileEnd), 
                                mdy(FileStart), 
                                units = "days"
                              ))) %>%
  select(-BedNights) %>%
  pivot_wider(names_from = HHType, values_from = ApproxBeds,
              names_prefix = "Estimated")

# Bed Capacity ------------------------------------------------------------

BedCapacity <- overflow_and_rrh_beds %>%
  mutate(HHType = case_when(
    HouseholdType == 1 ~ "IND",
    HouseholdType == 3 ~ "FAM",
    HouseholdType == 4 ~ "CHILDONLY"
  )) %>% 
  select(ProjectID,
         HHType,
         BedInventory) %>%
  pivot_wider(names_from = HHType, values_from = BedInventory,
              names_prefix = "Actual")

EstimateComparisons <- EstimatedBeds %>%
  left_join(BedCapacity, by = "ProjectID")

# function for bed capacity at the bed record level

bed_capacity <- function(interval) {
  if_else(int_overlaps(BedCapacity$AvailableWindow, interval),
          (as.numeric(difftime(
            if_else(
              ymd(BedCapacity$InventoryEndAdjust) <=  int_end(interval),
              as.POSIXct(BedCapacity$InventoryEndAdjust),
              int_end(interval)
            ),
            if_else(
              ymd(BedCapacity$InventoryStartAdjust) >= int_start(interval),
              as.POSIXct(BedCapacity$InventoryStartAdjust),
              int_start(interval)
            ),
            units = "days"
          ))+1) * BedCapacity$BedInventory, NULL
  )
}

BedCapacity <- BedCapacity %>%
  mutate(FilePeriod = bed_capacity(FilePeriod)) %>% 
  select(
    -InventoryStartDate,
    -InventoryEndDate,
    -InventoryEndAdjust,-BedInventory,
    -InventoryStartAdjust,
    -AvailableWindow
  )

BedCapacity <- BedCapacity %>%
  group_by(ProjectID, ProjectName, ProjectType) %>%
  summarise(BedCapacity = sum(FilePeriod, na.rm = TRUE)) %>%
  ungroup()

# Bed Utilization ---------------------------------------------------------

utilization_bed <- 
  left_join(BedCapacity,
            BedNights,
            by = c("ProjectID", "ProjectName", "ProjectType")) %>%
  mutate(
    FilePeriod = BedNights / BedCapacity, accuracy = .1
  ) %>% 
  select(ProjectID, ProjectName, ProjectType, FilePeriod) %>%
  ungroup()

rm(BedCapacity, BedNights) 

# Inf means there were no beds but there were clients served.
# %NaN means there were no beds and no clients served that month.

# HH Utilization of Units -------------------------------------------------

HHUtilizers <- Utilizers %>%
  mutate(
    EntryAdjust = case_when(
      ProjectType %in% c(lh_project_types) ~ EntryDate,
      ProjectType %in% c(3, 9) ~ MoveInDate
    ),
    ExitAdjust = if_else(
      is.na(ExitDate) & ymd(EntryAdjust) <= mdy(FileEnd),
      mdy(FileEnd),
      ymd(ExitDate)
    ),
    StayWindow = interval(ymd(EntryAdjust), ymd(ExitAdjust))
  ) %>%
  filter(
    str_detect(HouseholdID, fixed("s_")) |
      (str_detect(HouseholdID, fixed("h_")) &
         RelationshipToHoH == 1) &
      int_overlaps(StayWindow, FilePeriod) &
      (
        (
          ProjectType %in% c(3, 9) &
            !is.na(EntryAdjust) &
            ymd(MoveInDate) >= ymd(EntryDate) &
            ymd(MoveInDate) <= ymd(ExitAdjust)
        ) |
          ProjectType %in% c(lh_project_types)
      ) &
      !ProjectID %in% c(1775, 1695, fake_projects)
  ) %>%
  select(-EntryDate,-MoveInDate,-HouseholdID,-RelationshipToHoH)

HHUtilizers <- HHUtilizers %>%
  mutate(FilePeriod = bed_nights_per_ee(HHUtilizers, FilePeriod))

HHUtilizers <- as.data.frame(HHUtilizers)

# making granularity by provider instead of by enrollment id
HHNights <- HHUtilizers %>%
  group_by(ProjectName, ProjectID, ProjectType) %>%
  summarise(HHNights = sum(FilePeriod, na.rm = TRUE)) %>%
  ungroup()

# leaving this one ^^ because the client-level 
# detail should be good enough for R minor elevated

rm(HHUtilizers) 


# Unit Capacity -----------------------------------------------------------

UnitCapacity <- Beds %>%
  select(ProjectID,
         ProjectName,
         ProjectType,
         HouseholdType,
         UnitInventory,
         BedInventory,
         InventoryStartDate,
         InventoryEndDate) %>%
  mutate(InventoryEndAdjust = if_else(is.na(InventoryEndDate),
                                      mdy(FileEnd),
                                      ymd(InventoryEndDate)),
         InventoryStartAdjust = if_else(ymd(InventoryStartDate) >= mdy(FileStart),
                                        ymd(InventoryStartDate),
                                        mdy(FileStart)),
         AvailableWindow = interval(ymd(InventoryStartAdjust),
                                    ymd(InventoryEndAdjust)),
         UnitCount = if_else(HouseholdType == 3,
                             UnitInventory, BedInventory)) 

# function to calculate unit capacity at the bed record level

unit_capacity <- function(interval) {
  if_else(
    int_overlaps(UnitCapacity$AvailableWindow, interval),
    (as.numeric(
      difftime(
        if_else(
          ymd(UnitCapacity$InventoryEndAdjust) <=  int_end(interval),
          as.POSIXct(UnitCapacity$InventoryEndAdjust),
          int_end(interval)
        ),
        if_else(
          ymd(UnitCapacity$InventoryStartAdjust) >= int_start(interval),
          as.POSIXct(UnitCapacity$InventoryStartAdjust),
          int_start(interval)
        ),
        units = "days"
      )
    ) + 1) * UnitCapacity$UnitCount,
    NULL
  )
}

UnitCapacity <- UnitCapacity %>%
  mutate(FilePeriod = unit_capacity(FilePeriod))

UnitCapacity <- UnitCapacity %>%
  group_by(ProjectID, ProjectName, ProjectType) %>%
  summarise(UnitCapacity = sum(FilePeriod, na.rm = TRUE)) %>%
  ungroup()

# Unit Utilization --------------------------------------------------------

utilization_unit <- left_join(UnitCapacity,
                              HHNights,
                              by = c("ProjectID", "ProjectName", "ProjectType")) %>%
  mutate(FilePeriod = HHNights / UnitCapacity,
         accuracy = .1) %>% 
  select(ProjectID,
         ProjectName,
         ProjectType,
         FilePeriod)

rm(UnitCapacity, HHNights, Beds, Utilizers)

rm(bed_capacity, bed_nights_per_ee, unit_capacity)

small_project <- Project %>%
  filter(ProjectType %in% c(project_types_w_beds) &
           ymd(OperatingStartDate) <= today() &
           (is.na(OperatingEndDate) | OperatingEndDate >= today()) &
           is.na(Project$GrantType)) %>%
  select(ProjectID,
         ProjectName,
         ProjectType, 
         OrganizationName,
         HMISParticipatingProject)
