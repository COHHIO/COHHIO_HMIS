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

# ACTUAL BEDS and EXPECTED UTILIZATION ------------------------------------

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
    CoCCode %in% c("OH-507", "OH-504")) %>%
  mutate(
    BedStartDate = if_else(
      ymd(InventoryStartDate) > mdy(FileStart),
      InventoryStartDate,
      mdy(FileStart)
    ),
    BedEndDate = if_else(
      ymd(InventoryEndDate) < mdy(FileEnd) &
        !is.na(InventoryEndDate),
      InventoryEndDate,
      mdy(FileEnd)
    )
  ) %>% 
  select(
    ProjectID,
    HouseholdType,
    UnitInventory,
    BedInventory,
    BedStartDate,
    BedEndDate,
    Availability
  )

overflow_and_rrh_beds <-
  inner_join(small_project, small_inventory, by = "ProjectID")

BedCapacity <- small_project %>%
  inner_join(small_inventory, by = "ProjectID") %>%
  mutate(HHType = case_when(
    HouseholdType == 1 ~ "IND",
    HouseholdType == 3 ~ "FAM",
    HouseholdType == 4 ~ "CHILDONLY"
  ),
  PossibleBedNights = as.numeric(difftime(
    ymd(BedEndDate), 
    ymd(BedStartDate), 
    units = "days"
  ))) %>% 
  select(ProjectID,
         HHType,
         BedInventory,
         PossibleBedNights) %>%
  pivot_wider(
    names_from = HHType,
    values_from = BedInventory,
    names_prefix = "Actual",
    values_fill = 0
  )

# ACTUAL UTILIZATION AND EXPECTED BEDS ------------------------------------

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

Utilizers <- left_join(Utilizers, overflow_and_rrh_beds, by = "ProjectID") %>%
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

# filtering out any fake training providers
utilizers_clients <- Utilizers %>%
  mutate(StayWindow = interval(ymd(EntryAdjust), ymd(ExitAdjust))) %>%
  filter(
    int_overlaps(StayWindow, FilePeriod) &
      !ProjectID %in% c(1775, 1695, fake_projects))


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

utilizers_clients <- utilizers_clients %>%
  mutate(
    FilePeriod = bed_nights_per_ee(utilizers_clients, FilePeriod),
    HHType = if_else(str_detect(HouseholdID, fixed("s_")), "IND", "FAM")
  ) %>%
  select(ProjectName, ProjectID, ProjectType, PersonalID, EnrollmentID, 
         EntryDate, MoveInDate, ExitDate, FilePeriod, HHType)

utilizers_clients <- as.data.frame(utilizers_clients)

# making granularity by provider instead of by enrollment id
utilizers_clients <- utilizers_clients %>%
  group_by(ProjectID, ProjectName, ProjectType, HHType) %>%
  summarise(BedNights = sum(FilePeriod, na.rm = TRUE)) %>%
  ungroup()	  



# Estimates ---------------------------------------------------------------

EstimatedBeds <- utilizers_clients %>%
  mutate(ApproxBeds = BedNights / PossibleBedNights) %>%
  pivot_wider(names_from = HHType,
              values_from = ApproxBeds,
              names_prefix = "Estimated") %>%
  mutate(
    DifferenceFAM = round(EstimatedFAM - ActualFAM, digits = 1),
    DifferenceIND = round(EstimatedIND - ActualIND, digits = 1),
    EstimatedFAM = round(EstimatedFAM, digits = 1),
    EstimatedIND = round(EstimatedIND, digits = 1)
  )

write_csv(EstimatedBeds, "random_data/RRH_overflow_bed_changes.csv")
