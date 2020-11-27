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

FileEnd <- format.Date(today(), "%m-%d-%Y")
FileStart <- format.Date(mdy("10012019"), "%m-%d-%Y")
FilePeriod <- interval(mdy(FileStart), mdy(FileEnd))

# CURRENT BEDS and EXPECTED UTILIZATION -----------------------------------

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
# provider-hhtype granularity
overflow_and_rrh_beds <- small_project %>%
  inner_join(small_inventory, by = "ProjectID") %>%
  mutate(DaysBedsAvailable = difftime(ymd(BedEndDate), ymd(BedStartDate),
                                      units = "days")) %>%
  group_by(ProjectID, ProjectName, ProjectType, HouseholdType) %>%
  summarise(DaysBedsAvailable = sum(DaysBedsAvailable)) %>%
  ungroup()

# provider granularity
BedCapacity <- small_project %>%
  inner_join(small_inventory, by = "ProjectID") %>%
  mutate(HHType = case_when(
    HouseholdType == 1 ~ "IND",
    HouseholdType == 3 ~ "FAM",
    HouseholdType == 4 ~ "CHILDONLY"
  ),
  DaysBedsAvailable = as.numeric(difftime(
    ymd(BedEndDate), 
    ymd(BedStartDate), 
    units = "days"
  ))) %>% 
  select(ProjectID,
         HHType,
         BedInventory,
         DaysBedsAvailable) %>%
  pivot_wider(
    names_from = HHType,
    values_from = BedInventory,
    names_prefix = "Current",
    values_fill = 0
  )

# ACTUAL UTILIZATION AND EXPECTED BEDS ------------------------------------

# ee granularity
small_enrollment <- Enrollment %>% 
  select(PersonalID,
         EnrollmentID,
         ProjectID,
         ProjectName,
         ProjectType,
         EntryDate,
         EntryAdjust,
         MoveInDateAdjust,
         ExitDate,
         ExitAdjust,
         HouseholdID,
         RelationshipToHoH,
         MoveInDate) %>%
  filter(served_between(., FileStart, FileEnd))

# ee granularity, filtered to moved-in rrhs & es overflows
Utilizers <- small_enrollment %>%
  semi_join(overflow_and_rrh_beds, by = "ProjectID") %>%
  mutate(StayWindow = interval(ymd(EntryAdjust), ymd(ExitAdjust))) %>%
  filter(
    int_overlaps(StayWindow, FilePeriod))


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
          ))
          , NULL
  )
}
# adding bed nights data
utilizers_clients <- Utilizers %>%
  mutate(
    BedNights = bed_nights_per_ee(Utilizers, FilePeriod),
    HHType = if_else(str_detect(HouseholdID, fixed("s_")), "IND", "FAM")
  ) %>%
  select(ProjectID, PersonalID, EnrollmentID, 
         EntryDate, MoveInDate, ExitDate, BedNights, HHType)

# provider granularity
project_bed_nights <- utilizers_clients %>%
  group_by(ProjectID, HHType) %>%
  summarise(ProjectBedNightsServed = sum(BedNights, na.rm = TRUE)) %>%
  ungroup()	%>%
  pivot_wider(names_from = HHType,
              values_from = ProjectBedNightsServed,
              names_prefix = "BedNightsServed",
              values_fill = 0)

# Estimates ---------------------------------------------------------------

EstimatedBeds <- BedCapacity %>%
  left_join(project_bed_nights, by = "ProjectID") %>%
  left_join(small_project[c("ProjectID", "ProjectName")], by = "ProjectID") %>%
  mutate(
    EstimatedFAM = BedNightsServedFAM / DaysBedsAvailable,
    EstimatedIND = BedNightsServedIND / DaysBedsAvailable,
    DifferenceFAM = round(EstimatedFAM - CurrentFAM, digits = 1),
    DifferenceIND = round(EstimatedIND - CurrentIND, digits = 1),
    EstimatedFAM = round(EstimatedFAM, digits = 1),
    EstimatedIND = round(EstimatedIND, digits = 1),
    SortColumn = abs(DifferenceFAM + DifferenceIND)
  ) %>%
  arrange(desc(SortColumn)) %>%
  select(
    ProjectID,
    ProjectName,
    DaysBedsAvailable,
    CurrentFAM,
    BedNightsServedFAM,
    EstimatedFAM,
    DifferenceFAM,
    CurrentIND,
    BedNightsServedIND,
    EstimatedIND,
    DifferenceIND
  )

write_csv(EstimatedBeds, "random_data/RRH_overflow_bed_changes.csv")
