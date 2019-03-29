library(tidyverse)
library(lubridate)
load("data/COHHIOHMIS.RData")

# Creating Beds table -----------------------------------------------------

SmallProject <- Project %>%
  filter(ProjectType %in% c(1, 2, 3, 8, 9) &
           (OperatingEndDate > today() |
              is.na(OperatingEndDate)) &
           is.na(GrantType)) %>%
  select(ProjectID,
         ProjectName,
         OperatingStartDate,
         OperatingEndDate,
         ProjectType)
SmallInventory <- Inventory %>%
  filter(!is.na(HMISParticipatingBeds) & 
           CoCCode == "OH-507" &
           (InventoryEndDate > today() |
              is.na(InventoryEndDate))) %>%
  select(
    ProjectID,
    HouseholdType,
    UnitInventory,
    BedInventory,
    InventoryStartDate,
    InventoryEndDate,
    HMISParticipatingBeds
  ) 
Beds <- inner_join(SmallProject, SmallInventory, by = "ProjectID")

# Creating Utilizers table ------------------------------------------------

SmallEnrollment <- Enrollment %>% 
  select(PersonalID,
         EnrollmentID,
         ProjectID,
         EntryDate,
         HouseholdID,
         MoveInDate)
SmallExit <- Exit %>%
  select(EnrollmentID, ExitDate)
Utilizers <- semi_join(SmallEnrollment, Beds, by = "ProjectID") %>%
  left_join(., SmallExit, by = "EnrollmentID")
# adding in the Project Type column here to calculate differently based on 
# MoveInDate
Utilizers <- left_join(Utilizers, SmallProject, by = "ProjectID") %>%
  select(
    PersonalID,
    EnrollmentID,
    ProjectID,
    ProjectType,
    HouseholdID,
    EntryDate,
    MoveInDate,
    ExitDate
  )

# Cleaning up the house ---------------------------------------------------

save(Beds, Utilizers, file = "data/BedUtilization.Rdata")
rm(list = ls())
load("data/BedUtilization.Rdata")

# Bed Nights Utilized -----------------------------------------------------

# filtering out any PSH or RRH records without a proper Move-In Date plus the 
# fake training providers
Utilizers <- Utilizers %>%
  mutate(ExitAdjust = if_else(is.na(ExitDate), today(), ExitDate),
         ExitDate = NULL,
         EntryAdjust = case_when(
           ProjectType %in% c(1, 2, 8) ~ EntryDate,
           ProjectType %in% c(3, 9, 13) ~ MoveInDate)
           ) %>%
  filter((
    (
      ProjectType %in% c(3, 9, 13) &
        !is.na(EntryAdjust) &
        ymd(MoveInDate) < ymd(EntryDate) &
        ymd(MoveInDate) > ymd(ExitAdjust)
    ) |
      ProjectType %in% c(1, 2, 8)
  ) &
    !ProjectID %in% c(1775, 1695, 1849, 1032, 1030, 1031, 1317)) %>%
  select(-EntryDate, -MoveInDate)
# adding in calculated columns to help get to Bed Nights
Utilizers <- Utilizers %>%
  mutate(
    BedNights = 
        difftime(ymd(ExitAdjust), ymd(EntryAdjust),
                 units = "days"),
    StayWindow = interval(ymd(EntryAdjust), ymd(ExitAdjust))#,
#    Month = use StayWindow to determine if it croses a month/year
  ) %>%
  filter(BedNights > 0)
# bare bones list of bed nights per provider (no date ranges)
Utilization <- Utilizers %>% select(ProjectID, BedNights) %>% 
  group_by(ProjectID) %>% 
  summarise(BedNights = sum(BedNights))

# Possible Bed Nights -----------------------------------------------------

BedNights <- Beds %>%
  select(ProjectID, BedInventory) %>%
  group_by(ProjectID) %>%
  summarise(BedInventory =
              sum(BedInventory) * 30)
