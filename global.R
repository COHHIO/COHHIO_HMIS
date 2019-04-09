library(tidyverse)
library(shiny)
library(shinyWidgets)
library(lubridate)
library(shinydashboard)
library(scales)

load("data/COHHIOHMIS.Rdata")

updatedate <- file.info("data/COHHIOHMIS.Rdata")$mtime

# load("data/Utilization.Rdata")

SmallProject <- Project %>%
  filter(ProjectType %in% c(1, 2, 3, 8, 9, 13) &
           ymd(OperatingStartDate) <= today() &
           (is.na(OperatingEndDate) | OperatingEndDate >= today()) &
           is.na(Project$GrantType)) %>%
  select(ProjectID,
         ProjectName,
         ProjectType, 
         OrganizationName)

SmallInventory <- Inventory %>%
  filter((ymd(InventoryStartDate) <= today() &
            (
              ymd(InventoryEndDate) >= today() |
                is.na(InventoryEndDate)
            )) &
           !is.na(HMISParticipatingBeds) &
           Inventory$CoCCode == "OH-507") %>%
  select(
    ProjectID,
    HouseholdType,
    UnitInventory,
    BedInventory,
    InventoryStartDate,
    InventoryEndDate,
    HMISParticipatingBeds
  )

SmallInventory <- inner_join(SmallProject, SmallInventory, by = "ProjectID")

Capacity <- SmallInventory %>%
  select(ProjectID,
         ProjectName,
         ProjectType,
         OrganizationName,
         HouseholdType,
         UnitInventory,
         BedInventory,
         InventoryStartDate,
         InventoryEndDate) %>%
  mutate(UnitCount = if_else(HouseholdType == 3,
                             UnitInventory, BedInventory)) %>%
  group_by(ProjectID, ProjectName, ProjectType, OrganizationName) %>%
  summarise(UnitCount = sum(UnitCount),
            BedCount = sum(BedInventory)) %>%
  ungroup()

providerids <- Capacity %>% 
  select(ProjectID, ProjectName, OrganizationName) %>%
  arrange(ProjectName)

Clients <- Enrollment %>%
  left_join(., providerids, by = "ProjectID") %>%
  filter(is.na(ExitDate)) %>%
  group_by(ProjectID, ProjectName) %>%
  summarise(Clients = n_distinct(PersonalID)) %>%
  ungroup()

Households <- Enrollment %>%
  left_join(., providerids, by = "ProjectID") %>%
  filter(is.na(ExitDate)) %>%
  group_by(ProjectID, ProjectName) %>%
  summarise(Households = n_distinct(HouseholdID)) %>%
  ungroup()

Utilization <-
  left_join(Capacity, Clients,
            by = c("ProjectID", "ProjectName")) %>%
  left_join(., Households, 
            by = c("ProjectID", "ProjectName")) %>%
  filter(ProjectType %in% c(1, 2, 3, 8, 9)) %>%
  mutate(BedUtilization = percent(Clients/BedCount),
         UnitUtilization = percent(Households/UnitCount))
rm(Households, Clients, Capacity)

