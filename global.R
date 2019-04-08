library(tidyverse)
library(shiny)
library(shinyWidgets)
library(lubridate)
library(shinydashboard)

load("data/COHHIOHMIS.Rdata")

updatedate <- file.info("data/COHHIOHMIS.Rdata")$mtime

providerids <- Project %>% 
  select(ProjectID, ProjectName, OrganizationName) %>%
  arrange(ProjectName)

# load("data/Utilization.Rdata")

SmallProject <- Project %>%
  filter(ProjectType %in% c(1, 2, 3, 8, 9) &
           ymd(OperatingStartDate) <= today() &
           (is.na(OperatingEndDate) | OperatingEndDate >= today()) &
           is.na(Project$GrantType)) %>%
  select(ProjectID,
         ProjectName,
         ProjectType)

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

UnitCapacity <- SmallInventory %>%
  select(ProjectID,
         ProjectName,
         ProjectType,
         HouseholdType,
         UnitInventory,
         BedInventory,
         InventoryStartDate,
         InventoryEndDate) %>%
  mutate(UnitCount = if_else(HouseholdType == 3,
                             UnitInventory, BedInventory)) %>%
  group_by(ProjectID, ProjectName, ProjectType) %>%
  summarise(UnitCount = sum(UnitCount)) %>%
  ungroup()

BedCapacity <- SmallInventory %>%
  select(ProjectID,
         ProjectName,
         ProjectType,
         HouseholdType,
         UnitInventory,
         BedInventory,
         InventoryStartDate,
         InventoryEndDate) %>%
  group_by(ProjectID, ProjectName, ProjectType) %>%
  summarise(BedCount = sum(BedInventory)) %>%
  ungroup()





