library(tidyverse)
library(lubridate)
project <- read_csv("data/hudcsvoneday/Project.csv") %>%
  mutate(OperatingStartDate = format.Date(OperatingStartDate, "%Y-%m-%d"),
         OperatingEndDate = format.Date(OperatingEndDate, "%Y-%m-%d"),
         DateCreated = format.Date(DateCreated, "%Y-%m-%d %T"),
         DateUpdated = format.Date(DateUpdated, "%Y-%m-%d %T")) %>%
  filter(!is.na(ProjectType) & ProjectType %in% c(1:3, 8:11, 13))
write_csv(project, "data/hudcsvoneday/Project.csv", 
          na = "",  
          quote_escape = "backslash")

organization <- read_csv("data/hudcsvoneday/Organization.csv") %>%
  mutate(DateCreated = format.Date(DateCreated, "%Y-%m-%d %T"),
         DateUpdated = format.Date(DateUpdated, "%Y-%m-%d %T"))
write_csv(organization, "data/hudcsvoneday/Organization.csv", 
          na = "",  
          quote_escape = "backslash")

inventory <- read_csv("data/hudcsvoneday/Inventory.csv") %>%
  mutate(InventoryStartDate = format.Date(InventoryStartDate, "%Y-%m-%d"),
         InventoryEndDate = format.Date(InventoryEndDate, "%Y-%m-%d"),
         DateCreated = format.Date(DateCreated, "%Y-%m-%d %T"),
         DateUpdated = format.Date(DateUpdated, "%Y-%m-%d %T"),
         InformationDate = format.Date(InformationDate, "%Y-%m-%d"),
         HMISParticipatingBeds = 
           if_else(is.na(HMISParticipatingBeds), 0, HMISParticipatingBeds),
         BedType = 1) %>% # THIS SHOULD NOT BE NECESSARY!!!
  select(InventoryID, ProjectID, CoCCode, InformationDate, HouseholdType, 
         Availability, UnitInventory, BedInventory, CHBedInventory, 
         VetBedInventory, YouthBedInventory, BedType, InventoryStartDate,
         InventoryEndDate, HMISParticipatingBeds, DateCreated, DateUpdated,
         UserID, DateDeleted, ExportID)
write_csv(inventory, "data/hudcsvoneday/Inventory.csv", 
          na = "",  
          quote_escape = "backslash")

geography <- read_csv("data/hudcsvoneday/Geography.csv") %>%
  mutate(DateCreated = format.Date(DateCreated, "%Y-%m-%d %T"),
         DateUpdated = format.Date(DateUpdated, "%Y-%m-%d %T"),
         InformationDate = format.Date(InformationDate, "%Y-%m-%d")) %>%
  filter(!is.na(GeographyType))
write_csv(geography, "data/hudcsvoneday/Geography.csv", 
          na = "",  
          quote_escape = "backslash")

funder <- read_csv("data/hudcsvoneday/Funder.csv") %>%
  mutate(StartDate = format.Date(StartDate, "%Y-%m-%d"),
         EndDate = format.Date(EndDate, "%Y-%m-%d"),
         DateCreated = format.Date(DateCreated, "%Y-%m-%d %T"),
         DateUpdated = format.Date(DateUpdated, "%Y-%m-%d %T")) %>%
  filter(!is.na(Funder))
write_csv(funder, "data/hudcsvoneday/Funder.csv", 
          na = "",  
          quote_escape = "backslash")
