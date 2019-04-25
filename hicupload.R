library(tidyverse)
library(readxl)

# Project file ------------------------------------------------------------
project <- read_csv("data/hudcsvoneday/Project.csv") %>%
  mutate(OperatingStartDate = format.Date(OperatingStartDate, "%Y-%m-%d"),
         OperatingEndDate = format.Date(OperatingEndDate, "%Y-%m-%d"),
         DateCreated = format.Date(DateCreated, "%Y-%m-%d %T"),
         DateUpdated = format.Date(DateUpdated, "%Y-%m-%d %T")) %>%
  filter(!is.na(ProjectType) & ProjectType %in% c(1:3, 8:11, 13))

# Pulling in PIT data - this data comes from the 0630 and 0628 reports. i just had to 
# force-by-Excel the data into these columns. ugly but effective.
pit <- read_csv("data/hudcsvoneday/PIT2019.csv")

project <- project %>% select(-PITCount) %>%
  left_join(., pit, by = "ProjectID") %>%
  select(1:13, 19, 14:18) %>%
  mutate(ProjectName = if_else(is.na(ProjectCommonName), ProjectName, ProjectCommonName),
         PITCount = if_else(is.na(PITCount), 0, PITCount))

rm(pit)

write_csv(project, "data/hudcsvoneday/Project.csv", 
          na = "",  
          quote_escape = "backslash")

# Organization file -------------------------------------------------------
organization <- read_csv("data/hudcsvoneday/Organization.csv") %>%
  mutate(DateCreated = format.Date(DateCreated, "%Y-%m-%d %T"),
         DateUpdated = format.Date(DateUpdated, "%Y-%m-%d %T"))

write_csv(organization, "data/hudcsvoneday/Organization.csv", 
          na = "",  
          quote_escape = "backslash")

# Inventory file ----------------------------------------------------------
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

# Geography file ----------------------------------------------------------
geography <- read_csv("data/hudcsvoneday/Geography.csv") %>%
  mutate(DateCreated = format.Date(DateCreated, "%Y-%m-%d %T"),
         DateUpdated = format.Date(DateUpdated, "%Y-%m-%d %T"),
         InformationDate = format.Date(InformationDate, "%Y-%m-%d")) %>%
  filter(!is.na(GeographyType)) %>%
  select(-Address1, -Address2, -City, -State, -ZIP)
addresses <- read_xlsx("data/RMisc.xlsx",
                      sheet = 5,
                      range = cell_cols(c("A", "I:M"))) #SHOULD NOT BE NECESSARY
geography <- left_join(geography, addresses, by = "ProjectID") %>%
  mutate(Address1 = `Address Line1`,
         Address2 = `Address Line2`,
         City = `Address City`,
         State = `Address Province`,
         ZIP = `Address Postal Code`) %>%
  select(GeographyID, ProjectID, CoCCode, InformationDate, Geocode, 
         GeographyType, Address1, Address2, City, State, ZIP, DateCreated,
         DateUpdated, UserID, DateDeleted, ExportID)
write_csv(geography, "data/hudcsvoneday/Geography.csv", 
          na = "",  
          quote_escape = "backslash")

rm(addresses)

# Funder file -------------------------------------------------------------
funder <- read_csv("data/hudcsvoneday/Funder.csv") %>%
  mutate(StartDate = format.Date(StartDate, "%Y-%m-%d"),
         EndDate = format.Date(EndDate, "%Y-%m-%d"),
         DateCreated = format.Date(DateCreated, "%Y-%m-%d %T"),
         DateUpdated = format.Date(DateUpdated, "%Y-%m-%d %T")) %>%
  filter(!is.na(Funder))
write_csv(funder, "data/hudcsvoneday/Funder.csv", 
          na = "",  
          quote_escape = "backslash")




