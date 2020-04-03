library(tidyverse)
library(lubridate)
library(here)

a <- read_csv(here("data/youth_beds.csv")) 

a <- a %>% unique()

b <- read_csv(here("data/Inventory.csv"))

c <- a %>%
  left_join(b, by = "ProjectID") %>%
  unique() %>%
  select(Name,
         BedInventory,
         UnitInventory,
         InventoryStartDate,
         InventoryEndDate,
         HouseholdType) %>%
  filter(
    is.na(InventoryEndDate) &
      InventoryStartDate < today()
  ) 


single_beds <-  c %>%
  filter(HouseholdType %in% c(1, 4)) %>%
  group_by(Name) %>%
  summarise(SingleBeds = sum(BedInventory, na.rm = TRUE)) %>%
  ungroup()

family_units <-  c %>%
  filter(HouseholdType == 3) %>%
  group_by(Name) %>%
  summarise(FamilyUnits = sum(UnitInventory, na.rm = TRUE)) %>%
  ungroup()

final <- family_units %>%
  full_join(single_beds, by = "Name") %>%
  mutate(
    FamilyUnits = if_else(is.na(FamilyUnits), 0 , FamilyUnits),
    SingleBeds = if_else(is.na(SingleBeds), 0, SingleBeds),
    TotalUnits = FamilyUnits + SingleBeds
    ) %>%
  select(Name, TotalUnits)

write_csv(final, "reports/YouthProvidersUnits.csv")  
  
