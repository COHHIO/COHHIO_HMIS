library(tidyverse)
library(lubridate)
load("data/COHHIOHMIS.RData")

smallInventory <- Inventory %>%
  filter(beds_available_between(Inventory, "10012017", "09302018") == TRUE) %>%
  select(ProjectID, HouseholdType, BedInventory) %>%
  mutate(HouseholdType = case_when(
    HouseholdType == 1 ~ "Individual",
    HouseholdType == 3 ~ "Households",
    HouseholdType == 4 ~ "ChildrenOnly"
  ))

x <- rowid_to_column(smallInventory)

hhtypes <- spread(x, HouseholdType, BedInventory) %>%
  group_by(ProjectID) %>%
  summarise(ChildrenOnlyBeds = sum(ChildrenOnly, na.rm = TRUE),
            HouseholdBeds = sum(Households, na.rm = TRUE),
            IndividualBeds = sum(Individual, na.rm = TRUE)) %>%
  mutate(percentHHBeds = HouseholdBeds/
           (HouseholdBeds + IndividualBeds))

smallEnrollment <- Enrollment %>%
  filter(served_between(Enrollment, "10012017", "09302018") == TRUE) %>%
  select(ProjectID, HouseholdID) %>%
  mutate(HouseholdType = case_when(
    grepl("s_", HouseholdID) == TRUE ~ "IndividualsServed",
    grepl("h_", HouseholdID) == TRUE ~ "HouseholdsServed"
  )) %>% unique()

y <- smallEnrollment %>%
  group_by(ProjectID, HouseholdType) %>%
  summarise(Count = n())

hhserved <- spread(y, HouseholdType, Count, fill = 0) %>%
  mutate(percentHouseholds = HouseholdsServed/
           (HouseholdsServed + IndividualsServed))

z <- full_join(hhtypes, hhserved)

possibleissues <- z %>%
  mutate(
    Diff = abs(percentHHBeds - percentHouseholds)) %>%
  filter(Diff > .75)


