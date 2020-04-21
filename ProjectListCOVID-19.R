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

load("images/COHHIOHMIS.RData")

inventory <- Inventory %>%
  filter(beds_available_between(., "04212020", "04212020") &
         CoCCode == "OH-507") %>%  
  select(ProjectID,
         HouseholdType,
         BedInventory,
         InventoryStartDate,
         InventoryEndDate) %>%
  group_by(ProjectID) %>%
  summarise(
    householdtype = if_else(
      min(HouseholdType, na.rm = FALSE) != max(HouseholdType, na.rm = FALSE),
      0,
      min(HouseholdType, na.rm = FALSE)
    ),
    TotalBeds = sum(BedInventory, na.rm = TRUE)
  ) %>% 
  ungroup() %>%
  mutate(HouseholdType = case_when(
    householdtype == 0 ~ "Families and Individuals",
    householdtype == 1 ~ "Individuals",
    householdtype == 3 ~ "Families",
    householdtype == 4 ~ "Households with only children"
  )) %>%
  select(-householdtype)

project <- Project %>%
  select(ProjectID,
         OrganizationID,
         ProjectCommonName,
         ProjectType,
         ProjectName,
         HousingType,
         ProjectRegion)

organization <- Organization %>%
  select(OrganizationID,
         OrganizationName)

addresses <- ProjectCoC %>%
  select(ProjectID,
         Address1,
         Address2,
         City,
         State,
         ZIP)


final <- inventory %>%
  left_join(project, by = "ProjectID") %>%
  left_join(organization, by = "OrganizationID") %>%
  left_join(addresses, by = "ProjectID") %>%
  mutate(
    ProjectCommonName = if_else(is.na(ProjectCommonName),
                                ProjectName,
                                ProjectCommonName),
    HousingType = case_when(
      HousingType == 1 ~ "Site-based, single site",
      HousingType == 2 ~ "Site-based, multple sites",
      HousingType == 3 ~ "Tenant-based, scattered site"
    ),
    ProjectType = project_type(ProjectType)
  ) %>%
  select(
    OrganizationName,
    ProjectCommonName,
    ProjectType,
    HousingType,
    HouseholdType,
    TotalBeds,
    Address1,
    Address2,
    City,
    State,
    ZIP
  )

write_csv(final, "Reports/Project_List_COVID19.csv")
