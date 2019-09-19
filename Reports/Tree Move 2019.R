library(tidyverse)
library(lubridate)

load("images/COHHIOHMIS.RData")

providers_to_move <- c(954, 955, 975, 979, 978, 1865, 2012, 971)
orgs <- c(182, 182, 63, 370, 370, 789, 789, 2048)

moves <- as.data.frame(cbind(providers_to_move, orgs))

moves <- moves %>%
  left_join(Project[c("ProjectID", "ProjectName")],
            by = c("providers_to_move" = "ProjectID")) %>%
  left_join(Organization[c("OrganizationID", "Organizationname")],
            by = c("orgs" = "OrganizationID")) %>%
  select("ProjectID" = providers_to_move,
         ProjectName,
         "NewOrganizationID" = orgs,
         "NewOrganizationName" = Organizationname) %>%
  mutate(NewOrganizationName = if_else(NewOrganizationID == 2048,
                                       "Highland County CAO",
                                       NewOrganizationName))

# added the Org Name manually because it's too newly created to be in HMIS yet.

write_csv(moves, "Reports/2019_tree_moves.csv")

