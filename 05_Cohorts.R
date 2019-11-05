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
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details at 
#<https://www.gnu.org/licenses/>.

library(tidyverse)
library(lubridate)

load("images/COHHIOHMIS.RData")

ReportStart <- "01012018"
ReportEnd <- "12312018"

vars_we_want <- c(
  "PersonalID",
  "EnrollmentID",
  "ProjectName",
  "ProjectType",
  "HouseholdID",
  "RelationshipToHoH",
  "VeteranStatus",
  "EntryDate",
  "MoveInDate",
  "MoveInDateAdjust",
  "ExitDate",
  "ExitAdjust",
  "Destination"
)

# Leaver and Stayer HoHs who were served during the reporting period
co_hoh_served <-  Enrollment %>%
  filter(served_between(., ReportStart, ReportEnd) &
           RelationshipToHoH == 1) %>%
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)	

summary_hoh_served <- co_hoh_served %>%
  group_by(ProjectName) %>%
  summarise(hoh_served = n())

# Leaver HoHs who were served and moved in during the reporting period
co_hoh_served_leavers <-  Enrollment %>%
  filter(
    stayed_between(., ReportStart, ReportEnd) &
      exited_between(., ReportStart, ReportEnd) &
      RelationshipToHoH == 1
  ) %>% 
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)	

summary_hoh_served_leavers <- co_hoh_served_leavers %>%
  group_by(ProjectName) %>%
  summarise(hoh_served_leavers = n())

#	Leavers	who were Served During Reporting Period	Deaths
co_hoh_leavers_died <- Enrollment %>%
  filter(
      exited_between(., ReportStart, ReportEnd) &
      RelationshipToHoH == 1,
      Destination == 24
  ) %>% 
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)	

summary_hoh_leavers_died  <- co_hoh_leavers_died  %>%
  group_by(ProjectName) %>%
  summarise(hoh_leavers_died = n())

#	Leavers and Stayers	who were Served During Reporting Period	All
co_all_served <-  Enrollment %>%
  filter(served_between(., ReportStart, ReportEnd)) %>%
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)

summary_all_served <- co_all_served %>%
  group_by(ProjectName) %>%
  summarise(all_served = n())

#	Leavers and Stayers	who were Served During Reporting Period	Adults
co_adults_served <-  Enrollment %>%
  filter(served_between(., ReportStart, ReportEnd) &
           AgeAtEntry > 17) %>%
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)

summary_adults_served <- co_adults_served %>%
  group_by(ProjectName) %>%
  summarise(adults_served = n())

#	Leavers and Stayers	who	Entered During Reporting Period	Adults

co_adults_entered <-  Enrollment %>%
  filter(entered_between(., ReportStart, ReportEnd) &
           AgeAtEntry > 17) %>%
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)

summary_adults_entered <- co_adults_entered %>%
  group_by(ProjectName) %>%
  summarise(adults_who_entered = n())

#	Leavers and Stayers	who	Entered During Reporting Period	HoHs
co_hoh_enterers <-  Enrollment %>%
  filter(
    entered_between(., ReportStart, ReportEnd) &
      RelationshipToHoH == 1
  ) %>% 
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)	

summary_hoh_enterers <- co_hoh_enterers %>%
  group_by(ProjectName) %>%
  summarise(hoh_enterers = n())

#	Leavers and Stayers	who were Served During Reporting Period (and Moved In)	All
co_moved_in_served <-  Enrollment %>%
  filter(
    stayed_between(., ReportStart, ReportEnd)
  ) %>% 
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)

summary_moved_in_served <- co_moved_in_served %>%
  group_by(ProjectName) %>%
  summarise(moved_in_served = n())

#	Leavers and Stayers	who were Served During Reporting Period (and Moved In)	Adults
co_adults_moved_in <-  Enrollment %>%
  filter(stayed_between(., ReportStart, ReportEnd) &
           AgeAtEntry > 17) %>%
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)	

summary_adults_moved_in <- co_adults_moved_in %>%
  group_by(ProjectName) %>%
  summarise(adults_moved_in = n())

#	Leavers	who were	Served During Reporting Period (and Moved In)	All
co_client_moved_in_leavers <-  Enrollment %>%
  filter(exited_between(., ReportStart, ReportEnd) &
           stayed_between(., ReportStart, ReportEnd)) %>%
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)	

summary_client_moved_in_leavers <- co_client_moved_in_leavers %>%
  group_by(ProjectName) %>%
  summarise(client_moved_in_leavers = n())

#	Leavers	who were	Served During Reporting Period (and Moved In)	Adults
co_adults_moved_in_leavers <-  Enrollment %>%
  filter(exited_between(., ReportStart, ReportEnd) &
           stayed_between(., ReportStart, ReportEnd) &
           AgeAtEntry > 17) %>%
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want) 

summary_adults_moved_in_leavers <- co_adults_moved_in_leavers %>%
  group_by(ProjectName) %>%
  summarise(adults_moved_in_leavers = n())

summary <- summary_all_served %>%
  full_join(summary_moved_in_served, by = "ProjectName") %>%
  full_join(summary_adults_served, by = "ProjectName") %>%
  full_join(summary_adults_moved_in, by = "ProjectName") %>%
  full_join(summary_client_moved_in_leavers, by = "ProjectName") %>%
  full_join(summary_adults_moved_in_leavers, by = "ProjectName") %>%
  full_join(summary_hoh_served, by = "ProjectName") %>%
  full_join(summary_hoh_enterers, by = "ProjectName") %>%
  full_join(summary_hoh_served_leavers, by= "ProjectName") %>%
  full_join(summary_adults_entered, by = "ProjectName") %>%
  full_join(summary_hoh_leavers_died, by = "ProjectName")
  
rm(
  Affiliation,
  CaseManagers,
  Client,
  Disabilities,
  EmploymentEducation,
  Enrollment,
  EnrollmentCoC,
  Exit,
  Export,
  Funder,
  HealthAndDV,
  IncomeBenefits,
  Inventory,
  Organization,
  Project,
  ProjectCoC,
  Referrals,
  Regions,
  stray_services,
  Scores,
  Services,
  Users,
  Offers,
  vars_we_want,
  VeteranCE
)

rm(list = ls(pattern = "summary_"))

save.image("images/cohorts.RData")
