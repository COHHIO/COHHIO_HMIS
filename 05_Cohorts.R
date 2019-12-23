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

# Some definitions:
# PH = PSH + RRH
# household = one or more people who present for housing/homeless services
# served = the Entry to Exit Date range crosses the Report Date range
# entered = the Entry Date is inside the Report Date range
# served_leaver = (regardless of Move-In) the Exit Date is inside the Report
#     Date range
# moved_in_leaver = a subset of served_leaver, these stays include a Move-In Date
#     where that's relevant (PH projects)
# moved_in = any stay in a non-PH project where the Entry to Exit Date range
#     crosses the Report Date range PLUS any stay in a PH project where the 
#     Move In Date to the Exit Date crosses the Report Date range
# hohs = heads of household
# adults = all adults in a household
# clients = all members of the household

library(tidyverse)
library(lubridate)

load("images/COHHIOHMIS.RData")

ReportStart <- FileStart
ReportEnd <- FileEnd

vars_we_want <- c(
  "PersonalID",
  "EnrollmentID",
  "ProjectName",
  "ProjectID",
  "ProjectType",
  "HouseholdID",
  "AgeAtEntry",
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
co_hohs_served <-  Enrollment %>%
  filter(served_between(., ReportStart, ReportEnd) &
           RelationshipToHoH == 1) %>%
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)	

summary_hohs_served <- co_hohs_served %>%
  group_by(ProjectName) %>%
  summarise(hohs_served = n())

# Leaver HoHs served during the reporting period
co_hohs_served_leavers <-  Enrollment %>%
  filter(
      exited_between(., ReportStart, ReportEnd) &
      RelationshipToHoH == 1
  ) %>% 
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)	

summary_hohs_served_leavers <- co_hohs_served_leavers %>%
  group_by(ProjectName) %>%
  summarise(hohs_served_leavers = n())

#	Leavers	who were Served During Reporting Period	Deaths
co_hohs_served_leavers_died <- Enrollment %>%
  filter(
      exited_between(., ReportStart, ReportEnd) &
      RelationshipToHoH == 1,
      Destination == 24
  ) %>% 
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)	

summary_hohs_served_leavers_died  <- co_hohs_served_leavers_died  %>%
  group_by(ProjectName) %>%
  summarise(hohs_served_leavers_died = n())

#	Leavers and Stayers	who were Served During Reporting Period	All
co_clients_served <-  Enrollment %>%
  filter(served_between(., ReportStart, ReportEnd)) %>%
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)

summary_clients_served <- co_clients_served %>%
  group_by(ProjectName) %>%
  summarise(clients_served = n())

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
  summarise(adults_entered = n())

#	Leavers and Stayers	who	Entered During Reporting Period	HoHs
co_hohs_entered <- Enrollment %>%
  filter(
    entered_between(., ReportStart, ReportEnd) &
      RelationshipToHoH == 1
  ) %>% 
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)	

summary_hohs_entered <- co_hohs_entered %>%
  group_by(ProjectName) %>%
  summarise(hohs_entered = n())

#	Leavers and Stayers	who were Served During Reporting Period (and Moved In)	All
co_clients_moved_in <-  Enrollment %>%
  filter(
    stayed_between(., ReportStart, ReportEnd)
  ) %>% 
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)

summary_clients_moved_in <- co_clients_moved_in %>%
  group_by(ProjectName) %>%
  summarise(clients_moved_in = n())

#	Leavers and Stayers	who were Served During Reporting Period (and Moved In)	Adults
co_adults_moved_in <-  Enrollment %>%
  filter(stayed_between(., ReportStart, ReportEnd) &
           AgeAtEntry > 17) %>%
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)	

summary_adults_moved_in <- co_adults_moved_in %>%
  group_by(ProjectName) %>%
  summarise(adults_moved_in = n())

#	Leavers	who were Served During Reporting Period (and Moved In)	All
co_clients_moved_in_leavers <-  Enrollment %>%
  filter(exited_between(., ReportStart, ReportEnd) &
           stayed_between(., ReportStart, ReportEnd)) %>%
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)	

summary_clients_moved_in_leavers <- co_clients_moved_in_leavers %>%
  group_by(ProjectName) %>%
  summarise(clients_moved_in_leavers = n())

#	Leaver hohs	who were Served (and Moved In) During Reporting Period	HoHs
co_hohs_moved_in_leavers <-  Enrollment %>%
  filter(stayed_between(., ReportStart, ReportEnd) &
           exited_between(., ReportStart, ReportEnd) &
           RelationshipToHoH == 1) %>%
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want)	

summary_hohs_moved_in_leavers <- co_hohs_moved_in_leavers %>%
  group_by(ProjectName) %>%
  summarise(hohs_moved_in_leavers = n())

#	Leavers	who were Served During Reporting Period (and Moved In)	Adults
co_adults_moved_in_leavers <-  Enrollment %>%
  filter(exited_between(., ReportStart, ReportEnd) &
           stayed_between(., ReportStart, ReportEnd) &
           AgeAtEntry > 17) %>%
  left_join(Client, by = "PersonalID") %>%
  select(vars_we_want) 

summary_adults_moved_in_leavers <- co_adults_moved_in_leavers %>%
  group_by(ProjectName) %>%
  summarise(adults_moved_in_leavers = n())

summary <- summary_clients_served %>%
  full_join(summary_clients_moved_in, by = "ProjectName") %>%
  full_join(summary_hohs_moved_in_leavers, by = "ProjectName") %>%
  full_join(summary_adults_served, by = "ProjectName") %>%
  full_join(summary_adults_moved_in, by = "ProjectName") %>%
  full_join(summary_clients_moved_in_leavers, by = "ProjectName") %>%
  full_join(summary_adults_moved_in_leavers, by = "ProjectName") %>%
  full_join(summary_hohs_served, by = "ProjectName") %>%
  full_join(summary_hohs_entered, by = "ProjectName") %>%
  full_join(summary_hohs_served_leavers, by= "ProjectName") %>%
  full_join(summary_adults_entered, by = "ProjectName") %>% 
  full_join(summary_hohs_served_leavers_died, by = "ProjectName")
  
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
  regions,
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
