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

# PLEASE NOTE THIS SCRIPT OVERWRITES THE CLIENT.CSV FILE ON YOUR HARD DRIVE!
# IT REPLACES THE NAMES AND SSNS WITH DATA QUALITY SIGNIFIERS!
# IT CAN BE RUN ON A CLEAN CLIENT.CSV FILE OR ONE THAT'S BEEN OVERWRITTEN.

# Save your ReportWriter export zip files directly to the data folder. This 
# script will unzip and rename them appropriately.

# Currently, this file is expecting the following files in your data/ directory:

# RMisc.xlsx 
# (all the HUD CSV Export FY2020 .csv files)
# casemanagers.zip or .csv
# cevets.zip or .csv
# cocscoring.zip or .csv (during CoC Competition only)
# offers.zip or .csv
# referrals.zip or .csv
# scoresfam.zip or.csv
# scoresind.zip or .csv
# scorestay.zip or .csv
# services1.zip or .csv
# services2.zip or .csv

library(tidyverse)
library(lubridate)
library(readxl)

# Affiliation -------------------------------------------------------------

Affiliation <- 
  read_csv("data/Affiliation.csv", 
           col_types = "nnnTTnTn") 

# Client ------------------------------------------------------------------

if(ncol(read_csv("data/Client.csv")) == 36) {
  Client <-
    read_csv("data/Client.csv",
             col_types = "nccccncnDnnnnnnnnnnnnnnnnnnnnnnTTcTn") %>%
    filter(!PersonalID %in% c(5, 4216))
} else {
  Client <-
    read_csv("data/Client.csv",
             col_types = "ncncnDnnnnnnnnnnnnnnnnnnnnnnTTcTn") %>%
    filter(!PersonalID %in% c(5, 4216))
}

# Masking PII in the Client file (but not DOB) 

if(ncol(read_csv("data/Client.csv")) == 36)
{Client <- Client %>%
  mutate(
    FirstName = case_when(
      NameDataQuality %in% c(8, 9) ~ "DKR",
      NameDataQuality == 2 ~ "Partial",
      NameDataQuality == 99 |
        is.na(NameDataQuality) |
        FirstName == "Anonymous" ~ "Missing",!(
          NameDataQuality %in% c(2, 8, 9, 99) |
            is.na(NameDataQuality) |
            FirstName == "Anonymous"
        ) ~ "ok"
    ),
    LastName = NULL,
    MiddleName = NULL,
    NameSuffix = NULL,
    SSN = case_when(
      (is.na(SSN) & !SSNDataQuality %in% c(8, 9)) |
        is.na(SSNDataQuality) | SSNDataQuality == 99 ~ "Missing",
      SSNDataQuality %in% c(8, 9) ~ "DKR",
      # substr(SSN, 1, 1) == 0 |
      # substr(SSN, 1, 2) == "00" |
      (nchar(SSN) != 9 & SSNDataQuality != 2) |
        substr(SSN, 1, 3) %in% c("000", "666") |
        substr(SSN, 1, 1) == 9 |
        substr(SSN, 4, 5) == "00" |
        substr(SSN, 6, 9) == "0000" |
        SSNDataQuality == 2 |
        SSN %in% c(
          111111111,
          222222222,
          333333333,
          444444444,
          555555555,
          666666666,
          777777777,
          888888888,
          123456789
        ) ~ "Invalid",
      SSNDataQuality == 2 & nchar(SSN) != 9 ~ "Incomplete"
    )
  )

Client <- Client %>%
  mutate(SSN = case_when(
    is.na(SSN) ~ "ok",
    !is.na(SSN) ~ SSN
  ))}

# this overwrites the raw Client.csv file on your computer with the final Client
# object as a security measure.

if(ncol(Client) == 33)
{write_csv(Client, "data/Client.csv", append = FALSE)}

# CurrentLivingSituation <- 
#   read_csv("data/CurrentLivingSituation.csv",
#             col_types = "nnnTncnnnnncTTcTc") DON'T NEED YET

# Disabilities ------------------------------------------------------------

Disabilities <-
  read_csv("data/Disabilities.csv",
           col_types = "cnnDnnnnnnnnnnTTnTn")


# EmploymentEducation -----------------------------------------------------

EmploymentEducation <-
  read_csv("data/EmploymentEducation.csv",
           col_types = "cnnDnnnnnnTTnTn")

# Exit --------------------------------------------------------------------

Exit <-
  read_csv("data/Exit.csv",
           col_types = "nnnDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTnTn")

# Project -----------------------------------------------------------------

Project <- 
  read_csv("data/Project.csv",
           col_types = "nnccDDnnnnnnnnTTcTn") 

provider_extras <- read_xlsx("data/RMisc.xlsx",
                                sheet = 4,
                                range = cell_cols("A:H")) %>%
  mutate(ProjectRegion = if_else(ProviderRegion != "Homeless Planning Region 10",
                                 str_remove(ProviderRegion, "0"),
                                 ProviderRegion),
         ProviderRegion = NULL)

if(file.exists("data/cocscoring.zip")) {
  unzip(zipfile = "./data/cocscoring.zip", exdir = "./data")
  
  file.rename(paste0("data/", list.files("./data", pattern = "(report_)")),
              "data/cocscoring.csv")
  
  file.remove("data/cocscoring.zip")
}

coc_scoring <- read_csv("data/cocscoring.csv",
                        col_types = "dccd?iiii")

coc_scoring <- coc_scoring %>%
  mutate(DateReceivedPPDocs = mdy(DateReceivedPPDocs)) %>%
  select(1, 4:9)

Project <- Project %>%
  select(-ProjectName) %>%
  left_join(provider_extras, by = "ProjectID") %>%
  left_join(coc_scoring, by = "ProjectID") %>%
  mutate(HMISParticipatingProject = if_else(UsesSP == "Yes", 1, 0)) %>% 
  select(-UsesSP)

rm(coc_scoring)

# Regions

regions <- read_csv("data/Regions.csv",
                    col_types = "cn") %>%
  arrange(Region) %>%
  mutate(RegionName = paste("Homeless Planning Region", Region))
# 
# Project <- left_join(project_county, regions, by = "County")

# EnrollmentCoC -----------------------------------------------------------

EnrollmentCoC <- 
  read_csv("data/EnrollmentCoC.csv", 
           col_types = "cncnnDcnTTnTn")

# VeteranCE --------------------------------------------------------------

if(file.exists("data/cevets.zip")) {
  unzip(zipfile = "./data/cevets.zip", exdir = "./data")
  
  file.rename(paste0("data/", list.files("./data", pattern = "(report_)")),
              "data/cevets.csv")
  
  file.remove("data/cevets.zip")
}

VeteranCE <- read_csv("data/cevets.csv", col_types = "ii??ic?cccc")

VeteranCE <- 
  mutate(
    VeteranCE,
    DateVeteranIdentified = mdy(DateVeteranIdentified),
    ExpectedPHDate = mdy(ExpectedPHDate),
    MostRecentOfferDate = mdy(MostRecentOfferDate)
  )

# Enrollment --------------------------------------------------------------

Enrollment <-
  read_csv("data/Enrollment.csv",
           col_types =
             "nnnDcnnnlnDnnnDDDnnnncccnnDnnnncnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnTTnTn")

# from sheets 1 and 2, getting EE-related data, joining both to En --------
# will eventually come from aa: ees in ReportWriter, waiting on WS
counties <- read_xlsx("data/RMisc.xlsx",
                                 sheet = 1,
                                 range = cell_cols("A:D"),
                                 col_types = c("numeric", "numeric", "text", "text")) %>%
  select(EnrollmentID, CountyServed, CountyPrior)
  

bowman_entry_exits <- read_xlsx("data/RMisc.xlsx",
                                sheet = 2,
                                range = cell_cols("A:D"))


Enrollment <- Enrollment %>% #select(-RelationshipToHoH) %>%
  inner_join(bowman_entry_exits, by = "EnrollmentID") %>%
  inner_join(counties, by = "EnrollmentID") %>%
  left_join(VeteranCE %>% select(EnrollmentID, PHTrack, ExpectedPHDate), 
            by = "EnrollmentID")

rm(bowman_entry_exits, counties)

# Adding Exit Data to Enrollment because I'm not tryin to have one-to-one 
# relationships in this!

small_exit <- Exit %>% select(EnrollmentID, 
                              ExitDate, 
                              Destination, 
                              OtherDestination)

Enrollment <- left_join(Enrollment, small_exit, by = "EnrollmentID") %>%
  mutate(ExitAdjust = if_else(is.na(ExitDate), today(), ExitDate))

rm(small_exit)

# Adding ProjectType to Enrollment too bc we need EntryAdjust & MoveInAdjust
small_project <- Project %>%
  select(ProjectID, ProjectType, ProjectName) 

# getting HoH's Entry Dates bc WS is using that to overwrite MoveIn Dates
# only doing this for RRH and PSHs since Move In Date doesn't matter for ES, etc.
HoHsEntry <- Enrollment %>%
  left_join(small_project, by = "ProjectID") %>%
  filter(RelationshipToHoH == 1 &
           ProjectType %in% c(3, 9, 13)) %>%
  select(HouseholdID, "HoHsEntry" = EntryDate) %>%
  unique()

## ^^ this code causes a duplication for situations where a hh has two clients
# marked as Head of Household AND the HoHs have different Entry Dates. RARE,
# but possible. Not sure how to fix this, maybe it's just something to know.

Enrollment <- Enrollment %>%
  left_join(small_project, by = "ProjectID") %>%
  left_join(HoHsEntry, by = "HouseholdID") %>%
  mutate(
    MoveInDateAdjust = case_when(
      EntryDate < mdy("10012017") &
        ProjectType %in% c(3, 9)
      ~ EntryDate,
      EntryDate != HoHsEntry &
        ProjectType %in% c(3, 9, 13) ~ EntryDate,
      EntryDate >= mdy("10012017") &
        ProjectType %in% c(3, 9) &
        ymd(EntryDate) <= ymd(MoveInDate) &
        ymd(MoveInDate) <= ExitAdjust
      ~ MoveInDate,
      ymd(EntryDate) <= ymd(MoveInDate) &
        ymd(MoveInDate) <= ExitAdjust &
        ProjectType == 13 ~ MoveInDate
    ),
    EntryAdjust = case_when(
      ProjectType %in% c(1, 2, 4, 8, 12) ~ EntryDate,
      ProjectType %in% c(3, 9, 13) &
        !is.na(MoveInDateAdjust) ~ MoveInDateAdjust
    )
  )

rm(small_project, HoHsEntry)

# Client Location

y <- EnrollmentCoC %>%
  filter(DataCollectionStage == 1) %>%
  select(EnrollmentID, "ClientLocation" = CoCCode) 

Enrollment <- Enrollment %>%
  left_join(y, by = "EnrollmentID")

rm(y)

# Event <- 
#   read_csv("data/Event.csv",
#            col_types = "nnnDnnncDTTcTc") <- no data

# Export ------------------------------------------------------------------

Export <- 
  read_csv("data/Export.csv",
           col_types = "nnnccccncTDDccnnn")

# Funder ------------------------------------------------------------------

Funder <- 
  read_csv("data/Funder.csv",
           col_types = "nnnccDDTTcTn")

# HealthAndDV -------------------------------------------------------------

HealthAndDV <-
  read_csv("data/HealthAndDV.csv",
           col_types = "cnnDnnnnnnnDnTTnTn")

# IncomeBenefits ----------------------------------------------------------

IncomeBenefits <- 
  read_csv("data/IncomeBenefits.csv",
           col_types = 
             "cnnDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnncnnnnnnnnnnnnnnnnnnnncnnnnnnTTnTn")

# Inventory ---------------------------------------------------------------

Inventory <-
  read_csv("data/Inventory.csv",
           col_types = "nncnnnnnnnnnnnnDDTTcTn")

# Organization ------------------------------------------------------------

Organization <- 
  read_csv("data/Organization.csv",
           col_types = "ncncTTnTn")

# ProjectCoC --------------------------------------------------------------

ProjectCoC <- 
  read_csv("data/ProjectCoC.csv",
           col_types = "nncnccccnnTTcTn")

# Case Manager Records ----------------------------------------------------

if(file.exists("data/casemanagers.zip")) {
  unzip(zipfile = "./data/casemanagers.zip", exdir = "./data")
  
  file.rename(paste0("data/", list.files("./data", pattern = "(report_)")),
              "data/casemanagers.csv")
  
  file.remove("data/casemanagers.zip")
}

CaseManagers <- read_csv("data/casemanagers.csv",
                             col_types = "dccccc")


# Contacts ----------------------------------------------------------------
# only pulling in contacts made between an Entry Date and an Exit Date

suppressWarnings(Contacts <- read_xlsx(
  "data/RMisc.xlsx",
  sheet = 5,
  range = cell_cols("A:K"),
  col_types = c(
    "numeric",
    "text",
    "date",
    "date",
    "numeric",
    "text",
    "date",
    "date",
    "date",
    "text",
    "text"
  )
) %>%
  mutate(
    EntryDate = ymd(format.Date(EntryDate, "%Y-%m-%d")),
    ExitDate = ymd(format.Date(ExitDate, "%Y-%m-%d")),
    ContactDate = ymd(format.Date(ContactDate, "%Y-%m-%d")),
    ContactStartDate = ymd(format.Date(ContactStartDate, "%Y-%m-%d")),
    ContactEndDate = ymd(format.Date(ContactEndDate, "%Y-%m-%d")),
    ProjectName = str_remove(ProjectName, "\\(.*\\)")
  ) %>%
  filter(ContactDate >= EntryDate &
           ContactDate <= ExitDate))

# Scores ------------------------------------------------------------------

if(file.exists("data/scoresfam.zip")) {
  unzip(zipfile = "./data/scoresfam.zip", exdir = "./data")
  
  file.rename(paste0("data/", list.files("./data", pattern = "(report_)")),
              "data/scores.csv")
  
  file.remove("data/scoresfam.zip")
}

if(file.exists("data/scoresind.zip")) {
  unzip(zipfile = "./data/scoresind.zip", exdir = "./data")
  
  file.rename(paste0("data/", list.files("./data", pattern = "(report_)")),
              "data/scoresind.csv")
  
  file.remove("data/scoresind.zip")
}

if(file.exists("data/scorestay.zip")) {
  unzip(zipfile = "./data/scorestay.zip", exdir = "./data")
  
  file.rename(paste0("data/", list.files("./data", pattern = "(report_)")),
              "data/scorestay.csv")
  
  file.remove("data/scorestay.zip")
}

file.append("data/scores.csv", "data/scoresind.csv")

file.append("data/scores.csv", "data/scorestay.csv")

if(file.exists("data/scoresind.csv")) {
file.remove(c("data/scoresind.csv", "data/scorestay.csv"))
}

Scores <- read_csv("data/scores.csv",
                   col_types = "ccc") %>%
  filter(Score != "Score") %>%
  mutate(
    ScoreDate = mdy(ScoreDate),
    PersonalID = as.double(PersonalID),
    Score = as.double(Score)
  )

# Offers -----------------------------------------------------------------

if(file.exists("data/offers.zip")) {
  unzip(zipfile = "./data/offers.zip", exdir = "./data")
  
  file.rename(paste0("data/", list.files("./data", pattern = "(report_)")),
              "data/offers.csv")
  
  file.remove("data/offers.zip")
}

Offers <- read_csv("data/offers.csv", col_types = "i?c?c") %>%
  mutate(
    OfferDate = mdy(OfferDate),
    AcceptDeclineDate = mdy(AcceptDeclineDate)
  )

# Users ------------------------------------------------------------------
Users <- read_xlsx("data/RMisc.xlsx",
                   sheet = 3,
                   range = cell_cols("A:G")) %>%
  mutate(DefaultProvider = str_remove(DefaultProvider, "\\(.*\\)")) %>%
  left_join(provider_extras, by = c("DefaultProvider" = "ProjectName")) %>%
  select(
    UserCreating,
    UserID,
    UserName,
    UserTelephone,
    UserEmail,
    UserActive,
    DefaultProvider,
    "UserCounty" = ProjectCounty,
    "UserRegion" = ProjectRegion
  ) 

rm(provider_extras)

# some users don't have a County bc their Default Provider doesn't have an 
# address. 

# Services ----------------------------------------------------------------

# this comes from two ReportWriter reports: An Export: Services and 
# An Export: Services & Funds. Saving them as services1.csv and services2.csv.

if(file.exists("data/services1.zip")) {
  unzip(zipfile = "./data/services1.zip", exdir = "./data")
  
  file.rename(paste0("data/", list.files("./data", pattern = "(report_)")),
              "data/services1.csv")
  
  file.remove("data/services1.zip")
}

services_1 <- read_csv("data/services1.csv",
                      col_types = "nnnn??cccc")

services_1 <- services_1 %>%
  mutate(ServiceStartDate = mdy(ServiceStartDate),
         ServiceEndDate = mdy(ServiceEndDate))

if(file.exists("data/services2.zip")) {
  unzip(zipfile = "./data/services2.zip", exdir = "./data")
  
  file.rename(paste0("data/", list.files("./data", pattern = "(report_)")),
              "data/services2.csv")
  
  file.remove("data/services2.zip")
}

services_funds <- read_csv("data/services2.csv",
                      col_types = "ncd")

Services <- services_1 %>%
  left_join(services_funds, by = "ServiceID") %>%
  rename("ServiceHHID" = HouseholdID)

rm(services_1, services_funds)

staging_services <- Services[c("PersonalID",
                              "ServiceProvider",
                              "ServiceID",
                              "ServiceStartDate",
                              "ServiceEndDate")] %>%
  left_join(Enrollment[c("EnrollmentID",
                         "PersonalID",
                         "ProjectName",
                         "EntryDate",
                         "ExitAdjust")],
            by = "PersonalID") %>%
  mutate(
    ServiceEndAdjust = if_else(is.na(ServiceEndDate), today(), ServiceEndDate),
    ServiceRange = interval(ymd(ServiceStartDate), ymd(ServiceEndAdjust)),
    EERange = interval(ymd(EntryDate), ymd(ExitAdjust)),
    Valid = if_else(int_overlaps(ServiceRange, EERange) &
                      ServiceProvider == ProjectName, TRUE, FALSE)
  ) 

stray_services <- staging_services %>%
  filter(is.na(Valid))

staging_services <- staging_services %>%
  filter(Valid == TRUE) %>%
  select(PersonalID,
         ServiceID,
         EnrollmentID,
         ServiceProvider)

Services <- staging_services %>%
  left_join(Services, by = c("ServiceID", "PersonalID", "ServiceProvider"))

# the code above does not pull in Services that cannot be associated to an EE. 
# Any Services that don't align with an EE can be found in stray_services

rm(staging_services)

# Referrals ---------------------------------------------------------------

if(file.exists("data/referrals.zip")) {
  unzip(zipfile = "./data/referrals.zip", exdir = "./data")
  
  file.rename(paste0("data/", list.files("./data", pattern = "(report_)")),
              "data/referrals.csv")
  
  file.remove("data/referrals.zip")
}

Referrals <- read_csv("data/referrals.csv",
                      col_types = "nnn?cccccccccc")

Referrals <- Referrals %>%
  mutate(ReferralDate = mdy(ReferralDate),
         ReferralHHID = HouseholdID,
         HouseholdID = NULL,
         UserCreatingReferral = UserCreating,
         UserCreating = NULL)


# HUD CSV Specs -----------------------------------------------------------

HUD_specs <- read_csv("HUD/HUDSpecs.csv",
                      col_types = "ccnc") %>%
  as.data.frame()

# Age Function ------------------------------------------------------------

age_years <- function(earlier, later)
{
  lt <- data.frame(earlier, later)
  age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y"))
  
  dayOnLaterYear <- ifelse(
    format(lt[, 1], format = "%m-%d") != "02-29",
    as.Date(paste(
      format(lt[, 2], format = "%Y"), "-",
      format(lt[, 1], format = "%m-%d"),
      sep = ""
    )),
    ifelse(
      as.numeric(format(later, format = "%Y")) %%
        400 == 0 |
        as.numeric(format(later,
                          format =
                            "%Y")) %%
        100 != 0 &
        as.numeric(format(later, format = "%Y")) %%
        4 == 0,
      as.Date(paste(
        format(lt[, 2], format = "%Y"),
        "-",
        format(lt[, 1], format =
                 "%m-%d"),
        sep = ""
      )),
      as.Date(paste(
        format(lt[, 2], format = "%Y"),
        "-",
        "02-28",
        sep = ""
      ))
    )
  )
  
  age[which(dayOnLaterYear > lt$later)] <- age[which(dayOnLaterYear > lt$later)] - 1
  
  age
}

# Adding Age at Entry to Enrollment ---------------------------------------
small_client <- Client %>% select(PersonalID, DOB)
Enrollment <- Enrollment %>%
  left_join(small_client, by = "PersonalID") %>%
  mutate(AgeAtEntry = age_years(DOB, EntryDate)) %>%
  select(-DOB)
rm(small_client)

# Client Entry Exits Between Date Range Functions -------------------------------------

served_between <- function(table, start, end){
  served <- ymd(table$EntryDate) <= mdy(end) &
    (is.na(table$ExitDate) | ymd(table$ExitDate) >= mdy(start))
  served
}

entered_between <- function(table, start, end){
  entered <- between(ymd(table$EntryDate), mdy(start), mdy(end)) 
  entered
}

exited_between <- function(table, start, end){
  exited <- between(ymd(table$ExitDate), mdy(start), mdy(end)) 
  exited
}

stayed_between <- function(table, start, end){
  stayed <- ymd(table$EntryAdjust) <= mdy(end) &
    (is.na(table$ExitDate) | ymd(table$ExitDate) > mdy(start))
  stayed
}

# Projects Operating Between Date Range Function --------------------------

operating_between <- function(table, start, end) {
  operating <-  if_else(
    is.na(table$OperatingStartDate) |
      ymd(table$OperatingStartDate) > mdy(end) |
      (!is.na(table$OperatingEndDate) &
         ymd(table$OperatingEndDate) < mdy(start)),
    FALSE,
    TRUE
  )
  operating
}

# Beds Available Between --------------------------------------------------

beds_available_between <- function(table, start, end) {
  available <-  if_else(
    is.na(table$InventoryStartDate) |
      ymd(table$InventoryStartDate) > mdy(end) |
      (!is.na(table$InventoryEndDate) &
         ymd(table$InventoryEndDate) < mdy(start)),
    FALSE,
    TRUE
  )
  available
}

living_situation <- function(ReferenceNo) {
  case_when(
    ReferenceNo == 16 ~ "Place not meant for habitation",
    ReferenceNo == 1 ~ "Emergency shelter/ h/motel paid for by a third party/Host Home shelter",
    ReferenceNo == 18 ~ "Safe Haven",
    ReferenceNo == 15 ~ "Foster care home of foster care group home",
    ReferenceNo == 6 ~ " Hospital or other residential non-psychiatric medical facility",
    ReferenceNo == 7 ~ "Jail/prison/juvenile detention",
    ReferenceNo == 25 ~ "Long-term care facility or nursing home",
    ReferenceNo == 4 ~ "Psychiatric hospital/ other psychiatric facility",
    ReferenceNo == 5 ~ "Substance abuse treatment facility or detox center",
    ReferenceNo == 29 ~ "Residential project or halfway house with no homeless criteria",
    ReferenceNo == 14 ~ "H/Motel paid for by household",
    ReferenceNo == 2 ~ "Transitional housing",
    ReferenceNo == 32 ~ "Host Home (non-crisis)",
    ReferenceNo == 13 ~ "Staying or living with friends, temporary tenure",
    ReferenceNo == 36 ~ "Staying or living in a friend's room, apartment or house",
    ReferenceNo == 12 ~ "Staying or living with family, temporary tenure",
    ReferenceNo == 22 ~ "Staying or living with family, permanent tenure",
    ReferenceNo == 35 ~ "Staying or living in a family member's room, apartment, or house",
    ReferenceNo == 23 ~ "Staying or living with friends, permanent tenure",
    ReferenceNo == 26 ~ "Moved from one HOPWA funded project to HOPWA PH",
    ReferenceNo == 27 ~ "Moved from HOPWA funded project to HOPWA TH",
    ReferenceNo == 28 ~ "Rental by client, with GPD TIP housing subsidy",
    ReferenceNo == 19 ~ "Rental by client, with VASH housing subsidy",
    ReferenceNo == 3 ~ "Permanent housing (other than RRH) for formerly homeless persons",
    ReferenceNo == 31 ~ "Rental by client, with RRH or equivalent subsidy",
    ReferenceNo == 33 ~ "Rental by client, with HCV voucher",
    ReferenceNo == 34 ~ "Rental by client in a public housing unit",
    ReferenceNo == 10 ~ "Rental by client, no ongoing housing subsidy",
    ReferenceNo == 20 ~ "Rental by client, with other ongoing housing subsidy",
    ReferenceNo == 21 ~ "Owned by client, with ongoing housing subsidy",
    ReferenceNo == 11 ~ "Owned by client, no ongoing housing subsidy",
    ReferenceNo == 30 ~ "No exit interview completed",
    ReferenceNo == 17 ~ "Other",
    ReferenceNo == 24 ~ "Deceased",
    ReferenceNo == 37 ~ "Worker unable to determine",
    ReferenceNo == 8 ~ "Client doesn't know",
    ReferenceNo == 9 ~ "Client refused",
    ReferenceNo == 99 ~ "Data not collected"
  )
}

project_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "Emergency Shelter",
    ReferenceNo == 2 ~ "Transitional Housing",
    ReferenceNo == 3 ~ "Permanent Supportive Housing",
    ReferenceNo == 4 ~ "Street Outreach",
    ReferenceNo == 6 ~ "Services Only",
    ReferenceNo == 8 ~ "Safe Haven",
    ReferenceNo == 12 ~ "Prevention",
    ReferenceNo == 13 ~ "Rapid Rehousing",
    ReferenceNo == 14 ~ "Coordinated Entry"
  )
}

# HUD_value_to_description <-
#   function(table, element_name, element_column) {
#     element_name <- sym(element_name)
#     element_column <- enquo(element_column)
#     
#     a <- HUD_specs %>%
#       filter(DataElement == element_name) %>%
#       select("ReferenceNo", "Description")
#     
#     table$element_column <- with(a,
#                                  Description[match(table$element_column,
#                                                    HUD_specs$ReferenceNo)])
#   }
# 
# a <- subset(HUD_specs,
#             DataElement == "HouseholdType",
#             select = c("ReferenceNo", "Description"))
# Inventory$HouseholdType <- with(a,
#                                 Description[match(Inventory$HouseholdType,
#                                                   ReferenceNo)])


# # HMIS participating Between --------------------------------------------------
# 
# HMIS_participating_between <- function(table, start, end) {
#   HMISParticipating <-  if_else(
#     (table$HMISParticipatingBeds == 0 | is.na(table$HMISParticipatingBeds)) |
#     (is.na(table$InventoryStartDate) |
#       ymd(table$InventoryStartDate) > mdy(end)) |
#       (!is.na(table$InventoryEndDate) &
#          ymd(table$InventoryEndDate) < mdy(start)),
#     FALSE,
#     TRUE
#   )
#   HMISParticipating
# }
# not sure what the heck to do about this. :( will have to pull based
# on UsesSP which is super clunky and will leave out providers

FileEnd <- format.Date(file.info("data/Enrollment.csv")$mtime, "%m-%d-%Y")
FileStart <- format.Date(floor_date(mdy(FileEnd), "year") - years(2), "%m-%d-%Y")
FilePeriod <- interval(mdy(FileStart), mdy(FileEnd))
FileActualStart <- min(Enrollment$ExitDate, na.rm = TRUE)

# Update Date -------------------------------------------------------------

update_date <- file.info("data/Enrollment.csv")$mtime

# Save it out -------------------------------------------------------------
save.image(file = "images/COHHIOHMIS.RData")

