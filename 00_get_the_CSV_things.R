# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
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
# cevets.zip or .csv **
# cocscoring.zip or .csv (during CoC Competition only) **
# offers.zip or .csv **
# referrals.zip or .csv 
# scoresfam.zip or .csv
# scoresind.zip or .csv
# scorestay.zip or .csv
# services1.zip or .csv
# services2.zip or .csv

# ** cannot be obtained from the Youngstown site (maybe move to RMisc?)

library(tidyverse)
library(lubridate)
library(readxl)

# calling in HMIS-related functions

source("00_functions.R")

# type "live" or "sample" or "yo"
if(exists("dataset") == FALSE) {
  dataset <- "live"
} else {
  dataset <- dataset
}

directory <- case_when(dataset == "live" ~ "data",
                       dataset == "sample" ~ "sampledata",
                       dataset == "yo" ~ "youngstowndata")

# Affiliation -------------------------------------------------------------

Affiliation <- 
  read_csv(paste0(directory, "/Affiliation.csv"), 
           col_types = "nnnTTnTn") 

# Client ------------------------------------------------------------------

# This script later overwrites the Client.csv, masking Name and SSN PII. So
# this logic will read in the modified file - or - the raw one straight from SP

if(ncol(read_csv(paste0(directory, "/Client.csv"))) == 36) {
  Client <-
    read_csv(paste0(directory, "/Client.csv"),
             col_types = "nccccncnDnnnnnnnnnnnnnnnnnnnnnnTTcTn") %>%
    filter(!PersonalID %in% c(5, 4216)) # our fake Client IDs are 5 and 4216
} else {
  Client <-
    read_csv(paste0(directory, "/Client.csv"),
             col_types = "ncncnDnnnnnnnnnnnnnnnnnnnnnnTTcTn") %>%
    filter(!PersonalID %in% c(5, 4216))
}

# Masking PII in the Client file (but not DOB) 

if(ncol(read_csv(paste0(directory, "/Client.csv"))) == 36)
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
{write_csv(Client, paste0(directory, "/Client.csv"), append = FALSE)}

# CurrentLivingSituation <- 
#   read_csv(paste0(directory, "/CurrentLivingSituation.csv"),
#             col_types = "nnnTncnnnnncTTcTc") DON'T NEED YET

# Disabilities ------------------------------------------------------------

Disabilities <-
  read_csv(paste0(directory, "/Disabilities.csv"),
           col_types = "cnnDnnnnnnnnnnTTnTn")


# EmploymentEducation -----------------------------------------------------

EmploymentEducation <-
  read_csv(paste0(directory, "/EmploymentEducation.csv"),
           col_types = "cnnDnnnnnnTTnTn")

# Exit --------------------------------------------------------------------

Exit <-
  read_csv(paste0(directory, "/Exit.csv"),
           col_types = "nnnDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTnTn")

# Project -----------------------------------------------------------------

Project <- 
  read_csv(paste0(directory, "/Project.csv"),
           col_types = "nnccDDnnnnnnnnTTcTn") 

provider_extras <- read_xlsx(paste0(directory, "/RMisc.xlsx"),
                                sheet = 4,
                                range = cell_cols("A:H")) %>%
  mutate(ProjectRegion = if_else(ProviderRegion != "Homeless Planning Region 10",
                                 str_remove(ProviderRegion, "0"),
                                 ProviderRegion),
         ProviderRegion = NULL)

if(file.exists(paste0(directory, "/cocscoring.zip"))) {
  unzip(zipfile = paste0("./", directory, "/cocscoring.zip"), 
        exdir = paste0("./", directory))
  
  file.rename(paste0(directory, "/", list.files(paste0("./", directory), pattern = "(report_)")),
              paste0(directory, "/cocscoring.csv"))
  
  file.remove(paste0(directory, "/cocscoring.zip"))
}

coc_scoring <- read_csv(paste0(directory, "/cocscoring.csv"),
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

regions <- read_csv("public_data/Regions.csv",
                    col_types = "cn") %>%
  arrange(Region) %>%
  mutate(RegionName = if_else(
    Region == 0,
    "Mahoning CoC",
    paste("Homeless Planning Region", Region)))
# 
# Project <- left_join(project_county, regions, by = "County")

# EnrollmentCoC -----------------------------------------------------------

EnrollmentCoC <- 
  read_csv(paste0(directory, "/EnrollmentCoC.csv"), 
           col_types = "cncnnDcnTTnTn")

# VeteranCE --------------------------------------------------------------

if(file.exists(paste0(directory, "/cevets.zip"))) {
  unzip(zipfile = paste0("./", directory, "/cevets.zip"), 
        exdir = paste0("./", directory))
  
  file.rename(paste0(directory, "/", list.files(paste0("./", directory), 
                                                pattern = "(report_)")),
              paste0(directory, "/cevets.csv"))
  
  file.remove(paste0(directory, "/cevets.zip"))
}

VeteranCE <- read_csv(paste0(directory, "/cevets.csv"), col_types = "ii??ic?cccc")

VeteranCE <- 
  mutate(
    VeteranCE,
    DateVeteranIdentified = mdy(DateVeteranIdentified),
    ExpectedPHDate = mdy(ExpectedPHDate),
    MostRecentOfferDate = mdy(MostRecentOfferDate)
  )

# Enrollment --------------------------------------------------------------

Enrollment <-
  read_csv(paste0(directory, "/Enrollment.csv"),
           col_types =
             "nnnDcnnnlnDnnnDDDnnnncccnnDnnnncnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnTTnTn")

# from sheets 1 and 2, getting EE-related data, joining both to En --------
# will eventually come from aa: ees in ReportWriter, waiting on WS
counties <- read_xlsx(paste0(directory, "/RMisc.xlsx"),
                                 sheet = 1,
                                 range = cell_cols("A:D"),
                                 col_types = c("numeric", "numeric", "text", "text")) %>%
  select(EnrollmentID, CountyServed, CountyPrior)
  
bowman_entry_exits <- read_xlsx(paste0(directory, "/RMisc.xlsx"),
                                sheet = 2,
                                range = cell_cols("A:D"))

Enrollment <- Enrollment %>% 
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
#   read_csv(paste0(directory, "/Event.csv"),
#            col_types = "nnnDnnncDTTcTc") <- no data

# Export ------------------------------------------------------------------

Export <- 
  read_csv(paste0(directory, "/Export.csv"),
           col_types = "nnnccccncTDDccnnn")

# Funder ------------------------------------------------------------------

Funder <- 
  read_csv(paste0(directory, "/Funder.csv"),
           col_types = "nnnccDDTTcTn")

# HealthAndDV -------------------------------------------------------------

HealthAndDV <-
  read_csv(paste0(directory, "/HealthAndDV.csv"),
           col_types = "cnnDnnnnnnnDnTTnTn")

# IncomeBenefits ----------------------------------------------------------

IncomeBenefits <- 
  read_csv(paste0(directory, "/IncomeBenefits.csv"),
           col_types = 
             "cnnDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnncnnnnnnnnnnnnnnnnnnnncnnnnnnTTnTn")

# Inventory ---------------------------------------------------------------

Inventory <-
  read_csv(paste0(directory, "/Inventory.csv"),
           col_types = "nncnnnnnnnnnnnnDDTTcTn")

# Organization ------------------------------------------------------------

Organization <- 
  read_csv(paste0(directory, "/Organization.csv"),
           col_types = "ncncTTnTn")

# ProjectCoC --------------------------------------------------------------

ProjectCoC <- 
  read_csv(paste0(directory, "/ProjectCoC.csv"),
           col_types = "nncnccccnnTTcTn")

# Case Manager Records ----------------------------------------------------

if(file.exists(paste0(directory, "/casemanagers.zip"))){
  unzip(zipfile = paste0("./", directory, "/casemanagers.zip"), 
        exdir = paste0("./", directory))
  
  file.rename(paste0(directory, "/", list.files(paste0("./", directory), 
                                                pattern = "(report_)")),
              paste0(directory, "/casemanagers.csv"))
  
  file.remove(paste0(directory, "/casemanagers.zip"))
}

CaseManagers <- read_csv(paste0(directory, "/casemanagers.csv"),
                             col_types = "dccccc") %>%
  mutate(
    CMStartDate = mdy(CMStartDate),
    CMEndDate = mdy(CMEndDate)
  )


# Contacts ----------------------------------------------------------------

Contacts <- read_xlsx(paste0(directory, "/RMisc.xlsx"),
                      sheet = 5) %>%
  mutate(
    ContactDate = ymd(as.Date(ContactDate, origin = "1899-12-30")),
    ContactProvider = str_remove(ContactProvider, "\\(.*\\)")
  )

# Scores ------------------------------------------------------------------

if(file.exists(paste0(directory, "/scoresfam.zip"))) {
  unzip(zipfile = paste0("./", directory, "/scoresfam.zip"), 
        exdir = paste0("./", directory))
  
  file.rename(paste0(directory, "/", list.files(paste0("./", directory), 
                                                pattern = "(report_)")),
              paste0(directory, "/scores.csv"))
  
  file.remove(paste0(directory, "/scoresfam.zip"))
}

if(file.exists(paste0(directory, "/scoresind.zip"))) {
  unzip(zipfile = paste0("./", directory, "/scoresind.zip"), 
        exdir = paste0("./", directory))
  
  file.rename(paste0(directory, "/", list.files(paste0("./", directory), 
                                                pattern = "(report_)")),
              paste0(directory, "/scoresind.csv"))
  
  file.remove(paste0(directory, "/scoresind.zip"))
}

if(file.exists(paste0(directory, "/scorestay.zip"))) {
  unzip(zipfile = paste0("./", directory, "/scorestay.zip"), 
        exdir = paste0("./", directory))
  
  file.rename(paste0(directory, "/", list.files(paste0("./", directory), pattern = "(report_)")),
              paste0(directory, "/scorestay.csv"))
  
  file.remove(paste0(directory, "/scorestay.zip"))
}

file.append(paste0(directory, "/scores.csv"), paste0(directory, "/scoresind.csv"))

file.append(paste0(directory, "/scores.csv"), paste0(directory, "/scorestay.csv"))

if(file.exists(paste0(directory, "/scoresind.csv"))) {
  file.remove(c(
    paste0(directory, "/scoresind.csv"),
    paste0(directory, "/scorestay.csv")
  ))
}

Scores <- read_csv(paste0(directory, "/scores.csv"),
                   col_types = "ccc") %>%
  filter(Score != "Score") %>%
  mutate(
    ScoreDate = mdy(ScoreDate),
    PersonalID = as.double(PersonalID),
    Score = as.double(Score)
  ) %>%
  unique()

# Offers -----------------------------------------------------------------

if(file.exists(paste0(directory, "/offers.zip"))) {
  unzip(zipfile = paste0("./", directory, "/offers.zip"), 
        exdir = paste0("./", directory))
  
  file.rename(paste0(directory, "/", list.files(paste0("./", directory), pattern = "(report_)")),
              paste0(directory, "/offers.csv"))
  
  file.remove(paste0(directory, "/offers.zip"))
}

Offers <- read_csv(paste0(directory, "/offers.csv"), col_types = "i?c?c") %>%
  mutate(
    OfferDate = mdy(OfferDate),
    AcceptDeclineDate = mdy(AcceptDeclineDate)
  )

# Users ------------------------------------------------------------------
Users <- read_xlsx(paste0(directory, "/RMisc.xlsx"),
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


# COVID-19 ----------------------------------------------------------------

if(file.exists(paste0(directory, "/covid19.zip"))) {
  unzip(zipfile = paste0("./", directory, "/covid19.zip"), 
        exdir = paste0("./", directory))
  
  file.rename(paste0(directory, "/", list.files(paste0("./", directory), 
                                                pattern = "(report_)")),
              paste0(directory, "/covid19.csv"))
  
  file.remove(paste0(directory, "/covid19.zip"))
}

covid19 <- read_csv(paste0(directory, "/covid19.csv"),
                    col_types = "ncccccccccccccccccccccccccccccc") %>%
  mutate(
    COVID19AssessmentDate = mdy(COVID19AssessmentDate),
    ContactWithConfirmedDate = mdy(ContactWithConfirmedDate),
    ContactWithUnderInvestigationDate = mdy(ContactWithUnderInvestigationDate),
    TestDate = mdy(TestDate),
    DateUnderInvestigation = mdy(DateUnderInvestigation)
  ) 

# Services ----------------------------------------------------------------

# this comes from two ReportWriter reports: An Export: Services and 
# An Export: Services & Funds. Saving them as services1.csv and services2.csv.

if(file.exists(paste0(directory, "/services1.zip"))) {
  unzip(zipfile = paste0("./", directory, "/services1.zip"), 
        exdir = paste0("./", directory))
  
  file.rename(paste0(directory, "/", list.files(paste0("./", directory), 
                                                pattern = "(report_)")),
              paste0(directory, "/services1.csv"))
  
  file.remove(paste0(directory, "/services1.zip"))
}

services_1 <- read_csv(paste0(directory, "/services1.csv"),
                      col_types = "nnnn??cccc")

services_1 <- services_1 %>%
  mutate(ServiceStartDate = mdy(ServiceStartDate),
         ServiceEndDate = mdy(ServiceEndDate))

if(file.exists(paste0(directory, "/services2.zip"))) {
  unzip(zipfile = paste0("./", directory, "/services2.zip"), 
        exdir = paste0("./", directory))
  
  file.rename(paste0(directory, "/", list.files(paste0("./", directory), 
                                                pattern = "(report_)")),
              paste0(directory, "/services2.csv"))
  
  file.remove(paste0(directory, "/services2.zip"))
}

services_funds <- read_csv(paste0(directory, "/services2.csv"),
                      col_types = "ncd") %>% 
  group_by(ServiceID, Fund) %>%
  summarise(Amount = sum(Amount)) %>%
  ungroup()

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

if(file.exists(paste0(directory, "/referrals.zip"))) {
  unzip(zipfile = paste0("./", directory, "/referrals.zip"), 
        exdir = paste0("./", directory))
  
  file.rename(paste0(directory, "/", list.files(paste0("./", directory), 
                                                pattern = "(report_)")),
              paste0(directory, "/referrals.csv"))
  
  file.remove(paste0(directory, "/referrals.zip"))
}

Referrals <- read_csv(paste0(directory, "/referrals.csv"),
                      col_types = "nnn?cccccccccc")

Referrals <- Referrals %>%
  mutate(ReferralDate = mdy(ReferralDate),
         ReferralHHID = HouseholdID,
         HouseholdID = NULL,
         UserCreatingReferral = UserCreating,
         UserCreating = NULL)


# HUD CSV Specs -----------------------------------------------------------

HUD_specs <- read_csv("public_data/HUDSpecs.csv",
                      col_types = "ccnc") %>%
  as.data.frame()

# Adding Age at Entry to Enrollment ---------------------------------------
small_client <- Client %>% select(PersonalID, DOB)
Enrollment <- Enrollment %>%
  left_join(small_client, by = "PersonalID") %>%
  mutate(AgeAtEntry = age_years(DOB, EntryDate)) %>%
  select(-DOB)

rm(small_client)

# Metadata ----------------------------------------------------------------

FileEnd <- format.Date(file.info(paste0(directory, "/Enrollment.csv"))$mtime, 
                       "%m-%d-%Y")
FileStart <- format.Date(floor_date(mdy(FileEnd), "year") - years(2), "%m-%d-%Y")
FilePeriod <- interval(mdy(FileStart), mdy(FileEnd))
FileActualStart <- min(Enrollment$ExitDate, na.rm = TRUE)

# Update Date -------------------------------------------------------------

update_date <- file.info(paste0(directory, "/Enrollment.csv"))$mtime

# Save it out -------------------------------------------------------------

if(dataset == "yo") {
  save.image(file = "images/YOHMIS.RData")
} else{
  save.image(file = "images/COHHIOHMIS.RData")
}


