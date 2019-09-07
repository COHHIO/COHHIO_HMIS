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
library(readxl)

# Pulling in the CSVs -----------------------------------------------------

Affiliation <- 
  read_csv("data/Affiliation.csv", 
           col_types = "nnnTTnTn")

if(ncol(read_csv("data/Client.csv")) == 36) {
  Client <-
    read_csv("data/Client.csv",
             col_types = "nccccncnDnnnnnnnnnnnnnnnnnnnnnnTTnTn") %>%
    filter(!PersonalID %in% c(5, 4216))
} else {
  Client <-
    read_csv("data/Client.csv",
             col_types = "ncncnnnnnnnnnnnnnnnnnnnnnnnTTnTn") %>%
    filter(!PersonalID %in% c(5, 4216))
}

Disabilities <-
  read_csv("data/Disabilities.csv",
           col_types = "cnnDnnnnnnnnnnTTnTn")
EmploymentEducation <-
  read_csv("data/EmploymentEducation.csv",
           col_types = "cnnDnnnnnnTTnTn")
Enrollment <-
  read_csv("data/Enrollment.csv",
           col_types =
             "nnnDcnnnlnDnnnDDDnnnncccnnDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnTTnTn")
EnrollmentCoC <- 
  read_csv("data/EnrollmentCoC.csv", 
           col_types = "cncnnDcnTTnTn")
Exit <-
  read_csv("data/Exit.csv",
           col_types = "nnnDncncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTnTn")
Export <- 
  read_csv("data/Export.csv",
           col_types = "nnnccccncTDDccnnn")
Funder <- 
  read_csv("data/Funder.csv",
           col_types = "nnncDDTTnTn")
Geography <- 
  read_csv("data/Geography.csv",
           col_types = "nncDnnccccnTTnTn")
HealthAndDV <-
  read_csv("data/HealthAndDV.csv",
           col_types = "cnnDnnnnnnnDnTTnTn")
IncomeBenefits <- 
  read_csv("data/IncomeBenefits.csv",
           col_types = 
             "cnnDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnncnnnnnnnnnnnnnnnnnnnncnnnnnnTTnTn")
Inventory <- 
  read_csv("data/Inventory.csv",
           col_types = "nncnnnnnnnnDDnTTDnTn")
Organization <- 
  read_csv("data/Organization.csv",
           col_types = "nccTTnTn")
Project <- 
  read_csv("data/Project.csv",
           col_types = "nnccDDnnnnnnnnTTnTn")
ProjectCoC <- 
  read_csv("data/ProjectCoC.csv",
           col_types = "nncTTnTn")

# not pulling in Services from the HUD CSVs because the CSV Export does not 
# pull in all the kinds of Services we collect. (Services will be imported
# from two ReportWriter reports.)
# Services <- 
#   read_csv("data/Services.csv", 
#            col_types = "cnnDnncnnnTTnTn")

# - All other data comes from either the RMisc ART report or ReportWriter #

# Youth Beds not coming through correctly ---------------------------------

youth_beds <- read_xlsx("data/RMisc.xlsx",
                       sheet = 8,
                       range = cell_cols("A:C"))
Inventory <- left_join(Inventory, youth_beds, by = "InventoryID") %>%
  select(1, ProjectID = 2, 3:9, YouthBedInventory = 22, 11:20) 

rm(youth_beds)


# from sheet 1, creating a Scores table -----------------------------------
Scores <- read_xlsx("data/RMisc.xlsx",
                    sheet = 1,
                    range = cell_cols("A:E"))
Scores <- mutate(Scores, StartDate = as.Date(StartDate, origin = "1899-12-30"))

# from sheets 2 and 3, getting EE-related data, joining both to En --------
counties <- read_xlsx("data/RMisc.xlsx",
                      sheet = 2,
                      range = cell_cols("A:C"))
bowman_entry_exits <- read_xlsx("data/RMisc.xlsx",
                          sheet = 3,
                          range = cell_cols("A:D"))
Enrollment <- left_join(Enrollment, bowman_entry_exits, by = "EnrollmentID") %>%
  left_join(., counties, by = "EnrollmentID") 

rm(bowman_entry_exits, counties)

# grabbing extra provider data from sheet 5 -------------------------------
# overwriting HUD CSV columns bc of the 50 character limit
provider_extras <- read_xlsx("data/RMisc.xlsx",
                            sheet = 5,
                            range = cell_cols("A:M")) %>%
  mutate(OrganizationName = str_remove(OrganizationName, "\\(.*\\)"))

Project <- Project %>%
  select(-ProjectName,-ProjectCommonName) %>%
  left_join(., provider_extras, by = "ProjectID")

rm(provider_extras)

# Region data -------------------------------------------------------------
Regions <- read_csv("data/Regions.csv", col_types = "cd") %>%
  mutate(RegionName = paste("Homeless Planning Region", Region)) %>%
  select(Region, County, RegionName) %>%
  arrange(Region)

project_county <- Project %>%
  mutate(County = str_remove(ProjectName, "zz"),
         County = if_else(word(County, 1) == "Van", "Van Wert",
                          word(County, 1))
         ) 

Project <- left_join(project_county, Regions, by = "County")

rm(project_county)
# Custom Veteran Data -----------------------------------------------------
VeteranCE <- read_xlsx("data/RMisc.xlsx",
                         sheet = 6,
                         range = cell_cols("A:J"))
VeteranCE <- 
  mutate(
    VeteranCE,
    DateVeteranIdentified = as.Date(DateVeteranIdentified, origin = "1899-12-30"),
    ExpectedPHDate = as.Date(ExpectedPHDate, origin = "1899-12-30"),
    MostRecentOfferDate = as.Date(MostRecentOfferDate, origin = "1899-12-30")
  )

# Offers of Housing -------------------------------------------------------
Offers <- read_xlsx("data/RMisc.xlsx",
                    sheet = 7,
                    range = cell_cols("A:G"))
Offers <- 
  mutate(
    Offers,
    DateAdded = as.Date(DateAdded, origin = "1899-12-30"),
    OfferDate = as.Date(OfferDate, origin = "1899-12-30"),
    AcceptDeclineDate = as.Date(AcceptDeclineDate, origin = "1899-12-30")
  )

# User Contact Info from ART ----------------------------------------------
Users <- read_xlsx("data/RMisc.xlsx",
                   sheet = 4,
                   range = cell_cols("A:G"))

# Adding Exit Data to Enrollment because c'mon ----------------------------
small_exit <- Exit %>% select(EnrollmentID, 
                             ExitDate, 
                             Destination, 
                             OtherDestination)

Enrollment <- left_join(Enrollment, small_exit, by = "EnrollmentID") %>%
  mutate(ExitAdjust = if_else(is.na(ExitDate), today(), ExitDate))

rm(small_exit)

# Adding ProjectType to Enrollment too bc we need EntryAdjust & MoveInAdjust

small_project <- Project %>%
  select(ProjectID, ProjectType) 

Enrollment <- Enrollment %>%
  left_join(small_project, by = "ProjectID") %>%
  mutate(
    MoveInDateAdjust = case_when(
      ymd(EntryDate) <= ymd(MoveInDate) &
        ymd(MoveInDate) <= ExitAdjust &
        ProjectType %in% c(3, 9) &
        EntryDate < mdy("10012017") ~ EntryDate,
      ymd(EntryDate) <= ymd(MoveInDate) &
        ymd(MoveInDate) <= ExitAdjust &
        ProjectType %in% c(3, 9) &
        EntryDate >= mdy("10012017") ~ MoveInDate,
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

rm(small_project)


# Services ----------------------------------------------------------------

# this comes from two ReportWriter reports: An Export: Services and 
# An Export: Services & Funds. Saving them as services1.csv and services2.csv.

services_1 <- read_csv("data/services1.csv",
                      col_types = "nnnn??cccc")

services_1 <- services_1 %>%
  mutate(ServiceStartDate = mdy(ServiceStartDate),
         ServiceEndDate = mdy(ServiceEndDate))

services_funds <- read_csv("data/services2.csv",
                      col_types = "ncd")

Services <- services_1 %>%
  left_join(services_funds, by = "ServiceID") %>%
  rename("ServiceHHID" = HouseholdID)

rm(services_1, services_funds)

staging_services <- Services[c("PersonalID",
                              "ServiceID",
                              "ServiceStartDate",
                              "ServiceEndDate",
                              "ServiceHHID")] %>%
  left_join(Enrollment[c("EnrollmentID",
                         "PersonalID",
                         "EntryDate",
                         "ExitAdjust")],
            by = "PersonalID") %>%
  mutate(
    ServiceEndAdjust = if_else(is.na(ServiceEndDate), today(), ServiceEndDate),
    ServiceRange = interval(ymd(ServiceStartDate), ymd(ServiceEndAdjust)),
    EERange = interval(ymd(EntryDate), ymd(ExitAdjust)),
    Valid = if_else(int_overlaps(ServiceRange, EERange), TRUE, FALSE)
  ) %>%
  filter(Valid == TRUE) %>%
  select(PersonalID,
         ServiceID,
         EnrollmentID,
         ServiceHHID)

# the code above pulls in the HHID associated with the SERVICE, not the EE. In
# general, the ServiceHHID does not help anything except finding obscure data
# quality problems, like that the Service was built using a different HH than
# the associated EE. It's not a big deal anymore, since we're not even
# entering Services/Referrals onto non-HoHs anyway. Currently I cannot think of
# a reason to use the Service's HH ID ever but I'll keep it in just in case.

Services <- staging_services %>%
  full_join(Services, by = c("ServiceID", "PersonalID", "ServiceHHID"))

# the code above pulls in Services that cannot be associated to an EE, so some 
# EnrollmentIDs will be NULL. These should be filtered out when reporting on
# Services, but they're needed for Data Quality checking so I'm leaving them in.

rm(staging_services)

# Referrals ---------------------------------------------------------------

Referrals <- read_csv("data/Referrals.csv",
                      col_types = "nnn?ccccccccc")

Referrals <- Referrals %>%
  mutate(ReferralDate = mdy(ReferralDate),
         ReferralHHID = HouseholdID,
         HouseholdID = NULL)

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
    (is.na(table$ExitDate) | ymd(table$ExitDate) > mdy(start))
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

# HMIS participating Between --------------------------------------------------

HMIS_participating_between <- function(table, start, end) {
  HMISParticipating <-  if_else(
    (table$HMISParticipatingBeds == 0 | is.na(table$HMISParticipatingBeds)) |
    (is.na(table$InventoryStartDate) |
      ymd(table$InventoryStartDate) > mdy(end)) |
      (!is.na(table$InventoryEndDate) &
         ymd(table$InventoryEndDate) < mdy(start)),
    FALSE,
    TRUE
  )
  HMISParticipating
}

FileEnd <- format.Date(file.info("data/Client.csv")$mtime, "%m-%d-%Y")
FileStart <- format.Date(floor_date(mdy(FileEnd), "month") - years(2), "%m-%d-%Y")
FilePeriod <- interval(mdy(FileStart), mdy(FileEnd))


# Masking PII in the Client file (but not DOB) ----------------------------

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
      substr(SSN, 1, 1) == 0 |
        substr(SSN, 1, 2) == "00" |
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

if(ncol(read_csv("data/Client.csv")) == 33)
{write_csv(Client, "data/Client.csv", append = FALSE)}

# Update Date -------------------------------------------------------------

update_date <- file.info("data/Enrollment.csv")$mtime

# Save it out -------------------------------------------------------------
save.image(file = "images/COHHIOHMIS.RData")

rm(list = ls())
