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
Client <-
  read_csv("data/Client.csv",
           col_types = "nccccncnDnnnnnnnnnnnnnnnnnnnnnnTTnTn")
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
Services <- 
  read_csv("data/Services.csv", 
           col_types = "cnnDnncnnnTTnTn")

# --- All other data comes ART > Ohio BoS > COHHIO Only > RMisc --- #

# Youth Beds not coming through correctly ---------------------------------

YouthBeds <- read_xlsx("data/RMisc.xlsx",
                       sheet = 8,
                       range = cell_cols("A:C"))
Inventory <- left_join(Inventory, YouthBeds, by = "InventoryID") %>%
  select(1, ProjectID = 2, 3:9, YouthBedInventory = 22, 11:20) 

rm(YouthBeds)


# from sheet 1, creating a Scores table -----------------------------------
Scores <- read_xlsx("data/RMisc.xlsx",
                    sheet = 1,
                    range = cell_cols("A:E"))
Scores <- mutate(Scores, StartDate = as.Date(StartDate, origin = "1899-12-30"))

# from sheets 2 and 3, getting EE-related data, joining both to En --------
counties <- read_xlsx("data/RMisc.xlsx",
                      sheet = 2,
                      range = cell_cols("A:C"))
bowmanentryexits <- read_xlsx("data/RMisc.xlsx",
                          sheet = 3,
                          range = cell_cols("A:D"))
Enrollment <- left_join(Enrollment, bowmanentryexits, by = "EnrollmentID") %>%
  left_join(., counties, by = "EnrollmentID") 

rm(bowmanentryexits, counties)

# grabbing extra provider data from sheet 5 -------------------------------
# overwriting HUD CSV columns bc of the 50 character limit
providerextras <- read_xlsx("data/RMisc.xlsx",
                            sheet = 5,
                            range = cell_cols("A:M")) %>%
  mutate(OrganizationName = str_remove(OrganizationName, "\\(.*\\)"))
Project <- Project %>% select(-ProjectName, -ProjectCommonName) %>%
  left_join(., providerextras, by = "ProjectID")
rm(providerextras)


# Region data -------------------------------------------------------------
Regions <- read_csv("data/Regions.csv", col_types = "cd") %>%
  mutate(RegionName = paste("Homeless Planning Region", Region)) %>%
  select(Region, County, RegionName) %>%
  arrange(Region)

x <- Project %>%
  mutate(County = str_remove(ProjectName, "zz"),
         County = if_else(word(County, 1) == "Van", "Van Wert",
                          word(County, 1))
         ) 

Project <- left_join(x, Regions, by = "County")

rm(x)
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
smallExit <- Exit %>% select(EnrollmentID, 
                             ExitDate, 
                             Destination, 
                             OtherDestination)

Enrollment <- left_join(Enrollment, smallExit, by = "EnrollmentID") %>%
  mutate(ExitAdjust = if_else(is.na(ExitDate), today(), ExitDate))

rm(smallExit)

# Adding ProjectType to Enrollment too bc we need EntryAdjust & MoveInAdjust

smallProject <- Project %>%
  select(ProjectID, ProjectType) 

Enrollment <- Enrollment %>%
  left_join(smallProject, by = "ProjectID") %>%
  mutate(
    MoveInDateAdjust = if_else(
      ymd(EntryDate) <= ymd(MoveInDate) &
        ymd(MoveInDate) <= ExitAdjust &
        ProjectType %in% c(3, 9, 13),
      MoveInDate,
      NULL
    ),
    EntryAdjust = case_when(
      ProjectType %in% c(1, 2, 4, 8, 12) ~ EntryDate,
      ProjectType %in% c(3, 9, 13) &
        !is.na(MoveInDateAdjust) ~ MoveInDateAdjust
    )
  )

rm(smallProject)

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
smallClient <- Client %>% select(PersonalID, DOB)
Enrollment <- Enrollment %>%
  left_join(smallClient, by = "PersonalID") %>%
  mutate(AgeAtEntry = age_years(DOB, EntryDate)) %>%
  select(-DOB)
rm(smallClient)

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
  stayed <- between(ymd(table$EntryAdjust), mdy(start), mdy(end))
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
FileStart <- format.Date(mdy(FileEnd) - years(2), "%m-%d-%Y")
FilePeriod <- interval(mdy(FileStart), mdy(FileEnd))


# Masking PII in the Client file (but not DOB) ----------------------------
Client <- Client %>%
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
      is.na(SSN) |
        is.na(SSNDataQuality) | SSNDataQuality == 99 ~ "Missing",
      SSNDataQuality %in% c(8, 9) ~ "DKR",
      substr(SSN, 1, 1) == 0 |
        substr(SSN, 1, 2) == "00" |
        nchar(SSN) != 9 |
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
        ) ~ "Invalid or Incomplete"
    )
  )

Client <- Client %>%
  mutate(SSN = case_when(
    is.na(SSN) ~ "ok",
    !is.na(SSN) ~ SSN
  ))

# Update Date -------------------------------------------------------------

updatedate <- file.info("data/Client.csv")$mtime

# Save it out -------------------------------------------------------------
save.image(file = "images/COHHIOHMIS.RData")

rm(list = ls())
