library(tidyverse)
library(lubridate)
library(readxl)

# Pulling in the CSVs -----------------------------------------------------

Affiliation <- read_csv("data/Affiliation.csv")
Client <- read_csv("data/Client.csv")
Disabilities <- read_csv("data/Disabilities.csv",
                         col_types = "cnnDnnnnnnnnnnTTnln") 
EmploymentEducation <- read_csv("data/EmploymentEducation.csv",
                                col_types = "cnnDnnnnnnTTnln") 
Enrollment <- 
  read_csv("data/Enrollment.csv",
           col_types = 
             "nnnDcnnnlnDnnnDDDnnnncccnnDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnTTnln")
EnrollmentCoC <- read_csv("data/EnrollmentCoC.csv")
Exit <- read_csv("data/Exit.csv",
                 col_types = "nnnDncncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTnln")
Export <- read_csv("data/Export.csv")
Funder <- read_csv("data/Funder.csv")
Geography <- read_csv("data/Geography.csv")
HealthAndDV <- read_csv("data/HealthAndDV.csv",
                        col_types = "cnnDnnnnnnnDnTTnln")
IncomeBenefits <- 
  read_csv("data/IncomeBenefits.csv",
           col_types = 
             "cnnDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnncnnnnnnnnnnnnnnnnnnnncnnnnnnTTnln")
Inventory <- read_csv("data/Inventory.csv")
Organization <- read_csv("data/Organization.csv")
Project <- read_csv("data/Project.csv")
ProjectCoC <- read_csv("data/ProjectCoC.csv")
Services <- read_csv("data/Services.csv", col_types = "cnnDnncnnnTTnln")

# --- All other data comes ART > Ohio BoS > COHHIO Only > RMisc ---

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
providerextras <- read_xlsx("data/RMisc.xlsx",
                            sheet = 5,
                            range = cell_cols("A:H"))
Project <- Project %>% select(-ProjectName, -ProjectCommonName) %>%
  left_join(., providerextras, by = "ProjectID")
rm(providerextras)

# User Contact Info from ART ----------------------------------------------
Users <- read_xlsx("data/RMisc.xlsx",
                   sheet = 4,
                   range = cell_cols("A:G"))

# Masking PII in the Client file (but not DOB) ----------------------------
Client <- Client %>%
  mutate(FirstName = case_when(
    NameDataQuality %in% c(8,9) ~ "DKR",
    NameDataQuality == 2 ~ "Partial",
    NameDataQuality == 99 | is.na(NameDataQuality) | FirstName == 0 ~ "Missing",
    !(NameDataQuality %in% c(2, 8, 9, 99) | 
        is.na(NameDataQuality) | 
        FirstName == "Anonymous") ~ "ok"),
    LastName = NULL,
    MiddleName = NULL,
    NameSuffix = NULL,
    SSN = case_when(
    is.na(SSN) | is.na(SSNDataQuality) | SSNDataQuality == 99 ~ "Missing",
    SSNDataQuality %in% c(8, 9) ~ "DKR",
    ifelse((
      substr(SSN, 1, 1) != "0" &
        substr(SSN, 1, 2) != "00"
    ),
    nchar(as.numeric(SSN)) != 9, FALSE) |
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
        123456789) ~ "Invalid or Incomplete"
  ))

Client <- Client %>%
  mutate(SSN = case_when(
    is.na(SSN) ~ "ok",
    !is.na(SSN) ~ SSN
  ))


# Adding Exit Data to Enrollment ------------------------------------------
smallExit <- Exit %>% select(EnrollmentID, ExitDate, Destination, OtherDestination)
Enrollment <- left_join(Enrollment, smallExit, by = "EnrollmentID")
rm(smallExit)

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

# Client Entry Exits Between Date Range Function --------------------------------------

served_between <- function(table, start, end){
  served <- ymd(table$EntryDate) <= mdy(end) &
    (is.na(table$ExitDate) | ymd(table$ExitDate) >= mdy(start))
  served
}

entered_between <- function(table, start, end){
  entered <- between(ymd(table$EntryDate), start, end) 
  entered
}
# Projects Operating Between Date Range Function -----------------------------------

operating_between <- function(table, start, end) {
  operating <- ymd(table$OperatingStartDate) <= mdy(end) &
    (is.na(table$OperatingEndDate) | table$OperatingEndDate >= mdy(start))
  operating
}

# ReportDate prompts ------------------------------------------------------

ReportStart <- "01012018"
ReportEnd <- "12312018"
ReportingPeriod <- interval(mdy(ReportStart), mdy(ReportEnd))

# Adding Month Intervals --------------------------------------------------

FirstMonth  <-  interval(mdy(ReportStart),
                         seq(as.Date(mdy(ReportStart) %m+% months(1)),
                             length = 1, by = "1 month") - 1) 
SecondMonth  <-  interval(mdy(ReportStart) %m+% months(1),
                          seq(as.Date(mdy(ReportStart) %m+% months(2)),
                              length=1, by="1 month") -1)
ThirdMonth <- interval(mdy(ReportStart) %m+% months(2),
                       seq(as.Date(mdy(ReportStart) %m+% months(3)),
                           length=1, by="1 month") -1)
FourthMonth <- interval(mdy(ReportStart) %m+% months(3),
                        seq(as.Date(mdy(ReportStart) %m+% months(4)),
                            length=1, by="1 month") -1)
FifthMonth <- interval(mdy(ReportStart) %m+% months(4),
                       seq(as.Date(mdy(ReportStart) %m+% months(5)),
                           length=1, by="1 month") -1)
SixthMonth <- interval(mdy(ReportStart) %m+% months(5),
                       seq(as.Date(mdy(ReportStart) %m+% months(6)),
                           length=1, by="1 month") -1)
SeventhMonth <- interval(mdy(ReportStart) %m+% months(6),
                         seq(as.Date(mdy(ReportStart) %m+% months(7)),
                             length=1, by="1 month") -1)
EighthMonth <- interval(mdy(ReportStart) %m+% months(7),
                        seq(as.Date(mdy(ReportStart) %m+% months(8)),
                            length=1, by="1 month") -1)
NinthMonth <- interval(mdy(ReportStart) %m+% months(8),
                       seq(as.Date(mdy(ReportStart) %m+% months(9)),
                           length=1, by="1 month") -1)
TenthMonth <- interval(mdy(ReportStart) %m+% months(9),
                       seq(as.Date(mdy(ReportStart) %m+% months(10)),
                           length=1, by="1 month") -1)
EleventhMonth <- interval(mdy(ReportStart) %m+% months(10),
                          seq(as.Date(mdy(ReportStart) %m+% months(11)),
                              length=1, by="1 month") -1)
TwelfthMonth <- interval(mdy(ReportStart) %m+% months(11),
                         seq(as.Date(mdy(ReportStart) %m+% months(12)),
                             length=1, by="1 month") -1)

# Save it out -------------------------------------------------------------
save.image(file = "data/COHHIOHMIS.RData")
