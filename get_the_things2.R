library(xml2)
library(tidyverse)
library(lubridate)
library(readxl)
library(data.table)
library(tools)
# pulls in the CSV files which comes from the ServicePoint export
# files <- list.files(path= "data/", full.names = TRUE, pattern='*.csv')
# filenames <-  file_path_sans_ext(basename(files)) 
# 
# function(files, filenames){
#   filenames <- read_csv(files)
# }
# 
# for(i in 1:length(files)){
#   filenames <- filenames[i] 
#   <- read_csv(files[i])
# }

Affiliation <- read_csv("data/Affiliation.csv")
Client <- read_csv("data/Client.csv")
Disabilities <- read_csv("data/Disabilities.csv")
EmploymentEducation <- read_csv("data/EmploymentEducation.csv")
Enrollment <- read_csv("data/Enrollment.csv")
EnrollmentCoC <- read_csv("data/EnrollmentCoC.csv")
Exit <- read_csv("data/Exit.csv")
Export <- read_csv("data/Export.csv")
Funder <- read_csv("data/Funder.csv")
Geography <- read_csv("data/Geography.csv")
HealthAndDV <- read_csv("data/HealthAndDV.csv")
IncomeBenefits <- read_csv("data/IncomeBenefits.csv")
Inventory <- read_csv("data/Inventory.csv")
Organization <- read_csv("data/Organization.csv")
Project <- read_csv("data/Project.csv")
ProjectCoC <- read_csv("data/ProjectCoC.csv")
Services <- read_csv("data/Services.csv")

# all other data comes from the RMisc ART report
Users <- read_xlsx("data/RMisc.xlsx",
                   sheet = 4,
                   range = cell_cols("A:G"))

Scores <- read_xlsx("data/RMisc.xlsx",
                    sheet = 1,
                    range = cell_cols("A:E"))
Scores <- mutate(Scores, StartDate = as.Date(StartDate, origin = "1899-12-30"))

counties <- read_xlsx("data/RMisc.xlsx",
                      sheet = 2,
                      range = cell_cols("A:C"))
Enrollment <- left_join(Enrollment, counties, by = "EnrollmentID")
rm(counties)

bowmanentryexits <- read_xlsx("data/RMisc.xlsx",
                          sheet = 3,
                          range = cell_cols("A:D"))
Enrollment <- left_join(Enrollment, bowmanentryexits, by = "EnrollmentID")
rm(bowmanentryexits)

providerextras <- read_xlsx("data/RMisc.xlsx",
                            sheet = 5,
                            range = cell_cols("A:E"))
Project <- left_join(Project, providerextras, by = "ProjectID")
rm(providerextras)

Client <- Client %>%
  mutate(FirstName = case_when(
    NameDataQuality %in% c(8,9) ~ "DKR",
    NameDataQuality == 2 ~ "Partial",
    NameDataQuality == 99 | is.na(NameDataQuality) | FirstName == 0 ~ "Missing",
    !(NameDataQuality %in% c(2, 8, 9, 99) | is.na(NameDataQuality) | FirstName == "Anonymous") ~ "ok"),
    LastName = NULL,
    MiddleName = NULL,
    NameSuffix = NULL)

Client <- Client %>%
  mutate(SSN = case_when(
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

save.image(file = "data/COHHIOHMIS.RData")
