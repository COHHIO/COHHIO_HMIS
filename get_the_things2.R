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
Scores <- mutate(scores, StartDate = as.Date(StartDate, origin = "1899-12-30"))

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
