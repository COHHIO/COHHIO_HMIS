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
#<https://www.gnu.org/licenses/>.

# Run this whenever either one of these scripts has fundamentally changed or the
# data has been refreshed.

# Each script here creates an image file which is then sym linked to both
# R minor and R minor elevated. Running this after updating the data files should
# be all that's necessary in order to be sure the apps are getting the most
# recent data and code.

# clearing the environment prior to running all the scripts
rm(list = ls())

# quick folder check

if(length(list.files("./data", pattern = "(report_)")) > 0){
  "There is an unnamed ReportWriter download in the data/ folder. Please either name it properly or delete it."
} else {"You renamed all your ReportWriter files, yay!"}

if(format.Date(file.info("data/Enrollment.csv")$mtime, "%F") != today()){
  "The HUD CSV Export files are not up to date. Please be sure you unzipped the 
  export."
} else{"You exported and unzipped the HUD CSV Export correctly."}

if(format.Date(file.info("data/RMisc.xlsx")$mtime, "%F") != today()){
  "The RMisc.xlsx file is not up to date. Please run this ART report and 
  overwrite the current RMisc.xlsx with the new one."
} else{"RMisc.xlsx looks good."}

if(length(list.files("./data", pattern = "(casemanagers)")) == 0){
  "The casemanagers.zip file is missing or named incorrectly."
} else{"Your casemanagers file looks good."}

if(length(list.files("./data", pattern = "(referrals)")) == 0){
  "The referrals.zip file is missing or named incorrectly."
} else{"Your referrals file is ok."}

if(length(list.files("./data", pattern = "(services1)")) == 0){
  "The services1.zip file is missing or named incorrectly."
} else{"Your services1 file is fabulous!"}

if(length(list.files("./data", pattern = "(services2)")) == 0){
  "The services2.zip file is missing or named incorrectly."
} else{"Your services2 file is perfect!"}

if(length(list.files("./data", pattern = "(offers)")) == 0){
  "The offers.zip file is missing or named incorrectly."
} else{"Your offers file is fine!"}

if(length(list.files("./data", pattern = "(cevets)")) == 0){
  "The cevets.zip file is missing or named incorrectly."
} else{"Your cevets file is all good."}

if(length(list.files("./data", pattern = "(providers)")) == 0){
  "The providers.zip file is missing or named incorrectly."
} else{"Your providers file could not be better."}

if((!file.exists("./data/scoresind.zip") |
   !file.exists("./data/scoresfam.zip") |
   !file.exists("./data/scorestay.zip")) &
   (!file.exists("./data/scores.csv")|
    format.Date(file.info("data/scores.csv")$mtime, "%F") != today())){
  "The scoresxxx.zip files are missing or out of date."
} else{"Your scores data is finery."}

source("00_get_the_CSV_things.R")

# checking that the earliest Exit Dates in the data = what the reporting is 
# expecting
if (ymd(FileActualStart) <= mdy(FileStart)) {
  rm(list = ls())
  
  print("working on Bed_Unit_Utilization")
  source("01_Bed_Unit_Utilization.R")
  rm(list = ls())
  
  print("working on QPR_SPDATs")
  source("02_QPR_SPDATs.R")
  rm(list = ls())
  
  print("working on QPR_EEs")
  source("02_QPR_EEs.R")
  rm(list = ls())
  
  print("working on Veterans")
  source("03_Veterans.R")
  rm(list = ls())
  
  print("working on Data Quality")
  source("04_DataQuality.R")
  rm(list = ls())
  
  print("working on Cohorts")
  source("05_Cohorts.R")
  rm(list = ls())
  
  print("Done!")
} else
{
  print("Check your export Start and End Dates")
}

if(length(list.files("./data", pattern = "(odod_live_hudcsv)")) > 0){
  "Don't forget to delete the .7z file in your /data folder. It has PII in it!"
} else {"Your data folder looks good."}
