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

# ************
# Run this whenever the data has been refreshed (usually every weekday morning)

# Each script here creates an image file which is sym linked to both
# R minor and R minor elevated. Running this after updating the data files 
# should be all that's necessary in order to be sure the apps are getting the 
# most recent data and code. This script checks that you've downloaded all the
# correct files in the correct way.

# There's some date-checking, so we need the lubridate package.
library(lubridate)
library(tidyverse)

# clearing the environment prior to running all the scripts
rm(list = ls())
stop <- 0

# type "live" or "sample"
dataset <- "live" 

directory <- case_when(dataset == "live" ~ "data",
                       dataset == "sample" ~ "sampledata",
                       dataset == "yo" ~ "youngstowndata")

# folder check

if(length(list.files(paste0("./", directory), pattern = "(report_)")) > 0){
  stop <- 1
  "There is an unnamed ReportWriter download in the data/ folder. Please either 
  name it properly or delete it."
} else {
  "OK"}

if(format.Date(file.info(paste0(directory, "/Enrollment.csv"))$mtime, 
               "%F") != today()) {
  stop <- 1
  "The HUD CSV Export files are not up to date. Please be sure you unzipped the
  export."
} else{
  "OK"
}

if(format.Date(file.info(paste0(directory, "/RMisc.xlsx"))$mtime, "%F") != today()){
  stop <- 1
  "The RMisc.xlsx file is not up to date. Please run this ART report and 
  overwrite the current RMisc.xlsx with the new one."
} else{"OK"}

if(length(list.files(paste0("./", directory), pattern = "(casemanagers.zip)")) == 0){
  stop <- 1
  "The casemanagers.zip file is missing or named incorrectly."
} else{"OK"}

if(length(list.files(paste0("./", directory), pattern = "(referrals.zip)")) == 0){
  stop <- 1
  "The referrals.zip file is missing or named incorrectly."
} else{"OK"}

if(length(list.files(paste0("./", directory), pattern = "(services1.zip)")) == 0){
  stop <- 1
  "The services1.zip file is missing or named incorrectly."
} else{"OK"}

if(length(list.files(paste0("./", directory), pattern = "(services2.zip)")) == 0){
  stop <- 1
  "The services2.zip file is missing or named incorrectly."
} else{"OK"}

if(length(list.files(paste0("./", directory), pattern = "(offers.zip)")) == 0){
  stop <- 1
  "The offers.zip file is missing or named incorrectly."
} else{"OK"}

if(length(list.files(paste0("./", directory), pattern = "(cevets.zip)")) == 0){
  stop <- 1
  "The cevets.zip file is missing or named incorrectly."
} else{"OK"}

if((!file.exists(paste0("./", directory, "/scoresind.zip")) |
   !file.exists(paste0("./", directory, "/scoresfam.zip")) |
   !file.exists(paste0("./", directory, "/scorestay.zip"))) &
   (!file.exists(paste0("./", directory, "/scores.csv"))|
    format.Date(file.info(paste0(directory, "/scores.csv"))$mtime, "%F") != today())){
  stop <- 1
  "The scoresxxx.zip files are missing or out of date."
} else{"OK"}

if(length(list.files(paste0("./", directory), pattern = "(odod_live_hudcsv)")) > 0){
  stop <- 1
  "Don't forget to delete the .7z file in your /data folder. It has PII in it!"
} else {"OK"}

if(stop == 0){
  print("Everything's good!")
  source("00_get_the_CSV_things.R")
} else {"Something went wrong"}

if(ymd(FileActualStart) > mdy(FileStart)){
  stop <- 1
  "Check that you ran your HUD CSV Export on the correct dates."
} else{"OK"}

# if the data folder passes all the tests above, let's run the rest of the 
# scripts 
if (stop == 0) {
  rm(list = ls())
  
  print("working on Cohorts")
  source("00_cohorts.R")
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
  
  print("working on Project Evaluation")
  source("06_Project_Evaluation.R")
  rm(list = ls())
  
  print("working on SPMs")
  source("07_SPMs.R")
  rm(list = ls())
  
  print("working on Active List")
  source("08_Active_List.R")
  rm(list = ls())
  
  print(paste("Done! All images are updated."))
} else
{
  print("Check your data folder for errors")
}

# all scripts together take about 7 1/2 minutes

