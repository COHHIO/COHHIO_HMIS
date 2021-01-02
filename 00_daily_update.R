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

# Each script here creates an image file which is copied to both R minor and R 
# minor elevated. Running this after updating the data files should be all 
# that's necessary in order to be sure the apps are getting the most recent data
# and code. This script checks that you've downloaded all the correct files in 
# the correct way, runs them all, then copies the images to Rm/Rme.

library(lubridate)
library(tidyverse)

# clearing the environment prior to running all the scripts
rm(list = ls())
stop <- 0

# if there's not already an images directory, create it
if (!dir.exists("images")) dir.create("images")

# type "live" or "sample"
dataset <- "live" 

directory <- case_when(dataset == "live" ~ "data",
                       dataset == "sample" ~ "sampledata",
                       dataset == "yo" ~ "youngstowndata")

# folder check
including_data_back_to <- mdy("01012018")

export_meta <- read_csv("data/Export.csv",
                        col_types = c("iicccccccTDDcciii"))

if(floor_date(ymd_hms(export_meta$ExportDate), unit = "days") != today()) {
  stop <- 1
  cat("The HUD CSV Export files are not up to date. Please be sure you unzipped the
  export.\n")
} else{
  cat("OK\n")
}

if(ymd(export_meta$ExportStartDate) != 
   including_data_back_to |
   ymd(export_meta$ExportEndDate) != today()) {
  stop <- 1
  cat("The HUD CSV Export was not run on the correct date range. Please rerun.\n")
} else{
  cat("OK\n")
}

if(format.Date(file.info(paste0(directory, "/RMisc2.xlsx"))$mtime, "%F") != today()){
  stop <- 1
  cat("The RMisc2.xlsx file is not up to date. Please run this ART report and 
  overwrite the current RMisc2.xlsx with the new one.\n")
} else{cat("OK\n")}

if(length(list.files(paste0("./", directory), pattern = "(odod_live_hudcsv)")) > 0){
  stop <- 1
  cat("Don't forget to delete the .7z file in your /data folder. It has PII in it!\n")
} else {cat("OK\n")}

# if the data folder passes all the tests above, let's run the scripts 
if (stop == 0) {
  rm(list = ls())

  cat("Importing raw HMIS data..\n")
  source("00_get_Export_and_ART.R")

  rm(list = ls())

  cat("working on Cohorts\n")
  source("00_cohorts.R")

  rm(list = ls())  

  cat("working on Bed_Unit_Utilization\n")
  source("01_Bed_Unit_Utilization.R")

  rm(list = ls())

  cat("working on QPR_SPDATs\n")
  source("02_QPR_SPDATs.R")

  rm(list = ls())

  cat("working on QPR_EEs\n")
  source("02_QPR_EEs.R")

  rm(list = ls())

  cat("working on Veterans\n")
  source("03_Veterans.R")

  rm(list = ls())

  cat("working on Data Quality\n")
  source("04_DataQuality.R")

  rm(list = ls())

  print("working on Project Evaluation")
  source("05_Veterans_Active_List.R")
  
  # rm(list = ls())
  # 
  # print("working on Project Evaluation")
  # source("06_Project_Evaluation.R")

  rm(list = ls())

  cat("working on SPMs\n")
  source("07_SPMs.R")

  rm(list = ls())

  cat("working on Active List\n")
  source("08_Active_List.R")
  
  rm(list = ls())

  cat("copying images to app directories\n")
  source("00_copy_images.R")

  rm(list = ls())

  cat("Done! All images are updated.\n")
} else
{
  cat("Check your data folder for errors\n")
}



