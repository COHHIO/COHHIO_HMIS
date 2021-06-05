# COHHIO_HMIS
# Copyright (C) 2021  Coalition on Homelessness and Housing in Ohio (COHHIO)
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

# clearing the environment prior to running all the scripts
rm(list = ls())

source("00_functions.R")
req_pkgs <- c(
  "cli",
  "feather",
  "tidyverse",
  "lubridate",
  "readxl",
  "rlang",
  "scales",
  "janitor",
  "devtools",
  # "urbnmapr",
  "sf",
  # "choroplethrMaps",
  "plotly"
)

loaded <- purrr::map_lgl(req_pkgs, ~rlang::exec(require, package = .x))

if (!all(loaded)) {
  stop_with_instructions(
    "You are missing the following packages that are required to run this script:", 
    req_pkgs[!loaded])
}

library(dplyr)



# extract archive and delete it
  zip_file <- list.files("data", pattern = "7z$", 
                         full.names = TRUE, 
                         recursive = FALSE)

  if (!rlang::is_empty(zip_file)) {
    archive::archive_extract(zip_file, "data")
    file.remove(zip_file)
  } else if (ncol(readr::read_csv("data/Client.csv")) != 33 &&
             readr::read_csv("data/Export.csv",
                      col_types = c("iicccccccTDDcciii")) %>%
             dplyr::mutate(ExportEndDate = lubridate::ymd(ExportEndDate)) %>%
             dplyr::pull(ExportEndDate) != Sys.Date()) {
    stop_with_instructions("Please download the HUD CSV Export to the data/ folder.")
  }
  
source("00_dates.R")
  
# if there's not already an images directory, create it
if (!dir.exists("images")) dir.create("images")

# type "live" or "sample"
dataset <- "live" 

directory <- case_when(dataset == "live" ~ "data",
                       dataset == "sample" ~ "sampledata")

# folder check
# CHANGED The code on line 31 above does the extraction for the user now
# if(meta_HUDCSV_Export_End != today()) {
#   stop <- 1
#   cat("The HUD CSV Export files are not up to date. Please be sure you unzipped the
#   export.\n")
# } else{
#   cat("OK\n")
# }

if(ymd(meta_HUDCSV_Export_Start) != ymd(hc_data_goes_back_to) |
   ymd(meta_HUDCSV_Export_End) != today()) 
  stop_with_instructions("The HUD CSV Export was not run on the correct date range.
                         Please rerun.\n")


if(meta_Rmisc_last_run_date != today()) 
  stop_with_instructions("The RMisc2.xlsx file is not up to date. Please run 
                         this ART report and overwrite the current RMisc2.xlsx 
                         with the new one.")

increment("Importing raw HMIS data\n")
COHHIO_HMIS <- environment()
source("00_get_Export_and_ART.R", local = COHHIO_HMIS)

increment("working on Cohorts")
Cohorts <- rlang::env(COHHIO_HMIS) # creating child environment
# rlang::env_binding_lock(COHHIO_HMIS, ls(COHHIO_HMIS)) # locking COHHIO_HMIS
source("00_cohorts.R", local = Cohorts) # populating Cohorts env
# rlang::env_binding_lock(Cohorts, ls(Cohorts)) # locking Cohorts

increment("working on Bed_Unit_Utilization")
source("01_Bed_Unit_Utilization.R", local = rlang::env(Cohorts)) # running inside a 
# child environment OF Cohorts

increment("working on QPR_SPDATs")
source("02_QPR_SPDATs.R", local = rlang::env(COHHIO_HMIS)) # doesn't need Cohorts here

increment("working on QPR_EEs")
source("02_QPR_EEs.R", local = rlang::env(Cohorts)) # these envs don't get saved in memory

increment("working on Veterans data")
source("03_Veterans.R", local = rlang::env(Cohorts)) # 

increment("working on Data Quality")
DataQuality <- rlang::env(Cohorts) # creating a child env inside Cohorts env
source("04_DataQuality.R", local = DataQuality)
# rlang::env_binding_lock(DataQuality, ls(DataQuality))

increment("working on Veterans Active List")
source("05_Veterans_Active_List.R", local = rlang::env(Cohorts))

increment("working on SPMs")
source("07_SPMs.R", local = new.env())

increment("working on Active List")
source("08_Active_List.R", local = rlang::env(Cohorts))

increment("getting covid vaccine data together")
source("09_covid.R", local = new.env())

dir <- "pe_dataset_final"
# files <- freeze_pe(dir) # run on freeze day ONLY
pe <- rlang::new_environment(list(dir = dir), parent = .BaseNamespaceEnv)

load("images/COHHIOHMIS.RData", envir = pe)
load("images/Data_Quality.RData", envir = pe)
load("images/cohorts.RData", envir = pe)

increment("working on Project Evaluation")
source("CopyOf06_Project_Evaluation.R", local = pe)

increment("copying images to app directories")
rm(Cohorts, COHHIO_HMIS)
source("00_copy_images.R", local = new.env())

increment("Done! All images are updated.")

