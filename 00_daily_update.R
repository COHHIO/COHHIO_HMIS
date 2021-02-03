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

stop_with_instructions <- function(...) {
  cli::cli_alert_danger(cli::col_red(paste0(..., collapse = "\n")))
  cli::cli_alert_info("See instructions for details:\nhttps://docs.google.com/document/d/1iT_dgf0HtBzGOO8PqFNvyS_djA78JcYZsWaeZQYJC9E/edit#heading=h.xvdv7715aoi1")
  stop("See above.", call. = FALSE)
}

increment <- function(..., cenv = rlang::caller_env()) {
  # pre allocate file path for previous timer
  .lt_path <- "data/last_timer.rds"
  
  # if the first step remove tracking objects from env (if there were previous failures)
  if (stringr::str_detect(paste0(...), "Importing raw")) suppressWarnings(rm(.update, .timer, .step, envir = cenv))
  
  # start the status progress process if its not active
  if (is.null(cenv$.update)) cenv$.update <- cli::cli_process_start("Parsing COHHIO_HMIS data", .auto_close = FALSE, .envir = cenv)
  # if the last timer data exists load it and compute the total time from the previous run
  if (file.exists(.lt_path) && is.null(cenv$.last_timer)) {
    cenv$.last_timer <- readRDS(.lt_path)
    cenv$.total_time <- difftime(tail(cenv$.last_timer, 1)$ts, head(cenv$.last_timer, 1)$ts, units = "mins")
    cenv$.total_steps<- tail(cenv$.last_timer, 1)$step
    cli::cli_status_update(cenv$.update, cli::col_blue("Expected time of completion: ", Sys.time() + cenv$.total_time))
  }
  # create the step object or increment it
  if (is.null(cenv$.step)) {
    cenv$.step <- 1 
  } else {
    cenv$.step <- cenv$.step + 1
  }
  
  # send the status message to console
  cli::cli_status_update(cenv$.update, msg = "Step {cenv$.step}/{rlang::`%||%`(cenv$.total_steps, 11)}: {paste0(...)}...\n")
  
  if (is.null(cenv$.timer)) cenv$.timer <- data.frame(ts = Sys.time(), step = cenv$.step, msg = paste0(...))
  else {
    cenv$.timer <- rbind.data.frame(cenv$.timer, data.frame(ts = Sys.time(), step = cenv$.step, msg = paste0(...)))
  }
  if (stringr::str_detect(paste0(...),"^Done!")) {
    cli::cli_process_done(cenv$.update)
    saveRDS(cenv$.timer, .lt_path)
    cli::col_blue("Timing data saved to ", .lt_path)
    return(.lt_path)
  }
  # If no previous timer data, just give the elapsed time
  .elapsed <- round(difftime(tail(cenv$.timer, 1)$ts, head(cenv$.timer, 1)$ts, units = "mins"),2)
  if (is.null(cenv$.last_timer)) {
    cli::cli_status_update(cenv$.update, cli::col_grey("Time elapsed: ", .elapsed, " mins"))
  } else {
    cli::cli_status_update(cenv$.update,  cli::cli_verbatim(cli::col_grey("Time elapsed: ", .elapsed," mins - ",paste0(round(as.numeric(.elapsed) / as.numeric(cenv$.total_time), 2) * 100, "% complete\nApprox. completion at: ", cenv$.total_time - .elapsed + Sys.time()))))
  }
}


# extract archive and delete it
if (file.info("data/Assessment.csv")$mtime < Sys.Date()) {
  . <- list.files("data", pattern = "7z$", full.names = TRUE)
  if (!is.character(.)) stop_with_instructions("Please download the HUD CSV Export.")
  archive::archive_extract(., "data")
  file.remove(.)
}



# some preliminary parameters

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
   ymd(meta_HUDCSV_Export_End) != today()) stop_with_instructions("The HUD CSV Export was not run on the correct date range. Please rerun.\n")


if(meta_Rmisc_last_run_date != today()) stop_with_instructions("The RMisc2.xlsx file is not up to date. Please run this ART report and overwrite the current RMisc2.xlsx with the new one.")


increment("Importing raw HMIS data")
COHHIO_HMIS <- environment()
source("00_get_Export_and_ART.R", local = COHHIO_HMIS)

increment("working on Cohorts")
Cohorts <- rlang::child_env(COHHIO_HMIS)
source("00_cohorts.R", local = Cohorts)

increment("working on Bed_Unit_Utilization")
source("01_Bed_Unit_Utilization.R", local = rlang::child_env(Cohorts))

increment("working on QPR_SPDATs")
source("02_QPR_SPDATs.R", local = rlang::child_env(COHHIO_HMIS))

increment("working on QPR_EEs")
source("02_QPR_EEs.R", local = rlang::child_env(Cohorts))

increment("working on Veterans")
source("03_Veterans.R", local = rlang::child_env(Cohorts))

increment("working on Data Quality")
source("04_DataQuality.R", local = rlang::child_env(Cohorts))

increment("working on Project Evaluation")
source("05_Veterans_Active_List.R", local = rlang::child_env(Cohorts))

# rm(list = ls())
# 
# print("working on Project Evaluation")
# source("06_Project_Evaluation.R", local = new.env())

increment("working on SPMs")
source("07_SPMs.R", local = new.env())

increment("working on Active List")
source("08_Active_List.R", local = rlang::child_env(Cohorts))

increment("copying images to app directories")
source("00_copy_images.R", local = rlang::child_env(Cohorts))

increment("Done! All images are updated.")
