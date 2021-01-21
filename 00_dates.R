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

library(lubridate)
library(tidyverse)

# hc = hard-coded here, used elsewhere
# meta = result comes from meta data
# calc = result is calculated

# Hard-coded Dates --------------------------------------------------------

hc_data_goes_back_to <- mdy("01012018") # the date we should run the Export back to

hc_check_dq_back_to <- mdy("10012018") # the default ReportStart for DQ reporting

hc_psh_started_collecting_move_in_date <- mdy("10012017") 

hc_began_collecting_covid_data <- mdy("04012020")

hc_outreach_to_cls <- mdy("10012019")

hc_began_requiring_spdats <- mdy("01012019")

hc_project_eval_start <- mdy("01012019")

hc_project_eval_end <- mdy("12312019")

hc_unsheltered_data_start <- mdy("01012019")

hc_prior_living_situation_required <- mdy("10012016")

hc_check_eligibility_back_to <- mdy("10012016")

hc_no_more_svcs_on_hh_members <- mdy("02012019")

# Dates from Metadata -----------------------------------------------------

meta_HUDCSV_Export_Date <- read_csv("data/Export.csv",
                                col_types = c("iicccccccTDDcciii")) %>%
  mutate(ExportDate = ymd_hms(ExportDate)) %>%
  pull(ExportDate)

meta_HUDCSV_Export_Start <- read_csv("data/Export.csv",
                                     col_types = c("iicccccccTDDcciii")) %>%
  mutate(ExportStartDate = ymd(ExportStartDate)) %>%
  pull(ExportStartDate)

meta_HUDCSV_Export_End <- read_csv("data/Export.csv",
                                   col_types = c("iicccccccTDDcciii")) %>%
  mutate(ExportEndDate = ymd(ExportEndDate)) %>%
  pull(ExportEndDate)

meta_Rmisc_last_run_date <- floor_date(file.info("data/RMisc2.xlsx")$mtime, 
                                       unit = "day")

# Calculated Dates --------------------------------------------------------

calc_data_goes_back_to <-
  read_csv("data/Exit.csv",
           col_types = "nnnDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTnTn") %>%
  mutate(ExitDate = ymd(ExitDate)) %>%
  arrange(ExitDate) %>%
  head(1) %>% 
  pull(ExitDate)

calc_full_date_range <- interval(ymd(meta_HUDCSV_Export_End),
                                ymd(calc_data_goes_back_to))

calc_2_yrs_prior_end <- floor_date(today(), "month") - days(1)
calc_2_yrs_prior_start <-
  floor_date(ymd(calc_2_yrs_prior_end), "month") - years(2) + months(1)

calc_2_yrs_prior_range <- interval(ymd(calc_2_yrs_prior_start),
                                   ymd(calc_2_yrs_prior_end))

# Dates to Replace or Retire ----------------------------------------------

# should use meta_HUDCSV_Export_End here
# FileEnd <- format.Date(file.info("data/Exit.csv")$mtime, "%m-%d-%Y")

# should use cal_data_goes_back_to
# FileStart <- format.Date(floor_date(mdy(FileEnd), "year") - years(2), "%m-%d-%Y")

# should use calc_full_date_range
# FilePeriod <- interval(mdy(FileStart), mdy(FileEnd))

# should not need to make this distinction, should not use at all
# FileActualStart <- min(Exit$ExitDate, na.rm = TRUE)

# should use meta_HUDCSV_Export_Date instead
# update_date <- Export$ExportDate 


