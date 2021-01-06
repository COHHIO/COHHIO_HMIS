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

# Hard-coded Dates --------------------------------------------------------

hc_data_goes_back_to <- mdy("01012018")

hc_check_dq_back_to <- mdy("10012018")

hc_psh_started_collecting_move_in_date <- mdy("10012017")

# Metadata and Calculated Dates -------------------------------------------

# meta_HUDCSV_Export_Date <- read_csv("data/Export.csv",
#                                 col_types = c("iicccccccTDDcciii")) %>%
#   mutate() %>%
#   pull(format.Date(ymd_hms(ExportDate), "%m%d%Y"))

# cal_data_goes_back_to <- min(Enrollment$ExitDate, na.rm = TRUE)

# Dates to Replace or Retire ----------------------------------------------
Export <- read_csv("data/Export.csv",
                   col_types = c("iicccccccTDDcciii"))
Exit <-
  read_csv("data/Exit.csv",
           col_types = "nnnDncnnnnnnnnnnnnnnnnnnnnnnnnnDnnnnnnTTnTn")

FileEnd <- format.Date(file.info("data/Exit.csv")$mtime, "%m-%d-%Y")
FileStart <- format.Date(floor_date(mdy(FileEnd), "year") - years(2), "%m-%d-%Y")
FilePeriod <- interval(mdy(FileStart), mdy(FileEnd))
FileActualStart <- min(Exit$ExitDate, na.rm = TRUE)


update_date <- Export$ExportDate

rm(Export, Exit)

