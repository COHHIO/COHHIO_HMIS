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

# PLEASE NOTE: FOR THESE SCRIPTS TO WORK YOU HAVE TO:
# 1. RIGHT-CLICK R STUDIO AND OPEN IT BY CLICKING "RUN AS ADMINISTRATOR"
# 2. REPLACE r_directory WITH YOUR OWN
# 3. REPLACE YOUR DOCUMENT AND PATH NAMES AS APPROPRIATE

# "original" means where the data you're wanting to link to is located
# "link" means where the link is located

r_directory <- "C:\\Users\\HMIS\\Documents\\R\\"

originating_project_name <- "COHHIO_HMIS"

originating_path <- "images"

originating_file_name <- "Data_Quality.RData"

linking_project_name <- "Rminor"

linking_project_path <- "data"

link_name <- "Data_Quality.RData"

original <- paste0(r_directory,
                  originating_project_name,
                  "\\",
                  originating_path,
                  "\\",
                  originating_file_name)

new_link <- paste0(r_directory,
                   linking_project_name,
                   "\\",
                   linking_project_path,
                   "\\",
                   link_name)

shell(sprintf("mklink %s %s", 
              normalizePath(new_link, mustWork = FALSE),
              normalizePath(original)
))
