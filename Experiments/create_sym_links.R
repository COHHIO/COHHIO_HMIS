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

# PLEASE NOTE:
# This script BUILDS the script you need, then you run that script in your
# command prompt. Slashes for Macs will likely be different.

# 1. Modify the text to fit what your paths and filenames are
# 2. Copy the resulting command from this script
# 3. Right-click the cmd executable and open it by selecting "Run as Administrator"
# 4. Paste the command in and press Enter.

directory_where_all_your_projects_live <-
  "C:\\Users\\HMIS\\Documents\\R\\"

project_and_folder_that_contains_image <- "COHHIO_HMIS\\images\\"

image_filename <- "Data_Quality.RData"

project_and_folder_you_wish_had_the_image <- "Rminor\\data\\"

command_to_copy_into_command_prompt <-
  cat(paste0(
    "mklink \"",
    directory_where_all_your_projects_live,
    project_and_folder_you_wish_had_the_image,
    image_filename,
    "\" \"",
    directory_where_all_your_projects_live,
    project_and_folder_that_contains_image,
    image_filename
  ))
