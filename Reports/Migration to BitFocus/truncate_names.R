#COHHIO_HMIS
#Copyright (C) 2021  Coalition on Homelessness and Housing in Ohio (COHHIO)

#This program is free software: you can redistribute it and/or modify
#it under the terms of the GNU Affero General Public License as published
#by the Free Software Foundation, either version 3 of the License, or
#any later version.

#This program is distributed in the hope that it will be useful,
#but WITHOUT ANY WARRANTY; without even the implied warranty of
#MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
#GNU Affero General Public License for more details at 
#<https://www.gnu.org/licenses/>.

library(tidyverse)
library(lubridate)
library(readxl)
library(here)

providers_set_to_migrate <- read_xlsx(here("random_data/reporting_groups.xlsx")) %>%
  select("Group" = 1, "ProjectName" = 2) %>%
  filter(Group == "01 Providers Coming to Clarity")

provider_name_too_long <- providers_set_to_migrate %>%
  mutate(characters = nchar(ProjectName), 
         first50 = substr(ProjectName, 1, 50)) %>%
  filter(characters > 50)
