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
# <https://www.gnu.org/licenses/>.

library(tidyverse)
library(lubridate)
library(scales)
library(here)

cvi <- read_csv(here("Ohio/Ohio_COUNTY.csv")) %>%
  select(COUNTY,
         "SocioeconomicStatus" = RPL_THEME1,
         "HouseholdCompositionDisability" = RPL_THEME2,
         "MinorityStatusLanguage" = RPL_THEME3,
         "HousingTypeTransportation" = RPL_THEME4,
         RPL_THEMES) %>%
  mutate(
    SocioeconomicStatus_pct = percent(SocioeconomicStatus),
    HouseholdCompositionDisability_pct = percent(HouseholdCompositionDisability),
    MinorityStatusLanguage_pct = percent(MinorityStatusLanguage),
    HousingTypeTransportation_pct = percent(HousingTypeTransportation),
    RPL_THEMES_pct = percent(RPL_THEMES)
  )
  
svi <- read_csv(here("Ohio/Ohio.csv")) %>%
  select(ST_ABBR,
         COUNTY,
         FIPS,
         LOCATION,
         "SocioeconomicStatus" = RPL_THEME1,
         "HouseholdCompositionDisability" = RPL_THEME2,
         "MinorityStatusLanguage" = RPL_THEME3,
         "HousingTypeTransportation" = RPL_THEME4,
         RPL_THEMES)

