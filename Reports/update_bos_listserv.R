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

load("images/COHHIOHMIS.RData")

active_users <- Users %>%
  filter(UserActive == "Yes") %>%
  select(UserName, UserEmail)

non_users_on_listserv <- tibble(
  UserName = c(
    "Collen Kosta",
    "Amy Bullard",
    "Christie Watson",
    "Lequita Potter",
    "Nicole Michaelson",
    "Patrick Hart",
    "Rich Agnello",
    "Sally Hammitt",
    "Scott Gary",
    "Shannon Prince",
    "Vernon McNeil"
  ),
  UserEmail = c(
    "colleen.kosta@mahoningcountyoh.gov",
    "Amy.Bullard@development.ohio.gov",
    "christie.watson@va.gov",
    "lequita.potter@va.gov",
    "nicole.michaelson@va.gov",
    "patrick.hart@development.ohio.gov",
    "Rich.Agnello@va.gov",
    "Sally.Hammitt@va.gov",
    "scott.gary@development.ohio.gov",
    "shannon.prince@development.ohio.gov",
    "vernon.mcneil@development.ohio.gov"
  )
)

add_to_listserv <- rbind(active_users, non_users_on_listserv)

