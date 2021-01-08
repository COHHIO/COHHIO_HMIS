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
library(writexl)

load("images/COHHIOHMIS.RData")

active_users <- Users %>%
  filter(
    UserActive == "Yes" &
      !UserEmail %in% c(
        "gwenbeebe@gmail.com",
        "sholsen@alumni.emory.edu",
        "awilson.msw@gmail.com",
        "rmatthews@fwsadl.com",
        "dameca.neal@use.salvationarmy.org"
      ) &
      !UserName %in% c("BIS ART Support User", "User, Art"),
      !DefaultProvider %in% c(
        "Bowman Systems, LLC",
        "Compass Family & Community Services",
        "Catholic Charities Regional Agency",
        "zzMahoning - Meridian Services - Homeless Solutions SRO - PSH",
        "Mahoning - Beatitude House - CH Permanent Supportive Housing - PSH",
        "Mahoning - Beatitude House - Permanent Supportive Housing Program - PSH",
        "Mahoning - Catholic Charities - ESG City - RRH",
        "Mahoning - Community Action Agency of Columbiana - SSVF RRH",
        "Mahoning - Compass Family & Community Services - Daybreak - RHY-BCPes",
        "Mahoning - Coordinated Entry - CE",
        "Mahoning - Help Network - PATH - Street Outreach",
        "Mahoning - Help Network - Shelter Plus Care - PSH",
        "Mahoning - Meridian Services - Homeless Solutions SRO II - PSH",
        "Mahoning - Meridian Services - Marian Commons - PSH",
        "Mahoning - Meridian Services - Phoenix Court - PSH",
        "Mahoning - Rescue Mission of Mahoning Valley Family Services Division - ES",
        "Mahoning - Rescue Mission of Mahoning Valley Mens Division - ES",
        "Mahoning - Ursuline Center - Merici - PSH",
        "Mahoning - Ursuline Center - Merici Shelter - ES",
        "Mahoning - YWCA Barbara Wick Transitional Home - TH",
        "Mahoning - YWCA Permanent Housing for Disabled Families - PSH",
        "Mahoning - YWCA Scattered Site Housing II - PSH",
        "Mahoning Valley Dispute Resolution Services",
        "Mahoning Valley Dispute Resolution Services - ESG - HP",
        "Rescue Mission of Mahoning Valley",
        "zzBeatitude House - A House of Blessing, Youngstown - TH",
        "zzFamily Promise of Mahoning Valley",
        "Mahoning - Ursuline Center - Merici - PSH"
      )
  ) %>%
  select(UserName, UserEmail)

non_users_on_listserv <- tibble(
  UserName = c(
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

add_to_listserv <- rbind(active_users, non_users_on_listserv) %>%
  select(UserEmail)

write_csv(add_to_listserv, "random_data/upload_to_bos_listserv.csv")

# Who are we losing/gaining -----------------------------------------------

# email the following subject line to boshmis-request@cohhio.org: who ***** (<- pw)

# replace below with the results of getting the list of current members ^^

current <- data.frame(
  UserEmail = c(
   "replacethis@fake.com"
  )
)

not_a_current_user <-
  current %>% anti_join(active_users, by = "UserEmail")

current_user_not_on_list <-
  active_users %>% anti_join(current, by = "UserEmail")

write_xlsx(x = list(lost = not_a_current_user, 
                    gained = current_user_not_on_list),
           "random_data/bos_listserv_changes.xlsx")


