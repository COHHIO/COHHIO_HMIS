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

library(tidyverse)
library(lubridate)

load("images/Data_Quality.RData")
load("images/ProjectEvaluation.RData")

# VI-SPDATs missing going into PH -----------------------------------------

vispdats_entering_ph <- dq_2019 %>%
  right_join(pe_coc_funded, by = c("ProjectID", "ProjectType", "ProjectName")) %>%
  filter(Issue == "Non-Veteran Non-DV HoHs Entering PH without SPDAT") %>%
  select(ProjectName, PersonalID) %>% 
  group_by(ProjectName) %>% 
  summarise(ClientsWithNoSPDAT = n()) %>%
  view()

# looked up some clients in Miami Family Abuse and the clients do not appear to 
# be fleeing DV, acc. to the assessment data. Same goes for RROhio.
# when we get calls/questions about this DQ item we find:
# sometimes they're expired
# most the time they really are missing

# DQ_flag Point Forfeits --------------------------------------------------

pe_coc_funded %>% select(ProjectName, DQ_flags) %>% view()

# (11/93) have a flag
# need to add in missing data as flags
# need to add a way for users to know why they missed the objective bc of DQ

# Compare HoHs entering to Adults entering --------------------------------

summary(summary_pe_homeless_history_index) # all adults
summary(summary_pe_homeless_history_index_test) # hohs only
# adults: SLIGHTLY lower Means than HoHs

summary(summary_pe_long_term_homeless) # all adults
summary(summary_pe_long_term_homeless_test) # hohs only
# adults: a little higher Means than HoHs

# Non-Cash Benefits Scoring Structure -------------------------------------

th_ncbs <- summary_pe_non_cash_at_exit %>% filter(ProjectType == 2)
psh_ncbs <- summary_pe_non_cash_at_exit %>% filter(ProjectType == 3)
rrh_ncbs <- summary_pe_non_cash_at_exit %>% filter(ProjectType == 13)
sh_ncbs <- summary_pe_non_cash_at_exit %>% filter(ProjectType == 8)


summary(th_ncbs)
summary(psh_ncbs)
summary(rrh_ncbs)
summary(sh_ncbs)

hist(th_ncbs$NCBsAtExitPercent, main = "Transitional Housing") 
hist(psh_ncbs$NCBsAtExitPercent, main = "PSH") 
hist(rrh_ncbs$NCBsAtExitPercent, main = "Rapid Rehousing") 

