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
  summarise(n()) %>%
  view()

# looked up some clients in Miami Family Abuse and the clients do not appear to 
# be fleeing DV, acc. to the assessment data. Same goes for RROhio.
# when we get calls/questions about this DQ item we find:
# sometimes they're expired
# most the time they really are missing

# DQ_flag Point Forfeits --------------------------------------------------

dq_flags 
#(11/93) have flags

# Compare HoHs entering to Adults entering --------------------------------

summary(summary_pe_homeless_history_index)
summary(summary_pe_homeless_history_index_test)
# adults: SLIGHTLY lower Means than HoHs

summary(summary_pe_long_term_homeless)
summary(summary_pe_long_term_homeless_test)
# adults: a little higher Means than HoHs


