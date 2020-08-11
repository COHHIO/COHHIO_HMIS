
dq_main_current <- read_csv("dq_main_current.csv")

dq_main_new <- read_csv("dq_main_new.csv")

in_new_not_in_current <- anti_join(dq_main_new, dq_main_current)

in_current_not_in_new <- anti_join(dq_main_current, dq_main_new)

# the differences here relate to there being more Services coming in than there
# were before, so more instances of "Non-HoH Service Transaction)


dq_unsh_current <- read_csv("dq_unsheltered_current.csv")

dq_unsh_new <- read_csv("dq_unsheltered_new.csv")

in_new_not_in_current <- anti_join(dq_unsh_new, dq_unsh_current)

in_current_not_in_new <- anti_join(dq_unsh_current, dq_unsh_new)

# CaseManagers files are different, so different errors are simply not working
# anymore

in_new_not_in_current <- setdiff(new_CaseManagers$PersonalID,
                                 current_CaseManagers$PersonalID)

in_current_not_in_new <- setdiff(current_CaseManagers$PersonalID, 
                                   new_CaseManagers$PersonalID)

current_CaseManagers %>% filter(PersonalID %in% c(in_current_not_in_new)) %>%
  view()

# none of the CaseManagers from the zzDiversion provider were coming into the ART 
# report. Turns out that makes sense, so I modified the RW report.





