
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

in_new_not_in_current <- anti_join(Services_new, Services_current)
in_current_not_in_new <- anti_join(Services_current, Services_new)

# some fundsources aren't coming in for New BECAUSE THEY WERE VOIDED (New is right)

in_new_not_in_current <- setdiff(Services_new$PersonalID, Services_current$PersonalID)

in_current_not_in_new <- setdiff(Services_current$PersonalID, Services_new$PersonalID)

# same exact Client IDs and ServiceIDs from both sides, it's just the lack of 
# Fund/Source messing up the anti_join() In RW, the voided Funds come in with
# no way of filtering them out, so that is actually wrong anyway

load("images/Data_Quality.RData")
write_csv(dq_unsheltered, "dq_unsheltered_new.csv")
# rerun everything
load("images/Data_Quality.RData")
write_csv(dq_unsheltered, "dq_unsheltered_current.csv")

dq_unsh_current <- read_csv("dq_unsheltered_current.csv")

dq_unsh_new <- read_csv("dq_unsheltered_new.csv")

in_new_not_in_current <- anti_join(dq_unsh_new, dq_unsh_current)

in_current_not_in_new <- anti_join(dq_unsh_current, dq_unsh_new)

# new section

load("images/QPR_SPDATs.RData")
write_csv(qpr_spdats_county, "spdats_county_new.csv")
# rerun everything
load("images/QPR_SPDATs.RData")
write_csv(qpr_spdats_county, "spdats_county_current.csv")

spdats_county_current <- read_csv("spdats_county_current.csv")

spdats_county_new <- read_csv("spdats_county_new.csv")

in_new_not_in_current <- anti_join(spdats_county_new, spdats_county_current)

in_current_not_in_new <- anti_join(spdats_county_current, spdats_county_new)

# new section

load("images/QPR_EEs.RData")
write_csv(qpr_spending, "qpr_spending_new.csv")
# rerun everything
load("images/QPR_EEs.RData")
write_csv(qpr_spending, "qpr_spending_current.csv")

spending_current <- read_csv("qpr_spending_current.csv")

spending_new <- read_csv("qpr_spending_new.csv")

in_new_not_in_current <- anti_join(spending_new, spending_current)

in_current_not_in_new <- anti_join(spending_current, spending_new)

# this is all about that one service where a source was voided and then two
# more that added up to the original amount added. an outlier and "new" is
# doing the right thing.


