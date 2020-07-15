library(testthat)

source(here("Purge_Compare.R"))

ees_missing_from_demo <- live_ees %>% anti_join(demo_ees)

services_missing_from_demo <- live_services %>% anti_join(demo_services)

# Check that demo has no clients who exited earlier than the cutoff date.

if_else(demo_ees %>% filter(ymd(Exit) > cutoff_date) %>% nrow() > 0,
        "at least one client on the demo site exited after the cutoff date (ng)",
        "no client exited after the cutoff date (ok)")





