
library(tidyverse)
library(lubridate)
library(knitr)
library(here)
library(readxl)

cutoff_date <- mdy("01012013")

highest_clientid_on_demo <- 244417

highest_ee_on_demo <- 395799

highest_serviceid_on_demo <- 549938

live_services <- read_xlsx(here("data/LIVEARTCompare.xlsx"), sheet = 2) %>%
  filter(ServiceID <= highest_serviceid_on_demo)
live_ees <- read_xlsx(here("data/LIVEARTCompare.xlsx"), sheet = 1) %>%
  filter(ClientID <= highest_clientid_on_demo &
           EEID <= highest_ee_on_demo)

demo_services <- read_xlsx(here("data/DEMOARTCompare.xlsx"), sheet = 2)
demo_ees <- read_xlsx(here("data/DEMOARTCompare.xlsx"), sheet = 1)

ees_missing_from_demo <- live_ees %>% anti_join(demo_ees)

services_missing_from_demo <- live_services %>% anti_join(demo_services)



# Check that demo has no clients who exited earlier than the cutoff date.


if_else(demo_ees %>% filter(ymd(Exit) > cutoff_date) %>% nrow() > 0,
   "at least one client on the demo site exited after the cutoff date (ng)",
   "no client exited after the cutoff date (ok)")











