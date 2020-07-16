
library(tidyverse)
library(lubridate)
library(knitr)
library(here)
library(readxl)

cutoff_date <- mdy("7/1/2013")

highest_clientid_on_demo <- 244010

highest_ee_on_demo <- 394982

highest_serviceid_on_demo <- 548448

raw_services_live <- read_xlsx(here("data/LIVEARTCompare.xlsx"), sheet = 2) 

projects <- read_csv("data/Project.csv")

services_live <- raw_services_live %>%
  filter(ServiceID <= highest_serviceid_on_demo) %>%
  mutate(ServiceStart = ymd_hms(ServiceStart) - hours(1),
         ServiceEnd = ymd_hms(ServiceEnd) - hours(1))

raw_ees_live <- read_xlsx(here("data/LIVEARTCompare.xlsx"), sheet = 1)

ees_live <- raw_ees_live %>%
  filter(ClientID <= highest_clientid_on_demo &
           (EEID <= highest_ee_on_demo |
              is.na(EEID))) %>%
  mutate(Entry = ymd_hms(Entry) - hours(1),
         Exit = ymd_hms(Exit) - hours(1))

raw_services_demo <- read_xlsx(here("data/DEMOARTCompare.xlsx"), sheet = 2)

services_demo <- raw_services_demo

raw_ees_demo <- read_xlsx(here("data/DEMOARTCompare.xlsx"), sheet = 1)

ees_demo <- raw_ees_demo


# Building A --------------------------------------------------------------

clients_with_real_open_ees <- ees_live %>%
  mutate(Provider = str_remove(Provider, "\\(.*\\)")) %>%
  left_join(projects %>%
              select(ProjectName, ProjectType), by = c("Provider" = "ProjectName")) %>%
  filter(is.na(Exit) &
           ClientInactive == "No" &
           EEInactive == "No" &
           str_starts(Provider, "zz", negate = TRUE) &
           !is.na(ProjectType))

most_recent_exits_by_client_live <- ees_live %>%
  filter(ClientInactive == "No" & EEInactive == "No" & !is.na(Exit)) %>%
  group_by(ClientID) %>%
  summarise(MostRecentExit = format(max(Exit), '%Y%m%d')) %>%
  ungroup() %>%
  filter(ymd(MostRecentExit) > ymd(cutoff_date))


a_group <- c(
  clients_with_real_open_ees$ClientID,
  most_recent_exits_by_client_live$ClientID
) %>% unique()

demo_client_ids <- ees_demo$ClientID %>% unique()



# Building B --------------------------------------------------------------

clients_who_have_no_real_ees <- ees_live %>%
  filter(ClientInactive == "No") %>%
  mutate(RealEE = if_else(EEInactive == "Yes" | is.na(EEID), 0, 1)) %>%
  group_by(ClientID) %>%
  summarise(RealEEs = sum(RealEE)) %>%
  ungroup() %>%
  filter(RealEEs == 0)





# 
# ees_missing_from_demo <- ees_live %>% anti_join(ees_demo)
# 
# services_missing_from_demo <- services_live %>% anti_join(services_demo)
# 
# most_recent_exits_by_client_demo <- ees_demo %>%
#   filter(ClientInactive == "No" & EEInactive == "No" & !is.na(Exit)) %>%
#   group_by(ClientID) %>%
#   summarise(MostRecentExit = format(max(Exit), '%Y%m%d')) %>%
#   ungroup()
# 
# clients_who_exited_prior_to_cutoff_date_but_are_on_demo <-
#   most_recent_exits_by_client_demo %>%
#   filter(ymd(MostRecentExit) < ymd(cutoff_date))










