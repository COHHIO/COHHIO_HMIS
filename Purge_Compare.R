
library(tidyverse)
library(lubridate)
library(knitr)
library(here)
library(readxl)

cutoff_date <- mdy("7/1/2013")

highest_clientid_on_demo <- 244010

highest_ee_on_demo <- 394982

highest_serviceid_on_demo <- 548448

live_services <- read_xlsx(here("data/LIVEARTCompare.xlsx"), sheet = 2) %>%
  filter(ServiceID <= highest_serviceid_on_demo)
live_ees <- read_xlsx(here("data/LIVEARTCompare.xlsx"), sheet = 1) %>%
  filter(ClientID <= highest_clientid_on_demo &
           EEID <= highest_ee_on_demo)

demo_services <- read_xlsx(here("data/DEMOARTCompare.xlsx"), sheet = 2)
demo_ees <- read_xlsx(here("data/DEMOARTCompare.xlsx"), sheet = 1)













