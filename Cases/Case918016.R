
# Case 918016

library(tidyverse)
library(lubridate)

DateOpened <- mdy("02142020")

difftime(today(), DateOpened, units = "days")

# Importance between 1 and 10

Importance <- 2


# The Problem -------------------------------------------------------------

# HUD CSV has nulls in Organization ID in Project.csv when the specs don't
# allow for that

# Test that it's corrected ------------------------------------------------

if(nrow(
  Project <-
  read_csv("data/Project.csv",
           col_types = "nnccDDnnnnnnnnTTcTn") %>%
  filter(is.na(OrganizationID))
) > 0) {
  print("It's not fixed!")
} else{
  print("It's fixed!")
}



