
# Case 864582

library(tidyverse)
library(lubridate)

DateOpened <- mdy("09012019")

difftime(today(), DateOpened, units = "days")

# Importance between 1 and 10

Importance <- 6


# The Problem -------------------------------------------------------------

# HUD CSV export null Assessment Data showing as 1's

# WS was adjusting the Yes/No based on the subs if the Yes/No is blank. This
# is ok if ALL other reports are making this leap, but I don't think they are.
# Which means the R Data Quality report needs to see it the way it is in SP.

# Test that it's corrected ------------------------------------------------

# Either use Client 219137 or go to SP and find a client with a null Yes/No
# for Income or NCBs but a complete set of subs. Use ReportWriter if necessary.

example_client <- 219137

IncomeBenefits <- 
  read_csv("data/IncomeBenefits.csv",
           col_types = 
             "cnnDnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnncnnnnnnncnnnnnnnnnnnnnnnnnnnncnnnnnnTTnTn")

if(nrow(
  IncomeBenefits %>%
  filter(PersonalID == example_client &
         (is.na(BenefitsFromAnySource) |
          BenefitsFromAnySource == 99))
) > 0){
  print("It's fixed")
} else{
  print("It's not fixed")
}





