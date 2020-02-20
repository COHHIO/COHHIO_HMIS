
# Case 877960

library(tidyverse)
library(lubridate)

DateOpened <- mdy("10092019")

difftime(today(), DateOpened, units = "days")

# Importance between 1 and 10

Importance <- 1


# The Problem -------------------------------------------------------------

# There's no data in the LastPermanentAddressState column in the Enrollment.csv 

# Test that it's corrected ------------------------------------------------

Enrollment <-
  read_csv("data/Enrollment.csv",
           col_types =
             "nnnDcnnnlnDnnnDDDnnnncccnnDnnnncnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnTTnTn")

if(nrow(Enrollment %>%
        filter(!is.na(LastPermanentAddressState))) > 0){
  print("It's fixed")
} else{
  print("It's not fixed")
}





