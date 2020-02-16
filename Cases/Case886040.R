
# Case 886040

library(tidyverse)
library(lubridate)

DateOpened <- mdy("11022019")

difftime(today(), DateOpened, units = "days")

# Importance between 1 and 10

Importance <- 1


# The Problem -------------------------------------------------------------

# CurrentLivingSituation column shouldn't be null but it is in our file.

# Test that it's corrected ------------------------------------------------

CurrentLivingSituation <-
  read_csv("data/CurrentLivingSituation.csv",
            col_types = "nnnTncnnnnncTTcTc")

if(nrow(CurrentLivingSituation %>%
   filter(is.na(CurrentLivingSituation))) > 0){
  print("It's fixed")
} else{
  print("It's not fixed")
}




