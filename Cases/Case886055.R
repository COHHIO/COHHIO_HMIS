
# Case 886055

library(tidyverse)
library(lubridate)

DateOpened <- mdy("11032019")

difftime(today(), DateOpened, units = "days")

# Importance between 1 and 10

Importance <- 5


# The Problem -------------------------------------------------------------

# All users should be in the file, and the ones who have been deleted should 
# have DateDeleted dates because otherwise the UserIDs in all the other files
# have nothing to tie back to.

# Test that it's corrected ------------------------------------------------

Users <-
  read_csv("data/User.csv",
           col_types = "ncccncTTTn")

if(nrow(Users %>%
        filter(!is.na(DateDeleted))) > 0){
  print("It's fixed")
} else{
  print("It's not fixed")
}







