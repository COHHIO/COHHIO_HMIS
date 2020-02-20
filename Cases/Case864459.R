
# Case 864459

library(tidyverse)
library(lubridate)

DateOpened <- mdy("08302019")

difftime(today(), DateOpened, units = "days")

# Importance between 1 and 10

Importance <- 6


# The Problem -------------------------------------------------------------

# In the specs, it says LOSUnderThreshold should be populated with 0, 1, or 99, 
# but the CSV Export has that data as logical (true/false) data. 

# Test that it's corrected ------------------------------------------------

Enrollment <-
  read_csv("data/Enrollment.csv",
           col_types =
             "nnnDcnnnlnDnnnDDDnnnncccnnDnnnncnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnnTTnTn")

if(nrow(Enrollment %>%
        filter(LOSUnderThreshold == 99)) > 0){
  print("It's fixed")
} else{
  print("It's not fixed")
}





