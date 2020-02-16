
# Case 918025

library(tidyverse)
library(lubridate)
library(here)
library(janitor)

DateOpened <- mdy("02142020")
difftime(today(), DateOpened, units = "days")

# Importance between 1 and 10
Importance <- 10


# The Problem -------------------------------------------------------------

# Disability.csv not carrying open disability subs through subsequent EEs

# Please see client 10655. They have an open disability sub that starts in 
# Sept of 2018. It has a Yes for DisabilityResponse and Yes for 
# IndefiniteAndImpairs. In the Disability.csv, it shows correctly for the EE 
# that also starts in Sept 2018, but it does not show for any of the subsequent 
# EEs.

# Test that it's corrected ------------------------------------------------

Disabilities <-
  read_csv(here("data/Disabilities.csv"),
           col_types = "cnnDnnnnnnnnnnTTnTn")

x <- Disabilities %>%
  filter(PersonalID == 10655) %>%
  select(
    PersonalID,
    EnrollmentID,
    DataCollectionStage,
    InformationDate,
    DisabilityType,
    DisabilityResponse,
    IndefiniteAndImpairs
  )

if(nrow(x %>% filter(
  EnrollmentID == 346321 &
  DisabilityType == 9 &
  DisabilityResponse == 1
)) > 0) {
  print("It's fixed!")
} else{
  print("It's not fixed!")
}

