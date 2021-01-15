
load("images/COHHIOHMIS.RData")

library(tidyverse)
library(lubridate)
library(readxl)

Interims <-
  read_xlsx(paste0(directory, "/RMisc2.xlsx"), sheet = 20) %>%
  mutate(InterimDate = as.Date(InterimDate, origin = "1899-12-30"))

small_enrollment <- Enrollment %>%
  select(PersonalID, EnrollmentID, HouseholdID, MoveInDate, MoveInDateAdjust, HHMoveIn)




