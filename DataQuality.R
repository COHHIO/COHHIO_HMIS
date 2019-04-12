library(tidyverse)
library(janitor)
library(lubridate)

load("~/R/COHHIO_HMIS/data/COHHIOHMIS.RData")


# Missing Data ------------------------------------------------------------

missingClient <- Client %>%
  filter(
    is.na(DOB) |
      NameDataQuality == 99 |
      SSN == "missing" |
      DOBDataQuality == 99 |
      RaceNone == 1 |
      Ethnicity == 99 |
      Gender == 99 |
      VeteranStatus == 99
  ) # needs more logic (adult/child)

missingEnrollment <- Enrollment %>%
  filter(
    RelationshipToHoH == 99 |
      LivingSituation == 99 |
      LengthOfStay == 99 |
      is.na(PreviousStreetESSH) | # needs more logic
      is.na(DateToStreetESSH) | # needs more logic
      is.na(TimesHomelessPastThreeYears) |
      is.na(MonthsHomelessPastThreeYears) |
      is.na(DisablingCondition)
  )
