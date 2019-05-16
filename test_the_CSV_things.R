library(tidyverse)
library(janitor)
library(skimr)
library(tidylog)

load("images/COHHIOHMIS.RData")

# Client table testing ----------------------------------------------------
context('testing client file')

test_that('client file is the right shape', {
  expect_equal(ncol(Client), 33, label = "Wrong number of columns")
  expect_gte(nrow(Client), 35000, label = "Maybe not incomplete data set")
  expect_equal(nrow(get_dupes(Client, PersonalID)), 0, label = "Duplicate Client IDs")
})

test_that('data seems complete', {
  expect_equal(nrow(
    filter(Client, RaceNone %in% c(8, 9, 99) |
             AmIndAKNative + Asian + BlackAfAmerican + NativeHIOtherPacific +
             White != 0)), 
               nrow(Client),
    label = "RaceNone not calculated correctly")
  expect_equal(nrow(filter(Client, is.na(VeteranStatus))), 0, 
               label = "Null Veteran Status")
})

fromART <- read_csv("data/BOST_Client_.csv")

smallFromART <- fromART %>% mutate(
  AmIndAKNative =
    if_else(
      Race1 == "American Indian or Alaska Native (HUD)" |
        Race2 == "American Indian or Alaska Native (HUD)",
      1,
      0,
      missing = 0
    ),
  Asian =
    if_else(Race1 == "Asian (HUD)" |
              Race2 == "Asian (HUD)", 1, 0, missing = 0),
  BlackAfAmerican =
    if_else(
      Race1 == "Black or African American (HUD)" |
        Race2 == "Black or African American (HUD)",
      1,
      0,
      missing = 0
    ),
  NativeHIOtherPacific =
    if_else(
      Race1 == "Native Hawaiian or Other Pacific Islander (HUD)" |
        Race2 == "Native Hawaiian or Other Pacific Islander (HUD)",
      1,
      0,
      missing = 0
    ),
  White =
    if_else(Race1 == "White (HUD)" |
              Race2 == "White (HUD)", 1, 0, missing = 0),
  Ethnicity = case_when(
    Ethnicity == "Non-Hispanic/Non-Latino (HUD)" ~ 0,
    Ethnicity == "Hispanic/Latino (HUD)" ~ 1,
    Ethnicity == "Client doesn't know (HUD)" ~ 8,
    Ethnicity == "Client refused (HUD)" ~ 9,
    Ethnicity == "Data not collected (HUD)" | is.na(Ethnicity) ~ 99
    ),
  Gender = case_when(
    Gender == "Female" ~ 0,
    Gender == "Male" ~ 1,
    Gender == "Trans Male (FTM or Female to Male)" ~ 3,
    Gender == "Trans Female (MTF or Male to Female)" ~ 2,
    Gender == "Gender Non-Conforming (i.e. not exclusively male or female)" ~ 4,
    Gender == "Client doesn't know" ~ 8,
    Gender == "Client refused" ~ 9,
    Gender == "Data not collected" | is.na(Gender) ~ 99
  ),
  VeteranStatus = case_when(
    VeteranStatus == "No (HUD)" ~ 0,
    VeteranStatus == "Yes (HUD)" ~ 1,
    VeteranStatus == "Client doesn't know (HUD)" ~ 8,
    VeteranStatus == "Client refused (HUD)" ~ 9,
    VeteranStatus == "Data not collected (HUD)" | is.na(VeteranStatus) ~ 99)
) %>% select(ClientID, ClientSSN, DOB, Ethnicity, Gender, VeteranStatus)

smallClient <- 
  Client %>% 
  select(PersonalID, SSN, DOB, Ethnicity, Gender, VeteranStatus)

map2(smallClient, smallFromART, setdiff) %>% 
  map_int(length)

# Enrollment table testing ------------------------------------------------
get_dupes(Enrollment, EnrollmentID)

View(Enrollment %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))

# Domestic Violence testing -----------------------------------------------
get_dupes(HealthAndDV, EnrollmentID)

View(HealthAndDV %>% filter(PersonalID %in% c(10454, 29446, 42737, 210831, 114311)))
# Enrollment CoC testing --------------------------------------------------
get_dupes(EnrollmentCoC, EnrollmentID, DataCollectionStage)
View(EnrollmentCoC %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))

# EmploymentEducation testing ---------------------------------------------
get_dupes(EmploymentEducation, EnrollmentID, DataCollectionStage)
View(EmploymentEducation %>% filter(PersonalID %in% c(1287, 6570)))

# Disabilities testing ----------------------------------------------------
get_dupes(Disabilities, EnrollmentID, DataCollectionStage)
View(tablename %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))

# Health Insurance testing ------------------------------------------------
get_dupes(HealthAndDV, EnrollmentID, DataCollectionStage)
View(tablename %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))

# Income testing ----------------------------------------------------------
get_dupes(IncomeBenefits, EnrollmentID, DataCollectionStage)
View(tablename %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))


