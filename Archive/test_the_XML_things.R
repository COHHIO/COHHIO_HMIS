library(janitor)
library("skimr")
library("tidylog")
# can't get the hopwa psh funding source to flip to its number.
# maybe look at calculating Data Collection Stage by using Intervals like you're
  # doing for NCBs.
# consider filtering Connection with SOAR data to only pull in PATH and RHY 
  # Enrollment IDs
# idea to make things faster: mutate the entire assessment_data table so that the
  # values are all already HUD CSV'd
# install.packages("devtools")
# remotes::install_github("rstudio/gt")
# Client table testing -------------------ok-------------------------------
get_dupes(Client, PersonalID)

View(Client %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))
summary(Client)

skim(DomesticViolence1)

filter(e, DataCollectionStage == 5 & !is.na(DomesticViolenceVictim))
# Enrollment table testing ---------------ok-------------------------------
get_dupes(Enrollment, EnrollmentID)

View(Enrollment %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))

# Domestic Violence testing --------------ok-------------------------------
get_dupes(DomesticViolence1, EnrollmentID)
anti_join(DomesticViolence1, DomesticViolence1ex)

get_dupes(x, PersonalID, EnrollmentID, HouseholdID, ProjectID, Value, DataCollectionStage, DateEffective)
View(DomesticViolence %>% filter(PersonalID %in% c(10454, 29446, 42737, 210831, 114311)))
View(x %>% filter(PersonalID %in% c(171167, 213714, 211099, 210831, 114311)))
setDT(x)
# Enrollment CoC testing -----------------ok-------------------------------
get_dupes(EnrollmentCoC, EnrollmentID, DataCollectionStage)
View(EnrollmentCoC %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))

# EmploymentEducation testing ------------ok-------------------------------
get_dupes(EmploymentEducation, EnrollmentID, DataCollectionStage)
View(EmploymentEducation %>% filter(PersonalID %in% c(1287, 6570)))

# Connection w SOAR testing --------------ok-------------------------------
get_dupes(ConnectionWithSOAR, EnrollmentID, DataCollectionStage)
View(ConnectionWithSOAR %>% filter(PersonalID %in% c(219218, 130592, 196671, 20228, 193399)))

# NonCash testing ---------------------------------------------------------
get_dupes(NonCashBenefits, EnrollmentID, DataCollectionStage)
View(NonCashBenefits %>% filter(PersonalID %in% c(11675, 54017, 188869, 192304, 144707)))

# Disabilities testing ----------------------------------------------------
get_dupes(tablename, EnrollmentID, DataCollectionStage)
View(tablename %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))

# Health Insurance testing ------------------------------------------------
get_dupes(tablename, EnrollmentID, DataCollectionStage)
View(tablename %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))

# Income testing ----------------------------------------------------------
get_dupes(tablename, EnrollmentID, DataCollectionStage)
View(tablename %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))

# MoveInDate testing ------------------------------------------------------
get_dupes(tablename, EnrollmentID, DataCollectionStage)
View(tablename %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))

# Contacts ----------------------------------------------------------------
get_dupes(tablename, EnrollmentID, DataCollectionStage)
View(tablename %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))

# Referrals Services ------------------------------------------------------
get_dupes(tablename, EnrollmentID, DataCollectionStage)
View(tablename %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))

