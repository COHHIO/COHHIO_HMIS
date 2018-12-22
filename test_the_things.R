library(janitor)
# check that the subassessment dates are all only ymd and make necessary corrections
# can't get the hopwa psh funding source to flip to its number.

# Client table testing ----------------------------------------------------
get_dupes(Client, PersonalID)

View(Client %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))


# Enrollment table testing ------------------------------------------------
get_dupes(Enrollment, EnrollmentID)

View(Enrollment %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))

# Domestic Violence testing -----------------------------------------------
get_dupes(DomesticViolence, DataCollectionStage, EnrollmentID)

View(DomesticViolence %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))


# Enrollment CoC testing --------------------------------------------------
get_dupes(EnrollmentCoC, EnrollmentID, DataCollectionStage)
View(tablename %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))

# EmploymentEducation testing ---------------------------------------------
get_dupes(EmploymentEducation, EnrollmentID, DataCollectionStage)
View(tablename %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))


# Connection w SOAR testing -----------------------------------------------
get_dupes(tablename, EnrollmentID, DataCollectionStage)
View(tablename %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))


# NonCash testing ---------------------------------------------------------
get_dupes(tablename, EnrollmentID, DataCollectionStage)
View(tablename %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))


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


