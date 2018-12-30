library(janitor)
# check that the subassessment dates are all only ymd and make necessary corrections
# can't get the hopwa psh funding source to flip to its number.
# maybe look at calculating Data Collection Stage by using Intervals like you're
  # doing for NCBs.
# consider filtering Connection with SOAR data to only pull in PATH and RHY 
  # Enrollment IDs

# Client table testing -------------------ok-------------------------------
get_dupes(Client, PersonalID)

View(Client %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))
summary(Client)

# Enrollment table testing ---------------ok-------------------------------
get_dupes(Enrollment, EnrollmentID)

View(Enrollment %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))

# Domestic Violence testing --------------ok-------------------------------
get_dupes(DomesticViolence, DataCollectionStage, EnrollmentID)

View(DomesticViolence %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))

# Enrollment CoC testing -----------------ok-------------------------------
get_dupes(EnrollmentCoC, EnrollmentID, DataCollectionStage)
View(EnrollmentCoC %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))

# EmploymentEducation testing ------------ok-------------------------------
get_dupes(EmploymentEducation, EnrollmentID, DataCollectionStage)
View(EmploymentEducation %>% filter(PersonalID %in% c(81184, 187291, 56556, 177390, 191280)))

# Connection w SOAR testing --------------ok-------------------------------
get_dupes(ConnectionWithSOAR, EnrollmentID, DataCollectionStage)
View(ConnectionWithSOAR %>% filter(PersonalID %in% c(219218, 130592, 196671, 20228, 193399)))

# NonCash testing ---------------------------------------------------------
get_dupes(NonCashBenefits, EnrollmentID, DataCollectionStage, BenefitsFromAnySource, NoncashSource)
View(NonCashBenefits %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))

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


