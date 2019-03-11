library(janitor)
library(skimr)
library(tidylog)

# Client table testing ----------------------------------------------------
get_dupes(Client, PersonalID)

View(Client %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))
summary(Client)

skim(Client)

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


