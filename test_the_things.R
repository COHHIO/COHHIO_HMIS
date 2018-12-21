library(janitor)

# Client table testing ----------------------------------------------------
get_dupes(Client, PersonalID)

View(Client %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))
# Client table is all good


# Enrollment table testing ------------------------------------------------
get_dupes(Enrollment, EnrollmentID)

View(Enrollment %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))

# Domestic Violence testing -----------------------------------------------
get_dupes(DomesticViolence, DataCollectionStage, EnrollmentID)

View(DomesticViolence %>% filter(PersonalID %in% c(105108, 54017, 188869, 192304, 144707)))


# Enrollment CoC testing --------------------------------------------------


# check that the subassessment dates are all only ymd and make necessary corrections
# can't get the hopwa psh funding source to flip to its number.
