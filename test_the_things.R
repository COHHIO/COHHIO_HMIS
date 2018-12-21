library(janitor)
get_dupes(Client, PersonalID)

get_dupes(Enrollment, EnrollmentID)

# i don't understand why this doesn't work.
get_dupes(DomesticViolence, EnrollmentID, DataCollectionStage)

# check that the subassessment dates are all only ymd and make necessary corrections

