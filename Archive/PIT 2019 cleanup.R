library(tidyverse)
library(lubridate)
load("data/COHHIOHMIS.RData")

smallEnrollment <- Enrollment %>%
  filter(served_between(Enrollment, "01222019", "01232019")) %>%
  select(EnrollmentID, PersonalID, ProjectID, EntryDate, ExitDate, UserCreating)

smallEnrollment <-
  left_join(smallEnrollment, Project, by = "ProjectID") %>%
  filter(ProjectType == 1) %>%
  select(PersonalID, EnrollmentID, ProjectID, ProjectName, EntryDate, ExitDate, UserCreating)

longStayers <- smallEnrollment %>%
  filter(ymd(EntryDate) < mdy("07222018"))

write_csv(longStayers, "data/longstayers.csv")
