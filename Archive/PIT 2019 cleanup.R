# COHHIO_HMIS
# Copyright (C) 2019  Coalition on Homelessness and Housing in Ohio (COHHIO)
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details at 
#<https://www.gnu.org/licenses/>.

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
