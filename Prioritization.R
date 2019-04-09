library(tidyverse)
library(lubridate)

load("data/COHHIOHMIS.Rdata")

rm(Affiliation, EmploymentEducation, EnrollmentCoC, Export, ProjectCoC, Services)

# run on a geography, not project(s)

Chronicity <- 
  Enrollment %>%
  filter(
    served_between(Enrollment, ReportStart, ReportEnd) &
      ((ymd(DateToStreetESSH) + years(1) <= ymd(EntryDate)) |
      (
        MonthsHomelessPastThreeYears %in% c(112, 113) &
          TimesHomelessPastThreeYears == 4
      )) &
      DisablingCondition == 1
  )
