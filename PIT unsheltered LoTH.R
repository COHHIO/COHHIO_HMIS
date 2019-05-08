library(tidyverse)
library(lubridate)

load("data/COHHIOHMIS.RData")

unshelteredLoTH <- Enrollment %>%
  select(EnrollmentID, 
         PersonalID, 
         EntryDate, 
         ProjectID,
         UserCreating,
         LivingSituation, 
         LengthOfStay,
         LOSUnderThreshold, 
         PreviousStreetESSH, 
         DateToStreetESSH, 
         TimesHomelessPastThreeYears,
         MonthsHomelessPastThreeYears) %>%
  filter(ProjectID == 1695 &
           UserCreating %in% c("Mulryan, Erica/@COHHIO(1239)",
                               "Basting, Hannah/@COHHIO(1563)",
                               "Sechang, Sandy/@COHHIO(1426)",
                               "Walton, Valerie/@COHHIO(1628)",
                               "Hoffman, Carolyn/@COHHIO(1624)") &
           ymd(EntryDate) > mdy(01012019))

