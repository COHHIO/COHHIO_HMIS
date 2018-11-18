library("tidyverse")
library("lubridate")
client <- c("Mary", "Mary", "Mary", "Mary", "Rex", "Rex", "Jennifer", "Jennifer", "Jennifer", "Jennifer", "Jennifer", "Jennifer", "Hannah", "Hannah", "Hannah", "Hannah", "Hannah", "Hannah", "Lydia", "Lydia")
enrollment_id <- c(1552, 1552, 4828, 4828, 3831, 3171, 3336, 3336, 3336, 2554, 2554, 2554, 1778, 1778, 1131, 1131, 4062, 4062, 3199, 3199)
entry <- c("1/1/2016","1/1/2016","1/1/2018","1/1/2018","5/1/2017","3/1/2018","2/1/2018","2/1/2018","2/1/2018","3/15/2018","3/15/2018","3/15/2018","1/1/2018","1/1/2018","9/1/2016",  "9/1/2016","1/1/2017","1/1/2017","6/1/2018","6/1/2018")
exit <- c("9/1/2018","9/1/2018","3/1/2018","3/1/2018","6/1/2017","5/1/2018","3/1/2018","3/1/2018","3/1/2018", "5/1/2018", "5/1/2018","5/1/2018","9/1/2018","9/1/2018","10/1/2016","10/1/2016","9/1/2017","9/1/2017","9/1/2018","9/1/2018")
monthly_income  <- c(600, 0, 600, 0, 300, 700, 1400, 200, 1400, 1400, 200, 1400, 1200, 1500, 1200, 1500, 1200, 1500, 1600, 1800)
date_eff  <- c("1/1/2013","1/1/2018","1/1/2013","1/1/2018","5/1/2017","3/1/2018","1/1/2016","2/1/2018","4/1/2018","1/1/2016","2/1/2018","4/1/2018","9/1/2016","9/1/2017","9/1/2016","9/1/2017","9/1/2016","9/1/2017","6/1/2018","9/1/2018")
manual <- c(1, 2, 0, 1, 1, 1, 0, 1, 0, 0, 1, 2, 0, 1, 1, 0, 1, 3, 1, 3)
testdata <- data.frame(client, enrollment_id, entry, exit, monthly_income, date_eff, manual)
rm(client, enrollment_id, entry, exit, monthly_income, date_eff, manual)

# testdata <- testdata %>% filter(client == "Jennifer")

tmp <- testdata %>%
  group_by(enrollment_id) %>%
  mutate(datacollectionstage1 = max(mdy(date_eff)[mdy(entry) >= mdy(date_eff)]))
tmp <- tmp %>%
  group_by(enrollment_id) %>% 
  mutate(datacollectionstage2 = max(mdy(date_eff)[mdy(entry) < mdy(date_eff) & 
                                                              mdy(exit) > mdy(date_eff)]))
tmp <- tmp %>%
  group_by(enrollment_id) %>%
  mutate(datacollectionstage3 = case_when(mdy(date_eff) == mdy(exit) ~ mdy(date_eff)))

testdata <- tmp %>%
  mutate(collectionstage = 
           case_when(
             mdy(date_eff) == ymd(datacollectionstage1) ~ 1,
             mdy(date_eff) == ymd(datacollectionstage2) ~ 2,
             mdy(date_eff) == ymd(datacollectionstage3) ~ 3
           ),
         datacollectionstage1 = NULL,
         datacollectionstage2 = NULL,
         datacollectionstage3 = NULL,
         manual = NULL) %>%
  filter(!is.na(collectionstage))
rm(tmp)
# maybe what happens after this is you do this for every data element (or create a function that will do it) so you have
# a table like this for each data element (ugh!) and then you join them all together in the end. is this really the best way?