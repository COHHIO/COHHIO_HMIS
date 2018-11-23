library("tidyverse")
library("lubridate")
client <- c("Mary", "Mary", "Mary", "Mary", "Rex", "Rex", "Jennifer", "Jennifer", "Jennifer", "Jennifer", "Jennifer", "Jennifer", "Hannah", "Hannah", "Hannah", "Hannah", "Hannah", "Hannah", "Lydia", "Lydia")
enrollment_id <- c(1552, 1552, 4828, 4828, 3831, 3171, 3336, 3336, 3336, 2554, 2554, 2554, 1778, 1778, 1131, 1131, 4062, 4062, 3199, 3199)
entry <- c("2016-01-01","2016-01-01","2018-01-01","2018-01-01","2017-05-01","2018-03-01","2018-02-01","2018-02-01","2018-02-01","2018-03-15","2018-03-15","2018-03-15","2018-01-01","2018-01-01","2016-09-01","2016-09-01","2017-01-01","2017-01-01","2018-06-01","2018-06-01")
exit <- c("2018-09-01","2018-09-01","2018-03-01","2018-03-01","2017-06-01","2018-05-01","2018-03-01","2018-03-01","2018-03-01", "2018-05-01", "2018-05-01","2018-05-01","2018-09-01","2018-09-01","2016-10-01","2016-10-01","2017-09-01","2017-09-01","2018-09-01","2018-09-01")
monthly_income  <- c(600, 0, 600, 0, 300, 700, 1400, 200, 1400, 1400, 200, 1400, 1200, 1500, 1200, 1500, 1200, 1500, 1600, 1800)
date_eff  <- c("2013-01-01","2018-01-01","2013-01-01","2018-01-01","2017-05-01","2018-03-01","2016-01-01","2018-02-01","2018-04-01","2016-01-01","2018-02-01","2018-04-01","2016-09-01","2017-09-01","2016-09-01","2017-09-01","2016-09-01","2017-09-01","2018-06-01","2018-09-01")
manual <- c(1, 2, 0, 1, 1, 1, 0, 1, 0, 0, 1, 2, 0, 1, 1, 0, 1, 3, 1, 3)
testdata <- data.frame(client, enrollment_id, entry, exit, monthly_income, date_eff, manual)
rm(client, enrollment_id, entry, exit, monthly_income, date_eff, manual)

# testdata <- testdata %>% filter(client == "Jennifer")
stage1begin <- now()
tmp <- testdata %>%
  group_by(enrollment_id) %>%
  mutate(datacollectionstage1 = max(ymd(date_eff)[ymd(entry) >= ymd(date_eff)]))

stage2begin <- now()
tmp <- tmp %>%
  group_by(enrollment_id) %>% 
  mutate(datacollectionstage2 = max(ymd(date_eff)[ymd(entry) < ymd(date_eff) & 
                                                    ymd(exit) > ymd(date_eff)]))

stage3begin <- now()
tmp <- tmp %>%
  group_by(enrollment_id) %>%
  mutate(datacollectionstage3 = case_when(ymd(date_eff) == ymd(exit) ~ ymd(date_eff)))

putitalltogether <- now()
testdata <- tmp %>%
  mutate(collectionstage = 
           case_when(
             date_eff == datacollectionstage1 ~ 1,
             date_eff == datacollectionstage2 ~ 2,
             date_eff == datacollectionstage3 ~ 3
           ),
         datacollectionstage1 = NULL,
         datacollectionstage2 = NULL,
         datacollectionstage3 = NULL,
         manual = NULL) %>%
  filter(!is.na(collectionstage))
end <- now()
(stage1time <- stage2begin - stage1begin)
(stage2time <- stage3begin - stage2begin)
(stage3time <- putitalltogether - stage3begin)
(alltogether <- end - putitalltogether)

rm(tmp)
# maybe what happens after this is you do this for every data element (or create a function that will do it) so you have
# a table like this for each data element (ugh!) and then you join them all together in the end. is this really the best way?
# comparing dates takes up a lot of processing time.