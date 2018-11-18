client <- c("Mary", "Mary", "Mary", "Mary", "Rex", "Rex", "Jennifer", "Jennifer", "Jennifer", "Jennifer", "Jennifer", "Jennifer", "Hannah", "Hannah", "Hannah", "Hannah", "Hannah", "Hannah", "Lydia", "Lydia")
enrollmentid <- c(1552, 1552, 4828, 4828, 3831, 3171, 3336, 3336, 3336, 2554, 2554, 2554, 1778, 1778, 1131, 1131, 4062, 4062, 3199, 3199)
entry <- c("1/1/2016","1/1/2016","1/1/2018","1/1/2018","5/1/2017","3/1/2018","2/1/2018","2/1/2018","2/1/2018","3/15/2018","3/15/2018","3/15/2018","1/1/2018","1/1/2018","9/1/2016",  "9/1/2016","1/1/2017","1/1/2017","6/1/2018","6/1/2018")
exit <- c("9/1/2018","9/1/2018","3/1/2018","3/1/2018","6/1/2017","5/1/2018","3/1/2018","3/1/2018","3/1/2018", "5/1/2018", "5/1/2018","5/1/2018","9/1/2018","9/1/2018","10/1/2016","10/1/2016","9/1/2017","9/1/2017","9/1/2018","9/1/2018")
tmi <- c(600, 0, 600, 0, 300, 700, 1400, 200, 1400, 1400, 200, 1400, 1200, 1500, 1200, 1500, 1200, 1500, 1600, 1800)
tmieff <- c("1/1/2013","1/1/2018","1/1/2013","1/1/2018","5/1/2017","3/1/2018","1/1/2016","2/1/2018","4/1/2018","1/1/2016","2/1/2018","4/1/2018","9/1/2016","9/1/2017","9/1/2016","9/1/2017","9/1/2016","9/1/2017","6/1/2018","9/1/2018")
manual <- c(1, 2,  0, 1, 1, 1, 0, 1, 0, 0, 1, 2, 0, 1, 1, 0, 1, 3, 1, 3)
testdata <- data.frame(client, enrollmentid, entry, exit, tmi, tmieff, manual)
rm(client, enrollmentid, entry, exit, tmi, tmieff, manual)

# testdata <- testdata %>% filter(client == "Jennifer")

tmp <- testdata %>% 
  group_by(enrollmentid) %>%
  mutate(datacollectionstage1 = max(mdy(tmieff)[mdy(entry) >= mdy(tmieff)]))
tmp <- tmp %>%
  group_by(enrollmentid) %>% 
  mutate(datacollectionstage2 = max(mdy(tmieff)[mdy(entry) < mdy(tmieff) & mdy(exit) > mdy(tmieff)]))
tmp <- tmp %>%
  group_by(enrollmentid) %>%
  mutate(datacollectionstage3 = case_when(mdy(tmieff) == mdy(exit) ~ mdy(tmieff)))

tmp <- tmp %>%
  mutate(collectionstage = 
           case_when(
             mdy(tmieff) == ymd(datacollectionstage1) ~ 1,
             mdy(tmieff) == ymd(datacollectionstage2) ~ 2,
             mdy(tmieff) == ymd(datacollectionstage3) ~ 3
           ),
         datacollectionstage1 = NULL,
         datacollectionstage2 = NULL,
         datacollectionstage3 = NULL)
