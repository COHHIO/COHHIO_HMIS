# for playing around at home
w <- read_csv("C:\\Users\\laptop\\Documents\\sampledates.csv")
with_tz(ymd_hms(w$Date_Added), "America/New_York")

# trying some deduping with the janitor package:
library("janitor")
race2 %>% get_dupes(PersonalID)
w %>% get_dupes(Client_ID)