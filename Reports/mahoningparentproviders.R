library(readxl)
library(lubridate)

a <- read_csv("data/mahoningdataparentproviders.csv")

summary_mahoning <- a %>%
  group_by(Provider) %>%
  summarise(
    minEntry = min(mdy(EntryDate)),
    maxEntry = max(mdy(EntryDate)),
    maxExit = max(mdy(ExitDate)),
    clients = n()
  )

