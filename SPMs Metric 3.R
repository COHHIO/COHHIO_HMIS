library(tidyverse)
library(lubridate)
library(readxl)

Metric5_2017 <- read_xlsx("data/0704_2017.xlsx", sheet = 4, range = "B12:F400") %>%
  select(Provider2017 = 1, ProjectType = 3, Transactions2017 = 4, Clients2017 = 5) %>%
  filter(ProjectType %in% c("ES", "TH", "SH", "PSH", "RRH", "PH-H")) %>% 
  arrange(Provider2017)

Metric5_2018 <- read_xlsx("data/0704_2018.xlsx", sheet = 4, range = "B12:F400") %>%
  select(Provider2018 = 1, ProjectType = 3, Transactions2018 = 4, Clients2018 = 5) %>%
  filter(ProjectType %in% c("ES", "TH", "SH", "PSH", "RRH", "PH-H")) %>% 
  arrange(Provider2018)

both <- Metric5_2017 %>%
  full_join(Metric5_2018,
            by = c("Provider2017" = "Provider2018", "ProjectType"))

both[is.na(both)] <- 0

both <- both %>% select(1:3, 5, 4, 6) %>%
  mutate(
    Clients2018 = as.numeric(Clients2018),
    Clients2017 = as.numeric(Clients2017),
    Transactions2017 = as.numeric(Transactions2017),
    Transactions2018 = as.numeric(Transactions2018),
    ClientDiffs = Clients2018 - Clients2017,
         TransactionDiffs = Transactions2018 - Transactions2017)

write_csv(both, "data/Differences.csv")
