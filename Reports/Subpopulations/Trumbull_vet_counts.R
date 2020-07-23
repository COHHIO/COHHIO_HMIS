
# something for Sister Jean
library(tidyverse)
library(lubridate)

load("images/Veterans.RData")
load("images/QPR_EEs.RData")

counts <- VeteranHHs %>%
  filter(served_between(., "01012019", "12312019") &
           ProjectType %in% c(1, 2, 3, 4, 8, 13) &
           CountyServed == "Trumbull") %>%
  select(PersonalID) %>%
  unique() %>%
  count()

becoming <- VeteranHHs %>%
  filter(CountyServed == "Trumbull" &
           ((
             entered_between(., "01012019", "12312019") &
               ProjectType %in% c(1, 2, 3, 4, 8, 13)
           ) |
             (
               exited_between(., "01012019", "12312019") &
                 Destination %in% c(1, 16, 18)
             )
           )) %>%
  select(PersonalID) %>%
  unique() %>%
  count()

exiting <- VeteranHHs %>%
  filter(
    CountyServed == "Trumbull" &
      exited_between(., "01012019", "12312019") &
      Destination %in% c(3, 10, 11, 19:23, 28, 31, 33, 34, 36)
  ) %>% 
  select(PersonalID) %>%
  unique() %>%
  count()

days <- qpr_leavers %>%
  right_join(counts, by = "PersonalID") %>%
  mutate(ExitAdjust = if_else(ymd(ExitAdjust) > ymd("12312019"),
                              ymd("12312019")) %>%
           dayshomeless = if_else(ProjectType %in% c(1, 2, 8),
                                  difftime(ExitAdjust, EntryDate, units = "days"),
                                  difftime(ExitAdjust, MoveInDateAdjust)))
