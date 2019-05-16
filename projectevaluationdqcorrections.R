library(tidyverse)
library(readxl)

x <- read_xlsx("data/dataqualitypointscorrected.xlsx",
               sheet = 1) %>%
  select(1, 6)

y <- read_xlsx("data/dataqualitypointscorrected.xlsx",
               sheet = 2)

z <- y %>% left_join(x, by = c("Project" = "Provider")) %>%
  mutate(DQScoreOLD = if_else(is.na(DQScoreOLD), 0, DQScoreOLD),
         Points = if_else(is.na(Points), 0, Points))

a <- z %>%
  filter(Points != DQScoreOLD)

b <- z %>% filter(DQScoreOLD > Points)

c <- z %>% filter(DQScoreOLD < Points)
