library(tidyverse)
library(readxl)

x <- read_xlsx("data/dataqualitypointscorrected.xlsx",
               sheet = 1) %>%
  select(1, 6)

y <- read_xlsx("data/dataqualitypointscorrected.xlsx",
               sheet = 2)

z <- y %>% left_join(x, by = c("Project" = "Provider")) %>%
  mutate(DQScoreOLD = if_else(is.na(DQScoreOLD), 0, DQScoreOLD),
         CorrectedPoints = if_else(is.na(Points), 0, Points),
         Points = NULL)
# all projects that don't match old points to new points
scoresNotTheSame <- z %>%
  filter(CorrectedPoints != DQScoreOLD)
# these projects should not lose points as they were adjusted for consolidation
theyLostPoints <- z %>% filter(DQScoreOLD > CorrectedPoints)
# these projects should gain the points (sent to Erica)
theyGainedPoints <- z %>% filter(DQScoreOLD < CorrectedPoints)
