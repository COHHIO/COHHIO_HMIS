library(tidyverse)
library(readxl)

# getting the Recurrence data for Project Evaluation
# run the ART report called Recurrence for Export in the Project Evaluation>2017
# remove the spaces and apostrophes out of the headers

recurrence <- read_xlsx("data/Recurrence.xlsx", sheet = 1)
served <- read_xlsx("data/Recurrence.xlsx", sheet = 3)

recurrence <-
  recurrence %>% filter(SecondStaysPTC %in% c("ES", "TH", "SH") &
                          !ProjectType %in% c("Emergency Shelter (HUD)")) %>%
  mutate(
    ProjectType = case_when(
      ProjectType == "Transitional housing (HUD)" ~ 2,
      ProjectType == "PH - Rapid Re-Housing (HUD)" ~ 13,
      ProjectType == "PH - Permanent Supportive Housing (disability required for entry) (HUD)" ~ 3,
      ProjectType == "PH - Housing only (HUD)" ~ 9,
      ProjectType == "Safe Haven (HUD)" ~ 8
    ),
    DaysBetweenStays = case_when(
      DaysBetweenStays == "Between 6-24 months" ~ 2,
      DaysBetweenStays == "Within 6 months" ~ 1
    )
  ) %>%
  group_by(Provider, ProjectType, DaysBetweenStays) %>%
  summarise(recurred = n()) %>%
  ungroup()

recurrence <- recurrence %>% spread(DaysBetweenStays, recurred) %>%
  rename("UpToSixMonths" = `1`, "UpTo2yrs" = `2`)

served <- served %>%
  filter(!`Project Type Code` == "Emergency Shelter (HUD)") %>%
  select(Provider, PermanentExits = ExitsPermanent)

recurrence <- left_join(recurrence, served, by = "Provider") %>%
  mutate(
    percent0to6 = UpToSixMonths / PermanentExits,
    percent2yrs = UpTo2yrs / PermanentExits,
    points0to6 = case_when(
      # TH & RRH
      ProjectType %in% c(13, 2) &
        percent0to6 <= .07 ~ 10,
      ProjectType %in% c(13, 2) &
        percent0to6 <= .09 &
        percent0to6 > .07 ~ 7.5,
      ProjectType %in% c(13, 2) &
        percent0to6 <= .12 &
        percent0to6 > .09 ~ 5,
      ProjectType %in% c(13, 2) &
        percent0to6 > .12 ~ 0,
      # PSH
      ProjectType %in% c(3, 9) &
        percent0to6 <= .02 ~ 10,
      ProjectType %in% c(3, 9) &
        percent0to6 <= .04 &
        percent0to6 > .02 ~ 7.5,
      ProjectType %in% c(3, 9) &
        percent0to6 <= .06 &
        percent0to6 > .04 ~ 5,
      ProjectType %in% c(3, 9) &
        percent0to6 > .06 ~ 0,
      # SH
      ProjectType == 8 &
        percent0to6 <= .15 ~ 10,
      ProjectType == 8 &
        percent0to6 <= .17 &
        percent0to6 > .15 ~ 7.5,
      ProjectType == 8 &
        percent0to6 <= .19 &
        percent0to6 > .17 ~ 5,
      ProjectType == 8 &
        percent0to6 > .19 ~ 0
    ),
    points2yrs = case_when(
      # TH & RRH
      ProjectType %in% c(13, 2) &
        percent2yrs <= .12 ~ 10,
      ProjectType %in% c(13, 2) &
        percent2yrs <= .14 &
        percent2yrs > .12 ~ 7.5,
      ProjectType %in% c(13, 2) &
        percent2yrs <= .17 &
        percent2yrs > .14 ~ 5,
      ProjectType %in% c(13, 2) &
        percent2yrs > .17 ~ 0,
      # PSH
      ProjectType %in% c(3, 9) &
        percent2yrs <= .05 ~ 10,
      ProjectType %in% c(3, 9) &
        percent2yrs <= .08 &
        percent2yrs > .05 ~ 7.5,
      ProjectType %in% c(3, 9) &
        percent2yrs <= .09 &
        percent2yrs > .08 ~ 5,
      ProjectType %in% c(3, 9) &
        percent2yrs > .09 ~ 0,
      # SH
      ProjectType == 8 &
        percent2yrs <= .2 ~ 10,
      ProjectType == 8 &
        percent2yrs <= .22 &
        percent2yrs > .2 ~ 7.5,
      ProjectType == 8 &
        percent2yrs <= .24 &
        percent2yrs > .22 ~ 5,
      ProjectType == 8 &
        percent2yrs > .24 ~ 0
    )
  ) 



write_csv(recurrence, "data/RecurrenceProjectEvaluation.csv")
