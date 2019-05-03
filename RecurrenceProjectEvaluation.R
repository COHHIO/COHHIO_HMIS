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
  summarise(recurred = n())

served <- served %>%
  filter(!`Project Type Code` == "Emergency Shelter (HUD)") %>%
  select(Provider, PermanentExits = ExitsPermanent)

recurrence <- left_join(recurrence, served, by = "Provider") %>%
  mutate(
    percent = recurred / PermanentExits,
    points = case_when(
# TH & RRH
      ProjectType %in% c(13, 2) &
        DaysBetweenStays == 1 &
        percent <= .07 ~ 10,
      ProjectType %in% c(13, 2) &
        DaysBetweenStays == 1 &
        percent <= .09 &
        percent > .07 ~ 7.5,
      ProjectType %in% c(13, 2) &
        DaysBetweenStays == 1 &
        percent <= .12 &
        percent > .09 ~ 5,
      ProjectType %in% c(13, 2) &
        DaysBetweenStays == 1 &
        percent > .12 ~ 0,
      ProjectType %in% c(13, 2) &
        DaysBetweenStays == 2 &
        percent <= .12 ~ 10,
      ProjectType %in% c(13, 2) &
        DaysBetweenStays == 2 &
        percent <= .14 &
        percent > .12 ~ 7.5,
      ProjectType %in% c(13, 2) &
        DaysBetweenStays == 2 &
        percent <= .17 &
        percent > .14 ~ 5,
      ProjectType %in% c(13, 2) &
        DaysBetweenStays == 2 &
        percent > .17 ~ 0,
# PSH
      ProjectType %in% c(3, 9) &
        DaysBetweenStays == 1 &
        percent <= .02 ~ 10,
      ProjectType %in% c(3, 9) &
        DaysBetweenStays == 1 &
        percent <= .04 &
        percent > .02 ~ 7.5,
      ProjectType %in% c(3, 9) &
        DaysBetweenStays == 1 &
        percent <= .06 &
        percent > .04 ~ 5,
      ProjectType %in% c(3, 9) &
        DaysBetweenStays == 1 &
        percent > .06 ~ 0,
      ProjectType %in% c(3, 9) &
        DaysBetweenStays == 2 &
        percent <= .05 ~ 10,
      ProjectType %in% c(3, 9) &
        DaysBetweenStays == 2 &
        percent <= .08 &
        percent > .05 ~ 7.5,
      ProjectType %in% c(3, 9) &
        DaysBetweenStays == 2 &
        percent <= .09 &
        percent > .08 ~ 5,
      ProjectType %in% c(3, 9) &
        DaysBetweenStays == 2 &
        percent > .09 ~ 0,
# SH
      ProjectType == 8 &
        DaysBetweenStays == 2 &
        percent <= .2 ~ 10,
      ProjectType == 8 &
        DaysBetweenStays == 2 &
        percent <= .22 &
        percent > .2 ~ 7.5,
      ProjectType == 8 &
        DaysBetweenStays == 2 &
        percent <= .24 &
        percent > .22 ~ 5,
      ProjectType == 8 &
        DaysBetweenStays == 2 &
        percent > .24 ~ 0,
      ProjectType == 8 &
        DaysBetweenStays == 1 &
        percent <= .15 ~ 10,
      ProjectType == 8 &
        DaysBetweenStays == 1 &
        percent <= .17 &
        percent > .15 ~ 7.5,
      ProjectType == 8 &
        DaysBetweenStays == 1 &
        percent <= .19 &
        percent > .17 ~ 5,
      ProjectType == 8 &
        DaysBetweenStays == 1 &
        percent > .19 ~ 0
    )
  )

write_csv(recurrence, "data/RecurrenceProjectEvaluation.csv")
