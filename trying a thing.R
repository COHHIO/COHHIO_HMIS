"Correct MoveInDateAdjust so that if the Relationship to HoH is not 1 and the 
Entry Date is not equal to the HoH's Entry Date, then return the Entry Date of 
the non-HoH as the MoveInDateAdjust."

library(tidyverse)
library(janitor)
library(lubridate)

load("images/COHHIOHMIS.RData")

# x <- Enrollment %>%
#   filter(HouseholdID == "h_141639") %>%
#   select(PersonalID, HouseholdID, EntryDate, MoveInDate, RelationshipToHoH, 
#          ProjectType, ExitAdjust) %>%
#   mutate(EntryDate = if_else(PersonalID == "108665", mdy("06012019"), EntryDate))

HoHsEntry <- Enrollment %>%
  filter(RelationshipToHoH == 1) %>%
  select(HouseholdID, "HoHsEntry" = EntryDate) %>%
  unique()

y <- Enrollment %>%
  left_join(HoHsEntry, by = "HouseholdID")

z <- y %>%
  mutate(
    MoveInDateAdjust = case_when(
      EntryDate < mdy("10012017") &
        ProjectType %in% c(3, 9)
      ~ EntryDate,
      EntryDate != HoHsEntry &
        ProjectType %in% c(3, 9, 13) ~ EntryDate,
      EntryDate >= mdy("10012017") &
        ProjectType %in% c(3, 9) &
        ymd(EntryDate) <= ymd(MoveInDate) &
        ymd(MoveInDate) <= ExitAdjust
      ~ MoveInDate,
      ymd(EntryDate) <= ymd(MoveInDate) &
        ymd(MoveInDate) <= ExitAdjust &
        ProjectType == 13 ~ MoveInDate
    ),
    Different = if_else(HoHsEntry != EntryDate, 1, 0)
  ) %>% select(
    PersonalID,
    HouseholdID,
    Different,
    EntryDate,
    HoHsEntry,
    MoveInDate,
    MoveInDateAdjust,
    ExitAdjust,
    RelationshipToHoH,
    ProjectType
  )

count(HoHsEntry, HouseholdID) %>% view()
