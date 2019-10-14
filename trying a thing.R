"Correct MoveInDateAdjust so that if the Relationship to HoH is not 1 and the 
Entry Date is not equal to the HoH's Entry Date, then return the Entry Date of 
the non-HoH as the MoveInDateAdjust."

x <- Enrollment %>%
  filter(HouseholdID == "h_141639") %>%
  select(PersonalID, HouseholdID, EntryDate, MoveInDate, RelationshipToHoH, 
         ProjectType, ExitAdjust) %>%
  mutate(EntryDate = if_else(PersonalID == "108665", mdy("06012019"), EntryDate))

HoHsEntry <- x %>%
  filter(RelationshipToHoH == 1) %>%
  select(HouseholdID, "HoHsEntry" = EntryDate)

y <- x %>%
  left_join(HoHsEntry, by = "HouseholdID")

z <- y %>%
  mutate(
    MoveInDateAdjust = case_when(
    EntryDate < mdy("10012017") &
      ProjectType %in% c(3, 9)
    ~ EntryDate,
    EntryDate != HoHsEntry &
      RelationshipToHoH != 1 ~ EntryDate,
    EntryDate >= mdy("10012017") &
      ProjectType %in% c(3, 9) &
      ymd(EntryDate) <= ymd(MoveInDate) &
      ymd(MoveInDate) <= ExitAdjust
    ~ MoveInDate,
    ymd(EntryDate) <= ymd(MoveInDate) &
      ymd(MoveInDate) <= ExitAdjust &
      ProjectType == 13 ~ MoveInDate
  ))
view(z)
