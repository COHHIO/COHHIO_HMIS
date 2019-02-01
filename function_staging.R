x <- filter(assessment_data, DataElement == "hud_extentofdv")
x <- x %>% group_by(PersonalID, DateEffective) %>%
  filter(DateAdded == max(DateAdded)) %>%
  select(-DataElement, -DateAdded)
x <- left_join(x, small_enrollment, by = "PersonalID") 
x <- as.data.frame(x)
x <-
  mutate(
    x,
    projectstay = interval(ymd_hms(EntryDate), ymd_hms(ExitAdjust)),
    responseduringproject = ymd_hms(DateEffective) %within% projectstay,
    projectstay = NULL)
x <- group_by(x, EnrollmentID) %>%
  mutate(
    mostrecentresponsesinceentry = max(DateEffective[EntryDate >= DateEffective]),
    mostrecentresponseduringstay = max(DateEffective[ExitAdjust >= DateEffective]),
    DataCollectionStage = if_else(
      responseduringproject == TRUE,
      case_when(
        EntryDate == DateEffective ~ 1,
        DateEffective > EntryDate &
          DateEffective < ExitAdjust &
          (InterimType == "update" |
             is.na(InterimType)) ~ 2,
        ExitDate == DateEffective ~ 3,
        DateEffective > EntryDate &
          DateEffective < ExitAdjust &
          InterimType == "annual assessment" ~ 5
      ),
      NULL
    ),
    DataCollectionStage = if_else(
      responseduringproject == FALSE & is.na(DataCollectionStage),
      case_when(mostrecentresponsesinceentry == DateEffective ~ 1),
      DataCollectionStage
    )
  ) %>%
  select(-responseduringproject,-mostrecentresponsesinceentry)
x <- filter(x, !is.na(DataCollectionStage) 
) 
x <- setDT(x, key = c("EnrollmentID", "DataCollectionStage"))
