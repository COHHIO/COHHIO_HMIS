the_assessment_questions <- select(assessment_data, DataElement) %>% unique()

# ONE answer per CLIENt ---------------------------------------------------
client_level_value <- function(dataelement) {
  relevant_data <- filter(assessment_data, DataElement == dataelement)
  no_dupes <- relevant_data %>%
    group_by(PersonalID) %>%
    summarise(max(DateEffective), max(DateAdded))
  x <- semi_join(relevant_data, no_dupes,
                 by = c("PersonalID", "DateAdded" = "max(DateAdded)",
                        "DateEffective" = "max(DateEffective)"))
  Client <- left_join(Client, relevant_data, by = "PersonalID") %>%
    select(-DataElement, -DateEffective, -DateAdded) 
  return(Client)
}
Client <- client_level_value("svpprofdob") 
Client <- Client %>% mutate(DOB = strftime(ymd_hms(Value), "%F"), Value = NULL)
Client <- client_level_value("svpprofdobtype") %>% rename(DOBDataQuality = Value)
Client <- client_level_value("svpprofeth") %>% rename(Ethnicity = Value)
Client <- client_level_value("hud_disablingcondition") %>% rename(DisablingCondition = Value)

race <-
  assessment_data %>% filter(
    DataElement %in% c("svpprofrace", "svpprofsecondaryrace") &
    Value != "" &
      (DataElement == "svpprofrace" |
    (DataElement == "svpprofsecondaryrace" &
        !(Value %in% c("client doesn't know (hud)", 
                       "client refused (hud)", 
                       "data not collected (hud)"))
    ))
  ) 
x <- race %>% group_by(PersonalID, Value) %>%  # eliminates the two identical races saved problem
  summarise(max(DateEffective), max(DateAdded)) # only keeps the most recent record
race <- mutate(x,
  AmIndAKNative = if_else(Value == "american indian or alaska native (hud)", 1, 0),
  Asian = if_else(Value == "asian (hud)", 1, 0),
  BlackAfAmerican = if_else(Value == "black or african american (hud)", 1, 0),
  NativeHIOtherPacific = if_else(Value == "native hawaiian or other pacific islander (hud)", 1, 0),
  White = if_else(Value == "white (hud)", 1, 0),
  DataElement = NULL, Value = NULL, DateEffective = NULL, DateAdded = NULL)
race <- race %>% group_by(PersonalID) %>%
  summarise(
    AmIndAKNative = sum(AmIndAKNative),
    Asian = sum(Asian),
    BlackAfAmerican = sum(BlackAfAmerican),
    NativeHIOtherPacific = sum(NativeHIOtherPacific),
    White = sum(White)
  )
Client <- left_join(Client, race, by = "PersonalID")

# tidy up the Client table
Client <- mutate(
  Client,
  DOBDataQuality = case_when(
    DOBDataQuality == "full dob reported (hud)" ~ 1,
    DOBDataQuality == "approximate or partial dob reported (hud)" ~ 2,
    DOBDataQuality == "client doesn't know (hud)" ~ 8,
    DOBDataQuality == "client refused (hud)" ~ 9,
    is.na(DOBDataQuality) |
      DOBDataQuality == "data not collected (hud)" ~ 99
  ),
  Ethnicity = case_when(
    Ethnicity == "non-hispanic/non-latino (hud)" ~ 0,
    Ethnicity == "hispanic/latino (hud)" ~ 1,
    Ethnicity == "client doesn't know (hud)" ~ 8,
    Ethnicity == "client refused (hud)" ~ 9,
    is.na(Ethnicity) |
      Ethnicity == "" | Ethnicity == "data not collected (hud)" ~ 99
  ),
  DisablingCondition = case_when(
    DisablingCondition == "yes (hud)" ~ 1,
    DisablingCondition == "no (hud)" ~ 2,
    DisablingCondition == "client doesn't know (hud)" ~ 8,
    DisablingCondition == "client refused (hud)" ~ 9,
    DisablingCondition == "data not collected (hud)" |
      is.na(DisablingCondition) ~ 99
  ),
  RaceNone = if_else(
    AmIndAKNative + Asian + BlackAfAmerican + NativeHIOtherPacific + White == 0,
    1,
    0
  ),
  DOB = ymd(DOB)
)
rm(race, x)
# ONE answer per ENROLLMENT -----------------------------------------------
setDT(assessment_data)
small_enrollment <- Enrollment %>% select(EnrollmentID, PersonalID, HouseholdID, EntryDate, ExitDate, ExitAdjust)
small_enrollment <- setDT(small_enrollment)
# function to pull assessment data into Enrollment table
enrollment_level_value <- function(dataelement) {
  relevant_data <- assessment_data %>% filter(DataElement == dataelement)
  no_dupes <- relevant_data %>%
    group_by(PersonalID, DateEffective) %>%
    summarise(DateAdded = max(DateAdded))
  relevant_data <- semi_join(relevant_data, no_dupes, by = c("PersonalID", "DateAdded"))
  x <- left_join(small_enrollment, relevant_data, by = "PersonalID") 
  x <- setDT(x)[,.SD[which.max(DateEffective[EntryDate >= DateEffective])], 
                keyby = EnrollmentID]
  Enrollment <- left_join(Enrollment, x, 
                          by = c("PersonalID", "EnrollmentID", "EntryDate", "ExitDate")) %>%
    select(-DataElement, -DateEffective, -DateAdded) 

  return(Enrollment)
}

Enrollment <- enrollment_level_value("hud_relationtohoh") %>% rename(RelationshipToHoH = Value)
Enrollment <- enrollment_level_value("typeoflivingsituation") %>% rename(LivingSituation = Value)
Enrollment <- enrollment_level_value("hud_lengthofstay") %>% rename(LengthOfStay = Value)
Enrollment <- enrollment_level_value("hud_lengthstay_less90days") %>% rename(LoS90d = Value)
Enrollment <- enrollment_level_value("hud_lengthstay_less7nights") %>% rename(LoS7d = Value)
Enrollment <- enrollment_level_value("hud_nightbeforestreetessh") %>% rename(PreviousStreetESSH = Value)
Enrollment <- enrollment_level_value("hud_homelessstartdate") %>% rename(DateToStreetESSH = Value)
Enrollment <- enrollment_level_value("hud_numberoftimestreetessh") %>% rename(TimesHomelessPastThreeYears = Value)
Enrollment <- enrollment_level_value("hud_nomonthstreetesshin3yrs") %>% rename(MonthsHomelessPastThreeYears = Value)

# All Data Collection Stages ----------------------------------------------


all_the_stages <- function(dataelement) {
  x <- filter(assessment_data, DataElement == dataelement)
  x <- x %>% group_by(PersonalID, Value, DateEffective) %>%
    summarise(DateAdded = max(DateAdded))
  x <- left_join(small_enrollment, x, by = "PersonalID") %>%
    group_by(EnrollmentID) %>%
    mutate(
      datacollectionstage1 = max(DateEffective[EntryDate >= DateEffective]),
      datacollectionstage2 = max(DateEffective[EntryDate < DateEffective &
                                                 ExitAdjust > DateEffective]),
      datacollectionstage3 = case_when(DateEffective == ExitDate ~ DateEffective)
    )
  x <- x %>% mutate(
    DataCollectionStage = 
      case_when(
        DateEffective == datacollectionstage1 ~ 1,
        DateEffective == datacollectionstage2 ~ 2,
        DateEffective == datacollectionstage3 ~ 3
      ),
    datacollectionstage1 = NULL,
    datacollectionstage2 = NULL,
    datacollectionstage3 = NULL,
    EntryDate = NULL,
    ExitDate = NULL,
    ExitAdjust = NULL,
    DateAdded = NULL,
    DateEffective = NULL
  ) %>%
    filter(!is.na(DataCollectionStage)
  )
  return(x)
}

# Domestic Violence -------------------------------------------------------

DomesticViolence1 <- all_the_stages("domesticviolencevictim") %>% 
  rename(DomesticViolenceVictim = Value)
DomesticViolence2 <- all_the_stages("hud_extentofdv") %>% 
  rename(WhenOccurred = Value)
DomesticViolence3 <- all_the_stages("hud_extenttofdv2") %>% 
  rename(CurrentlyFleeing = Value)
DomesticViolence <-
  left_join(DomesticViolence1, DomesticViolence2, 
            by = c("EnrollmentID", "PersonalID", "HouseholdID", "DataCollectionStage")
  ) %>%
  left_join(., DomesticViolence3,
    by = c("EnrollmentID", "PersonalID", "HouseholdID", "DataCollectionStage")
  )
rm(DomesticViolence1, DomesticViolence2, DomesticViolence3)

# EnrollmentCoC -----------------------------------------------------------

EnrollmentCoC <- all_the_stages("hud_cocclientlocation") %>% 
  rename(EnrollmentCoC = Value)

# Employment Education ----------------------------------------------------

LastGrade <- all_the_stages("hud_cocclientlocation") %>% 
  rename(LastGradeCompleted = Value)
SchoolStatus <- all_the_stages("hud_cocclientlocation") %>% 
  rename(SchoolStatus = Value)
Employed <- all_the_stages("hud_cocclientlocation") %>% 
  rename(Employed = Value)
EmploymentType <- all_the_stages("hud_cocclientlocation") %>% 
  rename(EmploymentType = Value)
NotEmployedReason <- all_the_stages("hud_cocclientlocation") %>% 
  rename(NotEmployedReason = Value)
EmploymentEducation <- left_join(
  LastGrade,
  SchoolStatus,
  by = c(
    "EnrollmentID",
    "PersonalID",
    "HouseholdID",
    "DataCollectionStage"
  )
) %>%
  left_join(
    .,
    Employed,
    by = c(
      "EnrollmentID",
      "PersonalID",
      "HouseholdID",
      "DataCollectionStage"
    )
  ) %>%
  left_join(
    .,
    EmploymentType,
    by = c(
      "EnrollmentID",
      "PersonalID",
      "HouseholdID",
      "DataCollectionStage"
    )
  ) %>%
  left_join(
    .,
    NotEmployedReason,
    by = c(
      "EnrollmentID",
      "PersonalID",
      "HouseholdID",
      "DataCollectionStage"
    )
  )
rm(LastGrade, SchoolStatus, Employed, EmploymentType, NotEmployedReason)

# Connection w SOAR -------------------------------------------------------
ConnectionWithSOAR <- all_the_stages("dataelementname") %>% 
  rename(ConnectionWithSOAR = Value)

# Non Cash ----------------------------------------------------------------
NCByn <- all_the_stages("dataelementname") %>% 
  rename(BenefitsFromAnySource = Value)

# Disabilities ------------------------------------------------------------


# Health Insurance --------------------------------------------------------
Insuranceyn <- all_the_stages("dataelementname") %>% 
  rename(InsuranceFromAnySource = Value)

# HealthStatus ------------------------------------------------------------
table <- all_the_stages("dataelementname") %>% 
  rename(variablename = Value)

# MedicalAssistance -------------------------------------------------------
table <- all_the_stages("dataelementname") %>% 
  rename(variablename = Value)

# Income and Sources ------------------------------------------------------
table <- all_the_stages("dataelementname") %>% 
  rename(variablename = Value)

# Move In Date ------------------------------------------------------------
table <- all_the_stages("dataelementname") %>% 
  rename(variablename = Value)

# Contacts ----------------------------------------------------------------


# Referrals Services ------------------------------------------------------


# Exit RHY ----------------------------------------------------------------


# EntryRHSP ---------------------------------------------------------------


# EntryRHY ----------------------------------------------------------------


