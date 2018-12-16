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
# this pulls a data element out of the assessment_data table into data from the
# small_enrollment table and outputs that data along with Data Collection Stage
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
DomesticViolence <- DomesticViolence[, c(2, 1, 3, 5, 4, 6:7)]
# EnrollmentCoC -----------------------------------------------------------

EnrollmentCoC <- all_the_stages("hud_cocclientlocation") %>% 
  rename(EnrollmentCoC = Value)
EnrollmentCoC <- EnrollmentCoC[, c(2, 1, 3:5)]

# Employment Education ----------------------------------------------------

LastGrade <- all_the_stages("rhymislastgradecompleted") %>% 
  rename(LastGradeCompleted = Value)
SchoolStatus <- all_the_stages("rhymisschoolstatus") %>% 
  rename(SchoolStatus = Value)
Employed <- all_the_stages("hud_employed") %>% 
  rename(Employed = Value)
EmploymentType <- all_the_stages("hud_employmenttype") %>% 
  rename(EmploymentType = Value)
NotEmployedReason <- all_the_stages("hud_noemployreason") %>% 
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
EmploymentEducation <- EmploymentEducation[, c(2, 1, 3, 5, 4, 6:9)]
# Connection w SOAR -------------------------------------------------------
ConnectionWithSOAR <- all_the_stages("hud_connectwithsoar") %>% 
  rename(ConnectionWithSOAR = Value)

# Non Cash ----------------------------------------------------------------
NCByn <- all_the_stages("svp_anysource30daynoncash") %>% 
  rename(BenefitsFromAnySource = Value)
NCByn <- left_join(NCByn, small_enrollment, by = c(
  "EnrollmentID", "PersonalID", "HouseholdID"
))
NCByn <-
  mutate(
    NCByn,
    EntryDate = as.Date(EntryDate, "%Y-%m-%d", tz = "America/New_York"),
    ExitDate = as.Date(ExitDate, "%Y-%m-%d", tz = "America/New_York"),
    ExitAdjust = as.Date(ExitAdjust, "%Y-%m-%d", tz = "America/New_York")
  )

noncash2 <- mutate(
  noncash,
  SNAP = if_else(
    NoncashSource == "supplemental nutrition assistance program (food stamps) (hud)",
    1,
    0
  ),
  WIC = if_else(
    NoncashSource == "special supplemental nutrition program for wic (hud)", 
    1, 
    0
    ),
  TANFChildCare = if_else(NoncashSource == "tanf child care services (hud)", 1, 0),
  TANFTransportation = if_else(NoncashSource == "tanf transportation services (hud)", 1, 0),
  OtherTANF = if_else(NoncashSource == "other tanf-funded services (hud)", 1, 0),
  OtherSource = if_else(NoncashSource == "other source (hud)", 1, 0),
  NoncashSource = NULL
)
noncash2 <-
  left_join(noncash2, small_enrollment, by = "PersonalID") %>%
  mutate(
    EntryDate = format.Date(EntryDate, "%Y-%m-%d"),
    ExitDate = format.Date(ExitDate, "%Y-%m-%d"),
    ExitAdjust = format.Date(ExitAdjust, "%Y-%m-%d"),
    NoncashStartDate = ymd(NoncashStartDate),
    NoncashEndDate = ymd(NoncashEndDate),
    NoncashEndDateAdjust = if_else(is.na(NoncashEndDate), today(), NoncashEndDate),
    EntryDate = ymd(EntryDate),
    ExitDate = ymd(ExitDate),
    ExitAdjust = ymd(ExitAdjust),
    inproject = interval(EntryDate, ExitAdjust),
    benefitactive = interval(NoncashStartDate, NoncashEndDateAdjust),
    DataCollectionStage = if_else(
      int_overlaps(benefitactive, inproject),
      case_when(
        NoncashStartDate <= EntryDate ~ 1,
        NoncashStartDate > EntryDate &
          NoncashStartDate < ExitAdjust ~ 2,
        NoncashStartDate == ExitDate ~ 3
      ),
      NULL
    ),
    inproject = NULL,
    benefitactive = NULL
  ) 
noncash2 <- filter(noncash2, !is.na(DataCollectionStage))
NonCashBenefits <- full_join(noncash2, NCByn, by = c(
  "PersonalID", "EnrollmentID", "HouseholdID", "DataCollectionStage", "EntryDate",
  "ExitDate", "ExitAdjust"
)) 
NonCashBenefits <- NonCashBenefits[, c(1, 11:15, 17:18, 5:10)]
rm(noncash2, NCByn)
# Disabilities ------------------------------------------------------------

disability2 <-
  left_join(Disability, small_enrollment, by = "PersonalID") %>%
  mutate(
    EntryDate = format.Date(EntryDate, "%Y-%m-%d"),
    ExitDate = format.Date(ExitDate, "%Y-%m-%d"),
    ExitAdjust = format.Date(ExitAdjust, "%Y-%m-%d"),
    DisabilityStartDate = ymd(NoncashStartDate),
    DisabilityEndDate = ymd(NoncashEndDate),
    DisabilityEndDateAdjust = if_else(is.na(DisabilityEndDate), today(), DisabilityEndDate),
    EntryDate = ymd(EntryDate),
    ExitDate = ymd(ExitDate),
    ExitAdjust = ymd(ExitAdjust),
    inproject = interval(EntryDate, ExitAdjust),
    disabilityactive = interval(NoncashStartDate, NoncashEndDateAdjust),
    DataCollectionStage = if_else(
      int_overlaps(disabilityactive, inproject),
      case_when(
        DisabilityStartDate <= EntryDate ~ 1,
        DisabilityStartDate > EntryDate &
          DisabilityStartDate < ExitAdjust ~ 2,
        DisabilityStartDate == ExitDate ~ 3
      ),
      NULL
    ),
    inproject = NULL,
    benefitactive = NULL
  ) 
disability2 <- filter(disability2, !is.na(DataCollectionStage))
# at some point you'll just want this to be named Disability
Disability <- Disability[, c("order of the columns")]

# Health Insurance --------------------------------------------------------
Insuranceyn <- all_the_stages("hud_coveredbyhlthins") %>% 
  rename(InsuranceFromAnySource = Value)

# Income and Sources ------------------------------------------------------
Incomeyn <- all_the_stages("svp_anysource30dayincome") %>% 
  rename(variablename = Value)

# Move In Date ------------------------------------------------------------
MoveInDate <- all_the_stages("hud_housingmoveindate") %>% 
  rename(MoveInDate = Value)

# Contacts ----------------------------------------------------------------


# Referrals Services ------------------------------------------------------


# Exit RHY ----------------------------------------------------------------


# EntryRHSP ---------------------------------------------------------------


# EntryRHY ----------------------------------------------------------------


# SPDAT Scores ------------------------------------------------------------


