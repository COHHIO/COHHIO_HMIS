start <- now()
the_assessment_questions <- select(assessment_data, DataElement) %>% unique()
x <- select(Enrollment, MonthsHomelessPastThreeYears) %>% unique()
#rm(hitypes)
# this can be used in pairing EEs to assessment data.
small_enrollment <- Enrollment %>% select(EnrollmentID, PersonalID, HouseholdID, EntryDate, ExitDate, ExitAdjust)
small_enrollment <- setDT(small_enrollment)
setDT(assessment_data)
# subs only store ymd type dates, so we only want to compare them to the same
# type of date when we're joining the assessment's y/n data back to the subs data
sub_enrollment <- small_enrollment %>%
  mutate(EntryDate = as.Date(EntryDate, "%Y-%m-%d", tz = "America/New_York"),
         ExitDate = as.Date(ExitDate, "%Y-%m-%d", tz = "America/New_York"),
         ExitAdjust = as.Date(ExitAdjust, "%Y-%m-%d", tz = "America/New_York"))
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

# function to pull assessment data into Enrollment table
enrollment_level_value <- function(dataelement) {
  relevant_data <- assessment_data %>% filter(DataElement == dataelement)
  relevant_data <- relevant_data %>%
    group_by(PersonalID, DateEffective) %>%
    filter(DateAdded == max(DateAdded))
  x <- left_join(small_enrollment, relevant_data, by = "PersonalID") 
  x <- setDT(x)[,.SD[which.max(DateEffective[EntryDate >= DateEffective])], 
                keyby = EnrollmentID]
  Enrollment <- left_join(Enrollment, x, 
                          by = c("PersonalID", "EnrollmentID", "EntryDate", "ExitDate", "HouseholdID", "ExitAdjust")) %>%
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

Enrollment <- mutate(
  Enrollment,
  RelationshipToHoH = case_when(
    RelationshipToHoH == "self (head of household)" ~ 1,
    RelationshipToHoH == "head of household's child" ~ 2,
    RelationshipToHoH == "head of household's spouse or partner" ~ 3,
    RelationshipToHoH == "head of household's other relation member (other relation to head of household)" ~ 4,
    RelationshipToHoH == "other: non-relation member" ~ 5,
    RelationshipToHoH == "data not collected" |
      RelationshipToHoH == "" |
      is.na(RelationshipToHoH) ~ 99
  ),
  LivingSituation = case_when(
    LivingSituation == "emergency shelter, including hotel or motel paid for with emergency shelter voucher (hud)" ~ 1,
    LivingSituation == "transitional housing for homeless persons (including homeless youth) (hud)" ~ 2,
    LivingSituation == "permanent housing (other than rrh) for formerly homeless persons (hud)" ~ 3,
    LivingSituation == "psychiatric hospital or other psychiatric facility (hud)" ~ 4,
    LivingSituation == "substance abuse treatment facility or detox center (hud)" ~ 5,
    LivingSituation == "hospital or other residential non-psychiatric medical facility (hud)" ~ 6,
    LivingSituation == "jail, prison or juvenile detention facility (hud)" ~ 7,
    LivingSituation == "client doesn't know (hud)" ~ 8,
    LivingSituation == "client refused (hud)" ~ 9,
    LivingSituation == "staying or living in a family member's room, apartment or house (hud)" ~ 12,
    LivingSituation == " staying or living in a friend's room, apartment or house (hud)" ~ 13,
    LivingSituation == "hotel or motel paid for without emergency shelter voucher (hud)" ~ 14,
    LivingSituation == "foster care home or foster care group home (hud)" ~ 15,
    LivingSituation == "place not meant for habitation (hud)" ~ 16,
    LivingSituation == "safe haven (hud)" ~ 18,
    LivingSituation == "rental by client, with vash subsidy (hud)" ~ 19,
    LivingSituation == "rental by client, with other ongoing housing subsidy (including rrh) (hud)" ~ 20,
    LivingSituation == "owned by client, with ongoing housing subsidy (hud)" ~ 21,
    LivingSituation == "rental by client, no ongoing housing subsidy (hud)" ~ 22,
    LivingSituation == "owned by client, no ongoing housing subsidy (hud)" ~ 23,
    LivingSituation == "long-term care facility or nursing home (hud)" ~ 24,
    LivingSituation == "rental by client, with gpd tip subsidy (hud)" ~ 25,
    LivingSituation == "residential project or halfway house with no homeless criteria (hud)" ~ 26,
    LivingSituation == "interim housing" ~ 27,
    LivingSituation == "data not collected (hud)"|
      LivingSituation == "" |
      is.na(LivingSituation) ~ 99
  ),
  LengthOfStay = case_when(
    LengthOfStay == "one week or more, but less than one month" ~ 2,
    LengthOfStay == "one month or more, but less than 90 days" ~ 3,
    LengthOfStay == "90 days or more, but less than one year" ~ 4,
    LengthOfStay == "one year or longer (hud)" ~ 5,
    LengthOfStay == "client doesn't know (hud)" ~ 8,
    LengthOfStay == "client refused (hud)" ~ 9,
    LengthOfStay == "one night or less" ~ 10,
    LengthOfStay == "two to six nights" ~ 11,
    LengthOfStay == "data not collected (hud)" |
      LengthOfStay == "" |
      is.na(LengthOfStay) ~ 99
  ),
  LOSUnderThreshold = case_when(
    LoS90d == "true" & 
      PreviousStreetESSH == "true" & 
      (LoS7d == "true" | LoS7d == "") ~ 1,
    LoS90d == "true" & 
      PreviousStreetESSH == "false" & 
      (LoS7d == "true" | LoS7d == "") ~ 0,    
    LoS90d == "false" & 
      (LoS7d == "false" | LoS7d == "") ~ 0,
    LoS7d == "true" & 
      PreviousStreetESSH == "false" & 
      (LoS7d == "true" | LoS7d == "") ~ 0,    
    LoS7d == "true" & 
      PreviousStreetESSH == "true" & 
      (LoS90d == "true" | LoS90d == "") ~ 1,
    LoS7d == "false" & 
      (LoS90d == "false" | LoS90d == "") ~ 0,
    (LoS7d == "true" | LoS90d == "true") &
      PreviousStreetESSH == "" ~ 99
  ),
  TimesHomelessPastThreeYears = case_when(
    TimesHomelessPastThreeYears == "one time (hud)" ~ 1,
    TimesHomelessPastThreeYears == "two times (hud)" ~ 2,
    TimesHomelessPastThreeYears == "three times (hud)" ~ 3,
    TimesHomelessPastThreeYears == "four or more times (hud)" ~ 4,
    TimesHomelessPastThreeYears == "client doesn't know (hud)" ~ 8,
    TimesHomelessPastThreeYears == "client refused (hud)" ~ 9,
    TimesHomelessPastThreeYears == "data not collected (hud)" |
      TimesHomelessPastThreeYears == "" |
      is.na(TimesHomelessPastThreeYears) ~ 99
  ),
  MonthsHomelessPastThreeYears = case_when(
    MonthsHomelessPastThreeYears == "client doesn't know (hud)" ~ 8,
    MonthsHomelessPastThreeYears == "client refused (hud)" ~ 9,
    MonthsHomelessPastThreeYears == "data not collected (hud)" |
      MonthsHomelessPastThreeYears == "" |
      is.na(MonthsHomelessPastThreeYears) ~ 99,
    MonthsHomelessPastThreeYears == "one month (this time is the first month) (hud)" ~ 101,
    MonthsHomelessPastThreeYears == "2" ~ 102,
    MonthsHomelessPastThreeYears == "3" ~ 103,
    MonthsHomelessPastThreeYears == "4" ~ 104,
    MonthsHomelessPastThreeYears == "5" ~ 105,
    MonthsHomelessPastThreeYears == "6" ~ 106,
    MonthsHomelessPastThreeYears == "7" ~ 107,
    MonthsHomelessPastThreeYears == "8" ~ 108,
    MonthsHomelessPastThreeYears == "9" ~ 109,
    MonthsHomelessPastThreeYears == "10" ~ 110,
    MonthsHomelessPastThreeYears == "11" ~ 111,
    MonthsHomelessPastThreeYears == "12" ~ 112,
    MonthsHomelessPastThreeYears == "more than 12 months (hud)" ~ 113
  ),
  LoS7d = NULL,
  LoS90d = NULL,
  PreviousStreetESSH = NULL
)
Enrollment <- Enrollment[, c(1, 3, 5:7, 4, 8:9, 11, 10, 13:17, 21, 18:20, 2, 12)]
# All Data Collection Stages ----------------------------------------------
# this pulls a data element out of the assessment_data table into data from the
# small_enrollment table and outputs that data along with Data Collection Stage
all_the_stages <- function(dataelement) {
  x <- filter(assessment_data, DataElement == dataelement)
  x <- x %>% group_by(PersonalID, DateEffective) %>%
    filter(DateAdded == max(DateAdded)) %>%
    select(-DataElement)
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
  rename(BenefitsFromAnySource = Value) %>%
  mutate(BenefitsFromAnySource = case_when(
    BenefitsFromAnySource == "yes (hud)" ~ 1,
    BenefitsFromAnySource == "no (hud)" ~ 0,
    BenefitsFromAnySource == "client doesn't know (hud)" ~ 8,
    BenefitsFromAnySource == "client refused (hud)" ~ 9,
    BenefitsFromAnySource == "data not collected (hud)" |
      is.na(BenefitsFromAnySource) ~ 99
  ))
# 
# NCByn <- left_join(NCByn, sub_enrollment, by = c("EnrollmentID", "PersonalID", "HouseholdID"))
# 
# noncash2 <- mutate(
#   noncash,
#   SNAP = if_else(
#     NoncashSource == "supplemental nutrition assistance program (food stamps) (hud)",
#     1,
#     0
#   ),
#   WIC = if_else(
#     NoncashSource == "special supplemental nutrition program for wic (hud)", 
#     1, 
#     0
#     ),
#   TANFChildCare = if_else(NoncashSource == "tanf child care services (hud)", 1, 0),
#   TANFTransportation = if_else(NoncashSource == "tanf transportation services (hud)", 1, 0),
#   OtherTANF = if_else(NoncashSource == "other tanf-funded services (hud)", 1, 0),
#   OtherSource = if_else(NoncashSource == "other source (hud)", 1, 0),
#   NoncashSource = NULL
# )
noncash2 <-
  left_join(noncash, sub_enrollment, by = "PersonalID") %>%
  mutate(
    NoncashSource = case_when(
      NoncashSource == "supplemental nutrition assistance program (food stamps) (hud)" ~ 3,
      NoncashSource == "special supplemental nutrition program for wic (hud)" ~ 4,
      NoncashSource == "other source (hud)" ~ 9,
      NoncashSource == "tanf child care services (hud)" ~ 5,
      NoncashSource == "tanf transportation services (hud)" ~ 6,
      NoncashSource == "other tanf-funded services (hud)" ~ 7
    ),
    NoncashStartDate = ymd(NoncashStartDate),
    NoncashEndDate = ymd(NoncashEndDate),
    NoncashEndDateAdjust = if_else(is.na(NoncashEndDate), today(), NoncashEndDate),
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
NonCashBenefits <- NonCashBenefits[, c(1, 6:10, 12:13, 3:5, 11)]
# NonCashBenefits <- NonCashBenefits %>% group_by(PersonalID, EnrollmentID, DataCollectionStage) %>%
#   summarise(
#     SNAP = sum(SNAP),
#     WIC = sum(WIC),
#     TANFChildCare = sum(TANFChildCare),
#     TANFTransportation = sum(TANFTransportation),
#     OtherTANF = sum(OtherTANF),
#     OtherSource = sum(OtherSource)
#   )
# Data collection stage 3 shows as NA if nothing changed since Stage 1 or 2.
rm(noncash2, NCByn)
end <- now()
end - start
# Disabilities ------------------------------------------------------------

disability2 <-
  left_join(Disabilities, sub_enrollment, by = "PersonalID") %>%
  mutate(
    DisabilityStartDate = ymd(DisabilityStartDate),
    DisabilityEndDate = ymd(DisabilityEndDate),
    DisabilityEndDateAdjust = if_else(is.na(DisabilityEndDate), today(), DisabilityEndDate),
    inproject = interval(EntryDate, ExitAdjust),
    disabilityactive = interval(DisabilityStartDate, DisabilityEndDateAdjust),
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
    disabilityactive = NULL
  ) 
disability2 <- filter(disability2, !is.na(DataCollectionStage))
# at some point you'll just want this to be named Disability
Disabilities <- disability2[, c(2, 1, 7:8, 13, 5:6)]
rm(disability2)
# Health Insurance --------------------------------------------------------
Insuranceyn <- all_the_stages("hud_coveredbyhlthins") %>% 
  rename(InsuranceFromAnySource = Value)
Insuranceyn <- left_join(Insuranceyn,
                         sub_enrollment,
                         by = c("EnrollmentID", "PersonalID", "HouseholdID")) %>%
  mutate(InsuranceFromAnySource = case_when(
    InsuranceFromAnySource == "yes (hud)" ~ 1,
    InsuranceFromAnySource == "no (hud)" ~ 0,
    InsuranceFromAnySource == "client doesn't know (hud)" ~ 8,
    InsuranceFromAnySource == "client refused (hud)" ~ 9,
    InsuranceFromAnySource == "data not collected (hud)" |
      is.na(InsuranceFromAnySource) ~ 99
  ))
hi <- mutate(
  health_insurance,
  Medicaid = if_else(
    HealthInsuranceType == "medicaid", 1, 0),
  Medicare = if_else(
    HealthInsuranceType == "medicare", 1, 0),
  SCHIP = if_else(
    HealthInsuranceType == "state children's health insurance program", 1, 0),
  VAMedicalServices = if_else(
    HealthInsuranceType == "veteran's administration (va) medical services",  1, 0),
  EmployerProvided = if_else(
    HealthInsuranceType == "employer - provided health insurance", 1, 0),
  COBRA = if_else(
    HealthInsuranceType == "health insurance obtained through cobra", 1, 0),
  PrivatePay = if_else(
    HealthInsuranceType == "private pay health insurance", 1, 0),
  StateHealthInsAdults = if_else(
    HealthInsuranceType == "state health insurance for adults", 1, 0),
  IndianHealthServices = if_else(
    HealthInsuranceType == "indian health services program", 1, 0),
  OtherInsurance = if_else(
    HealthInsuranceType == "other", 1, 0),
  HealthInsuranceType = NULL
)
hi <-
  left_join(hi, sub_enrollment, by = "PersonalID") %>%
  mutate(
    HealthInsuranceStartDate = ymd(HealthInsuranceStartDate),
    HealthInsuranceEndDate = ymd(HealthInsuranceEndDate),
    HealthInsuranceEndDateAdjust = if_else(is.na(HealthInsuranceEndDate), today(), HealthInsuranceEndDate),
    inproject = interval(EntryDate, ExitAdjust),
    insuranceactive = interval(HealthInsuranceStartDate, HealthInsuranceEndDateAdjust),
    DataCollectionStage = if_else(
      int_overlaps(insuranceactive, inproject),
      case_when(
        HealthInsuranceStartDate <= EntryDate ~ 1,
        HealthInsuranceStartDate > EntryDate &
          HealthInsuranceStartDate < ExitAdjust ~ 2,
        HealthInsuranceStartDate == ExitDate ~ 3
      ),
      NULL
    ),
    inproject = NULL,
    insuranceactive = NULL
  ) 
hi <- filter(hi, !is.na(DataCollectionStage))
HealthInsurance <- full_join(Insuranceyn, hi, by = c(
  "PersonalID", "EnrollmentID", "HouseholdID", "DataCollectionStage", "EntryDate",
  "ExitDate", "ExitAdjust"
)) 
HealthInsurance <- HealthInsurance[, c(2, 1, 3, 5, 4, 12:21)]
rm(hi, Insuranceyn)
# Income and Sources ------------------------------------------------------
Incomeyn <- all_the_stages("svp_anysource30dayincome") %>% 
  rename(IncomeFromAnySource = Value) %>%
  mutate(IncomeFromAnySource = case_when(
    IncomeFromAnySource == "yes (hud)" ~ 1,
    IncomeFromAnySource == "no (hud)" ~ 0,
    IncomeFromAnySource == "client doesn't know (hud)" ~ 8,
    IncomeFromAnySource == "client refused (hud)" ~ 9,
    IncomeFromAnySource == "data not collected (hud)" |
      is.na(IncomeFromAnySource) ~ 99
  ))

TMI <- all_the_stages("hud_totalmonthlyincome") %>%
  rename(TotalMonthlyIncome = Value)

Incomeyn <-
  left_join(Incomeyn,
            sub_enrollment,
            by = c("EnrollmentID", "PersonalID", "HouseholdID")) %>%
  left_join(.,
            TMI,
            by = c(
              "EnrollmentID",
              "PersonalID",
              "HouseholdID",
              "DataCollectionStage"
            ))

incomesubs <- mutate(
  IncomeBenefits,
  Earned = if_else(
    IncomeSource == "earned income (hud)", 1, 0),
  EarnedAmount = if_else(
    IncomeSource == "earned income (hud)", IncomeAmount, NULL),
  Unemployment = if_else(
    IncomeSource == "unemployment insurance (hud)", 1, 0),
  UnemploymentAmount = if_else(
    IncomeSource == "unemployment insurance (hud)", IncomeAmount, NULL),
  SSI = if_else(
    IncomeSource == "ssi (hud)", 1, 0),
  SSIAmount = if_else(
    IncomeSource == "ssi (hud)", IncomeAmount, NULL),  
  SSDI = if_else(
    IncomeSource == "ssdi (hud)", 1, 0),
  SSDIAmount = if_else(
    IncomeSource == "ssdi (hud)", IncomeAmount, NULL),  
  VADisabilityService = if_else(
    IncomeSource == "va service connected disability compensation (hud)", 1, 0),
  VADisabilityServiceAmount = if_else(
    IncomeSource == "va service connected disability compensation (hud)", IncomeAmount, NULL),  
  VADisabilityNonService = if_else(
    IncomeSource == "va non-service connected disability compensation (hud)", 1, 0),
  VADisabilityNonServiceAmount = if_else(
    IncomeSource == "va non-service connected disability compensation (hud)", IncomeAmount, NULL),  
  PrivateDisability = if_else(
    IncomeSource == "private disability insurance (hud)", 1, 0),
  PrivateDisabilityAmount = if_else(
    IncomeSource == "private disability insurance (hud)", IncomeAmount, NULL),  
  WorkersComp = if_else(
    IncomeSource == "worker's compensation (hud)", 1, 0),
  WorkersCompAmount = if_else(
    IncomeSource == "worker's compensation (hud)", IncomeAmount, NULL),  
  TANF = if_else(
    IncomeSource == "tanf (hud)", 1, 0),
  TANFAmount = if_else(
    IncomeSource == "tanf (hud)", IncomeAmount, NULL),  
  GA = if_else(
    IncomeSource == "general assistance (hud)", 1, 0),
  GAAmount = if_else(
    IncomeSource == "general assistance (hud)", IncomeAmount, NULL),  
  SocSecRetirement = if_else(
    IncomeSource == "retirement income from social security (hud)", 1, 0),
  SocSecRetirementAmount = if_else(
    IncomeSource == "retirement income from social security (hud)", IncomeAmount, NULL),  
  Pension = if_else(
    IncomeSource == "pension or retirement income from another job (hud)", 1, 0),
  PensionAmount = if_else(
    IncomeSource == "pension or retirement income from another job (hud)", IncomeAmount, NULL),  
  ChildSupport = if_else(
    IncomeSource == "child support (hud)", 1, 0),
  ChildSupportAmount = if_else(
    IncomeSource == "child support (hud)", IncomeAmount, NULL),  
  Alimony = if_else(
    IncomeSource == "alimony (hud)", 1, 0),
  AlimonyAmount = if_else(
    IncomeSource == "alimony (hud)", IncomeAmount, NULL),  
  OtherIncomeSource = if_else(
    IncomeSource == "other (hud)", 1, 0),
  OtherIncomeSourceAmount = if_else(
    IncomeSource == "other (hud)", IncomeAmount, NULL),
  IncomeSource = NULL,
  IncomeAmount = NULL
)
incomesubs <-
  left_join(incomesubs, sub_enrollment, by = "PersonalID") %>%
  mutate(
    IncomeStart = ymd(IncomeStart),
    IncomeEnd = ymd(IncomeEnd),
    IncomeEndAdjust = if_else(is.na(IncomeEnd), today(), IncomeEnd),
    inproject = interval(EntryDate, ExitAdjust),
    insuranceactive = interval(IncomeStart, IncomeEndAdjust),
    DataCollectionStage = if_else(
      int_overlaps(insuranceactive, inproject),
      case_when(
        IncomeStart <= EntryDate ~ 1,
        IncomeStart > EntryDate &
          IncomeStart < ExitAdjust ~ 2,
        IncomeStart == ExitDate ~ 3
      ),
      NULL
    ),
    inproject = NULL,
    insuranceactive = NULL
  ) 
incomesubs <- filter(incomesubs, !is.na(DataCollectionStage))
Income <- full_join(Incomeyn, incomesubs, by = c(
  "PersonalID", "EnrollmentID", "HouseholdID", "DataCollectionStage", "EntryDate",
  "ExitDate", "ExitAdjust"
)) 
Income <- Income[, c(2, 1, 3, 5, 4, 9, 13:42)]
test <- Income %>% 
  select(PersonalID, EnrollmentID, DataCollectionStage, Unemployment, UnemploymentAmount, TotalMonthlyIncome) %>%  
  group_by(PersonalID, EnrollmentID, DataCollectionStage) %>%
  summarise(
    # Earned = sum(Earned),
    # EarnedAmount = sum(EarnedAmount),
    Unemployment = sum(Unemployment),
    UnemploymentAmount = sum(UnemploymentAmount)#,
    # SSI = sum(SSI),
    # SSIAmount = sum(SSIAmount),  
    # SSDI = sum(SSDI),
    # SSDIAmount = sum(SSDIAmount),  
    # VADisabilityService = sum(VADisabilityService),
    # VADisabilityServiceAmount = sum(VADisabilityServiceAmount),  
    # VADisabilityNonService = sum(VADisabilityNonService),
    # VADisabilityNonServiceAmount = sum(VADisabilityNonServiceAmount),  
    # PrivateDisability = sum(PrivateDisability),
    # PrivateDisabilityAmount = sum(PrivateDisabilityAmount),  
    # WorkersComp = sum(WorkersComp),
    # WorkersCompAmount = sum(WorkersCompAmount),  
    # TANF = sum(TANF),
    # TANFAmount = sum(TANFAmount),  
    # GA = sum(GA),
    # GAAmount = sum(GAAmount),  
    # SocSecRetirement = sum(SocSecRetirement),
    # SocSecRetirementAmount = sum(SocSecRetirementAmount),  
    # Pension = sum(Pension),
    # PensionAmount = sum(PensionAmount),  
    # ChildSupport = sum(ChildSupport),
    # ChildSupportAmount = sum(ChildSupportAmount),  
    # Alimony = sum(Alimony),
    # AlimonyAmount = sum(AlimonyAmount),  
    # OtherIncomeSource = sum(OtherIncomeSource),
    # OtherIncomeSourceAmount = sum(OtherIncomeSourceAmount)
  )
rm(incomesubs, Incomeyn)


# Move In Date ------------------------------------------------------------
MoveInDate <- all_the_stages("hud_housingmoveindate") %>% 
  rename(MoveInDate = Value)

# Contacts ----------------------------------------------------------------


# Referrals Services ------------------------------------------------------


# Exit RHY ----------------------------------------------------------------


# EntryRHSP ---------------------------------------------------------------


# EntryRHY ----------------------------------------------------------------


# SPDAT Scores ------------------------------------------------------------


