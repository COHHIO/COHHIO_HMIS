# COHHIO_HMIS
# Copyright (C) 2019  Coalition on Homelessness and Housing in Ohio (COHHIO)
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details at 
#<https://www.gnu.org/licenses/>.

# for reference (can be deleted once this is complete)
# the_assessment_questions <- select(assessment_data, DataElement) %>% unique()
# picklist_values <- EmploymentEducation %>% select(NotEmployedReason) %>% unique()

# smaller dataset used in pairing EEs to assessment data.
small_enrollment <-
  full_join(Enrollment,
            interims,
            by = c("EnrollmentID", "PersonalID")) %>%
  select(EnrollmentID, 
         PersonalID, 
         HouseholdID, 
         ProjectID, 
         EntryDate, 
         ExitDate, 
         ExitAdjust, 
         InterimDate, 
         InterimType)

# ONE answer per CLIENt ---------------------------------------------------
# function that pulls client-level data from assessments df and adds it to the 
  # Client table based on the MOST RECENT answer
client_level_value <- function(dataelement) {
  relevant_data <- filter(assessment_data, DataElement == dataelement)
  no_dupes <- relevant_data %>%
    group_by(PersonalID) %>%
    filter(DateEffective == max(DateEffective), DateAdded == max(DateAdded))
  x <- semi_join(relevant_data, no_dupes,
                 by = c("PersonalID", "DateAdded", "DateEffective"))
  Client <- left_join(Client, x, by = "PersonalID") %>%
    select(-DataElement, -DateEffective, -DateAdded) 
  return(Client)
} 
# applying this function to each client-level data element to add it to Clients
Client <- client_level_value("svpprofdob") 
# formatting DOB date
Client <- Client %>% mutate(DOB = strftime(ymd_hms(Value), "%F"), Value = NULL)
# getting back to applying the function to the various client-level data elements
Client <- client_level_value("svpprofdobtype") %>% rename(DOBDataQuality = Value)
Client <- client_level_value("svpprofeth") %>% rename(Ethnicity = Value)
Client <- client_level_value("svpprofgender") %>% rename(Gender = Value)
# the HUD CSV stores Race in a more complicated way so ...
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
# eliminates the two identical races saved problem
x <- race %>% group_by(PersonalID, Value) %>%  
  # only keeps the most recent record
  summarise(max(DateEffective), max(DateAdded)) 
# turns each value to a column
race <- mutate(x,
  AmIndAKNative = 
    if_else(Value == "american indian or alaska native (hud)", 1, 0),
  Asian = 
    if_else(Value == "asian (hud)", 1, 0),
  BlackAfAmerican = 
    if_else(Value == "black or african american (hud)", 1, 0),
  NativeHIOtherPacific = 
    if_else(Value == "native hawaiian or other pacific islander (hud)", 1, 0),
  White = 
    if_else(Value == "white (hud)", 1, 0),
  DataElement = NULL, Value = NULL, DateEffective = NULL, DateAdded = NULL)
# collapses the rows so multiracial Clients will only show on one row
race <- race %>% group_by(PersonalID) %>%
  summarise(
    AmIndAKNative = sum(AmIndAKNative),
    Asian = sum(Asian),
    BlackAfAmerican = sum(BlackAfAmerican),
    NativeHIOtherPacific = sum(NativeHIOtherPacific),
    White = sum(White)
  )
# joining this all to Client
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
      DOBDataQuality == "" | 
      DOBDataQuality == "data not collected (hud)" ~ 99
  ),
  Ethnicity = case_when(
    Ethnicity == "non-hispanic/non-latino (hud)" ~ 0,
    Ethnicity == "hispanic/latino (hud)" ~ 1,
    Ethnicity == "client doesn't know (hud)" ~ 8,
    Ethnicity == "client refused (hud)" ~ 9,
    is.na(Ethnicity) |
      Ethnicity == "" | 
      Ethnicity == "data not collected (hud)" ~ 99
  ),
  RaceNone = if_else(
    AmIndAKNative + Asian + BlackAfAmerican + NativeHIOtherPacific + White == 0,
    1,
    0
  ),
  DOB = ymd(DOB),
  Gender = case_when(
    Gender == "female" ~ 0,
    Gender == "male" ~ 1,
    Gender == "trans female (mtf or male to female)" ~ 2,
    Gender == "trans male (ftm or female to male)" ~ 3,
    Gender == "gender non-conforming (i.e. not exclusively male or female)" ~ 4,
    Gender == "client doesn't know" ~ 8,
    Gender == "client refused" ~ 9,
    Gender == "data not collected" |
      Gender == "" |
      is.na(Gender) ~ 99
  )
)
rm(race, x)
# ONE answer per ENROLLMENT -----------------------------------------------
# function to pull assessment data into Enrollment 
# try piping relevant_data and check if you maybe should use data.table not setDT
enrollment_level_value <- function(dataelement) {
  relevant_data <- assessment_data %>% filter(DataElement == dataelement)
  relevant_data <- relevant_data %>%
    group_by(PersonalID, DateEffective) %>%
    filter(DateAdded == max(DateAdded))
  x <- left_join(small_enrollment, relevant_data, by = "PersonalID") 
  x <- setDT(x)[,.SD[which.max(DateEffective[EntryDate >= DateEffective])], 
                keyby = EnrollmentID]
  x <- select(x, EnrollmentID, Value)
  Enrollment <- left_join(Enrollment, x, 
                          by = "EnrollmentID")  
  return(Enrollment)
}
# applying the function to all the enrollment-level data elements
Enrollment <- enrollment_level_value("hud_relationtohoh") %>% 
  rename(RelationshipToHoH = Value)
Enrollment <- enrollment_level_value("typeoflivingsituation") %>% 
  rename(LivingSituation = Value)
Enrollment <- enrollment_level_value("hud_lengthofstay") %>% 
  rename(LengthOfStay = Value)
Enrollment <- enrollment_level_value("hud_lengthstay_less90days") %>% 
  rename(LoS90d = Value)
Enrollment <- enrollment_level_value("hud_lengthstay_less7nights") %>% 
  rename(LoS7d = Value)
Enrollment <- enrollment_level_value("hud_nightbeforestreetessh") %>% 
  rename(PreviousStreetESSH = Value)
Enrollment <- enrollment_level_value("hud_homelessstartdate") %>% 
  rename(DateToStreetESSH = Value)
Enrollment <- enrollment_level_value("hud_numberoftimestreetessh") %>% 
  rename(TimesHomelessPastThreeYears = Value)
Enrollment <- enrollment_level_value("hud_nomonthstreetesshin3yrs") %>% 
  rename(MonthsHomelessPastThreeYears = Value)
Enrollment <- enrollment_level_value("hud_disablingcondition") %>% 
  rename(DisablingCondition = Value)
# applying HUD CSV specs to the values
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
  DisablingCondition = case_when(
    DisablingCondition == "yes (hud)" ~ 1,
    DisablingCondition == "no (hud)" ~ 0,
    DisablingCondition == "client doesn't know (hud)" ~ 8,
    DisablingCondition == "client refused (hud)" ~ 9,
    DisablingCondition == "data not collected (hud)" |
      DisablingCondition == "" |
      is.na(DisablingCondition) ~ 99
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
  # three data elements decide if LOSUnderThreshold. 
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
  # dumping the three data elements we used to get to LOSUnderThreshold
  LoS7d = NULL,
  LoS90d = NULL,
  PreviousStreetESSH = NULL
)
Enrollment <- Enrollment[, c(1, 3, 5:7, 4, 12, 8:9, 11, 10, 13:14, 18, 15:17, 2)]
# All Data Collection Stages ----------------------------------------------
# this pulls a data element out of the assessment_data table into data from the
  # small_enrollment table and outputs that data along with Data Collection Stage
all_the_stages <- function(dataelement) {
  x <- filter(assessment_data, DataElement == dataelement)
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
      mostrecentresponse = max(DateEffective[EntryDate >= DateEffective]),
      mostrecentresponseduringstay = 
        max(DateEffective[ExitAdjust >= DateEffective]) == DateEffective,
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
        ), NULL),
      DataCollectionStage = if_else(
        responseduringproject == FALSE & is.na(DataCollectionStage),
        case_when(
          mostrecentresponse == DateEffective ~ 1
        ), 
        DataCollectionStage 
      )) %>%
    select(-responseduringproject, -mostrecentresponse)
  x <- filter(x, !is.na(DataCollectionStage) 
  ) 
  x <- setDT(x, key = c("EnrollmentID", "DataCollectionStage"))
  return(x)
}

# Domestic Violence -------------------------------------------------------
# DV data is collected from three data elements
DomesticViolence1 <- all_the_stages("domesticviolencevictim") %>%
  rename(DomesticViolenceVictim = Value) %>%
  mutate(
    DomesticViolenceVictim = case_when(
      DomesticViolenceVictim == "yes (hud)" ~ 1,
      DomesticViolenceVictim == "no (hud)" ~ 0,
      DomesticViolenceVictim == "client doesn't know (hud)" ~ 8,
      DomesticViolenceVictim == "client refused (hud)" ~ 9,
      DomesticViolenceVictim == "data not collected (hud)" |
        DomesticViolenceVictim == "" ~ 99
    ),
    DateEffective = NULL
  )
DomesticViolence2 <- all_the_stages("hud_extentofdv") %>%
  rename(WhenOccurred = Value) %>%
  mutate(
    WhenOccurred = case_when(
      WhenOccurred == "within the past three months (hud)" ~ 1,
      WhenOccurred == "three to six months ago (hud)" ~ 2,
      WhenOccurred == "from six to twelve months ago (hud)" ~ 3,
      WhenOccurred == "more than a year ago (hud)" ~ 4,
      WhenOccurred == "client doesn't know (hud)" ~ 8,
      WhenOccurred == "client refused (hud)" ~ 9,
      WhenOccurred == "data not collected (hud)" |
        WhenOccurred == "" ~ 99
    ),
    DateEffective = NULL
  )
DomesticViolence3 <- all_the_stages("hud_extenttofdv2") %>%
  rename(CurrentlyFleeing = Value) %>%
  mutate(
    CurrentlyFleeing = case_when(
      CurrentlyFleeing == "yes (hud)" ~ 1,
      CurrentlyFleeing == "no (hud)" ~ 0,
      CurrentlyFleeing == "client doesn't know (hud)" ~ 8,
      CurrentlyFleeing == "client refused (hud)" ~ 9,
      CurrentlyFleeing == "data not collected (hud)" |
        CurrentlyFleeing == "" ~ 99
    ),
    DateEffective = NULL
  )

# may want to add logic later to N/A any of the dependent questions based on 
# whether DVVictim == 1

# building a new table based on all EnrollmentIDs represented in all three tables
DV1 <- distinct(DomesticViolence1, EnrollmentID)
DV2 <- distinct(DomesticViolence2, EnrollmentID)
DV3 <- distinct(DomesticViolence3, EnrollmentID)

DV <- full_join(
  DV1,
  DV2,
  by = "EnrollmentID") %>%
  full_join(
    .,
    DV3,
    by = "EnrollmentID")
dfstages <- data.frame(DataCollectionStage = c(1, 2, 3, 5))
DV <- merge(DV, dfstages)
rm(DV1, DV2, DV3, dfstages)

DV <- left_join(DV, small_enrollment, by = "EnrollmentID")

e <- left_join(
  DV,
  DomesticViolence1,
  by = c(
    "PersonalID",
    "EnrollmentID",
    "HouseholdID",
    "ProjectID",
    "EntryDate",
    "ExitDate",
    "ExitAdjust",
    "InterimDate",
    "InterimType",
    "DataCollectionStage"
  )
)

e <- left_join(
  e,
  DomesticViolence2,
  by = c(
    "PersonalID",
    "EnrollmentID",
    "HouseholdID",
    "ProjectID",
    "EntryDate",
    "ExitDate",
    "ExitAdjust",
    "InterimDate",
    "InterimType",
    "DataCollectionStage",
    "mostrecentresponseduringstay"
  )
)
e <- left_join(
  e,
  DomesticViolence3,
  by = c(
    "PersonalID",
    "EnrollmentID",
    "HouseholdID",
    "ProjectID",
    "EntryDate",
    "ExitDate",
    "ExitAdjust",
    "InterimDate",
    "InterimType",
    "DataCollectionStage",
    "mostrecentresponseduringstay"
  )
)
# isolating all the most recent answers in the stay into f
f <- 
  filter(e,
    mostrecentresponseduringstay == TRUE
  ) %>%
  mutate(
    DataCollectionStage = 3
  ) %>%
  select(
    EnrollmentID,
    DataCollectionStage,
    DomesticViolenceVictim,
    WhenOccurred,
    CurrentlyFleeing
  )


DomesticViolence <-
  left_join(e, f, by = c("EnrollmentID",
                         "DataCollectionStage"))
DomesticViolence <- DomesticViolence %>%
  mutate(
         DomesticViolenceVictim.x = if_else(DataCollectionStage == 3, 
                                            DomesticViolenceVictim.y, 
                                            DomesticViolenceVictim.x),
         WhenOccurred.x = if_else(DataCollectionStage == 3, 
                                  WhenOccurred.y, 
                                  WhenOccurred.x),
         CurrentlyFleeing.x = if_else(DataCollectionStage == 3, 
                                      CurrentlyFleeing.y, 
                                      CurrentlyFleeing.x)) %>%
  select(-DomesticViolenceVictim.y, 
         -WhenOccurred.y, 
         -CurrentlyFleeing.y, 
         -mostrecentresponseduringstay) 
DomesticViolence <- DomesticViolence %>%
  rename(DomesticViolenceVictim = DomesticViolenceVictim.x,
         WhenOccurred = WhenOccurred.x,
         CurrentlyFleeing = CurrentlyFleeing.x)

rm(e, f, DV, DomesticViolence1, DomesticViolence2, DomesticViolence3)


# EnrollmentCoC -----------------------------------------------------------
# HUD has this as a separate table even though it seems like an enrollment-level
  # data element to me. I may move this into Enrollment.
EnrollmentCoC <- all_the_stages("hud_cocclientlocation") %>% 
  rename(EnrollmentCoC = Value)
EnrollmentCoC <- EnrollmentCoC[, c(2, 1, 3:5)]

# Employment Education ----------------------------------------------------
# these also seem like enrollment-level data elements to me but I think keeping
  # these separate makes sense as it will rarely if ever be used
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
EmploymentEducation <- full_join(
  LastGrade,
  SchoolStatus,
  by = c(
    "EnrollmentID",
    "PersonalID",
    "HouseholdID",
    "DataCollectionStage"
  )
) %>%
  full_join(
    .,
    Employed,
    by = c(
      "EnrollmentID",
      "PersonalID",
      "HouseholdID",
      "DataCollectionStage"
    )
  ) %>%
  full_join(
    .,
    EmploymentType,
    by = c(
      "EnrollmentID",
      "PersonalID",
      "HouseholdID",
      "DataCollectionStage"
    )
  ) %>%
  full_join(
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
EmploymentEducation <- mutate(
  EmploymentEducation,
  LastGradeCompleted = case_when(
    LastGradeCompleted == "less than grade 5" ~ 1,
    LastGradeCompleted == "grades 5 - 6" ~ 2,
    LastGradeCompleted == "grades 7 - 8" ~ 3,
    LastGradeCompleted == "grades 9 - 11" ~ 4,
    LastGradeCompleted == "grade 12 / high school diploma" ~ 5,
    LastGradeCompleted == "school program does not have grade levels" ~ 6,
    LastGradeCompleted == "ged" ~ 7,
    LastGradeCompleted == "client doesn't know" ~ 8,
    LastGradeCompleted == "client refused" ~ 9,
    LastGradeCompleted == "some college" ~ 10,
    LastGradeCompleted == "associate's degree" ~ 11,
    LastGradeCompleted == "bachelor's degree" ~ 12,
    LastGradeCompleted == "graduate degree" ~ 13,
    LastGradeCompleted == "vocational certification" ~ 14,
    LastGradeCompleted == "data not collected" |
      LastGradeCompleted == "" |
      is.na(LastGradeCompleted) ~ 99
  ),
  SchoolStatus = case_when(
    SchoolStatus == "attending school regularly" ~ 1,
    SchoolStatus == "attending school irregularly" ~ 2,
    SchoolStatus == "suspended" ~ 6,
    SchoolStatus == "dropped out" ~ 5,
    SchoolStatus == "graduated from high school" ~ 3,
    SchoolStatus == "obtained ged" ~ 4,
    SchoolStatus == "expelled" ~ 7,
    SchoolStatus == "client doesn't know" ~ 8,
    SchoolStatus == "client refused" ~ 9,
    SchoolStatus == "data not collected" |
      SchoolStatus == "" |
      is.na(SchoolStatus) ~ 99
  ),
  Employed = case_when(
    Employed == "no (hud)" ~ 0,
    Employed == "yes (hud)" ~ 1,
    Employed == "client doesn't know" ~ 8,
    Employed == "client refused" ~ 9,
    Employed == "data not collected" |
      Employed == "" |
      is.na(Employed) ~ 99
  ),
  EmploymentType = case_when(
    EmploymentType == "full-time" ~ 1,
    EmploymentType == "part-time" ~ 2,
    EmploymentType == "seasonal/sporadic (including day labor)" ~ 3,
    EmploymentType == "client doesn't know" ~ 8,
    EmploymentType == "client refused" ~ 9,
    EmploymentType == "data not collected" |
      EmploymentType == "" |
      is.na(EmploymentType) ~ 99
  ),
  NotEmployedReason = case_when(
    NotEmployedReason == "looking for work" ~ 1,
    NotEmployedReason == "not looking for work" ~ 3,
    NotEmployedReason == "unable to work" ~ 2,
    NotEmployedReason == "client doesn't know" ~ 8,
    NotEmployedReason == "client refused" ~ 9,
    NotEmployedReason == "data not collected" |
      NotEmployedReason == "" |
      is.na(NotEmployedReason) ~ 99
  )
)
# Connection w SOAR -------------------------------------------------------
# this also seems like enrollment-level data to me but I think keeping it 
  # separate makes sense as it will rarely if ever be used
ConnectionWithSOAR <- all_the_stages("hud_connectwithsoar") %>% 
  rename(Connected = Value)
ConnectionWithSOAR <-
  ConnectionWithSOAR %>% mutate(
    Connected = case_when(
      Connected == "yes (hud)" ~ 1,
      Connected == "no (hud)" ~ 0,
      Connected == "client doesn't know" ~ 8,
      Connected == "client refused" ~ 9,
      Connected == "data not collected" |
        Connected == "" |
        is.na(Connected) ~ 99
    )
  )
ConnectionWithSOAR <- ConnectionWithSOAR[, c(2, 1, 3, 5, 4)]

# Non Cash ----------------------------------------------------------------
start <- now()
NCByn <- all_the_stages("svp_anysource30daynoncash") %>%
  rename(BenefitsFromAnySource = Value) %>%
  mutate(
    BenefitsFromAnySource = case_when(
      BenefitsFromAnySource == "yes (hud)" ~ 1,
      BenefitsFromAnySource == "no (hud)" ~ 0,
      BenefitsFromAnySource == "client doesn't know (hud)" ~ 8,
      BenefitsFromAnySource == "client refused (hud)" ~ 9,
      BenefitsFromAnySource == "data not collected (hud)" |
        is.na(BenefitsFromAnySource) ~ 99
    )
  )

NCByn <-
  left_join(NCByn,
            small_enrollment,
            by = c("EnrollmentID", "PersonalID", "HouseholdID")
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
# applying HUD CSV specs and Data Collection Stage thanks to lubridate intervals!
noncash2 <-
  left_join(noncash2, small_enrollment, by = "PersonalID") %>%
  mutate(
    NoncashStartDate = ymd_hms(NoncashStartDate),
    NoncashEndDate = ymd_hms(NoncashEndDate),
    NoncashEndDateAdjust = if_else(is.na(NoncashEndDate), now(), NoncashEndDate),
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
# throwing out the noncash subs that are not being assigned a Data Collection Stage
noncash2 <- filter(noncash2, !is.na(DataCollectionStage))
# adding the data from the subs to the larger NCB data
NonCashBenefits <- full_join(
  noncash2,
  NCByn,
  by = c(
    "PersonalID",
    "EnrollmentID",
    "HouseholdID",
    "DataCollectionStage",
    "EntryDate",
    "ExitDate",
    "ExitAdjust"
  )
)
NonCashBenefits <- NonCashBenefits[, c(1, 11:15, 17:18, 5:10)]
NonCashBenefits <- NonCashBenefits %>% 
  group_by(PersonalID, EnrollmentID, DataCollectionStage, BenefitsFromAnySource) %>%
  summarise(
    SNAP = sum(SNAP),
    WIC = sum(WIC),
    TANFChildCare = sum(TANFChildCare),
    TANFTransportation = sum(TANFTransportation),
    OtherTANF = sum(OtherTANF),
    OtherSource = sum(OtherSource)
  )
rm(noncash2, NCByn)
setDT(NonCashBenefits, key = c("EnrollmentID", "DataCollectionStage"))
# STOP HERE
end <- now()
end - start
# Disabilities ------------------------------------------------------------
# similar to NCBs, but without the y/n since that's going to be in Enrollment
disability2 <-
  left_join(Disabilities, small_enrollment, by = "PersonalID") %>%
  mutate(
    DisabilityStartDate = ymd_hms(DisabilityStartDate),
    DisabilityEndDate = ymd_hms(DisabilityEndDate),
    DisabilityEndDateAdjust = if_else(is.na(DisabilityEndDate), 
                                      now(), 
                                      DisabilityEndDate),
    inproject = interval(EntryDate, ExitAdjust),
    disabilityactive = interval(DisabilityStartDate,
                                DisabilityEndDateAdjust),
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
# main departure from specs is ONLY yes records are included, also no HOPWA data
Disabilities <- disability2[, c(2, 1, 7:8, 13, 5:6)]
rm(disability2)
# Health Insurance --------------------------------------------------------
# getting the y/n from the assessment_Data
Insuranceyn <- all_the_stages("hud_coveredbyhlthins") %>% 
  rename(InsuranceFromAnySource = Value)
# joining the y/n data to the Enrollment data, applying HUD CSV specs to values
Insuranceyn <-
  left_join(Insuranceyn,
            small_enrollment,
            by = c("EnrollmentID", "PersonalID", "HouseholdID")) %>%
  mutate(
    InsuranceFromAnySource = case_when(
      InsuranceFromAnySource == "yes (hud)" ~ 1,
      InsuranceFromAnySource == "no (hud)" ~ 0,
      InsuranceFromAnySource == "client doesn't know (hud)" ~ 8,
      InsuranceFromAnySource == "client refused (hud)" ~ 9,
      InsuranceFromAnySource == "data not collected (hud)" |
        is.na(InsuranceFromAnySource) ~ 99
    )
  )

healthins <- mutate(
  health_insurance,
  HealthInsuranceStartDate = ymd_hms(HealthInsuranceStartDate),
  HealthInsuranceEndDate = ymd_hms(HealthInsuranceEndDate),
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
# joining health insurance sub data so enrollment data with Data Collection Stages
healthins <-
  left_join(healthins, small_enrollment, by = "PersonalID") %>%
  mutate(
    HealthInsuranceEndDateAdjust = if_else(is.na(HealthInsuranceEndDate), 
                                           now(), 
                                           HealthInsuranceEndDate),
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
# throwing out unnecessary subs
healthins <- filter(healthins, !is.na(DataCollectionStage))
# paring down where there is no change between Data Collection Stages
healthins <- 
# smushing into rows
healthins <- healthins %>% 
  group_by(PersonalID, EnrollmentID, HouseholdID, DataCollectionStage) %>%
  summarise(
    Medicaid = sum(Medicaid),
    Medicare = sum(Medicare),
    SCHIP = sum(SCHIP),
    VAMedicalServices = sum(VAMedicalServices),
    EmployerProvided = sum(EmployerProvided),
    COBRA = sum(COBRA),
    PrivatePay = sum(PrivatePay),
    StateHealthInsAdults = sum(StateHealthInsAdults),
    IndianHealthServices = sum(IndianHealthServices),
    OtherInsurance = sum(OtherInsurance)
  )
# joining sub data to y/n and Enrollment data
HealthInsurance <- full_join(
  Insuranceyn,
  healthins,
  by = c(
    "PersonalID",
    "EnrollmentID",
    "HouseholdID",
    "DataCollectionStage"
  )
) 
HealthInsurance <- HealthInsurance[, c(2, 1, 3, 5, 4, 9:18)]
rm(healthins, Insuranceyn)
# Income and Sources ------------------------------------------------------
# income uses two assessment data elements plus subs
Incomeyn <- all_the_stages("svp_anysource30dayincome") %>%
  rename(IncomeFromAnySource = Value) %>%
  mutate(
    IncomeFromAnySource = case_when(
      IncomeFromAnySource == "yes (hud)" ~ 1,
      IncomeFromAnySource == "no (hud)" ~ 0,
      IncomeFromAnySource == "client doesn't know (hud)" ~ 8,
      IncomeFromAnySource == "client refused (hud)" ~ 9,
      IncomeFromAnySource == "data not collected (hud)" |
        is.na(IncomeFromAnySource) ~ 99
    )
  )
# getting the second income-related data element (total monthly income)
TMI <- all_the_stages("hud_totalmonthlyincome") %>%
  rename(TotalMonthlyIncome = Value)
# joining the y/n and the tmi data together with Enrollment data
Incomeyn <-
  left_join(Incomeyn,
            small_enrollment,
            by = c("EnrollmentID", "PersonalID", "HouseholdID")) %>%
  left_join(.,
            TMI,
            by = c(
              "EnrollmentID",
              "PersonalID",
              "HouseholdID",
              "DataCollectionStage"
            ))

# incomesubs <- mutate(
#   IncomeBenefits,
#   Earned = if_else(
#     IncomeSource == "earned income (hud)", 1, 0),
#   EarnedAmount = if_else(
#     IncomeSource == "earned income (hud)", IncomeAmount, NULL),
#   Unemployment = if_else(
#     IncomeSource == "unemployment insurance (hud)", 1, 0),
#   UnemploymentAmount = if_else(
#     IncomeSource == "unemployment insurance (hud)", IncomeAmount, NULL),
#   SSI = if_else(
#     IncomeSource == "ssi (hud)", 1, 0),
#   SSIAmount = if_else(
#     IncomeSource == "ssi (hud)", IncomeAmount, NULL),  
#   SSDI = if_else(
#     IncomeSource == "ssdi (hud)", 1, 0),
#   SSDIAmount = if_else(
#     IncomeSource == "ssdi (hud)", IncomeAmount, NULL),  
#   VADisabilityService = if_else(
#     IncomeSource == "va service connected disability compensation (hud)", 1, 0),
#   VADisabilityServiceAmount = if_else(
#     IncomeSource == "va service connected disability compensation (hud)", IncomeAmount, NULL),  
#   VADisabilityNonService = if_else(
#     IncomeSource == "va non-service connected disability compensation (hud)", 1, 0),
#   VADisabilityNonServiceAmount = if_else(
#     IncomeSource == "va non-service connected disability compensation (hud)", IncomeAmount, NULL),  
#   PrivateDisability = if_else(
#     IncomeSource == "private disability insurance (hud)", 1, 0),
#   PrivateDisabilityAmount = if_else(
#     IncomeSource == "private disability insurance (hud)", IncomeAmount, NULL),  
#   WorkersComp = if_else(
#     IncomeSource == "worker's compensation (hud)", 1, 0),
#   WorkersCompAmount = if_else(
#     IncomeSource == "worker's compensation (hud)", IncomeAmount, NULL),  
#   TANF = if_else(
#     IncomeSource == "tanf (hud)", 1, 0),
#   TANFAmount = if_else(
#     IncomeSource == "tanf (hud)", IncomeAmount, NULL),  
#   GA = if_else(
#     IncomeSource == "general assistance (hud)", 1, 0),
#   GAAmount = if_else(
#     IncomeSource == "general assistance (hud)", IncomeAmount, NULL),  
#   SocSecRetirement = if_else(
#     IncomeSource == "retirement income from social security (hud)", 1, 0),
#   SocSecRetirementAmount = if_else(
#     IncomeSource == "retirement income from social security (hud)", IncomeAmount, NULL),  
#   Pension = if_else(
#     IncomeSource == "pension or retirement income from another job (hud)", 1, 0),
#   PensionAmount = if_else(
#     IncomeSource == "pension or retirement income from another job (hud)", IncomeAmount, NULL),  
#   ChildSupport = if_else(
#     IncomeSource == "child support (hud)", 1, 0),
#   ChildSupportAmount = if_else(
#     IncomeSource == "child support (hud)", IncomeAmount, NULL),  
#   Alimony = if_else(
#     IncomeSource == "alimony (hud)", 1, 0),
#   AlimonyAmount = if_else(
#     IncomeSource == "alimony (hud)", IncomeAmount, NULL),  
#   OtherIncomeSource = if_else(
#     IncomeSource == "other (hud)", 1, 0),
#   OtherIncomeSourceAmount = if_else(
#     IncomeSource == "other (hud)", IncomeAmount, NULL),
#   IncomeSource = NULL,
#   IncomeAmount = NULL
# )
incomesubs <-
  left_join(incomesubs, small_enrollment, by = "PersonalID") %>%
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


# Referrals ---------------------------------------------------------------

# Services ----------------------------------------------------------------

# EntrySSVF ---------------------------------------------------------------


# SPDAT Scores ------------------------------------------------------------



# PATHStatus --------------------------------------------------------------


# DateOfEngagement --------------------------------------------------------


