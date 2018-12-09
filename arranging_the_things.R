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
        !(Value %in% c("client doesn't know (hud)", "client refused (hud)", "data not collected (hud)"))
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
    AmIndAKNative +
      Asian +
      BlackAfAmerican +
      NativeHIOtherPacific +
      White == 0,
    1,
    0
  ),
  DOB = ymd(DOB)
)
rm(race, x)
# ONE answer per ENROLLMENT -----------------------------------------------
setDT(assessment_data)
small_enrollment <- Enrollment %>% select(EnrollmentID, PersonalID, EntryDate, ExitDate)
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

stage0begin <- now()
CoC_served <- assessment_data %>%
  filter(DataElement == "hud_cocclientlocation" &
           Value != "")
# filtering out "" values because they're not helpful for CoC_Served.
# CoC_served is only req'd for HoHs so generally when non_HoH's get a value, someone saves a "" over it
# old <- now()
CoC_served <- CoC_served %>% group_by(PersonalID, Value, DateEffective) %>%  
  summarise(max(DateAdded)) 
# new <- now()
# CoC_served <- assessment_data[DataElement == "hud_cocclientlocation" & Value != "",
#                               .(max(DateAdded)),
#                               by = list(PersonalID, CoCCode = Value, DateEffective)]
# endend <- now()
# new - old
# endend - new
# filters out duplicate answers with the same Eff Date and Value

# the plan is to pull in the value and the eff date into the Enrollment table, then use the date 
# to calculate a Collection Stage

test <- left_join(small_enrollment, CoC_served, by = "PersonalID")

stage1begin <- now()
# may not need to lubridate your dates here since it's already been done upstream
tmp <- test %>% mutate(DateEffective = ymd_hms(DateEffective), 
                       EntryDate = ymd_hms(EntryDate), 
                       ExitDate = ymd_hms(ExitDate)) %>%
  group_by(EnrollmentID) %>%
  mutate(datacollectionstage1 = max(DateEffective[EntryDate >= DateEffective]))

setDT(tmp)
stage2begin <- now()
tmp <- tmp %>% 
  group_by(EnrollmentID) %>% 
  mutate(
    datacollectionstage2 = max(DateEffective[EntryDate < DateEffective & 
                                      ExitAdjust > DateEffective]))
#this is me trying to use data.table for stage 2. it is not coming out with all the columns
#and the number of rows is incorrect as well. needs a lot of work but i think it will 
#speed it up a lot if i can pull this out!
tmp <- tmp[DateEffective %between% c(EntryDate, ExitAdjust, incbounds = FALSE), 
           .(datacollectionstage2 = max(DateEffective)), 
           by = EnrollmentID]

stage3begin <- now()
tmp <- tmp %>% #mutate(DateEffective = ymd_hms(DateEffective), ExitDate = ymd_hms(ExitDate)) %>%
  group_by(EnrollmentID) %>%
  mutate(datacollectionstage3 = case_when(DateEffective == ExitDate ~ DateEffective))

stage4begin <- now()
test <- tmp %>% mutate(

  collectionstage =
    case_when(
      DateEffective == datacollectionstage1 ~ 1,
      DateEffective == datacollectionstage2 ~ 2,
      DateEffective == datacollectionstage3 ~ 3
    ),
  datacollectionstage1 = NULL,
  datacollectionstage2 = NULL,
  datacollectionstage3 = NULL
) %>%
  filter(!is.na(collectionstage))
rm(tmp)
endend <- now()


(getdata <- stage1begin - stage0begin)
(datacollection1 <- stage2begin - stage1begin)
(datacollection2 <- stage3begin - stage2begin)
(datacollection3 <- stage4begin - stage3begin)
(alltogether <- endend - stage4begin)
(thewholething <- endend - stage0begin)

DV_yesno <- assessmenet_data %>% filter(DataElement == "domesticviolencevictim")
DV_when <- assessment_data %>% filter(DataElement == "hud_extentofdv")
DV_fleeing <- assessment_data %>% filter(DataElement == "hud_extentofdv2")