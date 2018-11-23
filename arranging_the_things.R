# ONE answer per CLIENt ---------------------------------------------------
client_level_value <- function(dataelement) {
  x <- dataelement %>% 
    group_by(PersonalID) %>% 
    summarise(max(DateEffective), max(DateAdded))
  dataelement <- semi_join(dataelement, x, 
                           by = c("PersonalID", 
                                  "DateAdded" = "max(DateAdded)", 
                                  "DateEffective" = "max(DateEffective)"))
  return(dataelement)
}
dob <- assessment_data %>% filter(DataElement == "svpprofdob")
dob <- client_level_value(dob)
dob <- mutate(dob, 
              DOB = strftime(ymd_hms(Value), "%F"),
              Value = NULL,
              DataElement = NULL,
              DateEffective = NULL,
              DateAdded = NULL)
dob_dq <- assessment_data %>% filter(DataElement == "svpprofdobtype")
dob_dq <- client_level_value(dob_dq)
dob_dq <- dob_dq %>% mutate(
  DOBDataQuality = Value,
  DataElement = NULL,
  DateEffective = NULL,
  DateAdded = NULL,
  Value = NULL
)
race <-
  assessment_data %>% filter(
    DataElement %in% c("svpprofrace", "svpprofsecondaryrace") &
    Value != "" &
      (DataElement == "svpprofrace" |
    (DataElement == "svpprofsecondaryrace" &
        !(Value %in% c(
          "client doesn't know (hud)",
          "client refused (hud)",
          "data not collected (hud)"))
    ))
  ) 
x <- race %>% group_by(PersonalID, Value) %>%  # eliminates the two identical races saved problem
  summarise(max(DateEffective), max(DateAdded)) # only keeps the most recent record
race <- mutate(
  x,
  AmIndAKNative = if_else(Value == "american indian or alaska native (hud)", 1, 0),
  Asian = if_else(Value == "asian (hud)", 1, 0),
  BlackAfAmerican = if_else(Value == "black or african american (hud)", 1, 0),
  NativeHIOtherPacific = if_else(Value == "native hawaiian or other pacific islander (hud)", 1, 0),
  White = if_else(Value == "white (hud)", 1, 0),
  DataElement = NULL,
  Value = NULL,
  DateEffective = NULL,
  DateAdded = NULL
  )
race <- race %>% group_by(PersonalID) %>% 
  summarise(AmIndAKNative = sum(AmIndAKNative),
            Asian = sum(Asian),
            BlackAfAmerican = sum(BlackAfAmerican),
            NativeHIOtherPacific = sum(NativeHIOtherPacific),
            White = sum(White))
ethnicity <- assessment_data %>% filter(DataElement == "svpprofeth")
ethnicity <- client_level_value(ethnicity)
ethnicity <- ethnicity %>% mutate(
  DataElement = NULL,
  Ethnicity = Value,
  DateEffective = NULL,
  DateAdded = NULL,
  Value = NULL
)
disabling_condition <- assessment_data %>% filter (DataElement == "hud_disablingcondition")
disabling_condition <- client_level_value(disabling_condition)
disabling_condition <- disabling_condition %>% mutate(
  DisablingCondition = Value,
  DataElement = NULL,
  Value = NULL,
  DateEffective = NULL,
  DateAdded = NULL
) #check to see if this is actually leaving you with one row per client 
# add all the client-level data elements into the Client table
Client <- left_join(Client, dob, by = "PersonalID")
Client <- left_join(Client, dob_dq, by = "PersonalID")
Client <- left_join(Client, race, by = "PersonalID")
Client <- left_join(Client, ethnicity, by = "PersonalID")
Client <- left_join(Client, disabling_condition, by = "PersonalID")
# tidy up the Client table
Client <- mutate(Client,
                 DOBDataQuality = case_when(
                   DOBDataQuality == "full dob reported (hud)" ~ 1,
                   DOBDataQuality == "approximate or partial dob reported (hud)" ~ 2,
                   DOBDataQuality == "client doesn't know (hud)" ~ 8,
                   DOBDataQuality == "client refused (hud)" ~ 9,
                   is.na(DOBDataQuality) | DOBDataQuality == "data not collected (hud)" ~ 99
                 ),
                 Ethnicity = case_when(
                   Ethnicity == "non-hispanic/non-latino (hud)" ~ 0,
                   Ethnicity == "hispanic/latino (hud)" ~ 1,
                   Ethnicity == "client doesn't know (hud)" ~ 8,
                   Ethnicity == "client refused (hud)" ~ 9,
                   is.na(Ethnicity) | Ethnicity == "" | Ethnicity == "data not collected (hud)" ~ 99
                 ),
                 DisablingCondition = case_when(
                   DisablingCondition == "yes (hud)" ~ 1,
                   DisablingCondition == "no (hud)" ~ 2,
                   DisablingCondition == "client doesn't know (hud)" ~ 8,
                   DisablingCondition == "client refused (hud)" ~ 9,
                   DisablingCondition == "data not collected (hud)" | is.na(DisablingCondition) ~ 99
                 ),
                 RaceNone = if_else(AmIndAKNative + 
                                      Asian + 
                                      BlackAfAmerican + 
                                      NativeHIOtherPacific + 
                                      White == 0, 1, 0)
                 )
rm(disabling_condition, dob, dob_dq, ethnicity, race, x)
# ONE answer per ENROLLMENT -----------------------------------------------
# filtering out "" values because they're not helpful for CoC_Served.
# CoC_served is only req'd for HoHs so generally when non_HoH's get a value, someone saves a "" over it
CoC_served <- assessment_data %>% filter(DataElement == "hud_cocclientlocation" & Value != "")
# filters out duplicate answers with the same Eff Date and Value
CoC_served <- CoC_served %>% group_by(PersonalID, Value, DateEffective) %>%  
  summarise(max(DateAdded)) %>% select(PersonalID, Value, DateEffective)
# the plan is to pull in the value and the eff date into the Enrollment table, then use the date 
# to calculate a Collection Stage
colnames(CoC_served) <- c("PersonalID", 
                          "CoCCode", 
                          "DateEffective")
small_enrollment <- Enrollment %>% select(EnrollmentID, PersonalID, EntryDate, ExitDate)
test <- left_join(small_enrollment, CoC_served, by = "PersonalID")

tmp <- test %>%
  group_by(EnrollmentID) %>%
  mutate(datacollectionstage1 = max(ymd_hms(DateEffective)[ymd_hms(EntryDate) >= ymd_hms(DateEffective)]))
tmp <- tmp %>%
  group_by(EnrollmentID) %>% 
  mutate(datacollectionstage2 = max(ymd_hms(DateEffective)[ymd_hms(EntryDate) < ymd_hms(DateEffective) & 
                                                             ymd_hms(ExitDate) > ymd_hms(DateEffective)]))
tmp <- tmp %>%
  group_by(EnrollmentID) %>%
  mutate(datacollectionstage3 = case_when(ymd_hms(DateEffective) == ymd_hms(ExitDate) ~ ymd_hms(DateEffective)))

test <- tmp %>%
  mutate(collectionstage = 
           case_when(
             ymd_hms(DateEffective) == ymd_hms(datacollectionstage1) ~ 1,
             ymd_hms(DateEffective) == ymd_hms(datacollectionstage2) ~ 2,
             ymd_hms(DateEffective) == ymd_hms(datacollectionstage3) ~ 3
           ),
         datacollectionstage1 = NULL,
         datacollectionstage2 = NULL,
         datacollectionstage3 = NULL) %>%
  filter(!is.na(collectionstage))
rm(tmp)

residence_prior <- assessment_data %>% filter(DataElement == "typeoflivingsituation")
length_of_time <- assessment_data %>% filter(DataElement == "hud_lengthofstay")
LH_prior_90days <- assessment_data %>% filter(DataElement == "hud_lengthstay_less90days")
LH_prior_7days <- assessment_data %>% filter(DataElement == "hud_lengthstay_less7nights")
Approx_date_homeless <- assessment_data %>% filter(DataElement == "hud_homelessstartdate")
DV_yesno <- assessmenet_data %>% filter(DataElement == "domesticviolencevictim")
DV_whenx <- assessment_data %>% filter(DataElement == "hud_extentofdv")
DV_fleeing <- assessment_data %>% filter(DataElement == "hud_extentofdv2")