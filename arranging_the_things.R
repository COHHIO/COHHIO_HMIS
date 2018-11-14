# need to figure out how to get the race tables into the Client table correctly.

# Function to get most recent value for CLIENT-LEVEL assessment data ------
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


# ONE answer per CLIENt ---------------------------------------------------
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
race <- assessment_data %>% filter(DataElement %in% c("svpprofrace", "svpprofsecondaryrace"))
x <- race %>% group_by(PersonalID, Value) %>% 
  summarise(max(DateEffective), max(DateAdded))
x <- filter(x, Value != "")
race <- semi_join(race, x, 
                         by = c("PersonalID", "Value",
                                "DateAdded" = "max(DateAdded)", 
                                "DateEffective" = "max(DateEffective)"))
race <- mutate(
  race,
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
)
# add all the client-level data elements into the Client table
Client <- left_join(Client, dob, by = "PersonalID")
Client <- left_join(Client, dob_dq, by = "PersonalID")
# Client <- left_join(Client, race, by = "PersonalID")
Client <- left_join(Client, ethnicity, by = "PersonalID")
Client <- left_join(Client, disabling_condition, by = "PersonalID")

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
                 )
                 )
rm(disabling_condition, dob, dob_dq, ethnicity)
# ONE answer per ENROLLMENT -----------------------------------------------

CoC_served <- assessment_data %>% filter(DataElement == "hud_cocclientlocation")
residence_prior <- assessment_data %>% filter(DataElement == "typeoflivingsituation")
length_of_time <- assessment_data %>% filter(DataElement == "hud_lengthofstay")
LH_prior_90days <- assessment_data %>% filter(DataElement == "hud_lengthstay_less90days")
LH_prior_7days <- assessment_data %>% filter(DataElement == "hud_lengthstay_less7nights")
Approx_date_homeless <- assessment_data %>% filter(DataElement == "hud_homelessstartdate")
DV_yesno <- assessmenet_data %>% filter(DataElement == "domesticviolencevictim")
DV_whenx <- assessment_data %>% filter(DataElement == "hud_extentofdv")
DV_fleeing <- assessment_data %>% filter(DataElement == "hud_extentofdv2")