# for playing around at home
w <- read_csv("C:\\Users\\laptop\\Documents\\sampledates.csv")
with_tz(ymd_hms(w$Date_Added), "America/New_York")

# trying some deduping with the janitor package:
library("janitor")
race2 %>% get_dupes(PersonalID)
w %>% get_dupes(Client_ID)

# sys.setenv()

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

# this ^^ does not work when used as a function, but it does work if run by itself (see below)

race2 <- assessment_data %>% filter(DataElement == "svpprofsecondaryrace")
summarised_race2 <- race2 %>% 
  group_by(PersonalID) %>% 
  summarise(max(DateEffective), max(DateAdded)) 
race2 <- 
  semi_join(race2, summarised_race2, by = c("PersonalID", 
                                            "DateAdded" = "max(DateAdded)", 
                                            "DateEffective" = "max(DateEffective)"))

# ONE answer per CLIENt ---------------------------------------------------
dob <- assessment_data %>% filter(DataElement == "svpprofdob")
dob_dq <- assessment_data %>% filter(DataElement == "svpprofdobtype")
race1 <- assessment_data %>% filter(DataElement == "svpprofrace")




ethnicity <- assessment_data %>% filter(DataElement == "svpprofeth")
disabling_condition <- assessment_data %>% filter (DataElement == "hud_disablingcondition")
# add all the client-level data elements into the Client table

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