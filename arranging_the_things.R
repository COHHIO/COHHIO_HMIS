# for playing around at home
w <- read_csv("C:\\Users\\laptop\\Documents\\sampledates.csv")

# trying some deduping with the janitor package:
library("janitor")
race2 %>% get_dupes(PersonalID)

# ONE answer per CLIENt ---------------------------------------------------
dob <- assessment_data %>% filter(DataElement == "svpprofdob")
dob_dq <- assessment_data %>% filter(DataElement == "svpprofdobtype")
race1 <- assessment_data %>% filter(DataElement == "svpprofrace")
race2 <- assessment_data %>% filter(DataElement == "svpprofsecondaryrace")
summarised_race2 <- race2 %>% group_by(PersonalID) %>% summarise(max(DateAdded))
ethnicity <- assessment_data %>% filter(DataElement == "svpprofeth")
disabling_condition <- assessment_data %>% filter (DataElement == "")
# add all the client-level data elements into the Client table

# ONE answer per ENROLLMENT -----------------------------------------------
county_served <- assessment_data %>% filter(DataElement == "")
CoC_served <- assessment_data %>% filter(DataElement == "")
residence_prior <- assessment_data %>% filter(DataElement == "")
length_of_time <- assessment_data %>% filter(DataElement == "")
county_prior <- assessment_data %>% filter(DataElement == "")
LH_prior <- assessment_data %>% filter(DataElement == "")
Approx_date_homeless <- assessment_data %>% filter(DataElement == "")
DV_yesno <- assessmenet_data %>% filter(DataElement == "")
DV_whenx <- assessment_data %>% filter(DataElement == "")
DV_fleeing <- assessment_data %>% filter(DataElement == "")