library("janitor")
# in order to filter out the most recent answers, I need to convert ALL the dates to something readable.

# Date Cleaning -----------------------------------------------------------
# tried substr() to split out the year, month, etc. with the idea of putting them back together, but it's very slow
race2 %>% get_dupes(Client_ID)

# Filtering out duplicative assessment records ----------------------------

# the aim here is to use the date fields to wind up with only one DOB and DOB_DQ per client.
# keep the most recent Effective Date, and if there's a tie, then the most recent Added Date


# Date of Birth -----------------------------------------------------------
dob <- assessment_data %>% filter(Data_Element == "svpprofdob")

dob_dq <- assessment_data %>% filter(Data_Element == "svpprofdobtype")

# Race --------------------------------------------------------------------
race1 <- assessment_data %>% filter(Data_Element == "svpprofrace")

race2 <- assessment_data %>% filter(Data_Element == "svpprofsecondaryrace")

summarised_race2 <- race2 %>% group_by(Client_ID) %>% summarise(max(Date_Effective), max(Date_Added))
# Ethnicity ---------------------------------------------------------------
ethnicity <- assessment_data %>% filter(Data_Element == "svpprofeth")
