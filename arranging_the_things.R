
# in order to filter out the most recent answers, I need to convert ALL the dates to something readable.

# Date Cleaning -----------------------------------------------------------
# tried substr() to split out the year, month, etc. with the idea of putting them back together, but it's very slow



# Date of Birth -----------------------------------------------------------
dob <- assessment_data %>% filter(Data_Element == "svpprofdob")
dob_dq <- assessment_data %>% filter(Data_Element == "svpprofdobtype")

dob <- mutate(rank = 1:nrow("Client_ID"))
# doesnt work ^^
dob_test <- dob %>% mutate(answer_count = group_size(by_Client_ID))
# also doesn't work ^^
# Race --------------------------------------------------------------------
race1 <- assessment_data %>% filter(Data_Element == "svpprofrace")

race2 <- assessment_data %>% filter(Data_Element == "svpprofsecondaryrace")

x <- race2 %>% select("Client_ID", "Data_Element", "Date_Effective", "Date_Added")
# Ethnicity ---------------------------------------------------------------
ethnicity <- assessment_data %>% filter(Data_Element == "svpprofeth")
