library("xml2")
library("tidyverse")
library("lubridate")
begin <- now()
#change to ..._41 when at home, ..._40 at work
y <- read_xml("data/Bowman_Payload_40.xml")
users <- read_csv("data/usercreating.csv")
counties <- read_csv("data/counties.csv")
# LIST OF THINGS
# add code at the end that replaces the PII with "what we need to know" about names and ssns so that you can base
#     your reporting on that. 
# sadly no county data is coming through in the provider address.
# can't get the hopwa psh funding source to flip to its number.

xml_to_df <- function(xml, path_name, cols) {
  records <- xml_find_all(xml, xpath = path_name)
  df <- as.data.frame(setNames(replicate(length(cols), character(0), simplify = F), cols))
  for(child in records){
    gchild <-  xml_children(child)
    data  <-  xml_text(gchild)
    names(data) <- xml_name(gchild)
    data  <- data[names(data) %in% cols]
    df <- bind_rows(df, data.frame(as.list(data)))
  }
  return(df)
}
# Provider records --------------------------------------------------------
# name nodes we want to pull
provider_start <- now()
cols <- c(
  "record_id",
  "name",
  "aka",
  "customHudOrganizationName",
  "programTypeCodeValue",
  "hudHousingType",
  "hudOrganization",
  "continuumFlag",
  "affiliatedResidentialProject",
  "principalSite",
  "hudTrackingMethod",
  "operatingStartDate",
  "operatingEndDate",
  "targetPopValue",
  "victimServiceProvider"
)
# run function to get xml data to a data frame
Project <- xml_to_df(y, "//*/Provider", cols)

# get ids, add them to the data frame
providers$record_id <- parse_number(xml_text(xml_find_all(y, "//records/providerRecords/Provider/@record_id")))

# clean up column names
colnames(Project) <- c(
  "record_id" = "Provider_ID",
  "name" = "Provider_Name",
  "aka" = "Common_Name",
  "customHudOrganizationName" = "Organization_Name",
  "programTypeCodeValue" = "Project_Type",
  "hudHousingType" = "Housing_Type",
  "hudOrganization" = "Organization_ID",
  "continuumFlag" = "Continuum_Project",
  "affiliatedResidentialProject" = "Affiliated_Residential_Project",
  "principalSite" = "Principal_Site",
  "hudTrackingMethod" = "Tracking_Method",
  "operatingStartDate" = "Operating_Start",
  "operatingEndDate" = "Operating_End",
  "targetPopValue" = "Target_Population",
  "victimServiceProvider" = "Victim_Services_Provider"
)
# replace data levels with what it has in the HUD CSV specs 
Project <- Project %>% mutate(
  Project_Type = case_when(
    Project_Type == "emergency shelter (hud)" ~ 1,
    Project_Type == "transitional housing (hud)" ~ 2,
    Project_Type == "ph - permanent supportive housing (disability required for entry) (hud)" ~ 3,
    Project_Type == "street outreach (hud)" ~ 4,
    Project_Type == "services only (hud)" ~ 6,
    Project_Type == "other (hud)" ~ 7,
    Project_Type == "safe haven (hud)" ~ 8,
    Project_Type == "ph - housing only (hud)" ~ 9,
    Project_Type == "ph - housing with services (hud)" ~ 10,
    Project_Type == "day shelter (hud)" ~ 11,
    Project_Type == "homelessness prevention (hud)" ~ 12,
    Project_Type == "ph - rapid re-housing (hud)" ~ 13,
    Project_Type == "coordinated assessment (hud)" ~ 14
  ),
  Tracking_Method = case_when(
    Tracking_Method == "entry/exit date" & Project_Type == 1 ~ 0,
    Tracking_Method == "service transaction model" & Project_Type == 1 ~ 3
  ),
  Target_Population = case_when(
    Target_Population == "dv: domestic violence victims" ~ 1,
    Target_Population == "hiv: persons with hiv/aids" ~3,
    Target_Population == "na: not applicable" ~ 4
  ),
  Housing_Type = case_when(
    Housing_Type == "site-based - single site" & Project_Type %in% c(1, 2, 3, 8, 9, 10, 13) ~ 1,
    Housing_Type == "site-based - clustered / multiple sites" & Project_Type %in% c(1, 2, 3, 8, 9, 10, 13) ~ 2,
    Housing_Type == "tenant-based - scattered site" & Project_Type %in% c(1, 2, 3, 8, 9, 10, 13) ~ 3
  ),
  Victim_Services_Provider = case_when(
    Victim_Services_Provider == "true" ~ 1,
    Victim_Services_Provider == "false" ~ 0,
    is.na(Victim_Services_Provider) ~ 99
  ),
  Continuum_Project = case_when(
    Continuum_Project == "true" ~ 1,
    Continuum_Project == "false" ~ 0,
    is.na(Continuum_Project) ~ 99
  ),
  Affiliated_Residential_Project = case_when(
    Affiliated_Residential_Project == "true" & Project_Type == 6 ~ 1,
    Affiliated_Residential_Project == "false" & Project_Type == 6 ~ 0,
    is.na(Affiliated_Residential_Project) & Project_Type == 6 ~ 99
  ),
  Principal_Site = case_when(
    Principal_Site == "true" ~ 1,
    Principal_Site == "false" ~ 0,
    is.na(Principal_Site) ~ 99
  )
)
# Provider CoC records ----------------------------------------------------
# name nodes we want to pull in
provider_CoC_start <- now()
cols <- c(
  "provider",
  "startDate",
  "endDate",
  "cocCode",
  "geographyType",
  "postalCode",
  "geocode"
)
# run function to get xml to a dataframe
ProjectCoC <- xml_to_df(y, "//*/ProviderCOCCode", cols)
# clean up column names
colnames(ProjectCoC) <- c(
  "provider" = "Provider_ID",
  "startDate" = "CoC_Start",
  "endDate" = "CoC_End",
  "cocCode" = "CoC_Code",
  "geographyType" = "Geography_Type",
  "postalCode" = "ZIP",
  "geocode" = "Geocode"
)
# clean up data to match with HUD CSV specs and make id field numeric
ProjectCoC <- ProjectCoC %>%
  mutate(
    Provider_ID = parse_number(Provider_ID),
    Geography_Type = case_when(
      Geography_Type == "urban" ~ 1,
      Geography_Type == "suburban" ~ 2,
      Geography_Type == "rural" ~ 3,
      is.na(Geography_Type) ~ 99
    )
  )

# Funding Sources ---------------------------------------------------------
# name nodes we want to pull in
funding_source_start <- now()
cols <- c(
  "provider",
  "grantStartDate",
  "grantEndDate",
  "federalPartnerProgram"
)
# node to count from
# run function to get xml to a dataframe
Funder <- xml_to_df(y, "//records/providerRecords/Provider/childProviderFedPartnerFundingSource/*[active = 'true']", cols)
# clean up column names
colnames(Funder) <- c(
  "provider" = "Provider_ID",
  "grantStartDate" = "Grant_Start",
  "grantEndDate" = "Grant_End",
  "federalPartnerProgram" = "Funding_Source"
)
# clean up data to match with HUD CSV specs and make id field numeric
Funder <- Funder %>%
  mutate(
    Provider_ID = parse_number(Provider_ID),
    Funding_Source = case_when(
      Funding_Source == "hud:coc - homelessness prevention (high performing comm. only)" ~ 1,
      Funding_Source == "hud:coc - permanent supportive housing" ~ 2,
      Funding_Source == "hud:coc - rapid re‐housing" ~ 3,
      Funding_Source == "hud:coc - supportive services only" ~ 4,
      Funding_Source == "hud:coc - transitional housing" ~ 5,
      Funding_Source == "hud:coc - safe haven" ~ 6,      
      Funding_Source == "hud:coc - single room occupancy (sro)" ~ 7,
      Funding_Source == "hud:esg - emergency shelter (operating and/or essential services)" ~ 8,
      Funding_Source == "hud:esg - homelessness prevention" ~ 9,      
      Funding_Source == "hud:esg - rapid rehousing" ~ 10,
      Funding_Source == "hud:esg - street outreach" ~ 11,
      Funding_Source == "hud:rural housing stability assistance program" ~ 12,      
      Funding_Source == "hud:hopwa - hotel/motel vouchers" ~ 13,
      Funding_Source == "hud:hopwa - housing information" ~ 14,
      Funding_Source == "hud:hopwa - permanent housing (facility based or tbra)" ~ 15,      
      Funding_Source == "hud:hopwa - permanent housing placement" ~ 16,
      Funding_Source == "hud:hopwa - short‐term rent, mortgage, utility assistance" ~ 17,
      Funding_Source == "hud:hopwa - short‐term supportive facility" ~ 18,      
      Funding_Source == "hud:hopwa - transitional housing (facility based or tbra)" ~ 19,
      Funding_Source == "hud:hud/vash" ~ 20,
      Funding_Source == "hhs:path - street outreach & supportive services only" ~ 21,      
      Funding_Source == "hhs:rhy - basic center program (prevention and shelter)" ~ 22,
      Funding_Source == "hhs:rhy - maternity group home for pregnant and parenting youth" ~ 23,
      Funding_Source == "hhs:rhy - transitional living program" ~ 24,      
      Funding_Source == "hhs:rhy - street outreach project" ~ 25,
      Funding_Source == "hhs:rhy - demonstration project" ~ 26,
      Funding_Source == "va: crs contract residential services" ~ 27,      
      Funding_Source == "va:community contract safe haven program" ~ 30,
      Funding_Source == "va compensated work therapy transitional residence" ~ 32,
      Funding_Source == "va:supportive services for veteran families" ~ 33,      
      Funding_Source == "n/a" ~ 34,
      Funding_Source == "hud:pay for success" ~ 35,
      Funding_Source == "hud:public and indian housing (pih) programs" ~ 36,      
      Funding_Source == "va:grant per diem - bridge housing" ~ 37,
      Funding_Source == "va:grant per diem - low demand" ~ 38,
      Funding_Source == "va:grant per diem - hospital to housing" ~ 39,      
      Funding_Source == "va:grant per diem - clinical treatment" ~ 40,
      Funding_Source == "va:grant per diem - service intensive transitional housing" ~ 41,
      Funding_Source == "va:grant per diem - transition in place" ~ 42,
      Funding_Source == "hud:coc - youth homeless demonstration program (yhdp)"~ 43
    )
  )
# ProviderAddresses -------------------------------------------------------
# name nodes we want to pull in
provider_address_start <- now()
cols <- c(
  "provider",
  "line1",
  "city",
  "addressType",
  "province",
  "postalCode"
)
# run function to get xml to a dataframe
Geography <- xml_to_df(y, "//*/childProviderAddress/*", cols)
# clean up column names
colnames(Geography) <- c(
  "provider" = "Provider_ID",
  "line1" = "Address",
  "city" = "City",
  "addressType" = "Address_Type",
  "province" = "State",
  "postalCode" = "ZIP"
)
Geography <- Geography %>%
  mutate(
    Provider_ID = parse_number(Provider_ID))
# Provider Inventory Records ----------------------------------------------
# name nodes we want to pull in
provider_inventory_start <- now()
cols <- c(
  "provider",
  "householdTypeValue",
  "bedTypeValue",
  "availabilityValue",
  "bedInventory",
  "unitInventory",
  "chBedInventory",
  "veteranBedInventory",
  "youthBedInventory",
  "inventoryStartDate",
  "inventoryEndDate",
  "hmisBeds",
  "cocCode"
)
# run function to get xml to a dataframe
Inventory <- xml_to_df(y, "//records/bedUnitInventoryRecords/BedUnitInventory[active = 'true']", cols)
# clean up column names
colnames(Inventory) <- c(
  "provider" = "Provider_ID", 
  "householdTypeValue" = "Household_Type", 
  "bedTypeValue" = "Bed_Type", 
  "availabilityValue" = "Availability",
  "bedInventory" = "Bed_Inventory", 
  "unitInventory" = "Unit_Inventory",
  "chBedInventory" = "Chronic_Beds",
  "veteranBedInventory" = "Vet_Beds",
  "youthBedInventory" = "Youth_Beds",
  "inventoryStartDate" = "Inventory_Start",
  "inventoryEndDate" = "Inventory_End",
  "hmisBeds" = "HMIS_Beds",
  "cocCode" = "CoC_Code"
)
# clean up data to match with HUD CSV specs and make id field numeric
Inventory <- Inventory %>%
  mutate(
    Provider_ID = as.numeric(str_extract(Provider_ID, "[0-9]+")),
    Household_Type = case_when(
      Household_Type == "households without children" ~ 1,
      Household_Type == "households with at least one adult and one child" ~ 3,
      Household_Type == "households with only children" ~ 4
    ),
    Availability = case_when(
      Availability == "year-round" ~ 1,
      Availability == "seasonal" ~ 2,
      Availability == "overflow" ~ 3
    ),
    Bed_Type = case_when(
      Bed_Type == "facility-based" ~ 1,
      Bed_Type == "voucher" ~ 2,
      Bed_Type == "other" ~ 3
    )
  )

# Entry Exit Records ------------------------------------------------------
# name nodes we want to pull in
ee_start <- now()
cols <- c(
  "system_id",
  "date_added",
  "client",
  "typeEntryExit",
  "group",
  "household",
  "provider",
  "entryDate",
  "exitDate",
  "destinationValue"
)
# run function to get xml to a dataframe
Enrollment <- xml_to_df(y, "//records/entryExitRecords/EntryExit[active = 'true']", cols)
# get attributes to the data frame
Enrollment$system_id <- parse_number(xml_text(xml_find_all(y, "//records/entryExitRecords/EntryExit[active = 'true']/@system_id")))
Enrollment$date_added <- xml_text(xml_find_all(y, "//records/entryExitRecords/EntryExit[active = 'true']/@date_added"))
# clean up column names
colnames(Enrollment) <- c(
  "system_id" = "EE_ID",
  "date_added" = "EE_Date_Added",
  "client" = "Client_ID",
  "typeEntryExit" = "EE_Type",
  "group" = "Group_ID",
  "household" = "Household_ID",
  "provider" = "Provider_ID",
  "entryDate" = "Entry_Date",
  "exitDate" = "Exit_Date",
  "destinationValue" = "Destination"
)
# clean up data to match with HUD CSV specs and make id field numeric
Enrollment <- Enrollment %>% mutate(
  Destination = case_when(
    Destination == "emergency shelter, including hotel or motel paid for with emergency shelter voucher (hud)" &
      !is.na(Exit_Date) ~  1,
    Destination == "transitional housing for homeless persons (including homeless youth) (hud)" &
      !is.na(Exit_Date) ~ 2,
    Destination == "permanent housing (other than rrh) for formerly homeless persons (hud)" &
      !is.na(Exit_Date) ~ 3,
    Destination == "psychiatric hospital or other psychiatric facility (hud)" &
      !is.na(Exit_Date) ~ 4,
    Destination == "substance abuse treatment facility or detox center (hud)" &
      !is.na(Exit_Date) ~ 5,
    Destination == "hospital or other residential non-psychiatric medical facility (hud)" &
      !is.na(Exit_Date) ~ 6,
    Destination == "jail, prison or juvenile detention facility (hud)" &
      !is.na(Exit_Date) ~ 7,
    Destination == "client doesn't know (hud)" &
      !is.na(Exit_Date) ~ 8,
    Destination == "client refused (hud)" &
      !is.na(Exit_Date) ~ 9,
    Destination == "rental by client, no ongoing housing subsidy (hud)" &
      !is.na(Exit_Date) ~ 10,
    Destination == "owned by client, no ongoing housing subsidy (hud)" &
      !is.na(Exit_Date) ~ 11,
    Destination == "staying or living with family, temporary tenure (e.g., room, apartment or house)(hud)" &
      !is.na(Exit_Date) ~ 12,
    Destination == "staying or living with friends, temporary tenure (e.g., room apartment or house)(hud)" &
      !is.na(Exit_Date) ~ 13,
    Destination == "hotel or motel paid for without emergency shelter voucher (hud)" &
      !is.na(Exit_Date) ~ 14,
    Destination == "foster care home or foster care group home (hud)" &
      !is.na(Exit_Date) ~ 15,
    Destination == "place not meant for habitation (hud)" &
      !is.na(Exit_Date) ~ 16,
    Destination == "other (hud)" &
      !is.na(Exit_Date) ~ 17,
    Destination == "safe haven (hud)" &
      !is.na(Exit_Date) ~ 18,
    Destination == "rental by client, with vash housing subsidy (hud)" &
      !is.na(Exit_Date) ~ 19,
    Destination == "rental by client, with other ongoing housing subsidy (hud)" &
      !is.na(Exit_Date) ~ 20,
    Destination == "owned by client, with ongoing housing subsidy (hud)" &
      !is.na(Exit_Date) ~ 21,
    Destination == "staying or living with family, permanent tenure (hud)" &
      !is.na(Exit_Date) ~ 22,
    Destination == "staying or living with friends, permanent tenure (hud)" &
      !is.na(Exit_Date) ~ 23,
    Destination == "deceased (hud)" &
      !is.na(Exit_Date) ~ 24,
    Destination == "long-term care facility or nursing home (hud)" &
      !is.na(Exit_Date) ~ 25,
    Destination == "moved from one hopwa funded project to hopwa ph (hud)" &
      !is.na(Exit_Date) ~ 26,
    Destination == "moved from one hopwa funded project to hopwa th (hud)" &
      !is.na(Exit_Date) ~ 27,
    Destination == "rental by client, with gpd tip housing subsidy (hud)" &
      !is.na(Exit_Date) ~ 28,
    Destination == "residential project or halfway house with no homeless criteria (hud)" &
      !is.na(Exit_Date) ~ 29,
    Destination == "no exit interview completed (hud)" &
      !is.na(Exit_Date) ~ 30,
    Destination == "rental by client, with rrh or equivalent subsidy (hud)" &
      !is.na(Exit_Date) ~ 31,
    Destination == "data not collected (hud)" &
      !is.na(Exit_Date) ~ 99 
  ),
  Client_ID = parse_number(Client_ID),
  Household_ID = parse_number(Household_ID),
  Provider_ID = parse_number(Provider_ID)
)
# add in User_Creating
Enrollment <- entry_exits %>% left_join(users, by = "EE_ID")
Enrollment <- left_join(Enrollment, counties, by = c("EE_ID", "Client_ID"))
# Interims ----------------------------------------------------------------
interims_start <- now()
cols <- c(
  "reviewDate",
  "reviewType",
  "review_id",
  "ee_id"
)
# this throws warnings but it's ok
interims <- xml_to_df(y, "//records/entryExitRecords/EntryExit[active = 'true']/childEntryExitReview/EntryExitReview", cols)
# getting record id's of the interim records
interims$review_id <- parse_number(xml_text(xml_find_all(y, "//records/entryExitRecords/EntryExit[active = 'true']/childEntryExitReview/EntryExitReview/@system_id")))
# establishing the node we're counting up from
interim_node <- xml_find_all(y, "//records/entryExitRecords/EntryExit[active = 'true']/childEntryExitReview/EntryExitReview")

ee_as_gparent <- xml_parent(xml_parent(interim_node))
# grabbing the attribute that's at those particular ee's

length_interim <- sapply(xml_parent(interim_node), function(x) length(xml_children(x)))

ee_id <- parse_number(xml_attr(ee_as_gparent, "record_id"))

ids <- data.frame(length_interim, ee_id)

ee_ids <- c()
for(i in 1:nrow(ids)) {
  ee_ids <- c(ee_ids, rep(ids[i,]$ee_id, ids[i,]$length_interim))
}
interims$ee_id <- ee_ids
# clean up column names
colnames(interims) <- c("Interim_Date", "Interim_Type", "Interim_ID", "EE_ID")
# clean up the house
rm(interim_node, ee_as_gparent, length_interim, ee_id, ids, ee_ids, users, counties)

# Client Records ----------------------------------------------------------
# name nodes we want to pull in
client_start <- now()
cols <- c(
  "record_id",
  "firstName",
  "socSecNoDashed",
  "ssnDataQualityValue",
  "nameDataQualityValue",
  "veteranStatus"
)
# run function to get xml to a dataframe
Client <- xml_to_df(y, "//records/clientRecords/Client", cols)
# get ids
Client$record_id <- parse_number(xml_text(xml_find_all(y, "//records/clientRecords/Client/@record_id")))
# clean up data to match with HUD CSV specs and make id field numeric
Client <- Client %>% mutate(
  nameDataQualityValue = case_when(
    nameDataQualityValue == "full name reported" ~ 1,
    nameDataQualityValue == "partial, street name, or code name reported" ~ 2,
    nameDataQualityValue == "client doesn't know" ~ 8,
    nameDataQualityValue == "client refused" ~ 9,
    is.na(nameDataQualityValue) | nameDataQualityValue == "data not collected (hud)" ~ 99
  ),
  ssnDataQualityValue = case_when(
    ssnDataQualityValue == "full ssn reported (hud)" ~ 1,
    ssnDataQualityValue == "approximate or partial ssn reported (hud)" ~ 2,
    ssnDataQualityValue == "client doesn't know (hud)" ~ 8,
    ssnDataQualityValue == "client refused" ~ 9,
    ssnDataQualityValue == "data not collected (hud)"| is.na(ssnDataQualityValue) ~ 99
  ),
  veteranStatus = case_when(
    veteranStatus == "yes (hud)" ~ 1,
    veteranStatus == "no (hud)" ~ 0,
    veteranStatus == "client doesn't know (hud)" ~ 8,
    veteranStatus == "client refused (hud)" ~ 9,
    veteranStatus == "data not collected (hud)" | is.na(veteranStatus) ~ 99
  )
)
# clean up column names
colnames(Client) <- c(
  "record_id" = "Client_ID",
  "firstName" = "First_Name",
  "socSecNoDashed" = "SSN",
  "ssnDataQualityValue" = "SSN_DQ",
  "nameDataQualityValue" = "Name_DQ",
  "veteranStatus" = "Veteran_Status"  
)
# Strip of PII ------------------------------------------------------------

Client <- Client %>%
  mutate(First_Name = case_when(
    Name_DQ %in% c(8,9) ~ "DKR",
    Name_DQ == 2 ~ "Partial",
    Name_DQ == 99 | is.na(Name_DQ) | First_Name == "Anonymous" ~ "Missing",
    !(Name_DQ %in% c(2, 8, 9, 99) | is.na(Name_DQ) | First_Name == "Anonymous") ~ "ok"))

Client <- Client %>%
  mutate(SSN = case_when(
    is.na(SSN) | is.na(SSN_DQ) | SSN_DQ == 99 ~ "Missing",
    SSN_DQ %in% c(8, 9) ~ "DKR",
    ifelse((
      substr(SSN, 1, 1) != "0" &
        substr(SSN, 1, 2) != "00"
    ),
    nchar(as.numeric(SSN)) != 9, FALSE) |
      substr(SSN, 1, 3) %in% c("000", "666") |
      substr(SSN, 1, 1) == 9 |
      substr(SSN, 4, 5) == "00" |
      substr(SSN, 6, 9) == "0000" |
      SSN_DQ == 2 |
      SSN %in% c(
        111111111,
        222222222,
        333333333,
        444444444,
        555555555,
        666666666,
        777777777,
        888888888,
        123456789) ~ "Invalid or Incomplete"
  ))

Client <- Client %>%
  mutate(SSN = case_when(
    is.na(SSN) ~ "ok",
    !is.na(SSN) ~ SSN
  ))
# Assessment Records ------------------------------------------------------
assessments_start <- now()
cols <- c(
  "client_id",
  "data_element",
  "value",
  "date_added",
  "date_effective"
)
# 2742036 nodes (assessment records, including subs)
assessmentData_child_nodes <- xml_find_all(y, xpath = "//records/clientRecords/Client/assessmentData/*")
# 42754 nodes (client records)
Client_ID_as_gparent <- xml_parent(xml_parent(assessmentData_child_nodes))
# 2742036 records
data_element <- xml_name(assessmentData_child_nodes)
# 2742036 records
value <- xml_text(assessmentData_child_nodes)
# 2742036 records
date_effective <- xml_attr(assessmentData_child_nodes, "date_effective")
# 2742036 records
date_added <- xml_attr(assessmentData_child_nodes, "date_added")
# 42754 records
client_id <- parse_number(xml_attr(Client_ID_as_gparent, "record_id"))
# 42754 records
length_assessments <- sapply(xml_parent(assessmentData_child_nodes), function(x) length(xml_children(x)))
# 42754 records
ids <- data.frame(length_assessments, client_id)
# getting the 42754 records onto the larger number of rows
a <- c()
for(i in 1:nrow(ids)) {
  a <- c(a, rep(ids[i,]$client_id, ids[i,]$length_assessments))
}
# and thank you
client_id <- a
# putting it all together
assessment_data <- bind_cols(list(client_id, data_element, value, date_effective, date_added))
# naming the columns
colnames(assessment_data) <- c(
  "client_id" = "Client_ID",
  "data_element" = "Data_Element",
  "value" = "Value",
  "date_effective" =  "Date_Effective",
  "date_added" = "Date_Added"
)
# clean the house
rm(assessmentData_child_nodes, 
   Client_ID_as_gparent, 
   data_element, 
   value, 
   date_effective, 
   date_added, 
   client_id, 
   a,
   i,
   length_assessments, 
   ids)
# delete records we don't need
assessment_data <- assessment_data %>%
  filter(!(
    Data_Element %in%
      c(
        "veteran",
        "monthlyincome",
        "svp_noncashbenefits",
        "disabilities_1",
        "hudhealthinsurancesuba",
        "hud_vamcstationno",
        "hud_currentlyinschool",
        "hud_healthcondition",
        "hud_receivedvoctraining",
        "hud_currentemptenure",
        "hud_hrsworkedlastweek",
        "svpjuvenileparent",
        "hud_lookingforwork",
        "hud_assessmentdispositionoth",
        "hud_assessmentdisposition",
        "hud_hpscreeningscore",
        "hud_inpermhousing",
        "address_1",
        "hud_vetinfo",
        "svp_outreach",
        "hud_hivaids",
        "hud_respriorentryothspec",
        "hud_apxstartdateessh",
        "hud_nomonthesshin3yrs",
        "hud_numberoftimeessh",
        "hud_conthomeless1year",
        "hud_numhomeless3yrs",
        "svpchronichomeless",
        "hud_monthsconthomeless",
        "hud_statusdocumented",
        "hud_totalnummonhomelesspast3yr",
        "hud_zipcodelastpermaddr",
        "hud_zipdataquality",
        "rhymisbcpexincartype",
        "hud_rhymisbcpexactmilp",
        "rhymisbcpexabusef",
        "rhymisbcpexabusey",
        "rhymisbcpexalcoholy",
        "rhymisbcpexhealthf",
        "rhymisbcpexhealthy",
        "rhymisbcpexhhdyn",
        "rhymisbcpexhousingf",
        "rhymisbcpexhousingy",
        "rhymisbcpexmentaly",
        "rhymisbcpexmentdisf",
        "rhymisbcpexmentdisy",
        "rhymisbcpexphysdisy",
        "rhymisbcpexschoolf",
        "rhymisbcpexschooly",
        "rhymisbcpexsexorif",
        "rhymisbcpexsexoriy",
        "rhymisbcpexunemployy",
        "hud_famreunifachieved",
        "rhymisbcpextrans171",
        "rhymisbcpextrans172",
        "rhymisbcpextrans173",
        "rhymisbcpextrans174",
        "rhymisbcpextrans175",
        "rhymisbcpextrans176",
        "rhymisbcpextrans177",
        "rhymisbcpextrans178",
        "rhymisbcpextrans179",
        "svphudothercrisis_numberemergencyroom",
        "svphudothercrisis_numberinpatientfacility",
        "svphudothercrisis_numberprison",
        "svp_hud_housingstatus",
        "svp_employed",
        "hud_counselingafterexit",
        "rhymistertiaryrace"
      )
  ))

# Needs ------------------------------------------------------------
# name nodes we want to pull in
needs_start <- now()
cols <- c(
  "record_id",
  "client",
  "provider",
  "group",
  "dateSet",
  "status",
  "outcome",
  "reasonUnmet",
  "note",
  "code"
)
# run function to get xml to a dataframe
needs <- xml_to_df(y, "//records/needRecords/Need", cols)
# get Need IDs to data frame
needs$record_id <- parse_number(xml_text(xml_find_all(y, "//records/needRecords/Need/@record_id")))
# make the Client IDs and Provider IDs numeric
needs <- mutate(needs, 
                client = parse_number(client),
                provider = parse_number(provider))
# clean up column names
colnames(needs) <- c(
  "record_id" = "Need_ID",
  "client" = "Client_ID",
  "provider" = "Provider_ID",
  "group" = "Group_ID",
  "dateSet" = "Need_Date",
  "status" = "Need_Status",
  "outcome" = "Need_Outcome",
  "reasonUnmet" = "Reason_Unmet",
  "note" = "Note",
  "code" = "Need_Code"
)

# Services and Referrals ---------------------------------------------------
# name nodes we want to pull in
services_start <- now()
cols <- c(
  "record_id",
  "client",
  "need",
  "needServiceGroup",
  "group",
  "referfromProvider",
  "provideProvider",
  "code",
  "provideStartDate",
  "provideEndDate",
  "household",
  "serviceNote"
) 
# run function to get xml to a dataframe (I think there are no inactive records coming in)
Services <- xml_to_df(y, "//records/needRecords/Need/childService/Service", cols)
# clean up column names
Services$record_id <- parse_number(xml_text(xml_find_all(y, "//records/needRecords/Need/childService/Service/@record_id")))
# make the Client IDs and Provider IDs numeric
Services <- mutate(Services, 
                "client" = parse_number(client),
                "referfromProvider" = parse_number(referfromProvider),
                "needServiceGroup" = parse_number(needServiceGroup),
                "need" = parse_number(need),
                "provideProvider" = parse_number(provideProvider),
                "household" = parse_number(household))
# clean up column names
colnames(Services) <- c(
  "record_id" = "Service_Referral_ID",
  "client" = "Client_ID",
  "need" = "Need_ID",
  "needServiceGroup" = "Group_UID",
  "group" = "Group_ID",
  "referfromProvider" = "Refer_From_Provider_ID",
  "provideProvider" = "Provider_ID",
  "code" = "Need_Code",
  "provideStartDate" = "Service_Start_Date",
  "provideEndDate" = "Service_End_Date",
  "household" = "Household_ID",
  "note" = "Note"
)
# Income ------------------------------------------------------------------
# name nodes we want to pull in
income_start <- now()
cols <- c(
  "client_id",
  "system_id",
  "amountmonthlyincome",
  "monthlyincomestart",
  "monthlyincomeend",
  "sourceofincome"
)
# income node to count up from
income_node <- xml_find_all(y, "//records/clientRecords/Client/assessmentData/monthlyincome[svp_receivingincomesource ='yes']")
# run function to get xml to a dataframe
IncomeBenefits <- xml_to_df(y, "//records/clientRecords/Client/assessmentData/monthlyincome[svp_receivingincomesource ='yes']", cols)
# get sub ids
IncomeBenefits$system_id <- parse_number(xml_attr(income_node, "system_id"))
# create an empty table
a <- c()
# populate the empty table with the client ids
for(i in 1:length(income_node)) {
  a <- c(a, parse_number(xml_attr(xml_parent(xml_parent(income_node[i])),"record_id")))
  }
# add client ids into the larger object
IncomeBenefits$client_id <- a
# clean up the house
rm(income_node, a)
# clean up column names
colnames(IncomeBenefits) <- c(
  "client_id" = "Client_ID",
  "system_id" = "Income_ID",
  "amountmonthlyincome" = "Income_Amount",
  "monthlyincomestart" = "Income_Start",
  "monthlyincomeend" = "Income_End",
  "sourceofincome" = "Income_Source"
  )
# Non Cash ----------------------------------------------------------------
# name nodes we want to pull in
noncash_start <- now()
cols <- c(
  "client_id",
  "system_id",
  "svp_noncashbenefitssource",
  "svp_noncashbenefitsstart",
  "svp_noncashbenefitsend"
)
# noncash node to count up from
noncash_node <- xml_find_all(y, "//records/clientRecords/Client/assessmentData/svp_noncashbenefits[svp_receivingbenefit = 'yes']")
# run function to get xml to a dataframe
noncash <- xml_to_df(y, "//records/clientRecords/Client/assessmentData/svp_noncashbenefits[svp_receivingbenefit = 'yes']", cols)
# get ids to data frame
noncash$system_id <- parse_number(xml_attr(noncash_node, "system_id"))
# create an empty table
a <- c()
# populate the empty table with the right number of client ids
for(i in 1:length(noncash_node)) {
  a <- c(a, parse_number(xml_attr(xml_parent(xml_parent(noncash_node[i])),"record_id")))
}
# add this column into the larger object
noncash$client_id <- a
# clean up the house
rm(noncash_node, a)
# clean up column names
colnames(noncash) <- c(
  "client_id" = "Client_ID",
  "system_id" = "Noncash_ID",
  "svp_noncashbenefitssource" = "Noncash_Source",
  "svp_noncashbenefitsstart" = "Noncash_Start_Date",
  "svp_noncashbenefitsend" = "Noncash_End_Date"
)

# Disabilities -----------------------------------------------------------
# name nodes we want to pull in
disabilities_start <- now()
cols <- c(
  "client_id",
  "system_id",
  "disabilities_1start",
  "disabilities_1end",
  "disabilitytype",
  "hud_impairabilityliveind"
)
# node to count from
disabilities_node <- xml_find_all(y, "//records/clientRecords/Client/assessmentData/disabilities_1[disabilitydetermine = 'yes (hud)']")
# run function to get xml to a dataframe
Disabilities <- xml_to_df(y, "//records/clientRecords/Client/assessmentData/disabilities_1[disabilitydetermine = 'yes (hud)']", cols)
# get ids to data frame
Disabilities$system_id <- parse_number(xml_text(xml_find_all(y, "//records/clientRecords/Client/assessmentData/disabilities_1[disabilitydetermine = 'yes (hud)']/@system_id")))
# create an empty table
a <- c()
# populate the empty table with the right number of client ids
for(i in 1:length(disabilities_node)) {
  a <- c(a, parse_number(xml_attr(xml_parent(xml_parent(disabilities_node[i])),"record_id")))
}
# add this column into the larger object
Disabilities$client_id <- a
# clean up data to match with HUD CSV specs 
Disabilities <- mutate(Disabilities,
  hud_impairabilityliveind = case_when(
    hud_impairabilityliveind == "yes (hud)" ~ 1,
    hud_impairabilityliveind == "no (hud)" ~ 0,
    hud_impairabilityliveind == "client doesn't know (hud)" ~ 8,
    hud_impairabilityliveind == "client refused (hud)" ~ 9,
    hud_impairabilityliveind == "data not collected (hud)" |
      is.na(hud_impairabilityliveind) ~ 99
  )
)
# clean up
rm(disabilities_node, a)
# rename columns
colnames(Disabilities) <- c(
  "client_id" = "Client_ID",
  "system_id" = "Disability_ID",
  "disabilities_1start" = "Disability_Start_Date",
  "disabilities_1end" = "Disability_End_Date",
  "disabilitytype" = "Disability_Type",
  "hud_impairabilityliveind" = "Long_Duration"
)

# Health Insurance -------------------------------------------------------
# name nodes we want to pull in
h_ins_start <- now()
cols <- c(
  "client_id",
  "system_id",
  "hudhealthinsurancesubastart",
  "hudhealthinsurancesubaend",
  "svphudhealthinsurancetype"
)
health_insurance_node <-
  xml_find_all(
    y,
    "//records/clientRecords/Client/assessmentData/hudhealthinsurancesuba[svphudhealthinscovered = 'yes']"
  )
# run function to get xml to a dataframe
health_insurance <-
  xml_to_df(
    y,
    "//records/clientRecords/Client/assessmentData/hudhealthinsurancesuba[svphudhealthinscovered = 'yes']",
    cols
  )
# get ids to data frame
health_insurance$system_id <-
  parse_number(xml_text(
    xml_find_all(
      y,
      "//records/clientRecords/Client/assessmentData/hudhealthinsurancesuba[svphudhealthinscovered = 'yes']/@system_id"
    )
  ))
# create empty table
a <- c()
# populate the empty table with the right number of client ids
for(i in 1:length(health_insurance_node)) {
  a <-  c(a, parse_number(xml_attr(xml_parent(xml_parent(health_insurance_node[i])), "record_id")))
}
# add this column into the larger object
health_insurance$client_id <- a
# clean up column names
colnames(health_insurance) <- c(
  "client_id" = "Client_ID",
  "system_id" = "Health_Insurance_ID",
  "hudhealthinsurancesubastart" = "Health_Insurance_Start_Date",
  "hudhealthinsurancesubaend" = "Health_Insurance_End_Date",
  "svphudhealthinsurancetype" = "Health_Insurance_Type"
)
# clean up the house
rm(cols, y, health_insurance_node, a, i)

# Timing ------------------------------------------------------------------
end <- now()
print(list("load xml file", provider_start - begin))
print(list("provider records", provider_CoC_start - provider_start))
print(list("provider CoC records", funding_source_start - provider_CoC_start))
print(list("funding", provider_address_start - funding_source_start))
print(list("addresses", provider_inventory_start - provider_address_start))
print(list("provider inventory", ee_start - provider_inventory_start))
print(list("entry exits", interims_start - ee_start))
print(list("interims", client_start - interims_start))
print(list("clients", assessments_start - client_start))
print(list("assessments", needs_start - assessments_start))
print(list("needs", services_start - needs_start))
print(list("services", income_start - services_start))
print(list("income", noncash_start - income_start))
print(list("noncash", disabilities_start - noncash_start))
print(list("disabilities", h_ins_start - disabilities_start))
print(list("health insurance", end - h_ins_start))
print(list("all the whole thing", end - begin))
rm(begin, provider_start, provider_CoC_start, provider_inventory_start, ee_start, interims_start,
   client_start, assessments_start, funding_source_start, provider_address_start, needs_start, 
   services_start, income_start, noncash_start, disabilities_start, h_ins_start, end)
