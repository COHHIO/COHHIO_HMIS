library("xml2")
library("tidyverse")
library("lubridate")
library("readxl")
library("data.table")
begin <- now()
# pulls in the XML file which comes from the ServicePoint export
y <- read_xml("data/Bowman_Payload_72.xml")
# all other data comes from the RMisc ART report
users <- read_xlsx("data/RMisc.xlsx",
                  sheet = 4,
                  range = cell_cols("A:G"))
scores <- read_xlsx("data/RMisc.xlsx",
                   sheet = 1,
                   range = cell_cols("A:E"))
counties <- read_xlsx("data/RMisc.xlsx",
                     sheet = 2,
                     range = cell_cols("A:C"))
usercreating <- read_xlsx("data/RMisc.xlsx",
                         sheet = 3,
                         range = cell_cols("A:B"))
# this function turns the XML data into data frames
xml_to_df <- function(xml, path_name, cols) {
  records <- xml_find_all(xml, xpath = path_name)
  df <- data.table(setNames(replicate(length(cols), character(0), simplify = F), cols))
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
Project$record_id <- parse_number(xml_text(xml_find_all(y, "//records/providerRecords/Provider/@record_id")))
# clean up column names
colnames(Project) <- c(
  "record_id" = "ProjectID",
  "name" = "ProjectName",
  "aka" = "ProjectCommonName",
  "customHudOrganizationName" = "OrganizationName",
  "programTypeCodeValue" = "ProjectType",
  "hudHousingType" = "HousingType",
  "hudOrganization" = "OrganizationID",
  "continuumFlag" = "ContinuumProject",
  "affiliatedResidentialProject" = "ResidentialAffiliation",
  "principalSite" = "PrincipalSite",
  "hudTrackingMethod" = "TrackingMethod",
  "operatingStartDate" = "OperatingStartDate",
  "operatingEndDate" = "OperatingEndDate",
  "targetPopValue" = "TargetPopulation",
  "victimServiceProvider" = "VictimServicesProvider"
)
# update data to match the HUD CSV specs 
Project <- Project %>% mutate(
  ProjectType = case_when(
    ProjectType == "emergency shelter (hud)" ~ 1,
    ProjectType == "transitional housing (hud)" ~ 2,
    ProjectType == "ph - permanent supportive housing (disability required for entry) (hud)" ~ 3,
    ProjectType == "street outreach (hud)" ~ 4,
    ProjectType == "services only (hud)" ~ 6,
    ProjectType == "other (hud)" ~ 7,
    ProjectType == "safe haven (hud)" ~ 8,
    ProjectType == "ph - housing only (hud)" ~ 9,
    ProjectType == "ph - housing with services (hud)" ~ 10,
    ProjectType == "day shelter (hud)" ~ 11,
    ProjectType == "homelessness prevention (hud)" ~ 12,
    ProjectType == "ph - rapid re-housing (hud)" ~ 13,
    ProjectType == "coordinated assessment (hud)" ~ 14
  ),
  TrackingMethod = case_when(
    TrackingMethod == "entry/exit date" & ProjectType == 1 ~ 0,
    TrackingMethod == "service transaction model" & ProjectType == 1 ~ 3
  ),
  TargetPopulation = case_when(
    TargetPopulation == "dv: domestic violence victims" ~ 1,
    TargetPopulation == "hiv: persons with hiv/aids" ~ 3,
    TargetPopulation == "na: not applicable" ~ 4
  ),
  HousingType = case_when(
    HousingType == "site-based - single site" & ProjectType %in% c(1, 2, 3, 8, 9, 10, 13) ~ 1,
    HousingType == "site-based - clustered / multiple sites" & ProjectType %in% c(1, 2, 3, 8, 9, 10, 13) ~ 2,
    HousingType == "tenant-based - scattered site" & ProjectType %in% c(1, 2, 3, 8, 9, 10, 13) ~ 3
  ),
  VictimServicesProvider = case_when(
    VictimServicesProvider == "true" ~ 1,
    VictimServicesProvider == "false" ~ 0,
    is.na(VictimServicesProvider) ~ 99
  ),
  ContinuumProject = case_when(
    ContinuumProject == "true" ~ 1,
    ContinuumProject == "false" ~ 0,
    is.na(ContinuumProject) ~ 99
  ),
  ResidentialAffiliation = case_when(
    ResidentialAffiliation == "true" & ProjectType == 6 ~ 1,
    ResidentialAffiliation == "false" & ProjectType == 6 ~ 0,
    is.na(ResidentialAffiliation) & ProjectType == 6 ~ 99
  ),
  PrincipalSite = case_when(
    PrincipalSite == "true" ~ 1,
    PrincipalSite == "false" ~ 0,
    is.na(PrincipalSite) ~ 99
  ),
  OperatingStartDate = with_tz(ymd_hms(OperatingStartDate)),
  OperatingEndDate = with_tz(ymd_hms(OperatingEndDate)),
  OrganizationID = parse_number(OrganizationID)
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
  "provider" = "ProjectID",
  "startDate" = "CoCStart", 
  "endDate" = "CoCEnd", 
  "cocCode" = "CoCCode",
  "geographyType" = "GeographyType",
  "postalCode" = "ZIP",
  "geocode" = "Geocode"
)
# update data to match the HUD CSV specs, make IDs numeric, and convert dates to EST
ProjectCoC <- ProjectCoC %>%
  mutate(
    ProjectID = parse_number(ProjectID),
    GeographyType = case_when(
      GeographyType == "urban" ~ 1,
      GeographyType == "suburban" ~ 2,
      GeographyType == "rural" ~ 3,
      is.na(GeographyType) ~ 99
    ),
    CoCStart = with_tz(ymd_hms(CoCStart)),
    CoCEnd = with_tz(ymd_hms(CoCEnd))
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
# run function to get xml to a dataframe
Funder <- xml_to_df(y, "//records/providerRecords/Provider/childProviderFedPartnerFundingSource/*[active = 'true']", cols)
# clean up column names
colnames(Funder) <- c(
  "provider" = "ProjectID",
  "grantStartDate" = "StartDate",
  "grantEndDate" = "EndDate",
  "federalPartnerProgram" = "Funder"
)
# clean up data to match with HUD CSV specs, make id field numeric, convert dates to EST
Funder <- Funder %>%
  mutate(
    ProjectID = parse_number(ProjectID),
    Funder = case_when(
      Funder == "hud:coc - homelessness prevention (high performing comm. only)" ~ 1,
      Funder == "hud:coc - permanent supportive housing" ~ 2,
      Funder == "hud:coc - rapid re‐housing" ~ 3,
      Funder == "hud:coc - supportive services only" ~ 4,
      Funder == "hud:coc - transitional housing" ~ 5,
      Funder == "hud:coc - safe haven" ~ 6,      
      Funder == "hud:coc - single room occupancy (sro)" ~ 7,
      Funder == "hud:esg - emergency shelter (operating and/or essential services)" ~ 8,
      Funder == "hud:esg - homelessness prevention" ~ 9,      
      Funder == "hud:esg - rapid rehousing" ~ 10,
      Funder == "hud:esg - street outreach" ~ 11,
      Funder == "hud:rural housing stability assistance program" ~ 12,      
      Funder == "hud:hopwa - hotel/motel vouchers" ~ 13,
      Funder == "hud:hopwa - housing information" ~ 14,
      Funder == "hud:hopwa - permanent housing (facility based or tbra)" ~ 15,      
      Funder == "hud:hopwa - permanent housing placement" ~ 16,
      Funder == "hud:hopwa - short‐term rent, mortgage, utility assistance" ~ 17,
      Funder == "hud:hopwa - short‐term supportive facility" ~ 18,      
      Funder == "hud:hopwa - transitional housing (facility based or tbra)" ~ 19,
      Funder == "hud:hud/vash" ~ 20,
      Funder == "hhs:path - street outreach & supportive services only" ~ 21,      
      Funder == "hhs:rhy - basic center program (prevention and shelter)" ~ 22,
      Funder == "hhs:rhy - maternity group home for pregnant and parenting youth" ~ 23,
      Funder == "hhs:rhy - transitional living program" ~ 24,      
      Funder == "hhs:rhy - street outreach project" ~ 25,
      Funder == "hhs:rhy - demonstration project" ~ 26,
      Funder == "va: crs contract residential services" ~ 27,      
      Funder == "va:community contract safe haven program" ~ 30,
      Funder == "va compensated work therapy transitional residence" ~ 32,
      Funder == "va:supportive services for veteran families" ~ 33,      
      Funder == "n/a" ~ 34,
      Funder == "hud:pay for success" ~ 35,
      Funder == "hud:public and indian housing (pih) programs" ~ 36,      
      Funder == "va:grant per diem - bridge housing" ~ 37,
      Funder == "va:grant per diem - low demand" ~ 38,
      Funder == "va:grant per diem - hospital to housing" ~ 39,      
      Funder == "va:grant per diem - clinical treatment" ~ 40,
      Funder == "va:grant per diem - service intensive transitional housing" ~ 41,
      Funder == "va:grant per diem - transition in place" ~ 42,
      Funder == "hud:coc - youth homeless demonstration program (yhdp)"~ 43
    ),
    StartDate = with_tz(ymd_hms(StartDate)),
    EndDate = with_tz(ymd_hms(EndDate))
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
  "provider" = "ProjectID",
  "line1" = "Address1", #is there an Address2 in the XML file? if so we should pull it in
  "city" = "City",
  "addressType" = "AddressType",
  "province" = "State",
  "postalCode" = "ZIP"
)
# make IDs numeric
Geography <- Geography %>%
  mutate(
    ProjectID = parse_number(ProjectID))
# Provider Inventory Records ----------------------------------------------
# name nodes we want to pull in
provider_inventory_start <- now()
cols <- c(
  "provider",
  "householdTypeValue",
  "bedTypeValue",
  "availabilityValue", #need to pull in Information Date
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
  "provider" = "ProjectID", 
  "householdTypeValue" = "HouseholdType", 
  "bedTypeValue" = "BedType", 
  "availabilityValue" = "Availability",
  "bedInventory" = "BedInventory", 
  "unitInventory" = "UnitInventory",
  "chBedInventory" = "CHBedInventory",
  "veteranBedInventory" = "VetBedInventory",
  "youthBedInventory" = "YouthBedInventory",
  "inventoryStartDate" = "InventoryStartDate",
  "inventoryEndDate" = "InventoryEndDate",
  "hmisBeds" = "HMISParticipatingBeds",
  "cocCode" = "CoCCode"
)
# clean up data to match with HUD CSV specs, make id field numeric, convert dates to EST
Inventory <- Inventory %>%
  mutate(
    ProjectID = parse_number(ProjectID),
    HouseholdType = case_when(
      HouseholdType == "households without children" ~ 1,
      HouseholdType == "households with at least one adult and one child" ~ 3,
      HouseholdType == "households with only children" ~ 4
    ),
    Availability = case_when(
      Availability == "year-round" ~ 1,
      Availability == "seasonal" ~ 2,
      Availability == "overflow" ~ 3
    ),
    BedType = case_when(
      BedType == "facility-based" ~ 1,
      BedType == "voucher" ~ 2,
      BedType == "other" ~ 3
    ),
    InventoryStartDate = with_tz(ymd_hms(InventoryStartDate)),
    InventoryEndDate = with_tz(ymd_hms(InventoryEndDate))
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
  "system_id" = "EnrollmentID",
  "date_added" = "DateCreated",
  "client" = "PersonalID",
  "typeEntryExit" = "EEType",
  "group" = "HouseholdID",
  "household" = "FamilyID",
  "provider" = "ProjectID",
  "entryDate" = "EntryDate",
  "exitDate" = "ExitDate",
  "destinationValue" = "Destination"
)
# clean up data to match with HUD CSV specs, make id field numeric, convert dates to EST
Enrollment <- Enrollment %>% mutate(
  Destination = case_when(
    Destination == "emergency shelter, including hotel or motel paid for with emergency shelter voucher (hud)" &
      !is.na(ExitDate) ~  1,
    Destination == "transitional housing for homeless persons (including homeless youth) (hud)" &
      !is.na(ExitDate) ~ 2,
    Destination == "permanent housing (other than rrh) for formerly homeless persons (hud)" &
      !is.na(ExitDate) ~ 3,
    Destination == "psychiatric hospital or other psychiatric facility (hud)" &
      !is.na(ExitDate) ~ 4,
    Destination == "substance abuse treatment facility or detox center (hud)" &
      !is.na(ExitDate) ~ 5,
    Destination == "hospital or other residential non-psychiatric medical facility (hud)" &
      !is.na(ExitDate) ~ 6,
    Destination == "jail, prison or juvenile detention facility (hud)" &
      !is.na(ExitDate) ~ 7,
    Destination == "client doesn't know (hud)" &
      !is.na(ExitDate) ~ 8,
    Destination == "client refused (hud)" &
      !is.na(ExitDate) ~ 9,
    Destination == "rental by client, no ongoing housing subsidy (hud)" &
      !is.na(ExitDate) ~ 10,
    Destination == "owned by client, no ongoing housing subsidy (hud)" &
      !is.na(ExitDate) ~ 11,
    Destination == "staying or living with family, temporary tenure (e.g., room, apartment or house)(hud)" &
      !is.na(ExitDate) ~ 12,
    Destination == "staying or living with friends, temporary tenure (e.g., room apartment or house)(hud)" &
      !is.na(ExitDate) ~ 13,
    Destination == "hotel or motel paid for without emergency shelter voucher (hud)" &
      !is.na(ExitDate) ~ 14,
    Destination == "foster care home or foster care group home (hud)" &
      !is.na(ExitDate) ~ 15,
    Destination == "place not meant for habitation (hud)" &
      !is.na(ExitDate) ~ 16,
    Destination == "other (hud)" &
      !is.na(ExitDate) ~ 17,
    Destination == "safe haven (hud)" &
      !is.na(ExitDate) ~ 18,
    Destination == "rental by client, with vash housing subsidy (hud)" &
      !is.na(ExitDate) ~ 19,
    Destination == "rental by client, with other ongoing housing subsidy (hud)" &
      !is.na(ExitDate) ~ 20,
    Destination == "owned by client, with ongoing housing subsidy (hud)" &
      !is.na(ExitDate) ~ 21,
    Destination == "staying or living with family, permanent tenure (hud)" &
      !is.na(ExitDate) ~ 22,
    Destination == "staying or living with friends, permanent tenure (hud)" &
      !is.na(ExitDate) ~ 23,
    Destination == "deceased (hud)" &
      !is.na(ExitDate) ~ 24,
    Destination == "long-term care facility or nursing home (hud)" &
      !is.na(ExitDate) ~ 25,
    Destination == "moved from one hopwa funded project to hopwa ph (hud)" &
      !is.na(ExitDate) ~ 26,
    Destination == "moved from one hopwa funded project to hopwa th (hud)" &
      !is.na(ExitDate) ~ 27,
    Destination == "rental by client, with gpd tip housing subsidy (hud)" &
      !is.na(ExitDate) ~ 28,
    Destination == "residential project or halfway house with no homeless criteria (hud)" &
      !is.na(ExitDate) ~ 29,
    Destination == "no exit interview completed (hud)" &
      !is.na(ExitDate) ~ 30,
    Destination == "rental by client, with rrh or equivalent subsidy (hud)" &
      !is.na(ExitDate) ~ 31,
    Destination == "data not collected (hud)" &
      !is.na(ExitDate) ~ 99 
  ),
  PersonalID = parse_number(PersonalID),
  HouseholdID = parse_number(HouseholdID), 
  FamilyID = parse_number(FamilyID),
  ProjectID = parse_number(ProjectID),
  HouseholdID = if_else(is.na(HouseholdID), EnrollmentID, HouseholdID), # this is the ART "Group UID"
  DateCreated = with_tz(ymd_hms(DateCreated)),
  EntryDate = with_tz(ymd_hms(EntryDate)),
  ExitDate = with_tz(ymd_hms(ExitDate)),
  ExitAdjust = if_else(is.na(ExitDate), now(), ExitDate),
  ExitAdjust = ymd_hms(ExitAdjust)
)
# add in UserCreating and County data
Enrollment <- Enrollment %>% 
  left_join(usercreating, by = "EnrollmentID") %>% 
  left_join(., counties, by = "EnrollmentID")
Enrollment <- data.table(Enrollment)
# Interims ----------------------------------------------------------------
interims_start <- now()
cols <- c(
  "reviewDate",
  "reviewType",
  "review_id",
  "ee_id"
)
# get data from the XML to a df
interims <- xml_to_df(y, "//records/entryExitRecords/EntryExit[active = 'true']/childEntryExitReview/EntryExitReview", cols)
# getting record id's of the interim records
interims$review_id <- parse_number(xml_text(xml_find_all(y, "//records/entryExitRecords/EntryExit[active = 'true']/childEntryExitReview/EntryExitReview/@system_id")))
# establishing the node we're counting up from
interim_node <- xml_find_all(y, "//records/entryExitRecords/EntryExit[active = 'true']/childEntryExitReview/EntryExitReview")
# grabbing the attribute that's at those particular ee's
ee_as_gparent <- xml_parent(xml_parent(interim_node))
# how many interims per ee?
length_interim <- sapply(xml_parent(interim_node), function(x) length(xml_children(x)))
# gets the ee ids
ee_id <- parse_number(xml_attr(ee_as_gparent, "record_id"))
# creates a df that tells us how many interims per ee id
ids <- data.frame(length_interim, ee_id)
# uses the info gained to repeat the number of ee ids for each interim
ee_ids <- c()
for(i in 1:nrow(ids)) {
  ee_ids <- c(ee_ids, rep(ids[i,]$ee_id, ids[i,]$length_interim))
}
# finally adds the ee ids to the interims df
interims$ee_id <- ee_ids
# clean up column names
colnames(interims) <- c("InterimDate", "InterimType", "InterimID", "EnrollmentID")
# clean up the house
rm(interim_node, ee_as_gparent, length_interim, ee_id, ids, ee_ids)
# adds Client IDs from the Enrollments table
x <- select(Enrollment, EnrollmentID, PersonalID)
interims <- inner_join(interims, x, by = "EnrollmentID") %>%
  mutate(InterimDate = with_tz(ymd_hms(InterimDate)))
rm(x)
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
  "record_id" = "PersonalID",
  "firstName" = "FirstName",
  "socSecNoDashed" = "SSN",
  "ssnDataQualityValue" = "SSNDataQuality",
  "nameDataQualityValue" = "NameDataQuality",
  "veteranStatus" = "VeteranStatus"  
)
# Strip of PII ------------------------------------------------------------

Client <- Client %>%
  mutate(FirstName = case_when(
    NameDataQuality %in% c(8,9) ~ "DKR",
    NameDataQuality == 2 ~ "Partial",
    NameDataQuality == 99 | is.na(NameDataQuality) | FirstName == 0 ~ "Missing",
    !(NameDataQuality %in% c(2, 8, 9, 99) | is.na(NameDataQuality) | FirstName == "Anonymous") ~ "ok"))

Client <- Client %>%
  mutate(SSN = case_when(
    is.na(SSN) | is.na(SSNDataQuality) | SSNDataQuality == 99 ~ "Missing",
    SSNDataQuality %in% c(8, 9) ~ "DKR",
    ifelse((
      substr(SSN, 1, 1) != "0" &
        substr(SSN, 1, 2) != "00"
    ),
    nchar(as.numeric(SSN)) != 9, FALSE) |
      substr(SSN, 1, 3) %in% c("000", "666") |
      substr(SSN, 1, 1) == 9 |
      substr(SSN, 4, 5) == "00" |
      substr(SSN, 6, 9) == "0000" |
      SSNDataQuality == 2 |
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
r <- now()
# gets all the nodes under the "assessmentData" node
assessmentData_child_nodes <- xml_find_all(y, xpath = "//records/clientRecords/Client/assessmentData/*")
b <- now()
# using the above, grabs Client IDs
Client_ID_as_gparent <- xml_parent(xml_parent(assessmentData_child_nodes))
c <- now()
# each node's name
data_element <- xml_name(assessmentData_child_nodes)
d <- now()
# each node's value
value <- xml_text(assessmentData_child_nodes)
e <- now()
# each node's date eff
date_effective <- xml_attr(assessmentData_child_nodes, "date_effective")
f <- now()
# each node's date added
date_added <- xml_attr(assessmentData_child_nodes, "date_added")
g <- now()
# making Client ID numeric
Client_IDs <- parse_number(xml_attr(Client_ID_as_gparent, "record_id"))
h <- now()
# trying something
# tmp <- xml_parent(assessmentData_child_nodes)
# works out how many assessments per Client ID
length_assessments <- sapply(xml_parent(assessmentData_child_nodes), function(x) length(xml_children(x)))
j <- now()
# returns how many assessments per Client ID
ids <- data.frame(length_assessments, Client_IDs)
k <- now()
# uses the info from above to repeat the Client IDs however many times as needed
# THIS IS THE PART THAT TAKES A LONG TIME
# TRY THIS:           w <- integer(nrow(ids))
w <- c()
for(i in 1:nrow(ids)) {
  w <- c(w, rep(ids[i,]$Client_IDs, ids[i,]$length_assessments))
}
Client_IDs <- w
l <- now()
# putting it all together
assessment_data <- bind_cols(list(Client_IDs, data_element, value, date_effective, date_added))
m <- now()
# naming the columns
colnames(assessment_data) <- c(
  "client_id" = "PersonalID",
  "data_element" = "DataElement",
  "value" = "Value",
  "date_effective" =  "DateEffective",
  "date_added" = "DateAdded"
)
n <- now()
# converting dates to EST
assessment_data <- mutate(assessment_data,
                          DateEffective = with_tz(ymd_hms(DateEffective)),
                          DateAdded = with_tz(ymd_hms(DateAdded)))
o <- now()
# clean the house
rm(assessmentData_child_nodes,
   Client_ID_as_gparent,
   data_element,
   value,
   date_effective,
   date_added,
   w,
   Client_IDs,
   length_assessments,
   ids)
# delete records we don't need
assessment_data <- assessment_data %>%
  filter(!(
    DataElement %in%
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
p <- now()
assessment_data <- data.table(assessment_data)
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
# make the Client IDs and Provider IDs numeric and converting date to EST
needs <- mutate(needs, 
                client = parse_number(client),
                provider = parse_number(provider),
                dateSet = with_tz(ymd_hms(dateSet)))
# clean up column names
colnames(needs) <- c(
  "record_id" = "NeedID",
  "client" = "PersonalID",
  "provider" = "ProjectID",
  "group" = "HouseholdID",
  "dateSet" = "NeedDate",
  "status" = "NeedStatus",
  "outcome" = "NeedOutcome",
  "reasonUnmet" = "ReasonUnmet",
  "note" = "Note",
  "code" = "NeedCode"
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
# add service ids to the df
Services$record_id <- parse_number(xml_text(xml_find_all(y, "//records/needRecords/Need/childService/Service/@record_id")))
# make the Client IDs and Provider IDs numeric and convert dates to EST
Services <- mutate(Services, 
                client = parse_number(client),
                referfromProvider = parse_number(referfromProvider),
                needServiceGroup = parse_number(needServiceGroup),
                need = parse_number(need),
                provideProvider = parse_number(provideProvider),
                household = parse_number(household),
                provideStartDate = with_tz(ymd_hms(provideStartDate)),
                provideEndDate = with_tz(ymd_hms(provideEndDate)))
# clean up column names
colnames(Services) <- c(
  "record_id" = "ServicesID",
  "client" = "PersonalID",
  "need" = "NeedID",
  "needServiceGroup" = "NeedServiceGroup",
  "group" = "HouseholdID",
  "referfromProvider" = "ReferFromProjectID",
  "provideProvider" = "ProjectID",
  "code" = "NeedCode",
  "provideStartDate" = "ServiceStartDate",
  "provideEndDate" = "ServiceEndDate",
  "household" = "FamilyID",
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
# run function to get xml to a dataframe
IncomeBenefits <- xml_to_df(y, "//records/clientRecords/Client/assessmentData/monthlyincome[svp_receivingincomesource ='yes']", cols)
# income node to count up from
income_node <- xml_find_all(y, "//records/clientRecords/Client/assessmentData/monthlyincome[svp_receivingincomesource ='yes']")
# add sub ids to the df
IncomeBenefits$system_id <- parse_number(xml_attr(income_node, "system_id"))
# create an empty table
a <- c()
# populate the empty table with the client ids
for(i in 1:length(income_node)) {
  a <- c(a, parse_number(xml_attr(xml_parent(xml_parent(income_node[i])),"record_id")))
  }
# add client ids to the df
IncomeBenefits$client_id <- a
# clean up the house
rm(income_node, a)
# clean up column names
colnames(IncomeBenefits) <- c(
  "client_id" = "PersonalID",
  "system_id" = "IncomeBenefitsID",
  "amountmonthlyincome" = "IncomeAmount",
  "monthlyincomestart" = "IncomeStart",
  "monthlyincomeend" = "IncomeEnd",
  "sourceofincome" = "IncomeSource"
  )
# convert dates to EST
IncomeBenefits <- mutate(IncomeBenefits,
                         IncomeStart = with_tz(ymd_hms(IncomeStart)),
                         IncomeEnd = with_tz(ymd_hms(IncomeEnd)))
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
# run function to get xml to a dataframe
noncash <- xml_to_df(y, "//records/clientRecords/Client/assessmentData/svp_noncashbenefits[svp_receivingbenefit = 'yes']", cols)
# noncash node to count up from
noncash_node <- xml_find_all(y, "//records/clientRecords/Client/assessmentData/svp_noncashbenefits[svp_receivingbenefit = 'yes']")
# get ids to df
noncash$system_id <- parse_number(xml_attr(noncash_node, "system_id"))
# create an empty table
a <- c()
# populate the empty table with the client ids
for(i in 1:length(noncash_node)) {
  a <- c(a, parse_number(xml_attr(xml_parent(xml_parent(noncash_node[i])),"record_id")))
}
# add this column to the df
noncash$client_id <- a
# clean up the house
rm(noncash_node, a)
# clean up column names
colnames(noncash) <- c(
  "client_id" = "PersonalID",
  "system_id" = "NoncashID",
  "svp_noncashbenefitssource" = "NoncashSource",
  "svp_noncashbenefitsstart" = "NoncashStartDate",
  "svp_noncashbenefitsend" = "NoncashEndDate"
)
# convert dates to EST
noncash <- mutate(
  noncash,
  NoncashStartDate = as.Date(NoncashStartDate, "%Y-%m-%d", tz = "America/New_York"),
  NoncashEndDate = as.Date(NoncashEndDate, "%Y-%m-%d", tz = "America/New_York")
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
# run function to get xml to a dataframe
Disabilities <- xml_to_df(y, "//records/clientRecords/Client/assessmentData/disabilities_1[disabilitydetermine = 'yes (hud)']", cols)
# node to count from
disabilities_node <- xml_find_all(y, "//records/clientRecords/Client/assessmentData/disabilities_1[disabilitydetermine = 'yes (hud)']")
# get ids to df
Disabilities$system_id <- parse_number(xml_text(xml_find_all(y, "//records/clientRecords/Client/assessmentData/disabilities_1[disabilitydetermine = 'yes (hud)']/@system_id")))
# create an empty table
a <- c()
# populate the empty table with the client ids
for(i in 1:length(disabilities_node)) {
  a <- c(a, parse_number(xml_attr(xml_parent(xml_parent(disabilities_node[i])),"record_id")))
}
# add this column to the df
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
  ),
  disabilitytype = case_when(
    disabilitytype == "physical (hud)" ~ 5,
    disabilitytype == "developmental (hud)" ~ 6,
    disabilitytype == "chronic health condition (hud)" ~ 7,
    disabilitytype == "hiv/aids (hud)" ~ 8,
    disabilitytype == "mental health problem (hud)" ~ 9,
    disabilitytype %in% c("both alcohol and drug abuse (hud)",
                          "alcohol abuse (hud)",
                          "drug abuse (hud)") ~ 10 
  )
)
# clean up
rm(disabilities_node, a)
# rename columns
colnames(Disabilities) <- c(
  "client_id" = "PersonalID",
  "system_id" = "DisabilitiesID",
  "disabilities_1start" = "DisabilityStartDate",
  "disabilities_1end" = "DisabilityEndDate",
  "disabilitytype" = "DisabilityType",
  "hud_impairabilityliveind" = "IndefiniteAndImpairs"
)
# convert dates to EST
Disabilities <- mutate(Disabilities,
                       DisabilityStartDate = with_tz(ymd_hms(DisabilityStartDate)),
                       DisabilityEndDate = with_tz(ymd_hms(DisabilityEndDate)))

# Health Insurance -------------------------------------------------------
# name nodes we want to pull in (Goes in the IncomeAndBenefits table)
h_ins_start <- now()
cols <- c(
  "client_id",
  "system_id",
  "hudhealthinsurancesubastart",
  "hudhealthinsurancesubaend",
  "svphudhealthinsurancetype"
)

# run function to get xml to a dataframe
health_insurance <-
  xml_to_df(
    y,
    "//records/clientRecords/Client/assessmentData/hudhealthinsurancesuba[svphudhealthinscovered = 'yes']",
    cols
  )
# get node to start from
health_insurance_node <-
  xml_find_all(
    y,
    "//records/clientRecords/Client/assessmentData/hudhealthinsurancesuba[svphudhealthinscovered = 'yes']"
  )
# get sub ids to data frame
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
# add this column ito the larger object
health_insurance$client_id <- a
# clean up column names
colnames(health_insurance) <- c(
  "client_id" = "PersonalID",
  "system_id" = "HealthInsuranceID",
  "hudhealthinsurancesubastart" = "HealthInsuranceStartDate",
  "hudhealthinsurancesubaend" = "HealthInsuranceEndDate",
  "svphudhealthinsurancetype" = "HealthInsuranceType"
)
# clean up the house
rm(cols, health_insurance_node, a, i)
# convert dates to EST
health_insurance <- mutate(health_insurance,
                           HealthInsuranceStartDate = with_tz(ymd_hms(HealthInsuranceStartDate)),
                           HealthInsuranceEndDate = with_tz(ymd_hms(HealthInsuranceEndDate)))
# Timing ------------------------------------------------------------------
end <- now()
t <- as.data.frame(
  list(
    "load xml file" = provider_start - begin,
    "provider records" = provider_CoC_start - provider_start,
    "provider CoC records" = funding_source_start - provider_CoC_start,
    "funding" = provider_address_start - funding_source_start,
    "addresses" = provider_inventory_start - provider_address_start,
    "provider inventory" = ee_start - provider_inventory_start,
    "entry exits" = interims_start - ee_start,
    "interims" = client_start - interims_start,
    "clients" = assessments_start - client_start,
    "assessments" = needs_start - assessments_start,
    "needs" = services_start - needs_start,
    "services" = income_start - services_start,
    "income" = noncash_start - income_start,
    "noncash" = disabilities_start - noncash_start,
    "disabilities" = h_ins_start - disabilities_start,
    "health insurance" = end - h_ins_start,
    "all the whole thing" = end - begin
  ))
write_csv(t, "C:\\Users\\Public\\HMIS-Share\\timing.csv", append = TRUE)
# rm(begin, provider_start, provider_CoC_start, provider_inventory_start, ee_start, interims_start,
#    client_start, assessments_start, funding_source_start, provider_address_start, needs_start, 
#    services_start, income_start, noncash_start, disabilities_start, h_ins_start, end)
as.data.frame(
  c(
    "select col names" = r - assessments_start,
    "all nodes under assessmentData" = b - r,
    "grabs Client IDs" = c - b,
    "each node's name" = d - c,
    "each node's value" = e - d,
    "each node's date eff" = f - e,
    "each node's date added" = g - f,
    "making Client ID numeric" = h - g,
    "# assessments per Client ID" = j - h,
    "df assessments per Client ID" = k - j,
    "for loop repeat the Client IDs however many times" = l - k,
    "putting it all together" = m - l,
    "naming the columns" = n - m,
    "converting dates to EST" = o - n,
    "clean the house" = p - o,
    "setDT" = needs_start - p,
    "all" = needs_start - assessments_start
  ))
