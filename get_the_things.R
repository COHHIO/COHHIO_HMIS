library("xml2")
library("tidyverse")
library("lubridate")
print(now())
y <- read_xml("data/Bowman_Payload_41.xml")

# LIST OF THINGS
# unable to connect assessment data to a client
# add ee_id to interims
# also assessment data is creating duplicate columns across the top for stacked answers. :(
# will need to pull in User Created for each Entry Exit ID and it will have to come from Qlik or somewhere
# how do I run this in Terminal so I can do other things in R while this runs? (saw a blog about it)
# can we get the perl script to also de-identify the data after checking for some things? Like incorrect SSN, Anonymous fname..?

xml_to_df <- function(xml, path_name, cols) {
  records <- xml_find_all(xml, xpath = path_name)
  df <- as.data.frame(setNames(replicate(length(cols), character(0), simplify = F), cols))
  for(child in records){
    child_ <-  xml_children(child)
    data  <-  xml_text(child_)
    names(data) <- xml_name(child_)
    data  <- data[names(data) %in% cols]
    df <- bind_rows(df, data.frame(as.list(data)))
  }
  return(df)
}

# Provider records --------------------------------------------------------
# name nodes we want to pull
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
providers <- xml_to_df(y, "//*/Provider", cols)

# get ids, add them to the data frame
providers$record_id <- parse_number(xml_text(xml_find_all(y, "//records/providerRecords/Provider/@record_id")))

# clean up column names
colnames(providers) <- c(
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
providers <- providers %>% mutate(
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
provider_cocs <- xml_to_df(y, "//*/ProviderCOCCode", cols)
# clean up column names
colnames(provider_cocs) <- c(
  "provider" = "Provider_ID",
  "startDate" = "CoC_Start",
  "endDate" = "CoC_End",
  "cocCode" = "CoC_Code",
  "geographyType" = "Geography_Type",
  "postalCode" = "ZIP",
  "geocode" = "Geocode"
)
# clean up data to match with HUD CSV specs and make id field numeric
provider_cocs <- provider_cocs %>%
  mutate(
    Provider_ID = as.numeric(str_extract(Provider_ID, "[0-9]+")),
    Geography_Type = case_when(
      Geography_Type == "urban" ~ 1,
      Geography_Type == "suburban" ~ 2,
      Geography_Type == "rural" ~ 3,
      is.na(Geography_Type) ~ 99
    )
  )

# Provider Inventory Records ----------------------------------------------
# name nodes we want to pull in
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
bed_inventory <- xml_to_df(y, "//records/bedUnitInventoryRecords/BedUnitInventory[active = 'true']", cols)
# clean up column names
colnames(bed_inventory) <- c(
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
bed_inventory <- bed_inventory %>%
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
entry_exits <- xml_to_df(y, "//records/entryExitRecords/EntryExit[active = 'true']", cols)
# get attributes to the data frame
entry_exits$system_id <- parse_number(xml_text(xml_find_all(y, "//records/entryExitRecords/EntryExit[active = 'true']/@system_id")))
entry_exits$date_added <- xml_text(xml_find_all(y, "//records/entryExitRecords/EntryExit[active = 'true']/@date_added"))
# clean up column names
colnames(entry_exits) <- c(
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
entry_exits <- entry_exits %>% mutate(
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
  )
)


# Interims ----------------------------------------------------------------
cols <- c(
  "reviewDate",
  "reviewType",
  "active",
  "review_id",
  "ee_id"
)

interims <- xml_to_df(y, "//records/entryExitRecords/EntryExit[active = 'true']/childEntryExitReview/EntryExitReview", cols)
# getting record id's of the interim records
interims$review_id <- parse_number(xml_text(xml_find_all(y, "//records/entryExitRecords/EntryExit[active = 'true']/childEntryExitReview/EntryExitReview/@system_id")))
# establishing the node we're counting up from
interim_node <- xml_find_all(y, "//records/entryExitRecords/EntryExit[active = 'true']/childEntryExitReview/EntryExitReview")
# going up the tree from there, stopping at ee's
ee_as_gparent <- xml_parent(xml_parent(interim_node))
# grabbing the attribute that's at those particular ee's
interims$ee_id <- parse_number(xml_attr(ee_as_gparent, "record_id"))
# clean up the house
rm(interim_node, ee_as_gparent)

# Client Records ----------------------------------------------------------
# name nodes we want to pull in
cols <- c(
  "record_id",
  "firstName",
  "socSecNoDashed",
  "ssnDataQualityValue",
  "nameDataQualityValue",
  "veteranStatus"
)
# run function to get xml to a dataframe
clients <- xml_to_df(y, "//records/clientRecords/Client", cols)
# get ids
clients$record_id <- parse_number(xml_text(xml_find_all(y, "//records/clientRecords/Client/@record_id")))
# clean up data to match with HUD CSV specs and make id field numeric
clients <- clients %>% mutate(
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
colnames(clients) <- c(
  "record_id" = "Client_ID",
  "firstName" = "First_Name",
  "socSecNoDashed" = "SSN",
  "ssnDataQualityValue" = "SSN_DQ",
  "nameDataQualityValue" = "Name_DQ",
  "veteranStatus" = "Veteran_Status"  
)
# Assessment Records ------------------------------------------------------
# name nodes we want to pull in
cols <- c(
  "record_id",
  "hud_housingmoveindate",
  "date_added",
  "date_effective"
)

print(now())
#returning 0 records
move_in_date <- xml_to_df(y, "//records/clientRecords/Client/assessmentData/hud_housingmoveindate", cols)

move_in_date_nodes <-
  xml_find_all(y, xpath = "//records/clientRecords/Client/assessmentData/hud_housingmoveindate")
df <- as.data.frame(setNames(replicate(length(cols), character(0), simplify = F), cols))
dates <- as.data.frame(xml_text(move_in_date_nodes), col.names = "hud_housingmoveindate")

#adding an extra column with a garbage name
df <- bind_rows(df, dates)

client_as_gparent <- xml_parent(xml_parent(move_in_date_nodes))
#move_in_date$hud_housingmoveindate <- xml_text(move_in_date_nodes)
move_in_date$record_id <- parse_number(xml_attr(client_as_gparent, "record_id"))
move_in_date$date_added <- xml_text(xml_find_all(y, "//records/clientRecords/Client/hud_housingmoveindate/@date_added"))
move_in_date$date_effective <- xml_text(xml_find_all(y, "//records/clientRecords/Client/hud_housingmoveindate/@date_effective"))

print(now())

df <- as.data.frame(setNames(replicate(length(cols), character(0), simplify = F), cols))
i <- 1
for(child in records){
  child_ = xml_children(child) #this is too slow
  assessment <- child_[length(child_)] # this is also slow
  df2 <- as.data.frame(setNames(replicate(length(cols), character(0), simplify = F), cols))
  idx <- c()
  effective_dates <- xml_text(xml_find_all(xml_children(assessment), "@date_effective"))
  for(child2 in assessment) {
    child__ <- xml_children(child2)
    data = xml_text(child__)
    names(data) <- xml_name(child__)
    idx <- which(names(data) %in% cols)
    data = data[idx]
    effective_dates <- effective_dates[idx]
    for(i in 1:length(data)) {
      names(effective_dates)[i] <- paste0(names(data)[i], ".effective")
    }
    data <- c(data, effective_dates)
    df2 <- bind_rows(df2, data.frame(as.list(data)))
  }
  df2$record_id = rep(ids[i], nrow(df2))
  df <- bind_rows(df, df2)
  print(i)
  i <- i + 1
}
assessment_data <- df
# clean up column names

# clean up data to match with HUD CSV specs and make id field numeric
z <- xml_find_all(y, xpath = "//records/clientRecords/Client/assessmentData")

# Needs ------------------------------------------------------------
# name nodes we want to pull in
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
                client = as.numeric(str_extract(client, "[0-9]+")),
                provider = as.numeric(str_extract(provider, "[0-9]+")))

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
cols <- c(
  "record_id",
  "client",
  "need",
  "needServiceGroup",
  "group",
  "referfromProvider",
  "serviceProvided",
  "provideProvider",
  "code",
  "provideStartDate",
  "provideEndDate",
  "household",
  "serviceNote"
)
# run function to get xml to a dataframe (I think there are no inactive records coming in)
services_referrals <- xml_to_df(y, "//records/needRecords/Need/childService/Service", cols)
# clean up column names
services_referrals$record_id <- parse_number(xml_text(xml_find_all(y, "//records/needRecords/Need/childService/Service/@record_id")))
# make the Client IDs and Provider IDs numeric
needs <- mutate(needs, 
                client = parse_number(client),
                provider = parse_number(provider))
# clean up column names
colnames(services_referrals) <- c(
  "record_id" = "Service_Referral_ID",
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
# Income ------------------------------------------------------------------
# name nodes we want to pull in
cols <- c(
  "system_id",
  "amountmonthlyincome",
  "monthlyincomestart",
  "monthlyincomeend",
  "sourceofincome"
)
# run function to get xml to a dataframe
income <- xml_to_df(y, "//records/clientRecords/Client/assessmentData/monthlyincome[svp_receivingincomesource ='yes']", cols)
# get ids
income$system_id <- parse_number(xml_text(xml_find_all(y, "//records/clientRecords/Client/assessmentData/monthlyincome[svp_receivingincomesource ='yes']/@system_id")))
# clean up column names
colnames(income) <- c(
  "system_id" = "Income_ID",
  "amountmonthlyincome" = "Income_Amount",
  "monthlyincomestart" = "Income_Start",
  "monthlyincomeend" = "Income_End",
  "sourceofincome" = "Income_Source"
  )

# Non Cash ----------------------------------------------------------------
# name nodes we want to pull in
cols <- c(
  "system_id",
  "svp_noncashbenefitssource",
  "svp_noncashbenefitsstart",
  "svp_noncashbenefitsend"
)
# run function to get xml to a dataframe
noncash <- xml_to_df(y, "//records/clientRecords/Client/assessmentData/svp_noncashbenefits[svp_receivingbenefit = 'yes']", cols)
# get ids to data frame
noncash$system_id <- parse_number(xml_text(xml_find_all(y, "//records/clientRecords/Client/assessmentData/svp_noncashbenefits[svp_receivingbenefit = 'yes']/@system_id")))
# clean up column names
colnames(noncash) <- c(
  "system_id" = "NCB_ID",
  "svp_noncashbenefitssource" = "NCB_Source",
  "svp_noncashbenefitsstart" = "NCB_Start_Date",
  "svp_noncashbenefitsend" = "NCB_End_Date"
)

# Disabilities -----------------------------------------------------------
# name nodes we want to pull in
cols <- c(
  "system_id",
  "disabilities_1start",
  "disabilities_1end",
  "disabilitytype",
  "hud_impairabilityliveind"
)
# run function to get xml to a dataframe
disabilities <- xml_to_df(y, "//records/clientRecords/Client/assessmentData/disabilities_1[disabilitydetermine = 'yes (hud)']", cols)
# get ids to data frame
disabilities$system_id <- parse_number(xml_text(xml_find_all(y, "//records/clientRecords/Client/assessmentData/disabilities_1[disabilitydetermine = 'yes (hud)']/@system_id")))
# rename columns
colnames(disabilities) <- c(
  "system_id" = "Disability_ID",
  "disabilities_1start" = "Disability_Start_Date",
  "disabilities_1end" = "Disability_End_Date",
  "disabilitytype" = "Disability_Type",
  "hud_impairabilityliveind" = "Long_Duration"
)
# clean up data to match with HUD CSV specs 
disabilities <- mutate(
  Long_Duration = case_when(
    Long_Duration == "yes (hud)" ~ 1,
    Long_Duration == "no (hud)" ~ 0,
    Long_Duration == "client doesn't know (hud)" ~ 8,
    Long_Duration == "client refused (hud)" ~ 9,
    Long_Duration == "data not collected (hud)" |
      is.na(Long_Duration) ~ 99
  )
)
# Health Insurance -------------------------------------------------------
# name nodes we want to pull in
cols <- c(
  "system_id",
  "hudhealthinsurancesubastart",
  "hudhealthinsurancesubaend",
  "svphudhealthinsurancetype"
)
# run function to get xml to a dataframe
health_insurance <- xml_to_df(y, "//records/clientRecords/Client/assessmentData/hudhealthinsurancesuba[svphudhealthinscovered = 'yes']", cols)
# get ids to data frame
health_insurance$system_id <- parse_number(xml_text(xml_find_all(y, "//records/clientRecords/Client/assessmentData/hudhealthinsurancesuba[svphudhealthinscovered = 'yes']/@system_id")))
# clean up column names
colnames(health_insurance) <- c(
  "system_id" = "Health_Insurance_ID",
  "hudhealthinsurancesubastart" = "Health_Insurance_Start_Date",
  "hudhealthinsurancesubaend" = "Health_Insurance_End_Date",
  "svphudhealthinsurancetype" = "Health_Insurance_Type"
)
# clean up data to match with HUD CSV specs and make id field numeric
health_insurance <- mutate(health_insurance,
                           Health_Insurance_Type = case_when(
                             Health_Insurance_Type == "employer - provided health insurance" ~ 1,
                             Health_Insurance_Type == "health insurance obtained through cobra" ~ 1,
                             Health_Insurance_Type == "indian health services program" ~ 1,
                             Health_Insurance_Type == "medicaid" ~ 1,
                             Health_Insurance_Type == "medicare" ~ 1,
                             Health_Insurance_Type == "other" ~ 1,
                             Health_Insurance_Type == "private pay health insurance" ~ 1,
                             Health_Insurance_Type == "state children's health insurance program" ~ 1,
                             Health_Insurance_Type == "state health insurance for adults" ~ 1,
                             Health_Insurance_Type == "veteran's administration (va) medical services" ~ 1
                           ))
rm(cols,ids, y)
print(now())


a <- 1:5
tibble(a, b = a * 2)
tibble(a, b = a * 2, c = 1)
tibble(x = runif(10), y = x * 2)
