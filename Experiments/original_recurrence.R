

library(tidyverse)
library(lubridate)
library(dplyr)

load("images/all_data.RData")

##  set up definitions and initial dataframe, adjust as needed
all_program_types <- c("Transitional housing", "PH - Permanent Supportive Housing (disability required for entry)",
                       "PH – Housing Only", "PH – Housing with Services (no disability required for entry)",
                       "Emergency shelter", "RETIRED", "Services Only", "Homelessness Prevention", "Street outreach",
                       "PH - Rapid Re-Housing",  "Day Shelter", "Safe Haven", "Coordinated Entry")   

return_program_types <- c("Transitional housing", "PH - Permanent Supportive Housing (disability required for entry)",
                          "PH – Housing Only", "PH – Housing with Services (no disability required for entry)",
                          "Emergency shelter", "Street outreach", "PH - Rapid Re-Housing", "Safe Haven",  "Coordinated Entry")

housing_program_types <- c("Transitional housing", "PH - Permanent Supportive Housing (disability required for entry)",
                           "PH – Housing Only", "PH – Housing with Services (no disability required for entry)",
                           "PH - Rapid Re-Housing")

ph_program_types <- c("PH - Permanent Supportive Housing (disability required for entry)", "PH – Housing Only", 
                      "PH – Housing with Services (no disability required for entry)", "PH - Rapid Re-Housing")

permanent_destinations <- c("Rental by client, no ongoing housing subsidy",
                            "Rental by client, other ongoing housing subsidy",
                            "Rental by client in a public housing unit",
                            "Permanent housing (other than RRH) for formerly homeless persons",
                            "Rental by client, VASH housing Subsidy",
                            "Moved from one HOPWA funded project to HOPWA PH",
                            "Rental by client, with GPD TIP housing subsidy",
                            "Owned by client, no ongoing housing subsidy",
                            "Rental by client with RRH or equivalent subsidy",
                            "Rental by client, with HCV voucher (tenant or project based)",
                            "Owned by client, with ongoing housing subsidy",
                            "Staying or living with family, permanent tenure",
                            "Staying or living with friends, permanent tenure")

##  Update to include diversion
include_diversion = TRUE
if (include_diversion) {
  
  all_data <- all_data %>%
    mutate(ProgramType = 
             if_else(ProgramName == "Diversion" 
                     | ProgramName == "IHN - Diversion"
                     | ProgramName == "YHDP - Diversion",
                     "Diversion", ProgramType)
    )
  
  return_program_types <- append(return_program_types, "Diversion")
  
}

df_for_returns <- all_data %>%
  filter(ProgramType %in% return_program_types) %>%
  select(ClientID, EnrollID, EnrollDate, ExitDate, ProgramType, ExitDestination) %>%
  mutate(two_weeks_after_exit = if_else(!is.na(ExitDate), ExitDate + ddays(14), NULL))

##  find all exits from TH or PH programs
housing_exits <- df_for_returns %>%
  filter(ProgramType %in% housing_program_types,
         !is.na(ExitDate)) %>%
  setNames(paste("H_Ex", colnames(df_for_returns), sep = "_"))

##GD: why exclude ES/SH/Outreach from exits to 

##  find all entries to PH programs
ph_enrollments <- df_for_returns %>%
  filter(ProgramType %in% ph_program_types) %>%
  setNames(paste("PH_En", colnames(df_for_returns), sep = "_")) %>%
  select(-PH_En_two_weeks_after_exit)

##  identify all PH enrollments within 14 days of a TH or PH exit
excluded_PH_entries <- ph_enrollments %>%
  left_join(housing_exits, by = c("PH_En_ClientID" = "H_Ex_ClientID")) %>%
  filter(PH_En_EnrollDate >= H_Ex_ExitDate &
           PH_En_EnrollDate <= H_Ex_two_weeks_after_exit) %>%
  select(PH_En_EnrollID) %>%
  distinct()

##  remove enrollments identified above from enrollments used to flag returns
returning_entries <- df_for_returns %>%
  anti_join(excluded_PH_entries, by = c("EnrollID" = "PH_En_EnrollID")) %>%
  setNames(paste("R_En", colnames(df_for_returns), sep = "_"))

##  get all enrollments with permanent exits
permanent_exits <- df_for_returns %>%
  filter(ExitDestination %in% permanent_destinations) %>%
  setNames(paste("PEx", colnames(df_for_returns), sep = "_")) %>%
  mutate(two_years_after_exit = PEx_ExitDate + dyears(2))

## create flag for all enrollments with a qualifying returning entry
return_flags <- permanent_exits %>%
  left_join(returning_entries, by = c("PEx_ClientID" = "R_En_ClientID")) %>%
  group_by(PEx_EnrollID) %>%
  mutate(return_flag = 
           if_else(
             R_En_EnrollID != PEx_EnrollID &
               ((R_En_ProgramType %in% housing_program_types &
                   R_En_EnrollDate >= PEx_two_weeks_after_exit &
                   R_En_EnrollDate <= two_years_after_exit) |
                  (!R_En_ProgramType %in% housing_program_types &
                     R_En_EnrollDate >= PEx_ExitDate &
                     R_En_EnrollDate <= two_years_after_exit)),
             1, 0
           ),
         return_flag = if_else(is.na(max(return_flag)), 0, max(return_flag))) %>%
  ungroup() %>%
  select(PEx_EnrollID, return_flag) %>%
  distinct() %>%
  rename(EnrollID = PEx_EnrollID)


rm(list = ls()[!(ls() %in% c("return_flags"))])
save.image("images/return_flags.RData")


