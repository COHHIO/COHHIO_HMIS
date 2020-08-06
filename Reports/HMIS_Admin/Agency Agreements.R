
# prep

library(tidyverse)
library(lubridate)
library(here)

load(here("images/COHHIOHMIS.RData"))

# agencies with HMIS-participating projects under them

agencies <- Project %>%
  select(
    ProjectID,
    OrganizationID,
    OperatingStartDate,
    OperatingEndDate,
    HMISParticipatingProject
  ) %>%
  filter(HMISParticipatingProject == 1 &
           !is.na(OperatingStartDate) &
           is.na(OperatingEndDate)) %>%
  left_join(Organization, by = "OrganizationID") %>%
  select(OrganizationName) %>%
  unique()

# agency agreements currently in effect

raw_agreements <- read_csv(here("Reports/HMIS_Admin/agency_agreements.csv"))

current_agreements <- raw_agreements %>%
  mutate(DateSubmitted = mdy(DateSubmitted),
         AgreementStartDate = mdy(AgreementStartDate),
         AgreementEndDate = mdy(AgreementEndDate)) %>%
  filter(ymd(AgreementStartDate) < today() &
           (ymd(AgreementEndDate) >= today() |
           is.na(AgreementEndDate)))

# agencies with no current agency agreement

all_agencies <- agencies %>%
  left_join(current_agreements, by = c("OrganizationName" = "Name")) %>%
  arrange(OrganizationName) %>%
  select(1, 3, 4)

# write it out

write_csv(all_agencies, here("Reports/HMIS_Admin/AgencyAgreementsToAW.csv"))

