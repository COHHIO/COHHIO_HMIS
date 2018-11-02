library("tidyverse")
library("lubridate")

# *** adding ExitDate to the rest of the Enrollment data 
entry_exits <- enrollment_csv %>%
  left_join(exit_csv, by = c("PersonalID", "EnrollmentID"))  %>%
  left_join(client_csv, by = "PersonalID") %>%
  left_join(project_csv, by = "ProjectID") %>%
  filter(ProjectType %in% c(1:6, 8:13)) %>%
  mutate(Age_At_Entry = as.period(interval(
    start = ymd(DOB), end = ymd(EntryDate)
  ))$year) %>%
  select(EnrollmentID,
         PersonalID,
         HouseholdID,
         ProjectID,
         ProjectType,
         RelationshipToHoH,
         Age_At_Entry,
         EntryDate,
         MoveInDate,
         ExitDate,
         Destination)

# *** households
households <-
  select(
    entry_exits,
    PersonalID,
    Age_At_Entry,
    HouseholdID,
    ProjectID,
    EnrollmentID,
    RelationshipToHoH,
    EntryDate
  ) 

# *** gathers all of the current enrollments (could be turned into a date range eventually)
current_enrollments <- filter(entry_exits,
                              ymd(EntryDate) < today() &
                                (ymd(ExitDate) > today() |
                                   is.na(ExitDate)))

# *** gathers all enrollments that were active during 2018

this_year_enrollments <- filter(entry_exits,
                                ymd(EntryDate) < today() &
                                  (ymd(ExitDate) > mdy(01012018) |
                                     is.na(ExitDate)))

# *** adds an Age_At_Entry column to enrollment data for easy filtering
current_adult_enrollments <- current_enrollments %>%
  left_join(client_csv, by = "PersonalID") %>%
  select(PersonalID, DOB, EntryDate, EnrollmentID, ProjectID) %>%
  mutate(Age_At_Entry = as.period(interval(
    start = ymd(DOB), end = ymd(EntryDate)
  ))$year) %>%
  filter(Age_At_Entry > 17)

# *** adds an Age_At_Entry column to enrollment data for easy filtering
this_years_adult_enrollments <- this_year_enrollments %>%
  left_join(client_csv, by = "PersonalID") %>%
  select(PersonalID, DOB, EntryDate, MoveInDate, ExitDate, EnrollmentID, ProjectID) %>%
  mutate(Age_At_Entry = as.period(interval(
    start = ymd(DOB), end = ymd(EntryDate)
  ))$year) %>%
  filter(Age_At_Entry > 17)

# ***gathers all current PH project IDs
current_permanent_housing_project_ids <- filter(
  project_csv,
  ProjectType %in% c(9, 10, 13) &
    ymd(OperatingStartDate) < today() &
    (is.na(OperatingEndDate) |
       ymd(OperatingEndDate) > today())
) %>%
  select(ProjectID, ProjectType)

# ***gathers all current PH project IDs
this_year_permanent_housing_project_ids <- filter(
  project_csv,
  ProjectType %in% c(9, 10, 13) &
    ymd(OperatingStartDate) < today() &
    (is.na(OperatingEndDate) |
       ymd(OperatingEndDate) > mdy(10012017))
) %>%
  select(ProjectID, ProjectType)

# ***gathers together important income data plus totals of subs
income <- this_years_adult_enrollments %>%
  left_join(income_benefits_csv, by = c("EnrollmentID", "PersonalID")) %>%
  select(
    PersonalID,
    ProjectID,
    EnrollmentID,
    Age_At_Entry,
    DataCollectionStage,
    EntryDate,
    ExitDate,
    IncomeFromAnySource,
    TotalMonthlyIncome,
    ends_with("Amount"),
    Earned,
    Unemployment,
    SSI,
    SSDI,
    VADisabilityNonService,
    VADisabilityService,
    PrivateDisability,
    WorkersComp,
    TANF,
    GA,
    SocSecRetirement,
    Pension,
    ChildSupport,
    Alimony,
    OtherIncomeSource
  ) %>%
  mutate(
    income_subs_total = 
      ifelse(is.na(EarnedAmount), 0, EarnedAmount) +
      ifelse(is.na(SSDIAmount), 0, SSDIAmount) +
      ifelse(is.na(SSIAmount), 0, SSIAmount) +
      # ifelse(is.na(UnemploymentAmount), 0, UnemploymentAmount) +
      ifelse(
        is.na(VADisabilityNonServiceAmount),
        0,
        VADisabilityNonServiceAmount
      ) +
      ifelse(
        is.na(VADisabilityServiceAmount),
        0,
        VADisabilityServiceAmount
      ) +
      # ifelse(is.na(PrivateDisabilityAmount), 0, PrivateDisabilityAmount) +
      # ifelse(is.na(WorkersCompAmount), 0, WorkersCompAmount) +
      ifelse(is.na(GAAmount), 0, GAAmount) +
      ifelse(is.na(TANFAmount), 0, TANFAmount) +
      ifelse(is.na(SocSecRetirementAmount), 0, SocSecRetirementAmount) +
      ifelse(is.na(PensionAmount), 0, PensionAmount) +
      ifelse(is.na(ChildSupportAmount), 0, ChildSupportAmount) +
      ifelse(is.na(AlimonyAmount), 0, AlimonyAmount) +
      ifelse(is.na(OtherIncomeAmount), 0, OtherIncomeAmount),
    income_sub_flagged = ifelse(
      (Earned == 1 & !is.na(Earned)) |
        (Unemployment == 1 & !is.na(Unemployment)) |
        (SSI == 1 & !is.na(SSI)) |
        (SSDI == 1 & !is.na(SSDI)) |
        (VADisabilityNonService == 1 &
           !is.na(VADisabilityNonService)) |
        (VADisabilityService == 1 & !is.na(VADisabilityService)) |
        (PrivateDisability == 1 & !is.na(PrivateDisability)) |
        (WorkersComp == 1 & !is.na(WorkersComp)) |
        (TANF == 1 & !is.na(TANF)) |
        (GA == 1 & !is.na(GA)) |
        (SocSecRetirement == 1 & !is.na(SocSecRetirement)) |
        (Pension == 1 & !is.na(Pension)) |
        (ChildSupport == 1 & !is.na(ChildSupport)) |
        (Alimony == 1 & !is.na(Alimony)) |
        (OtherIncomeSource == 1 & !is.na(OtherIncomeSource)),
      1,
      0
    )
  ) 

# *** gathers non-cash benefits for adults served in the date range.
noncash <- this_years_adult_enrollments %>%
  left_join(income_benefits_csv, by = c("EnrollmentID", "PersonalID")) %>%
  select(
    PersonalID,
    EnrollmentID,
    ProjectID,
    EntryDate,
    BenefitsFromAnySource,
    SNAP,
    WIC,
    TANFChildCare,
    TANFTransportation,
    OtherTANF,
    OtherBenefitsSource,
    DataCollectionStage
  ) %>%
  mutate(noncash_subs_flagged =
           ifelse((SNAP == 1 & !is.na(SNAP)) |
                    (WIC == 1 & !is.na(WIC)) |
                    (TANFChildCare == 1 & !is.na(TANFChildCare)) |
                    (TANFTransportation == 1 & !is.na(TANFTransportation)) |
                    (OtherTANF == 1 & !is.na(OtherTANF)) |
                    (OtherBenefitsSource == 1 &
                       !is.na(OtherBenefitsSource)),
                  1,
                  0
           ))

health_insurance <- this_year_enrollments %>%
  left_join(income_benefits_csv, by = c("EnrollmentID", "PersonalID")) %>%
  select(
    PersonalID,
    EnrollmentID,
    ProjectID,
    EntryDate,
    InsuranceFromAnySource,
    Medicaid,
    Medicare,
    SCHIP,
    VAMedicalServices,
    EmployerProvided,
    COBRA,
    PrivatePay,
    StateHealthIns,
    IndianHealthServices,
    OtherInsurance,
    HIVAIDSAssistance,
    ADAP,
    ConnectionWithSOAR,
    DataCollectionStage
  ) %>%
  mutate(health_insurance_subs_flagged =
           ifelse((Medicaid == 1 & !is.na(Medicaid)) |
                    (Medicare == 1 & !is.na(Medicare)) |
                    (SCHIP == 1 & !is.na(SCHIP)) |
                    (VAMedicalServices == 1 &
                       !is.na(VAMedicalServices)) |
                    (EmployerProvided == 1 &
                       !is.na(EmployerProvided)) |
                    (COBRA == 1 &
                       !is.na(COBRA)) |
                    (PrivatePay == 1 & !is.na(PrivatePay)) |
                    (StateHealthIns == 1 & !is.na(StateHealthIns)) |
                    (IndianHealthServices == 1 &
                       !is.na(IndianHealthServices)) |
                    (OtherInsurance == 1 & !is.na(OtherInsurance)) |
                    (HIVAIDSAssistance == 1 &
                       !is.na(HIVAIDSAssistance)) |
                    (ADAP == 1 & !is.na(ADAP)),
                  1,
                  0
           ))

# *** replaces null Funder with the number 50 to represent ODSA's grants, also only lists ONE
# funder per project! DO NOT REUSE without considering this last point!!!!!
funders <- funder_csv %>%
  group_by(ProjectID) %>%
  summarise(FunderID = max(FunderID)) %>%
  left_join(funder_csv, by = c("ProjectID", "FunderID")) %>%
  mutate(Funder = ifelse(is.na(Funder), 50, Funder)) %>%
  select(ProjectID, Funder, StartDate, EndDate) %>%
  filter(ymd(StartDate) < today() &
           (ymd(EndDate) >mdy(10012016) |
              is.na(EndDate)))

# *** gathers together all the EEs and their EE Types
ee_types <-
  left_join(entry_exits,
            counties_csv,
            by = c("EnrollmentID" = "Entry_Exit_ID")) %>%
  left_join(project_csv, by = c("ProjectID", "ProjectType")) %>%
  left_join(funders, by = "ProjectID") %>%
  filter((ymd(OperatingStartDate) < today() &
            (
              ymd(OperatingEndDate) > mdy(10012016) |
                is.na(OperatingEndDate)
            ))) %>%
  select(
    PersonalID,
    HouseholdID,
    ProjectType,
    ProjectID,
    Funder,
    EntryDate,
    Entry_Exit_Types
  )


#a rule to follow is that every output should include at least the PersonalID, ProjectID, HouseholdID, and EntryDate

# Missing Name ------------------------------------------------------------

missing_name <- this_year_enrollments %>%
  left_join(client_csv, by = c("PersonalID")) %>%
  filter((NameDataQuality == 99 |
            is.na(NameDataQuality)) &
           FirstName != "Anonymous") %>%
  mutate(Data_Quality_Issue = "Missing Name") %>%
  select(Data_Quality_Issue,
         PersonalID,
         ProjectID,
         EntryDate)

# DKR Name ----------------------------------------------------------------

dkr_name <- this_year_enrollments %>%
  left_join(client_csv, by = c("PersonalID")) %>%
  filter(NameDataQuality %in% c(8, 9) |
           FirstName == "Anonymous") %>%
  mutate(Data_Quality_Issue = "Don't Know/Refused Name") %>%
  select(Data_Quality_Issue,
         PersonalID,
         ProjectID,
         EntryDate)

# Name Reports as "Data Issues" in APR ------------------------------------

partial_name <- this_year_enrollments %>%
  left_join(client_csv, by = c("PersonalID")) %>%
  filter(NameDataQuality == 2) %>%
  mutate(Data_Quality_Issue = "Partial Name") %>%
  select(Data_Quality_Issue,
         PersonalID,
         ProjectID,
         EntryDate)

# Missing or Incorrect SSN -------------------------------------------------------------

missing_ssn <- this_year_enrollments %>%
  left_join(client_csv, by = c("PersonalID")) %>%
  filter(is.na(SSN) |
           is.na(SSNDataQuality) |
           SSNDataQuality == 99) %>%
  mutate(Data_Quality_Issue = "Missing SSN") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, EntryDate)


# Incorrect SSN -----------------------------------------------------------

incorrect_ssn <- this_year_enrollments %>%
  left_join(client_csv, by = c("PersonalID")) %>%
  filter(
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
        123456789
      )
  ) %>%
  mutate(Data_Quality_Issue = "Invalid SSN") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, EntryDate)


# Missing Date of Birth ---------------------------------------------------

missing_dob <- this_year_enrollments %>%
  left_join(client_csv, by = "PersonalID") %>%
  filter(is.na(DOB) |
           is.na(DOBDataQuality) |
           DOBDataQuality == 99) %>%
  mutate(Data_Quality_Issue = "Missing Date of Birth") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, EntryDate)

incorrect_dob <- this_year_enrollments %>%
  filter(Age_At_Entry < 0 | Age_At_Entry > 100)

# Missing Gender ----------------------------------------------------------

missing_gender <- this_year_enrollments %>%
  left_join(client_csv, by = c("PersonalID")) %>%
  filter(is.na(Gender) |
           Gender == 99) %>%
  mutate(Data_Quality_Issue = "Missing Gender") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, EntryDate)

# Missing Race ------------------------------------------------------------

missing_race <- this_year_enrollments %>%
  left_join(client_csv, by = c("PersonalID")) %>%
  filter(RaceNone == 99) %>%
  mutate(Data_Quality_Issue = "Missing Race") %>%
  select(Data_Quality_Issue,
         PersonalID,
         ProjectID,
         EntryDate)

# Missing Ethnicity -------------------------------------------------------

missing_ethnicity <- this_year_enrollments %>%
  left_join(client_csv, by = c("PersonalID")) %>%
  filter(Ethnicity == 99 |
           is.na(Ethnicity)) %>%
  mutate(Data_Quality_Issue = "Missing Ethnicity") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, EntryDate)

dkr_ethnicity <- this_year_enrollments %>%
  left_join(client_csv, by = c("PersonalID")) %>%
  filter(Ethnicity %in% c(8, 9)) %>%
  mutate(Data_Quality_Issue = "Don't Know/Refused Ethnicity") %>%
  select(Data_Quality_Issue,
         PersonalID,
         ProjectID,
         EntryDate,
         Ethnicity)

# Missing Residence Prior -------------------------------------------------

missing_residence_prior <- this_year_enrollments %>%
  left_join(enrollment_csv,
            by = c("PersonalID", "ProjectID", "EnrollmentID", "EntryDate")) %>%
  filter(is.na(LivingSituation) |
           LivingSituation == 99) %>%
  mutate(Data_Quality_Issue = "Missing Residence Prior") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, EntryDate)

dkr_residence_prior <- this_year_enrollments %>%
  left_join(enrollment_csv,
            by = c("PersonalID", "ProjectID", "EnrollmentID", "EntryDate")) %>%
  filter(LivingSituation %in% c(8, 9)) %>%
  mutate(Data_Quality_Issue = "Don't Know/Refused Residence Prior") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, EntryDate)

# Missing Veteran Status --------------------------------------------------

missing_veteran_status <- this_year_enrollments %>%
  left_join(client_csv, by = c("PersonalID")) %>%
  filter(VeteranStatus == 99 |
           is.na(VeteranStatus)) %>%
  mutate(Data_Quality_Issue = "Missing Veteran Status") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, EntryDate)

dkr_veteran_status <- this_year_enrollments %>%
  left_join(client_csv, by = c("PersonalID")) %>%
  filter(VeteranStatus %in% c(8, 9)) %>%
  mutate(Data_Quality_Issue = "Don't Know/Refused Veteran Status") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, EntryDate)

# Missing Relationship to Head of Household (client level) ----------------

missing_rel_to_hoh <- this_year_enrollments %>%
  left_join(enrollment_csv,
            by = c("PersonalID", "ProjectID", "EnrollmentID", "EntryDate")) %>%
  filter(is.na(RelationshipToHoH) |
           RelationshipToHoH == 99) %>%
  mutate(Data_Quality_Issue = "Missing Relationship to HoH") %>%
  select(Data_Quality_Issue,
         PersonalID,
         ProjectID,
         EntryDate)

# Missing Client Location -------------------------------------------------
#when was this implemented? is it retroactive?
missing_client_location <- this_year_enrollments %>%
  left_join(enrollment_coc_csv,
            by = c("PersonalID", "ProjectID", "EnrollmentID")) %>%
  filter(is.na(CoCCode)) %>%
  mutate(Data_Quality_Issue = "Missing Client Location") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, EntryDate)

# Missing DV questions ----------------------------------------------------
#gathering all the SSVF and PATH project IDs together
ssvf_and_path_project_ids <-
  funder_csv %>% select(ProjectID, Funder, StartDate, EndDate) %>%
  filter(ymd(StartDate) < today() &
           (is.na(EndDate) |
              ymd(EndDate) > mdy(10012017)),
         Funder %in% c(21, 33))

# gathers all the year's enrollments but excludes those into SSVF or PATH projects
this_years_ees_no_ssvf_path <-
  anti_join(this_year_enrollments, ssvf_and_path_project_ids, by = "ProjectID") %>%
  select(PersonalID, ProjectID, EnrollmentID, EntryDate, ExitDate)

# gathers all the current enrollments but excludes those into SSVF or PATH projects
current_ees_no_ssvf_path <-
  anti_join(current_enrollments, ssvf_and_path_project_ids, by = "ProjectID") %>%
  select(PersonalID, ProjectID, EnrollmentID, EntryDate, ExitDate)

#pulls everything together to list all non-SSVF, non-PATH adults with missing DV questions
missing_dv <- this_years_ees_no_ssvf_path %>%
  inner_join(this_years_adult_enrollments,
             by = c("PersonalID", "EnrollmentID", "ProjectID", "EntryDate")) %>%
  left_join(health_and_dv_csv, by = c("EnrollmentID", "PersonalID")) %>%
  filter(
    is.na(DomesticViolenceVictim) |
      DomesticViolenceVictim == 99 |
      (DomesticViolenceVictim == 1 &
         (
           is.na(WhenOccurred) |
             is.na(CurrentlyFleeing)
         ))
  ) %>%
  mutate(Data_Quality_Issue = "Missing Domestic Violence") %>%
  select(
    Data_Quality_Issue, 
    PersonalID,
    ProjectID,
    EntryDate
  )

dkr_dv <- this_years_ees_no_ssvf_path %>%
  inner_join(
    this_years_adult_enrollments,
    by = c("PersonalID", "EnrollmentID", "ProjectID", "EntryDate")
  ) %>%
  left_join(health_and_dv_csv, by = c("EnrollmentID", "PersonalID")) %>%
  filter(DomesticViolenceVictim %in% c(8, 9) |
           (
             DomesticViolenceVictim == 1 &
               (WhenOccurred %in% c(8, 9) &
                  !is.na(CurrentlyFleeing)) |
               (CurrentlyFleeing %in% c(8, 9) &
                  !is.na(WhenOccurred))
           )) %>%
  mutate(Data_Quality_Issue = "Don't Know/Refused Domestic Violence") %>%
  select(Data_Quality_Issue,
         PersonalID,
         ProjectID,
         EntryDate)

rm(ssvf_and_path_project_ids, this_years_ees_no_ssvf_path, current_ees_no_ssvf_path)

# Move In Date Data Quality -----------------------------------------------

q_move_in_date <- this_year_enrollments %>%
  inner_join(permanent_housing_project_ids, by = "ProjectID") %>%
  mutate(DaysToHouse = difftime(ymd(MoveInDate), ymd(EntryDate), units = "days")) %>%
  filter(DaysToHouse < 1 &
           ((ProjectType %in% c(9, 10) &
               ymd(EntryDate) >= mdy(10012017)) |
              (ProjectType == 13 &
                 ymd(EntryDate) >= mdy(10012015))
           )) %>%
  mutate(Data_Quality_Issue = "Questionable Move-In Date") %>%
  select(Data_Quality_Issue,
         PersonalID,
         ProjectID,
         EntryDate,
         MoveInDate,
         ExitDate,
         DaysToHouse)

# Missing Length of Time Homeless Questions -------------------------------
#more logic belongs inside of here- some of these aren't required based on other answers

missing_lot_questions <-
  this_years_adult_enrollments %>%
  left_join(enrollment_csv,
            by = c("EnrollmentID", "PersonalID", "ProjectID", "EntryDate")) %>%
  left_join(project_csv, by = "ProjectID") %>%
  mutate(Data_Quality_Issue = "Missing or Incorrect Length of Time Homeless Questions") %>%
  select(
    Data_Quality_Issue,
    PersonalID,
    EntryDate,
    ProjectID,
    ProjectType,
    LivingSituation,
    LOSUnderThreshold,
    LengthOfStay,
    PreviousStreetESSH,
    DateToStreetESSH,
    TimesHomelessPastThreeYears,
    MonthsHomelessPastThreeYears
  ) %>%
  filter(
    ymd(EntryDate) > mdy(10012016) &
      (
        is.na(LivingSituation) |
          #LengthOfStay logic when residence prior is institutional
          (
            ProjectType %in% c(2, 3, 9, 10, 13) &
              LivingSituation %in% c(4, 5, 6, 7, 15, 24) &
              (is.na(LengthOfStay) |
                 LengthOfStay %in% c(8, 9, 99))
          ) |
          #LengthOfStay logic when residence prior is housed
          (
            ProjectType %in% c(2, 3, 9, 10, 13) &
              (
                LivingSituation %in% c(14, 23, 21, 3, 22, 19, 25, 20, 26, 12, 13, 2, 8, 9) |
                  is.na(LivingSituation)
              ) &
              (LengthOfStay %in% c(8, 9, 99) |
                 is.na(LengthOfStay))
          )
        |
          ProjectType %in% c(1, 4, 8) &
          (
            is.na(DateToStreetESSH) |
              is.na(TimesHomelessPastThreeYears) |
              is.na(MonthsHomelessPastThreeYears)
          ) |
          ProjectType %in% c(2, 3, 9, 10, 13) &
          ((LivingSituation %in% c(1, 16, 18, 27) &
              (
                is.na(DateToStreetESSH) |
                  is.na(TimesHomelessPastThreeYears) |
                  is.na(MonthsHomelessPastThreeYears)
              )) |
             (
               LivingSituation %in% c(4, 5, 6, 7, 15, 24) &
                 LengthOfStay %in% c(2, 3, 11, 12) &
                 PreviousStreetESSH == 1 &
                 (
                   is.na(DateToStreetESSH) |
                     is.na(TimesHomelessPastThreeYears) |
                     is.na(MonthsHomelessPastThreeYears)
                 )
             ) |
             (
               LivingSituation %in% c(2, 3, 8, 9, 12, 13, 14, 19, 20, 21, 22, 23, 25, 26) &
                 LengthOfStay %in% c(10, 11) &
                 PreviousStreetESSH == 1
             )
          )
      )
  )

# Missing Destination -----------------------------------------------------

missing_destination <- filter(this_year_enrollments,
                              Destination %in% c(8, 9, 30)) %>%
  mutate(Data_Quality_Issue = "Missing Destination") %>%
  select(Data_Quality_Issue,
         PersonalID,
         ProjectID,
         EntryDate)

# Missing Housing Assessment at Exit --------------------------------------



# Missing or Incorrect Income at Entry ------------------------------------
# the income objects only pulls in adults during the reporting period
missing_income_entry <-  income %>%
  filter(
    # this income was collected at Entry
    DataCollectionStage == 1 &
      # IncomeFromAnySource is Yes but the TMI = 0
      ((IncomeFromAnySource == 1 &
          TotalMonthlyIncome == 0) |
         # IncomeFromAnySource is No but the TMI > 0
         (IncomeFromAnySource == 0 &
            TotalMonthlyIncome > 0) |
         # TMI doesn't add up to the amounts in the subs
         TotalMonthlyIncome != income_subs_total |
         # IncomeFromAnySource is Yes but there are no subs flagged
         (IncomeFromAnySource == 1 & income_sub_flagged == 0) |
         # IncomeFromAnySource is No but there are subs flagged
         (IncomeFromAnySource == 0 & income_sub_flagged == 1)
      )
  ) %>% 
  mutate(Data_Quality_Issue = "Missing Income at Entry") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, EntryDate)

# Missing or Incorrect Income at Interim ----------------------------------
missing_income_interim <- income %>%
  filter(
    # this income was collected at Interim
    DataCollectionStage == 2 &
      # IncomeFromAnySource is Yes but the TMI = 0
      ((IncomeFromAnySource == 1 &
          TotalMonthlyIncome == 0) |
         # IncomeFromAnySource is No but the TMI > 0
         (IncomeFromAnySource == 0 &
            TotalMonthlyIncome > 0) |
         # TMI doesn't add up to the amounts in the subs
         TotalMonthlyIncome != income_subs_total |
         # IncomeFromAnySource is Yes but there are no subs flagged
         (IncomeFromAnySource == 1 & income_sub_flagged == 0) |
         # IncomeFromAnySource is No but there are subs flagged
         (IncomeFromAnySource == 0 & income_sub_flagged == 1)
      )
  )  %>% 
  mutate(Data_Quality_Issue = "Missing Income at Interim") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, EntryDate)


# Missing or Incorrect Income at Exit -------------------------------------
missing_income_exit <- income %>%
  filter(
    # this income was collected at Exit
    DataCollectionStage == 3 &
      # IncomeFromAnySource is Yes but the TMI = 0
      ((IncomeFromAnySource == 1 &
          TotalMonthlyIncome == 0) |
         # IncomeFromAnySource is No but the TMI > 0
         (IncomeFromAnySource == 0 &
            TotalMonthlyIncome > 0) |
         # TMI doesn't add up to the amounts in the subs
         TotalMonthlyIncome != income_subs_total |
         # IncomeFromAnySource is Yes but there are no subs flagged
         (IncomeFromAnySource == 1 & income_sub_flagged == 0) |
         # IncomeFromAnySource is No but there are subs flagged
         (IncomeFromAnySource == 0 & income_sub_flagged == 1)
      )
  ) %>% 
  mutate(Data_Quality_Issue = "Missing Income at Exit") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, EntryDate)

# Missing Non-Cash Benefits -----------------------------------------------
missing_noncash_entry <- noncash %>%
  filter(# this income was collected at Entry
    DataCollectionStage == 1 &
      # NCBs from any source is Yes but there are no subs
      ((BenefitsFromAnySource == 1 &
          noncash_subs_flagged == 0) |
         # NCBs from any source says No but there's an active sub
         (BenefitsFromAnySource == 0 &
            noncash_subs_flagged == 1)
      )) %>%
  mutate(Data_Quality_Issue = "Missing Non-Cash at Entry") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, EntryDate)

missing_noncash_exit <- noncash %>%
  filter(# this income was collected at Exit
    DataCollectionStage == 3 &
      # NCBs from any source is Yes but there are no subs
      ((BenefitsFromAnySource == 1 &
          noncash_subs_flagged == 0) |
         # NCBs from any source says No but there's an active sub
         (BenefitsFromAnySource == 0 &
            noncash_subs_flagged == 1)
      )) %>%
  mutate(Data_Quality_Issue = "Missing Non-Cash at Entry") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, EntryDate)

# Missing Health Insurance ------------------------------------------------

missing_health_insurance_entry <- health_insurance %>%
  filter(# this income was collected at Entry
    DataCollectionStage == 1 &
      # Health Insurance is Yes but there are no subs
      ((InsuranceFromAnySource == 1 &
          health_insurance_subs_flagged == 0) |
         # Health Insurance says No but there's an active sub
         (InsuranceFromAnySource == 0 &
            health_insurance_subs_flagged == 1)
      )) %>%
  mutate(Data_Quality_Issue = "Missing Health Insurance at Entry") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, EntryDate)

missing_health_insurance_exit <- health_insurance %>%
  filter(# this income was collected at Exit
    DataCollectionStage == 3 &
      # Health Insurance is Yes but there are no subs
      ((InsuranceFromAnySource == 1 &
          health_insurance_subs_flagged == 0) |
         # Health Insurance says No but there's an active sub
         (InsuranceFromAnySource == 0 &
            health_insurance_subs_flagged == 1)
      )) %>%
  mutate(Data_Quality_Issue = "Missing Health Insurance at Exit") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, EntryDate)

# Missing Disabilities ----------------------------------------------------
# the csv is different from the way it's collected in SP. the Yes/No does not get 
# pulled into the csv I think??? Can't think of what needs to be checked.

# Questionable Disabilities -----------------------------------------------
q_disabilities_to_income <-
  filter(income, (SSI == 1 |
                    SSDI == 1) & DataCollectionStage == 1) %>%
  select(PersonalID,
         EnrollmentID,
         ProjectID,
         SSI,
         SSDI,
         DataCollectionStage) %>%
  left_join(this_year_enrollments,
            by = c("PersonalID", "EnrollmentID", "ProjectID")) %>%
  left_join(disabilities_csv, by = c("PersonalID", "EnrollmentID")) %>%
  filter(DisabilityResponse == 0) %>%
  mutate(Data_Quality_Issue = "If the SSI or SSDI belongs to the Client, Disability should be YES") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, EntryDate, SSI, SSDI)

# Future Entry Exits ------------------------------------------------------
future_dates <-
  filter(entry_exits, ymd(EntryDate) > today() |
           ymd(ExitDate) > today())

# Incorrect EE Type -------------------------------------------------------

# mostly right, but it pulls in all the sheltered clients who were entered for PIT, 
# probably other oddities too, but this is close! MORE WORK NEEDED

incorrect_ee_type <- mutate(
  funders,
  Correct_EE_Type =
    case_when(
      ProjectID == 1695 ~ "Standard",
      Funder %in% c(22, 23, 24, 25, 26) ~ "RHY",
      Funder %in% c(27:33, 37:42) ~ "VA",
      Funder == 22 ~ "PATH",
      Funder %in% c(1:20, 34, 43, 50) & ProjectID != 1695 ~ "HUD"
    )
) %>%
  left_join(ee_types, by = c("ProjectID", "Funder")) %>%
  mutate(Data_Quality_Issue = "Incorrect Entry Exit Type") %>%
  select(
    Data_Quality_Issue,
    PersonalID,
    ProjectID,
    HouseholdID,
    ProjectType,
    Funder,
    Entry_Exit_Types,
    Correct_EE_Type,
    EntryDate
  ) %>%
  filter(Correct_EE_Type != Entry_Exit_Types)

# VI SPDAT Check ----------------------------------------------------------
# would require that I create an export for this data

# Children Only Household -------------------------------------------------

child_hohs <-
  households %>%
  group_by(HouseholdID) %>%
  summarise(max_age_in_hh = max(Age_At_Entry)) %>%
  filter(max_age_in_hh < 18) %>%
  left_join(entry_exits, by = "HouseholdID") %>%
  select(PersonalID, ProjectID, HouseholdID, EntryDate, max_age_in_hh) %>%
  mutate(Data_Quality_Issue = "Child Head of Household")

# Missing Head of Household -----------------------------------------------
# this seems to only be picking up clients created before "Relationship to HoH" was
# added to the assessments so this shouldn't be that useful (but it is interesting anyway)
hoh_congruity <- households %>% mutate(StrippedHHID = substr(HouseholdID, 3, 7)) %>% 
  filter(StrippedHHID == EnrollmentID &
           RelationshipToHoH != 1)

missing_hoh <- 
  households %>%
  mutate(hoh_or_not = ifelse(RelationshipToHoH == 1, 1, 0)) %>%
  group_by(HouseholdID) %>%
  summarise(how_many_hohs = sum(hoh_or_not)) %>%
  filter(how_many_hohs == 0) %>%
  left_join(entry_exits, by = "HouseholdID") %>%
  select(PersonalID, HouseholdID, ProjectID, EntryDate, how_many_hohs) %>%
  mutate(Data_Quality_Issue = "Missing Head of Household")

mult_hohs <- households %>%
  mutate(hoh_or_not = ifelse(RelationshipToHoH == 1, 1, 0)) %>%
  group_by(HouseholdID) %>%
  summarise(how_many_hohs = sum(hoh_or_not)) %>%
  filter(how_many_hohs > 1) %>%
  left_join(entry_exits, by = "HouseholdID") %>%
  select(PersonalID, HouseholdID, ProjectID, EntryDate, how_many_hohs) %>%
  mutate(Data_Quality_Issue = "Multiple Heads of Household")

# Missing County Served ---------------------------------------------------

missing_county_served <- this_year_enrollments %>% 
  left_join(counties_csv, by = c("EnrollmentID" = "Entry_Exit_ID", "PersonalID" = "Client_ID")) %>%
  filter(is.na(County_Served)) %>%
  mutate(Data_Quality_Issue = "Missing County Served") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, HouseholdID, EntryDate)

# Missing County Prior ----------------------------------------------------

missing_county_prior <- this_year_enrollments %>% 
  left_join(counties_csv, by = c("EnrollmentID" = "Entry_Exit_ID", "PersonalID" = "Client_ID")) %>%
  filter(is.na(County_Prior)) %>%
  mutate(Data_Quality_Issue = "Missing County Prior") %>%
  select(Data_Quality_Issue, PersonalID, ProjectID, HouseholdID, EntryDate)
