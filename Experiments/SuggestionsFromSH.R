# https://github.com/COHHIO/COHHIO_HMIS/blob/7005a4b59aa8e7821a79df5899e75516c7f87ebb/00_get_Export_and_ART.R#L405

library(tidyverse)

with_mutate_at <- list(before = rlang::expr(covid19 <-
  read_xlsx(paste0(directory, "/RMisc2.xlsx"), sheet = 6) %>%
  mutate(
    COVID19AssessmentDate = ymd(as.Date(COVID19AssessmentDate,
                                        origin = "1899-12-30")),
    ContactWithConfirmedDate = ymd(as.Date(ContactWithConfirmedDate,
                                           origin = "1899-12-30")),
    ContactWithUnderInvestigationDate = ymd(
      as.Date(ContactWithUnderInvestigationDate,
              origin = "1899-12-30")
    ),
    TestDate = ymd(as.Date(TestDate,
                           origin = "1899-12-30")),
    DateUnderInvestigation = ymd(as.Date(DateUnderInvestigation,
                                         origin = "1899-12-30")),
    Tested = replace_yes_no(Tested),
    UnderInvestigation = replace_yes_no(UnderInvestigation),
    ContactWithConfirmedCOVID19Patient = replace_yes_no(
      ContactWithConfirmedCOVID19Patient
    ),
    ContactWithUnderCOVID19Investigation = replace_yes_no(
      ContactWithUnderCOVID19Investigation
    )
  ) %>%
  mutate_at(vars(matches("Symptom")), replace_yes_no) %>%
  mutate_at(vars(matches("HealthRisk")), replace_yes_no))
,
after = rlang::expr(covid19 <-
  read_xlsx(paste0(directory, "/RMisc2.xlsx"), sheet = 6) %>%
    # mutate_at each with a single function call
  mutate_at(
    dplyr::vars(COVID19AssessmentDate, ContactWithConfirmedDate, ContactWithUnderInvestigationDate, TestDate, DateUnderInvestigation), ~ymd(as.Date(.,
                                         origin = "1899-12-30"))) %>% 
    # this assumes that all names of the columns that include "Symptom" & "Healthrisk" are mutated in this way
  mutate_at(
    dplyr::vars(Tested, UnderInvestigation, ContactWithConfirmedCOVID19Patient, ContactWithUnderCOVID19Investigation, matches("Symptom"), matches("HealthRisk")), ~replace_yes_no(.)
  ))
)

# Reduction in characters
purrr::map_int(with_mutate_at, ~nchar(paste0(as.character(.x), collapse = " "))) %>% 
  {(.[2] - .[1]) / .[1] }



# https://github.com/COHHIO/COHHIO_HMIS/blob/7005a4b59aa8e7821a79df5899e75516c7f87ebb/01_Bed_Unit_Utilization.R#L153
with_purrr <- list(before = rlang::expr({FirstMonth <- nth_Month(1)
            SecondMonth <- nth_Month(2)
            ThirdMonth <- nth_Month(3)
            FourthMonth <- nth_Month(4)
            FifthMonth <- nth_Month(5)
            SixthMonth <- nth_Month(6)
            SeventhMonth <- nth_Month(7)
            EighthMonth <- nth_Month(8)
            NinthMonth <- nth_Month(9)
            TenthMonth <- nth_Month(10)
            EleventhMonth <- nth_Month(11)
            TwelfthMonth <- nth_Month(12)
            ThirteenthMonth <- nth_Month(13)
            FourteenthMonth <- nth_Month(14)
            FifteenthMonth <- nth_Month(15)
            SixteenthMonth <- nth_Month(16)
            SeventeenthMonth <- nth_Month(17)
            EighteenthMonth <- nth_Month(18)
            NineteenthMonth <- nth_Month(19)
            TwentiethMonth <- nth_Month(20)
            TwentyfirstMonth <- nth_Month(21)
            TwentysecondMonth <- nth_Month(22)
            TwentythirdMonth <- nth_Month(23)
            TwentyfourthMonth <- nth_Month(24)
            # adding in month columns with utilization numbers
            
            utilizers_clients <- utilizers_clients %>%
              mutate(
                # FilePeriod = bed_nights_per_ee(utilizers_clients, FilePeriod),
                Month1 = bed_nights_per_ee(utilizers_clients, FirstMonth),
                Month2 = bed_nights_per_ee(utilizers_clients, SecondMonth),
                Month3 = bed_nights_per_ee(utilizers_clients, ThirdMonth),
                Month4 = bed_nights_per_ee(utilizers_clients, FourthMonth),
                Month5 = bed_nights_per_ee(utilizers_clients, FifthMonth),
                Month6 = bed_nights_per_ee(utilizers_clients, SixthMonth),
                Month7 = bed_nights_per_ee(utilizers_clients, SeventhMonth),
                Month8 = bed_nights_per_ee(utilizers_clients, EighthMonth),
                Month9 = bed_nights_per_ee(utilizers_clients, NinthMonth),
                Month10 = bed_nights_per_ee(utilizers_clients, TenthMonth),
                Month11 = bed_nights_per_ee(utilizers_clients, EleventhMonth),
                Month12 = bed_nights_per_ee(utilizers_clients, TwelfthMonth),
                Month13 = bed_nights_per_ee(utilizers_clients, ThirteenthMonth),
                Month14 = bed_nights_per_ee(utilizers_clients, FourteenthMonth),
                Month15 = bed_nights_per_ee(utilizers_clients, FifteenthMonth),
                Month16 = bed_nights_per_ee(utilizers_clients, SixteenthMonth),
                Month17 = bed_nights_per_ee(utilizers_clients, SeventeenthMonth),
                Month18 = bed_nights_per_ee(utilizers_clients, EighteenthMonth),
                Month19 = bed_nights_per_ee(utilizers_clients, NineteenthMonth),
                Month20 = bed_nights_per_ee(utilizers_clients, TwentiethMonth),
                Month21 = bed_nights_per_ee(utilizers_clients, TwentyfirstMonth),
                Month22 = bed_nights_per_ee(utilizers_clients, TwentysecondMonth),
                Month23 = bed_nights_per_ee(utilizers_clients, TwentythirdMonth),
                Month24 = bed_nights_per_ee(utilizers_clients, TwentyfourthMonth)
              ) %>%
              select(ProjectName, ProjectID, ProjectType, PersonalID, EnrollmentID, 
                     EntryDate, MoveInDate, ExitDate, starts_with("Month"))
            utilizers_clients %>%
              group_by(ProjectName, ProjectID, ProjectType) %>%
              summarise(
                # BNY = sum(FilePeriod, na.rm = TRUE),
                BN1 = sum(Month1, na.rm = TRUE),
                BN2 = sum(Month2, na.rm = TRUE),
                BN3 = sum(Month3, na.rm = TRUE),
                BN4 = sum(Month4, na.rm = TRUE),
                BN5 = sum(Month5, na.rm = TRUE),
                BN6 = sum(Month6, na.rm = TRUE),
                BN7 = sum(Month7, na.rm = TRUE),
                BN8 = sum(Month8, na.rm = TRUE),
                BN9 = sum(Month9, na.rm = TRUE),
                BN10 = sum(Month10, na.rm = TRUE),
                BN11 = sum(Month11, na.rm = TRUE),
                BN12 = sum(Month12, na.rm = TRUE),
                BN13 = sum(Month13, na.rm = TRUE),
                BN14 = sum(Month14, na.rm = TRUE),
                BN15 = sum(Month15, na.rm = TRUE),
                BN16 = sum(Month16, na.rm = TRUE),
                BN17 = sum(Month17, na.rm = TRUE),
                BN18 = sum(Month18, na.rm = TRUE),
                BN19 = sum(Month19, na.rm = TRUE),
                BN20 = sum(Month20, na.rm = TRUE),
                BN21 = sum(Month21, na.rm = TRUE),
                BN22 = sum(Month22, na.rm = TRUE),
                BN23 = sum(Month23, na.rm = TRUE),
                BN24 = sum(Month24, na.rm = TRUE)
              ) %>%
              ungroup()
            
            })
, after = rlang::expr({
# iterate over 1:24 with purrr
  # TODO ensure that this vectorizes as expected and produces the same output
nth_m <- purrr::map(1:24, nth_Month) %>% 
  purrr::map_dfc(~bed_nights_per_ee(utilizers_clients, .x)) %>% 
  setNames(paste0("Month", 1:24))
  # Bind the new columns
utilizers_clients <- dplyr::bind_cols(utilizers_clients, nth_m) %>% 
  select(ProjectName, ProjectID, ProjectType, PersonalID, EnrollmentID, 
         EntryDate, MoveInDate, ExitDate, starts_with("Month"))

utilizers_clients %>% 
  # Summarize at the created variables starting with Month
  summarise_at(vars(starts_with("Month")), ~sum(., na.rm = TRUE)) %>% 
  # Rename with BN Prefix
  rename_with(vars(starts_with("Month")), ~{str_replace(.x, "Month", "BN")})

}))

# Reduction in characters
purrr::map_int(with_purrr, ~nchar(paste0(as.character(.x), collapse = " "))) %>% 
  {(.[2] - .[1]) / .[1] }


# https://github.com/COHHIO/Rminor/blob/84804ca0ad146dd613ae3d1cc0075c3cb3b98309/server.R#L92

with_mutate_at2 <- list(before = rlang::expr({
  summary_pe_final_scoring <- summary_pe_final_scoring %>%
    mutate(
      ExitsToPHMath = str_replace(ExitsToPHMath, "/", "÷"),
      OwnHousingMath = str_replace(OwnHousingMath, "/", "÷"),
      IncreasedIncomeMath = str_replace(IncreasedIncomeMath, "/", "÷"),
      BenefitsAtExitMath = str_replace(BenefitsAtExitMath, "/", "÷"),
      AverageLoSMath = str_replace(AverageLoSMath, "/", "÷"),
      LHResPriorMath = str_replace(LHResPriorMath, "/", "÷"),
      NoIncomeAtEntryMath = str_replace(NoIncomeAtEntryMath, "/", "÷"),
      MedianHHIMath = str_replace(MedianHHIMath, "/", "÷"),
      LongTermHomelessMath = str_replace(LongTermHomelessMath, "/", "÷"),
      ScoredAtEntryMath = str_replace(ScoredAtEntryMath, "/", "÷"),
      DQMath = str_replace(DQMath, "/", "÷"),
      CostPerExitMath = str_replace(CostPerExitMath, "/", "÷"),
      HousingFirstMath = str_replace(HousingFirstMath, "/", "÷"),
      ChronicPrioritizationMath = str_replace(ChronicPrioritizationMath, "/", "÷"),
      OnTrackSpendingMath = str_replace(OnTrackSpendingMath, "/", "÷"),
      UnspentFundsMath = str_replace(UnspentFundsMath, "/", "÷")
    )})
  , after = rlang::expr({
    # mutate at for all those ending with "Math" (assumes those needing mutation are the only ones ending in "Math")
    summary_pe_final_scoring <- summary_pe_final_scoring %>%
      mutate_at(vars(ends_with("Math")), ~str_replace(., "/", "÷"))
  }))

# Reduction
purrr::map_int(with_mutate_at2, ~nchar(paste0(as.character(.x), collapse = " "))) %>% 
  {(.[2] - .[1]) / .[1] }

#https://github.com/COHHIO/Rminor/blob/84804ca0ad146dd613ae3d1cc0075c3cb3b98309/server.R#L112
# Regex for re-arranging values
# ("[^"]+\")(\s\=\s)(\w+)
# This is here to make the reprex
.n <- c("ExitsToPHPoints",
"OwnHousingPoints",
"IncreasedIncomePoints",
"BenefitsAtExitPoints",
"AverageLoSPoints",
"LHResPriorPoints",
"NoIncomeAtEntryPoints",
"MedianHHIPoints",
"LongTermHomelessPoints",
"ScoredAtEntryPoints",
"DQPoints",
"CostPerExitScore",
"HousingFirstScore",
"ChronicPrioritizationScore",
"OnTrackSpendingScoring",
"UnspentFundsScoring"
)
df <- as.data.frame(matrix(rnorm(5 * length(.n)), nrow = 5)) %>% setNames(.n)
# END Reprex
# Create a mapping legend of the name changes that will happen for all variables
.map <- c(ExitsToPH = "Exits to Permanent Housing",
  OwnHousing = "Moved into Own Housing",
  IncreasedIncome = "Increased Income",
  BenefitsAtExit = "Benefits & Health Insurance at Exit",
  AverageLoS = "Average Length of Stay",
  LHResPrior = "Living Situation at Entry",
  NoIncomeAtEntry = "No Income at Entry",
  MedianHHI = "Median Homeless History Index",
  LongTermHomeless = "Long Term Homeless",
  ScoredAtEntry = "VISPDAT Completion at Entry",
  DQ = "Data Quality",
  CostPerExit = "Cost per Exit",
  HousingFirst = "Housing First",
  ChronicPrioritization = "Prioritization of Chronic",
  OnTrackSpending = "Spending On Track",
  UnspentFunds = "Unspent Funds within Range")

# This can be applied to all 
dplyr::rename_with(df, dplyr::ends_with(c("Points","Score","DQ", "Scoring")), .fn = ~{
  .i <- stringr::str_which(.x, names(.map))
  if (length(.i) > 0) .out <- .map[.i]
  else
    .out <- .x
  .out
})  
  
# TODO #Selects, filters and joins