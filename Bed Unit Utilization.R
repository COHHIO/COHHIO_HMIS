library(tidyverse)
library(lubridate)
library(scales)
load("data/COHHIOHMIS.RData")

# Creating Beds table -----------------------------------------------------

SmallProject <- Project %>%
  filter(ProjectType %in% c(1, 2, 3, 8, 9) &
           operating_between(Project, ReportStart, ReportEnd) &
           is.na(GrantType)) %>%
  select(ProjectID,
         ProjectName,
         ProjectType)
SmallInventory <- Inventory %>%
  filter(!is.na(HMISParticipatingBeds) & 
           CoCCode == "OH-507" &
           (ymd(InventoryStartDate) < mdy(ReportEnd) & 
              (ymd(InventoryEndDate) > mdy(ReportStart) |
              is.na(InventoryEndDate)))) %>%
  select(
    ProjectID,
    HouseholdType,
    UnitInventory,
    BedInventory,
    InventoryStartDate,
    InventoryEndDate,
    HMISParticipatingBeds
  ) 

Beds <- inner_join(SmallProject, SmallInventory, by = "ProjectID")

# Creating Utilizers table ------------------------------------------------

SmallEnrollment <- Enrollment %>% 
  select(PersonalID,
         EnrollmentID,
         ProjectID,
         EntryDate,
         ExitDate,
         HouseholdID,
         RelationshipToHoH,
         MoveInDate)

Utilizers <- semi_join(SmallEnrollment, Beds, by = "ProjectID") 

Utilizers <- left_join(Utilizers, SmallProject, by = "ProjectID") %>%
  select(
    PersonalID,
    EnrollmentID,
    ProjectID,
    ProjectName,
    ProjectType,
    HouseholdID,
    RelationshipToHoH,
    EntryDate,
    MoveInDate,
    ExitDate
  )

# Cleaning up the house ---------------------------------------------------

rm(Affiliation, Client, Disabilities, EmploymentEducation, Enrollment,
   EnrollmentCoC, Exit, Export, Funder, Geography, HealthAndDV, IncomeBenefits,
   Inventory, Organization, Project, ProjectCoC, Scores, Services, 
   SmallEnrollment, SmallInventory, SmallProject, Users)

# Client Bed Nights Utilized ----------------------------------------------

# filtering out any PSH or RRH records without a proper Move-In Date plus the 
# fake training providers
ClientUtilizers <- Utilizers %>%
  mutate(EntryAdjust = case_when(
           ProjectType %in% c(1, 2, 8) ~ EntryDate,
           ProjectType %in% c(3, 9, 13) ~ MoveInDate),
         ExitAdjust = if_else(is.na(ExitDate), mdy(ReportEnd), ymd(ExitDate)),
         ExitDate = NULL,
         StayWindow = interval(ymd(EntryAdjust), ymd(ExitAdjust))
           ) %>%
  filter(
    int_overlaps(StayWindow, ReportingPeriod) &
      (
    (
      ProjectType %in% c(3, 9, 13) &
        !is.na(EntryAdjust) &
        ymd(MoveInDate) >= ymd(EntryDate) &
        ymd(MoveInDate) < ymd(ExitAdjust)
    ) |
      ProjectType %in% c(1, 2, 8)
  ) &
    !ProjectID %in% c(1775, 1695, 1849, 1032, 1030, 1031, 1317)) %>%
  select(-EntryDate, -MoveInDate)

# adding in month columns with utilization numbers

ClientUtilizers <- ClientUtilizers %>%
  mutate(
    Month1 = if_else(int_overlaps(StayWindow, FirstMonth),
                     as.numeric(difftime(
                       if_else(
                         ymd(ExitAdjust) <=  int_end(FirstMonth),
                         as.POSIXct(ExitAdjust) + days(1),
                         int_end(FirstMonth) + days(1)
                       ),
                       if_else(
                         ymd(EntryAdjust) >= int_start(FirstMonth),
                         as.POSIXct(EntryAdjust),
                         int_start(FirstMonth)
                       ),
                       units = "days"
                     )), NULL
                     ),
    Month2 = if_else(int_overlaps(StayWindow, SecondMonth),
                     as.numeric(difftime(
                       if_else(
                         ymd(ExitAdjust) <=  int_end(SecondMonth),
                         as.POSIXct(ExitAdjust) + days(1),
                         int_end(SecondMonth) + days(1)
                       ),
                       if_else(
                         ymd(EntryAdjust) >= int_start(SecondMonth),
                         as.POSIXct(EntryAdjust),
                         int_start(SecondMonth)
                       ),
                       units = "days"
                     )), NULL
    ),
    Month3 = if_else(int_overlaps(StayWindow, ThirdMonth),
                     as.numeric(difftime(
                       if_else(
                         ymd(ExitAdjust) <=  int_end(ThirdMonth),
                         as.POSIXct(ExitAdjust) + days(1),
                         int_end(ThirdMonth) + days(1)
                       ),
                       if_else(
                         ymd(EntryAdjust) >= int_start(ThirdMonth),
                         as.POSIXct(EntryAdjust),
                         int_start(ThirdMonth)
                       ),
                       units = "days"
                     )), NULL
    ),
    Month4 = if_else(int_overlaps(StayWindow, FourthMonth),
                     as.numeric(difftime(
                       if_else(
                         ymd(ExitAdjust) <=  int_end(FourthMonth),
                         as.POSIXct(ExitAdjust) + days(1),
                         int_end(FourthMonth) + days(1)
                       ),
                       if_else(
                         ymd(EntryAdjust) >= int_start(FourthMonth),
                         as.POSIXct(EntryAdjust),
                         int_start(FourthMonth)
                       ),
                       units = "days"
                     )), NULL
    ),
    Month5 = if_else(int_overlaps(StayWindow, FifthMonth),
                     as.numeric(difftime(
                       if_else(
                         ymd(ExitAdjust) <=  int_end(FifthMonth),
                         as.POSIXct(ExitAdjust) + days(1),
                         int_end(FifthMonth) + days(1)
                       ),
                       if_else(
                         ymd(EntryAdjust) >= int_start(FifthMonth),
                         as.POSIXct(EntryAdjust),
                         int_start(FifthMonth)
                       ),
                       units = "days"
                     )), NULL
    ),
    Month6 = if_else(int_overlaps(StayWindow, SixthMonth),
                     as.numeric(difftime(
                       if_else(
                         ymd(ExitAdjust) <=  int_end(SixthMonth),
                         as.POSIXct(ExitAdjust) + days(1),
                         int_end(SixthMonth) + days(1)
                       ),
                       if_else(
                         ymd(EntryAdjust) >= int_start(SixthMonth),
                         as.POSIXct(EntryAdjust),
                         int_start(SixthMonth)
                       ),
                       units = "days"
                     )), NULL),
    Month7 = if_else(int_overlaps(StayWindow, SeventhMonth),
                     as.numeric(difftime(
                       if_else(
                         ymd(ExitAdjust) <=  int_end(SeventhMonth),
                         as.POSIXct(ExitAdjust) + days(1),
                         int_end(SeventhMonth) + days(1)
                       ),
                       if_else(
                         ymd(EntryAdjust) >= int_start(SeventhMonth),
                         as.POSIXct(EntryAdjust),
                         int_start(SeventhMonth)
                       ),
                       units = "days"
                     )), NULL),
    Month8 = if_else(int_overlaps(StayWindow, EighthMonth),
                     as.numeric(difftime(
                       if_else(
                         ymd(ExitAdjust) <=  int_end(EighthMonth),
                         as.POSIXct(ExitAdjust) + days(1),
                         int_end(EighthMonth) + days(1)
                       ),
                       if_else(
                         ymd(EntryAdjust) >= int_start(EighthMonth),
                         as.POSIXct(EntryAdjust),
                         int_start(EighthMonth)
                       ),
                       units = "days"
                     )), NULL),
    Month9 = if_else(int_overlaps(StayWindow, NinthMonth),
                     as.numeric(difftime(
                       if_else(
                         ymd(ExitAdjust) <=  int_end(NinthMonth),
                         as.POSIXct(ExitAdjust) + days(1),
                         int_end(NinthMonth) + days(1)
                       ),
                       if_else(
                         ymd(EntryAdjust) >= int_start(NinthMonth),
                         as.POSIXct(EntryAdjust),
                         int_start(NinthMonth)
                       ),
                       units = "days"
                     )), NULL ),
    Month10 = if_else(int_overlaps(StayWindow, TenthMonth),
                      as.numeric(difftime(
                        if_else(
                          ymd(ExitAdjust) <=  int_end(TenthMonth),
                          as.POSIXct(ExitAdjust) + days(1),
                          int_end(TenthMonth) + days(1)
                        ),
                        if_else(
                          ymd(EntryAdjust) >= int_start(TenthMonth),
                          as.POSIXct(EntryAdjust),
                          int_start(TenthMonth)
                        ),
                        units = "days"
                      )), NULL ),
    Month11 = if_else(int_overlaps(StayWindow, EleventhMonth),
                      as.numeric(difftime(
                        if_else(
                          ymd(ExitAdjust) <=  int_end(EleventhMonth),
                          as.POSIXct(ExitAdjust) + days(1),
                          int_end(EleventhMonth) + days(1)
                        ),
                        if_else(
                          ymd(EntryAdjust) >= int_start(EleventhMonth),
                          as.POSIXct(EntryAdjust),
                          int_start(EleventhMonth)
                        ),
                        units = "days"
                      )), NULL ),
    Month12 = if_else(int_overlaps(StayWindow, TwelfthMonth),
                      as.numeric(difftime(
                        if_else(
                          ymd(ExitAdjust) <=  int_end(TwelfthMonth),
                          as.POSIXct(ExitAdjust) + days(1),
                          int_end(TwelfthMonth) + days(1)
                        ),
                        if_else(
                          ymd(EntryAdjust) >= int_start(TwelfthMonth),
                          as.POSIXct(EntryAdjust),
                          int_start(TwelfthMonth)
                        ),
                        units = "days"
                      )), NULL )
  ) %>%
  select(ProjectName, ProjectID, ProjectType, PersonalID, EnrollmentID, starts_with("Month"))
ClientUtilizers <- as.data.frame(ClientUtilizers)

# making granularity by provider instead of by enrollment id
BedNights <- ClientUtilizers %>%
  group_by(ProjectName, ProjectID, ProjectType) %>%
  summarise(BN1 = sum(Month1, na.rm = TRUE),
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
            BN12 = sum(Month12, na.rm = TRUE))
rm(ClientUtilizers)
# Possible Bed Nights -----------------------------------------------------

BedCapacity <- Beds %>%
  select(ProjectID,
         ProjectName,
         ProjectType,
         BedInventory,
         InventoryStartDate,
         InventoryEndDate) %>%
  mutate(InventoryEndAdjust = if_else(is.na(InventoryEndDate),
                                      mdy(ReportEnd),
                                      ymd(InventoryEndDate)),
         InventoryStartAdjust = if_else(ymd(InventoryStartDate) >= mdy(ReportStart),
                                        ymd(InventoryStartDate),
                                        mdy(ReportStart)),
         AvailableWindow = interval(ymd(InventoryStartAdjust),
                                    ymd(InventoryEndAdjust))) 

BedCapacity <- BedCapacity %>%
  mutate(
    Month1 = if_else(int_overlaps(AvailableWindow, FirstMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(FirstMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(FirstMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(FirstMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(FirstMonth)
                       ),
                       units = "days"
                     ))+1) * BedInventory, NULL
    ),
    Month2 = if_else(int_overlaps(AvailableWindow, SecondMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(SecondMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(SecondMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(SecondMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(SecondMonth)
                       ),
                       units = "days"
                     ))+1) * BedInventory, NULL
    ),
    Month3 = if_else(int_overlaps(AvailableWindow, ThirdMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(ThirdMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(ThirdMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(ThirdMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(ThirdMonth)
                       ),
                       units = "days"
                     ))+1) * BedInventory, NULL
    ),
    Month4 = if_else(int_overlaps(AvailableWindow, FourthMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(FourthMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(FourthMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(FourthMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(FourthMonth)
                       ),
                       units = "days"
                     ))+1) * BedInventory, NULL
    ),
    Month5 = if_else(int_overlaps(AvailableWindow, FifthMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(FifthMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(FifthMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(FifthMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(FifthMonth)
                       ),
                       units = "days"
                     ))+1) * BedInventory, NULL
    ),
    Month6 = if_else(int_overlaps(AvailableWindow, SixthMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(SixthMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(SixthMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(SixthMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(SixthMonth)
                       ),
                       units = "days"
                     ))+1) * BedInventory, NULL
    ),
    Month7 = if_else(int_overlaps(AvailableWindow, SeventhMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(SeventhMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(SeventhMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(SeventhMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(SeventhMonth)
                       ),
                       units = "days"
                     ))+1) * BedInventory, NULL
    ),
    Month8 = if_else(int_overlaps(AvailableWindow, EighthMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(EighthMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(EighthMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(EighthMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(EighthMonth)
                       ),
                       units = "days"
                     ))+1) * BedInventory, NULL
    ),
    Month9 = if_else(int_overlaps(AvailableWindow, NinthMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(NinthMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(NinthMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(NinthMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(NinthMonth)
                       ),
                       units = "days"
                     ))+1) * BedInventory, NULL
    ),
    Month10 = if_else(int_overlaps(AvailableWindow, TenthMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(TenthMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(TenthMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(TenthMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(TenthMonth)
                       ),
                       units = "days"
                     ))+1) * BedInventory, NULL
    ),
    Month11 = if_else(int_overlaps(AvailableWindow, EleventhMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(EleventhMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(EleventhMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(EleventhMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(EleventhMonth)
                       ),
                       units = "days"
                     ))+1) * BedInventory, NULL
    ),
    Month12 = if_else(int_overlaps(AvailableWindow, TwelfthMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(TwelfthMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(TwelfthMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(TwelfthMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(TwelfthMonth)
                       ),
                       units = "days"
                     ))+1) * BedInventory, NULL
    ) 
  ) %>%
  select(-InventoryStartDate, -InventoryEndDate, -InventoryEndAdjust,
         -BedInventory, -InventoryStartAdjust, -AvailableWindow)

BedCapacity <- BedCapacity %>%
  group_by(ProjectID, ProjectName, ProjectType) %>%
  summarise(BC1 = sum(Month1, na.rm = TRUE),
            BC2 = sum(Month2, na.rm = TRUE),
            BC3 = sum(Month3, na.rm = TRUE),
            BC4 = sum(Month4, na.rm = TRUE),
            BC5 = sum(Month5, na.rm = TRUE),
            BC6 = sum(Month6, na.rm = TRUE),
            BC7 = sum(Month7, na.rm = TRUE),
            BC8 = sum(Month8, na.rm = TRUE),
            BC9 = sum(Month9, na.rm = TRUE),
            BC10 = sum(Month10, na.rm = TRUE),
            BC11 = sum(Month11, na.rm = TRUE),
            BC12 = sum(Month12, na.rm = TRUE))

BedUtilization <- left_join(BedCapacity,
                        BedNights,
                        by = c("ProjectID", "ProjectName", "ProjectType")) %>%
  mutate(Month1 = percent(BN1/BC1, accuracy = .1),
         Month2 = percent(BN2/BC2, accuracy = .1),
         Month3 = percent(BN3/BC3, accuracy = .1),
         Month4 = percent(BN4/BC4, accuracy = .1),
         Month5 = percent(BN5/BC5, accuracy = .1),
         Month6 = percent(BN6/BC6, accuracy = .1),
         Month7 = percent(BN7/BC7, accuracy = .1),
         Month8 = percent(BN8/BC8, accuracy = .1),
         Month9 = percent(BN9/BC9, accuracy = .1),
         Month10 = percent(BN10/BC10, accuracy = .1),
         Month11 = percent(BN11/BC11, accuracy = .1),
         Month12 = percent(BN12/BC12, accuracy = .1)) %>%
  select(ProjectID, ProjectName, ProjectType, Month1, Month2, Month3, Month4,
         Month5, Month6, Month7, Month8, Month9, Month10, Month11, Month12)

rm(BedCapacity, BedNights)

#Inf means there were no beds but there were clients served.
#%NaN means there were no beds and no clients served that month.


# HH Utilization of Units ------------------------------------------------

HHUtilizers <- Utilizers %>%
  mutate(EntryAdjust = case_when(
    ProjectType %in% c(1, 2, 8) ~ EntryDate,
    ProjectType %in% c(3, 9, 13) ~ MoveInDate),
    ExitAdjust = if_else(is.na(ExitDate), mdy(ReportEnd), ymd(ExitDate)),
    ExitDate = NULL,
    StayWindow = interval(ymd(EntryAdjust), ymd(ExitAdjust))
  ) %>%
  filter(
    (str_detect(HouseholdID, fixed("s_")) |
       str_detect(HouseholdID, fixed("h_")) &
       RelationshipToHoH == 1) &
    int_overlaps(StayWindow, ReportingPeriod) &
      (
        (
          ProjectType %in% c(3, 9, 13) &
            !is.na(EntryAdjust) &
            ymd(MoveInDate) >= ymd(EntryDate) &
            ymd(MoveInDate) < ymd(ExitAdjust)
        ) |
          ProjectType %in% c(1, 2, 8)
      ) &
      !ProjectID %in% c(1775, 1695, 1849, 1032, 1030, 1031, 1317)) %>%
  select(-EntryDate, -MoveInDate, -HouseholdID, -RelationshipToHoH)

HHUtilizers <- HHUtilizers %>%
  mutate(
    Month1 = if_else(int_overlaps(StayWindow, FirstMonth),
                     round(as.numeric(difftime(
                       if_else(
                         ymd(ExitAdjust) <=  int_end(FirstMonth),
                         as.POSIXct(ExitAdjust) + days(1),
                         int_end(FirstMonth) + days(1)
                       ),
                       if_else(
                         ymd(EntryAdjust) >= int_start(FirstMonth),
                         as.POSIXct(EntryAdjust),
                         int_start(FirstMonth)
                       )-1,
                       units = "days"
                     )), digits = 0), NULL
    ),
    Month2 = if_else(int_overlaps(StayWindow, SecondMonth),
                     round(as.numeric(difftime(
                       if_else(
                         ymd(ExitAdjust) <=  int_end(SecondMonth),
                         as.POSIXct(ExitAdjust) + days(1),
                         int_end(SecondMonth) + days(1)
                       ),
                       if_else(
                         ymd(EntryAdjust) >= int_start(SecondMonth),
                         as.POSIXct(EntryAdjust),
                         int_start(SecondMonth)
                       )-1,
                       units = "days"
                     )), digits = 0), NULL
    ),
    Month3 = if_else(int_overlaps(StayWindow, ThirdMonth),
                     round(as.numeric(difftime(
                       if_else(
                         ymd(ExitAdjust) <=  int_end(ThirdMonth),
                         as.POSIXct(ExitAdjust) + days(1),
                         int_end(ThirdMonth) + days(1)
                       ),
                       if_else(
                         ymd(EntryAdjust) >= int_start(ThirdMonth),
                         as.POSIXct(EntryAdjust),
                         int_start(ThirdMonth)
                       )-1,
                       units = "days"
                     )), digits = 0), NULL
    ),
    Month4 = if_else(int_overlaps(StayWindow, FourthMonth),
                     round(as.numeric(difftime(
                       if_else(
                         ymd(ExitAdjust) <=  int_end(FourthMonth),
                         as.POSIXct(ExitAdjust) + days(1),
                         int_end(FourthMonth) + days(1)
                       ),
                       if_else(
                         ymd(EntryAdjust) >= int_start(FourthMonth),
                         as.POSIXct(EntryAdjust),
                         int_start(FourthMonth)
                       )-1,
                       units = "days"
                     )), digits = 0), NULL
    ),
    Month5 = if_else(int_overlaps(StayWindow, FifthMonth),
                     round(as.numeric(difftime(
                       if_else(
                         ymd(ExitAdjust) <=  int_end(FifthMonth),
                         as.POSIXct(ExitAdjust) + days(1),
                         int_end(FifthMonth) + days(1)
                       ),
                       if_else(
                         ymd(EntryAdjust) >= int_start(FifthMonth),
                         as.POSIXct(EntryAdjust),
                         int_start(FifthMonth)
                       )-1,
                       units = "days"
                     )), digits = 0), NULL
    ),
    Month6 = if_else(int_overlaps(StayWindow, SixthMonth),
                     round(as.numeric(difftime(
                       if_else(
                         ymd(ExitAdjust) <=  int_end(SixthMonth),
                         as.POSIXct(ExitAdjust) + days(1),
                         int_end(SixthMonth) + days(1)
                       ),
                       if_else(
                         ymd(EntryAdjust) >= int_start(SixthMonth),
                         as.POSIXct(EntryAdjust),
                         int_start(SixthMonth)
                       )-1,
                       units = "days"
                     )), digits = 0), NULL),
    Month7 = if_else(int_overlaps(StayWindow, SeventhMonth),
                     round(as.numeric(difftime(
                       if_else(
                         ymd(ExitAdjust) <=  int_end(SeventhMonth),
                         as.POSIXct(ExitAdjust) + days(1),
                         int_end(SeventhMonth) + days(1)
                       ),
                       if_else(
                         ymd(EntryAdjust) >= int_start(SeventhMonth),
                         as.POSIXct(EntryAdjust),
                         int_start(SeventhMonth)
                       )-1,
                       units = "days"
                     )), digits = 0), NULL),
    Month8 = if_else(int_overlaps(StayWindow, EighthMonth),
                     round(as.numeric(difftime(
                       if_else(
                         ymd(ExitAdjust) <=  int_end(EighthMonth),
                         as.POSIXct(ExitAdjust) + days(1),
                         int_end(EighthMonth) + days(1)
                       ),
                       if_else(
                         ymd(EntryAdjust) >= int_start(EighthMonth),
                         as.POSIXct(EntryAdjust),
                         int_start(EighthMonth)
                       )-1,
                       units = "days"
                     )), digits = 0), NULL),
    Month9 = if_else(int_overlaps(StayWindow, NinthMonth),
                     round(as.numeric(difftime(
                       if_else(
                         ymd(ExitAdjust) <=  int_end(NinthMonth),
                         as.POSIXct(ExitAdjust) + days(1),
                         int_end(NinthMonth) + days(1)
                       ),
                       if_else(
                         ymd(EntryAdjust) >= int_start(NinthMonth),
                         as.POSIXct(EntryAdjust),
                         int_start(NinthMonth)
                       )-1,
                       units = "days"
                     )), digits = 0), NULL ),
    Month10 = if_else(int_overlaps(StayWindow, TenthMonth),
                      round(as.numeric(difftime(
                        if_else(
                          ymd(ExitAdjust) <=  int_end(TenthMonth),
                          as.POSIXct(ExitAdjust) + days(1),
                          int_end(TenthMonth) + days(1)
                        ),
                        if_else(
                          ymd(EntryAdjust) >= int_start(TenthMonth),
                          as.POSIXct(EntryAdjust),
                          int_start(TenthMonth)
                        )-1,
                        units = "days"
                      )), digits = 0), NULL ),
    Month11 = if_else(int_overlaps(StayWindow, EleventhMonth),
                      round(as.numeric(difftime(
                        if_else(
                          ymd(ExitAdjust) <=  int_end(EleventhMonth),
                          as.POSIXct(ExitAdjust) + days(1),
                          int_end(EleventhMonth) + days(1)
                        ),
                        if_else(
                          ymd(EntryAdjust) >= int_start(EleventhMonth),
                          as.POSIXct(EntryAdjust),
                          int_start(EleventhMonth)
                        )-1,
                        units = "days"
                      )), digits = 0), NULL ),
    Month12 = if_else(int_overlaps(StayWindow, TwelfthMonth),
                      round(as.numeric(difftime(
                        if_else(
                          ymd(ExitAdjust) <=  int_end(TwelfthMonth),
                          as.POSIXct(ExitAdjust) + days(1),
                          int_end(TwelfthMonth) + days(1)
                        ),
                        if_else(
                          ymd(EntryAdjust) >= int_start(TwelfthMonth),
                          as.POSIXct(EntryAdjust),
                          int_start(TwelfthMonth)
                        ),
                        units = "days"
                      )), digits = 0), NULL )
    )
HHUtilizers <- as.data.frame(HHUtilizers)
# making granularity by provider instead of by enrollment id
HHNights <- HHUtilizers %>%
  group_by(ProjectName, ProjectID, ProjectType) %>%
  summarise(HN1 = sum(Month1, na.rm = TRUE),
            HN2 = sum(Month2, na.rm = TRUE),
            HN3 = sum(Month3, na.rm = TRUE),
            HN4 = sum(Month4, na.rm = TRUE),
            HN5 = sum(Month5, na.rm = TRUE),
            HN6 = sum(Month6, na.rm = TRUE),
            HN7 = sum(Month7, na.rm = TRUE),
            HN8 = sum(Month8, na.rm = TRUE),
            HN9 = sum(Month9, na.rm = TRUE),
            HN10 = sum(Month10, na.rm = TRUE),
            HN11 = sum(Month11, na.rm = TRUE),
            HN12 = sum(Month12, na.rm = TRUE))
rm(HHUtilizers)

# Unit Capacity -----------------------------------------------------------

UnitCapacity <- Beds %>%
  select(ProjectID,
         ProjectName,
         ProjectType,
         HouseholdType,
         UnitInventory,
         BedInventory,
         InventoryStartDate,
         InventoryEndDate) %>%
  mutate(InventoryEndAdjust = if_else(is.na(InventoryEndDate),
                                      mdy(ReportEnd),
                                      ymd(InventoryEndDate)),
         InventoryStartAdjust = if_else(ymd(InventoryStartDate) >= mdy(ReportStart),
                                        ymd(InventoryStartDate),
                                        mdy(ReportStart)),
         AvailableWindow = interval(ymd(InventoryStartAdjust),
                                    ymd(InventoryEndAdjust)),
         UnitCount = if_else(HouseholdType == 3,
                             UnitInventory, BedInventory)) 
UnitCapacity <- UnitCapacity %>%
  mutate(
    Month1 = if_else(int_overlaps(AvailableWindow, FirstMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(FirstMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(FirstMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(FirstMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(FirstMonth)
                       ),
                       units = "days"
                     ))+1) * UnitCount, NULL
    ),
    Month2 = if_else(int_overlaps(AvailableWindow, SecondMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(SecondMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(SecondMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(SecondMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(SecondMonth)
                       ),
                       units = "days"
                     ))+1) * UnitCount, NULL
    ),
    Month3 = if_else(int_overlaps(AvailableWindow, ThirdMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(ThirdMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(ThirdMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(ThirdMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(ThirdMonth)
                       ),
                       units = "days"
                     ))+1) * UnitCount, NULL
    ),
    Month4 = if_else(int_overlaps(AvailableWindow, FourthMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(FourthMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(FourthMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(FourthMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(FourthMonth)
                       ),
                       units = "days"
                     ))+1) * UnitCount, NULL
    ),
    Month5 = if_else(int_overlaps(AvailableWindow, FifthMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(FifthMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(FifthMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(FifthMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(FifthMonth)
                       ),
                       units = "days"
                     ))+1) * UnitCount, NULL
    ),
    Month6 = if_else(int_overlaps(AvailableWindow, SixthMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(SixthMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(SixthMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(SixthMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(SixthMonth)
                       ),
                       units = "days"
                     ))+1) * UnitCount, NULL
    ),
    Month7 = if_else(int_overlaps(AvailableWindow, SeventhMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(SeventhMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(SeventhMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(SeventhMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(SeventhMonth)
                       ),
                       units = "days"
                     ))+1) * UnitCount, NULL
    ),
    Month8 = if_else(int_overlaps(AvailableWindow, EighthMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(EighthMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(EighthMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(EighthMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(EighthMonth)
                       ),
                       units = "days"
                     ))+1) * UnitCount, NULL
    ),
    Month9 = if_else(int_overlaps(AvailableWindow, NinthMonth),
                     (as.numeric(difftime(
                       if_else(
                         ymd(InventoryEndAdjust) <=  int_end(NinthMonth),
                         as.POSIXct(InventoryEndAdjust),
                         int_end(NinthMonth)
                       ),
                       if_else(
                         ymd(InventoryStartAdjust) >= int_start(NinthMonth),
                         as.POSIXct(InventoryStartAdjust),
                         int_start(NinthMonth)
                       ),
                       units = "days"
                     ))+1) * UnitCount, NULL
    ),
    Month10 = if_else(int_overlaps(AvailableWindow, TenthMonth),
                      (as.numeric(difftime(
                        if_else(
                          ymd(InventoryEndAdjust) <=  int_end(TenthMonth),
                          as.POSIXct(InventoryEndAdjust),
                          int_end(TenthMonth)
                        ),
                        if_else(
                          ymd(InventoryStartAdjust) >= int_start(TenthMonth),
                          as.POSIXct(InventoryStartAdjust),
                          int_start(TenthMonth)
                        ),
                        units = "days"
                      ))+1) * UnitCount, NULL
    ),
    Month11 = if_else(int_overlaps(AvailableWindow, EleventhMonth),
                      (as.numeric(difftime(
                        if_else(
                          ymd(InventoryEndAdjust) <=  int_end(EleventhMonth),
                          as.POSIXct(InventoryEndAdjust),
                          int_end(EleventhMonth)
                        ),
                        if_else(
                          ymd(InventoryStartAdjust) >= int_start(EleventhMonth),
                          as.POSIXct(InventoryStartAdjust),
                          int_start(EleventhMonth)
                        ),
                        units = "days"
                      ))+1) * UnitCount, NULL
    ),
    Month12 = if_else(int_overlaps(AvailableWindow, TwelfthMonth),
                      (as.numeric(difftime(
                        if_else(
                          ymd(InventoryEndAdjust) <=  int_end(TwelfthMonth),
                          as.POSIXct(InventoryEndAdjust),
                          int_end(TwelfthMonth)
                        ),
                        if_else(
                          ymd(InventoryStartAdjust) >= int_start(TwelfthMonth),
                          as.POSIXct(InventoryStartAdjust),
                          int_start(TwelfthMonth)
                        ),
                        units = "days"
                      ))+1) * UnitCount, NULL
    ) )


UnitCapacity <- UnitCapacity %>%
  group_by(ProjectID, ProjectName, ProjectType) %>%
  summarise(UC1 = sum(Month1, na.rm = TRUE),
            UC2 = sum(Month2, na.rm = TRUE),
            UC3 = sum(Month3, na.rm = TRUE),
            UC4 = sum(Month4, na.rm = TRUE),
            UC5 = sum(Month5, na.rm = TRUE),
            UC6 = sum(Month6, na.rm = TRUE),
            UC7 = sum(Month7, na.rm = TRUE),
            UC8 = sum(Month8, na.rm = TRUE),
            UC9 = sum(Month9, na.rm = TRUE),
            UC10 = sum(Month10, na.rm = TRUE),
            UC11 = sum(Month11, na.rm = TRUE),
            UC12 = sum(Month12, na.rm = TRUE))

UnitUtilization <- left_join(UnitCapacity,
                            HHNights,
                            by = c("ProjectID", "ProjectName", "ProjectType")) %>%
  mutate(Month1 = percent(HN1/UC1, accuracy = .1),
         Month2 = percent(HN2/UC2, accuracy = .1),
         Month3 = percent(HN3/UC3, accuracy = .1),
         Month4 = percent(HN4/UC4, accuracy = .1),
         Month5 = percent(HN5/UC5, accuracy = .1),
         Month6 = percent(HN6/UC6, accuracy = .1),
         Month7 = percent(HN7/UC7, accuracy = .1),
         Month8 = percent(HN8/UC8, accuracy = .1),
         Month9 = percent(HN9/UC9, accuracy = .1),
         Month10 = percent(HN10/UC10, accuracy = .1),
         Month11 = percent(HN11/UC11, accuracy = .1),
         Month12 = percent(HN12/UC12, accuracy = .1)) %>%
  select(ProjectID, ProjectName, ProjectType, Month1, Month2, Month3, Month4,
         Month5, Month6, Month7, Month8, Month9, Month10, Month11, Month12)
