library(tidyverse)
library(lubridate)
library(scales)

load("images/COHHIOHMIS.RData")

# Creating Beds table -----------------------------------------------------

SmallProject <- Project %>%
  select(ProjectID,
         ProjectName,
         ProjectType) %>%
  filter(ProjectType %in% c(1, 2, 3, 8, 9) &
           operating_between(Project, FileStart, FileEnd) &
           is.na(Project$GrantType))

SmallInventory <- Inventory %>%
  select(
    ProjectID,
    HouseholdType,
    UnitInventory,
    BedInventory,
    InventoryStartDate,
    InventoryEndDate,
    HMISParticipatingBeds
  )  %>%
  filter((
    ymd(InventoryStartDate) <= mdy(FileEnd) &
      (
        ymd(InventoryEndDate) >= mdy(FileStart) |
          is.na(InventoryEndDate)
      )
  ) &
    !is.na(HMISParticipatingBeds) &
    Inventory$CoCCode == "OH-507")

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

rm(Affiliation, Client, Disabilities, EmploymentEducation, EnrollmentCoC, Exit, 
   Export, Funder, Geography, HealthAndDV, IncomeBenefits, Organization, 
   ProjectCoC, Scores, Services, SmallEnrollment, SmallInventory, SmallProject, 
   Users, Offers, VeteranCE)
# Client Utilization of Beds ----------------------------------------------

# filtering out any PSH or RRH records without a proper Move-In Date plus the 
# fake training providers
ClientUtilizers <- Utilizers %>%
  mutate(EntryAdjust = case_when(
           ProjectType %in% c(1, 2, 8) ~ EntryDate,
           ProjectType %in% c(3, 9) ~ MoveInDate),
         ExitAdjust = if_else(is.na(ExitDate), today(), ymd(ExitDate)),
         StayWindow = interval(ymd(EntryAdjust), ymd(ExitAdjust))
           ) %>%
  filter(
    int_overlaps(StayWindow, FilePeriod) &
      (
    (
      ProjectType %in% c(3, 9) &
        !is.na(EntryAdjust) &
        ymd(MoveInDate) >= ymd(EntryDate) &
        ymd(MoveInDate) < ymd(ExitAdjust)
    ) |
      ProjectType %in% c(1, 2, 8)
  ) &
    !ProjectID %in% c(1775, 1695, 1849, 1032, 1030, 1031, 1317)) %>%
  select(-EntryDate, -MoveInDate, -ExitDate)

# function for adding bed nights per ee

bed_nights_per_ee <- function(table, interval) {
  if_else(int_overlaps(table$StayWindow, interval),
          as.numeric(difftime(
            if_else(
              ymd(table$ExitAdjust) <=  int_end(interval),
              as.POSIXct(table$ExitAdjust) + days(1),
              int_end(interval) + days(1)
            ),
            if_else(
              ymd(table$EntryAdjust) >= int_start(interval),
              as.POSIXct(table$EntryAdjust),
              int_start(interval)
            ),
            units = "days"
          )), NULL
  )
}

nth_Month <- function(n) {
  interval(mdy(FileStart) %m+% months(n - 1),
           seq(as.Date(mdy(FileStart) %m+% months(n)),
               length = 1, by = "1 month") - 1)
}
FirstMonth <- nth_Month(1)
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

ClientUtilizers <- ClientUtilizers %>%
  mutate(
    FilePeriod = bed_nights_per_ee(ClientUtilizers, FilePeriod),
    Month1 = bed_nights_per_ee(ClientUtilizers, FirstMonth),
    Month2 = bed_nights_per_ee(ClientUtilizers, SecondMonth),
    Month3 = bed_nights_per_ee(ClientUtilizers, ThirdMonth),
    Month4 = bed_nights_per_ee(ClientUtilizers, FourthMonth),
    Month5 = bed_nights_per_ee(ClientUtilizers, FifthMonth),
    Month6 = bed_nights_per_ee(ClientUtilizers, SixthMonth),
    Month7 = bed_nights_per_ee(ClientUtilizers, SeventhMonth),
    Month8 = bed_nights_per_ee(ClientUtilizers, EighthMonth),
    Month9 = bed_nights_per_ee(ClientUtilizers, NinthMonth),
    Month10 = bed_nights_per_ee(ClientUtilizers, TenthMonth),
    Month11 = bed_nights_per_ee(ClientUtilizers, EleventhMonth),
    Month12 = bed_nights_per_ee(ClientUtilizers, TwelfthMonth),
    Month13 = bed_nights_per_ee(ClientUtilizers, ThirteenthMonth),
    Month14 = bed_nights_per_ee(ClientUtilizers, FourteenthMonth),
    Month15 = bed_nights_per_ee(ClientUtilizers, FifteenthMonth),
    Month16 = bed_nights_per_ee(ClientUtilizers, SixteenthMonth),
    Month17 = bed_nights_per_ee(ClientUtilizers, SeventeenthMonth),
    Month18 = bed_nights_per_ee(ClientUtilizers, EighteenthMonth),
    Month19 = bed_nights_per_ee(ClientUtilizers, NineteenthMonth),
    Month20 = bed_nights_per_ee(ClientUtilizers, TwentiethMonth),
    Month21 = bed_nights_per_ee(ClientUtilizers, TwentyfirstMonth),
    Month22 = bed_nights_per_ee(ClientUtilizers, TwentysecondMonth),
    Month23 = bed_nights_per_ee(ClientUtilizers, TwentythirdMonth),
    Month24 = bed_nights_per_ee(ClientUtilizers, TwentyfourthMonth)
  ) %>%
  select(ProjectName, ProjectID, ProjectType, PersonalID, EnrollmentID, 
         FilePeriod, starts_with("Month"))
ClientUtilizers <- as.data.frame(ClientUtilizers)
# making granularity by provider instead of by enrollment id
BedNights <- ClientUtilizers %>%
  group_by(ProjectName, ProjectID, ProjectType) %>%
  summarise(
    BNY = sum(FilePeriod, na.rm = TRUE),
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
  )
# rm(ClientUtilizers) ClientUtilizers may be useful for R minor elevated
# Bed Capacity ------------------------------------------------------------

BedCapacity <- Beds %>%
  select(ProjectID,
         ProjectName,
         ProjectType,
         BedInventory,
         InventoryStartDate,
         InventoryEndDate) %>%
  mutate(InventoryEndAdjust = if_else(is.na(InventoryEndDate),
                                      mdy(FileEnd),
                                      ymd(InventoryEndDate)),
         InventoryStartAdjust = if_else(ymd(InventoryStartDate) >= mdy(FileStart),
                                        ymd(InventoryStartDate),
                                        mdy(FileStart)),
         AvailableWindow = interval(ymd(InventoryStartAdjust),
                                    ymd(InventoryEndAdjust))) 

# function for bed capacity at the bed record level

bed_capacity <- function(interval) {
  if_else(int_overlaps(BedCapacity$AvailableWindow, interval),
          (as.numeric(difftime(
            if_else(
              ymd(BedCapacity$InventoryEndAdjust) <=  int_end(interval),
              as.POSIXct(BedCapacity$InventoryEndAdjust),
              int_end(interval)
            ),
            if_else(
              ymd(BedCapacity$InventoryStartAdjust) >= int_start(interval),
              as.POSIXct(BedCapacity$InventoryStartAdjust),
              int_start(interval)
            ),
            units = "days"
          ))+1) * BedCapacity$BedInventory, NULL
  )
}

BedCapacity <- BedCapacity %>%
  mutate(
    FilePeriod = bed_capacity(FilePeriod),
    Month1 = bed_capacity(FirstMonth),
    Month2 = bed_capacity(SecondMonth),
    Month3 = bed_capacity(ThirdMonth),
    Month4 = bed_capacity(FourthMonth),
    Month5 = bed_capacity(FifthMonth),
    Month6 = bed_capacity(SixthMonth),
    Month7 = bed_capacity(SeventhMonth),
    Month8 = bed_capacity(EighthMonth),
    Month9 = bed_capacity(NinthMonth),
    Month10 = bed_capacity(TenthMonth),
    Month11 = bed_capacity(EleventhMonth),
    Month12 = bed_capacity(TwelfthMonth),
    Month13 = bed_capacity(ThirteenthMonth),
    Month14 = bed_capacity(FourteenthMonth),
    Month15 = bed_capacity(FifteenthMonth),
    Month16 = bed_capacity(SixteenthMonth),
    Month17 = bed_capacity(SeventeenthMonth),
    Month18 = bed_capacity(EighteenthMonth),
    Month19 = bed_capacity(NineteenthMonth),
    Month20 = bed_capacity(TwentiethMonth),
    Month21 = bed_capacity(TwentyfirstMonth),
    Month22 = bed_capacity(TwentysecondMonth),
    Month23 = bed_capacity(TwentythirdMonth),
    Month24 = bed_capacity(TwentyfourthMonth)
  ) %>%
  select(
    -InventoryStartDate,
    -InventoryEndDate,
    -InventoryEndAdjust,-BedInventory,
    -InventoryStartAdjust,
    -AvailableWindow
  )

BedCapacity <- BedCapacity %>%
  group_by(ProjectID, ProjectName, ProjectType) %>%
  summarise(
    BCY = sum(FilePeriod, na.rm = TRUE),
    BC1 = sum(Month1, na.rm = TRUE),
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
    BC12 = sum(Month12, na.rm = TRUE),
    BC13 = sum(Month13, na.rm = TRUE),
    BC14 = sum(Month14, na.rm = TRUE),
    BC15 = sum(Month15, na.rm = TRUE),
    BC16 = sum(Month16, na.rm = TRUE),
    BC17 = sum(Month17, na.rm = TRUE),
    BC18 = sum(Month18, na.rm = TRUE),
    BC19 = sum(Month19, na.rm = TRUE),
    BC20 = sum(Month20, na.rm = TRUE),
    BC21 = sum(Month21, na.rm = TRUE),
    BC22 = sum(Month22, na.rm = TRUE),
    BC23 = sum(Month23, na.rm = TRUE),
    BC24 = sum(Month24, na.rm = TRUE)
  )
# Bed Utilization ---------------------------------------------------------

BedUtilization <- 
  left_join(BedCapacity,
            BedNights,
            by = c("ProjectID", "ProjectName", "ProjectType")) %>%
  mutate(
    FilePeriod = BNY / BCY, accuracy = .1,
    Month1 = BN1 / BC1, accuracy = .1,
    Month2 = BN2 / BC2, accuracy = .1,
    Month3 = BN3 / BC3, accuracy = .1,
    Month4 = BN4 / BC4, accuracy = .1,
    Month5 = BN5 / BC5, accuracy = .1,
    Month6 = BN6 / BC6, accuracy = .1,
    Month7 = BN7 / BC7, accuracy = .1,
    Month8 = BN8 / BC8, accuracy = .1,
    Month9 = BN9 / BC9, accuracy = .1,
    Month10 = BN10 / BC10, accuracy = .1,
    Month11 = BN11 / BC11, accuracy = .1,
    Month12 = BN12 / BC12, accuracy = .1,
    Month13 = BN13 / BC13, accuracy = .1,
    Month14 = BN14 / BC14, accuracy = .1,
    Month15 = BN15 / BC15, accuracy = .1,
    Month16 = BN16 / BC16, accuracy = .1,
    Month17 = BN17 / BC17, accuracy = .1,
    Month18 = BN18 / BC18, accuracy = .1,
    Month19 = BN19 / BC19, accuracy = .1,
    Month20 = BN20 / BC20, accuracy = .1,
    Month21 = BN21 / BC21, accuracy = .1,
    Month22 = BN22 / BC22, accuracy = .1,
    Month23 = BN23 / BC23, accuracy = .1,
    Month24= BN24 / BC24, accuracy = .1
  ) %>% 
  select(ProjectID, ProjectName, ProjectType, FilePeriod, starts_with("Month"))

rm(BedCapacity, BedNights) #leaving this in as it's too aggregated for R minor 
# elevated

names(BedUtilization) <-
  c(
    "ProjectID",
    "ProjectName",
    "ProjectType",
    "FilePeriod",
    format.Date(int_start(FirstMonth), "%m%d%Y"),
    format.Date(int_start(SecondMonth), "%m%d%Y"),
    format.Date(int_start(ThirdMonth), "%m%d%Y"),
    format.Date(int_start(FourthMonth), "%m%d%Y"),
    format.Date(int_start(FifthMonth), "%m%d%Y"),
    format.Date(int_start(SixthMonth), "%m%d%Y"),
    format.Date(int_start(SeventhMonth), "%m%d%Y"),
    format.Date(int_start(EighthMonth), "%m%d%Y"),
    format.Date(int_start(NinthMonth), "%m%d%Y"),
    format.Date(int_start(TenthMonth), "%m%d%Y"),
    format.Date(int_start(EleventhMonth), "%m%d%Y"),
    format.Date(int_start(TwelfthMonth), "%m%d%Y"),
    format.Date(int_start(ThirteenthMonth), "%m%d%Y"),
    format.Date(int_start(FourteenthMonth), "%m%d%Y"),
    format.Date(int_start(FifteenthMonth), "%m%d%Y"),
    format.Date(int_start(SixteenthMonth), "%m%d%Y"),
    format.Date(int_start(SeventeenthMonth), "%m%d%Y"),
    format.Date(int_start(EighteenthMonth), "%m%d%Y"),
    format.Date(int_start(NineteenthMonth), "%m%d%Y"),
    format.Date(int_start(TwentiethMonth), "%m%d%Y"),
    format.Date(int_start(TwentyfirstMonth), "%m%d%Y"),
    format.Date(int_start(TwentysecondMonth), "%m%d%Y"),
    format.Date(int_start(TwentythirdMonth), "%m%d%Y"),
    format.Date(int_start(TwentyfourthMonth), "%m%d%Y")
  )

#Inf means there were no beds but there were clients served.
#%NaN means there were no beds and no clients served that month.

# HH Utilization of Units -------------------------------------------------

HHUtilizers <- Utilizers %>%
  mutate(
    EntryAdjust = case_when(
      ProjectType %in% c(1, 2, 8) ~ EntryDate,
      ProjectType %in% c(3, 9) ~ MoveInDate
    ),
    ExitAdjust = if_else(is.na(ExitDate) & ymd(EntryAdjust) <= mdy(FileEnd), 
                         mdy(FileEnd), 
                         ymd(ExitDate)),
    ExitDate = NULL,
    StayWindow = interval(ymd(EntryAdjust), ymd(ExitAdjust))
  ) %>% 
  filter(
    str_detect(HouseholdID, fixed("s_")) |
       (str_detect(HouseholdID, fixed("h_")) &
       RelationshipToHoH == 1) &
    int_overlaps(StayWindow, FilePeriod) &
      (
        (
          ProjectType %in% c(3, 9) &
            !is.na(EntryAdjust) &
            ymd(MoveInDate) >= ymd(EntryDate) &
            ymd(MoveInDate) <= ymd(ExitAdjust)
        ) |
          ProjectType %in% c(1, 2, 8)
      ) &
      !ProjectID %in% c(1775, 1695, 1849, 1032, 1030, 1031, 1317)) %>%
  select(-EntryDate, -MoveInDate, -HouseholdID, -RelationshipToHoH)

HHUtilizers <- HHUtilizers %>%
  mutate(
    FilePeriod = bed_nights_per_ee(HHUtilizers, FilePeriod),
    Month1 = bed_nights_per_ee(HHUtilizers, FirstMonth),
    Month2 = bed_nights_per_ee(HHUtilizers, SecondMonth),
    Month3 = bed_nights_per_ee(HHUtilizers, ThirdMonth),
    Month4 = bed_nights_per_ee(HHUtilizers, FourthMonth),
    Month5 = bed_nights_per_ee(HHUtilizers, FifthMonth),
    Month6 = bed_nights_per_ee(HHUtilizers, SixthMonth),
    Month7 = bed_nights_per_ee(HHUtilizers, SeventhMonth),
    Month8 = bed_nights_per_ee(HHUtilizers, EighthMonth),
    Month9 = bed_nights_per_ee(HHUtilizers, NinthMonth),
    Month10 = bed_nights_per_ee(HHUtilizers, TenthMonth),
    Month11 = bed_nights_per_ee(HHUtilizers, EleventhMonth),
    Month12 = bed_nights_per_ee(HHUtilizers, TwelfthMonth),
    Month13 = bed_nights_per_ee(HHUtilizers, ThirteenthMonth),
    Month14 = bed_nights_per_ee(HHUtilizers, FourteenthMonth),
    Month15 = bed_nights_per_ee(HHUtilizers, FifteenthMonth),
    Month16 = bed_nights_per_ee(HHUtilizers, SixteenthMonth),
    Month17 = bed_nights_per_ee(HHUtilizers, SeventeenthMonth),
    Month18 = bed_nights_per_ee(HHUtilizers, EighteenthMonth),
    Month19 = bed_nights_per_ee(HHUtilizers, NineteenthMonth),
    Month20 = bed_nights_per_ee(HHUtilizers, TwentiethMonth),
    Month21 = bed_nights_per_ee(HHUtilizers, TwentyfirstMonth),
    Month22 = bed_nights_per_ee(HHUtilizers, TwentysecondMonth),
    Month23 = bed_nights_per_ee(HHUtilizers, TwentythirdMonth),
    Month24 = bed_nights_per_ee(HHUtilizers, TwentyfourthMonth)
    )
HHUtilizers <- as.data.frame(HHUtilizers)
# making granularity by provider instead of by enrollment id
HHNights <- HHUtilizers %>%
  group_by(ProjectName, ProjectID, ProjectType) %>%
  summarise(
    HNY = sum(FilePeriod, na.rm = TRUE),
    HN1 = sum(Month1, na.rm = TRUE),
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
    HN12 = sum(Month12, na.rm = TRUE),
    HN13 = sum(Month13, na.rm = TRUE),
    HN14 = sum(Month14, na.rm = TRUE),
    HN15 = sum(Month15, na.rm = TRUE),
    HN16 = sum(Month16, na.rm = TRUE),
    HN17 = sum(Month17, na.rm = TRUE),
    HN18 = sum(Month18, na.rm = TRUE),
    HN19 = sum(Month19, na.rm = TRUE),
    HN20 = sum(Month20, na.rm = TRUE),
    HN21 = sum(Month21, na.rm = TRUE),
    HN22 = sum(Month22, na.rm = TRUE),
    HN23 = sum(Month23, na.rm = TRUE),
    HN24 = sum(Month24, na.rm = TRUE)
  )
rm(HHUtilizers) # leaving this one because the client-level detail should be
# good enough for R minor elevated

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
                                      mdy(FileEnd),
                                      ymd(InventoryEndDate)),
         InventoryStartAdjust = if_else(ymd(InventoryStartDate) >= mdy(FileStart),
                                        ymd(InventoryStartDate),
                                        mdy(FileStart)),
         AvailableWindow = interval(ymd(InventoryStartAdjust),
                                    ymd(InventoryEndAdjust)),
         UnitCount = if_else(HouseholdType == 3,
                             UnitInventory, BedInventory)) 

# function to calculate unit capacity at the bed record level

unit_capacity <- function(interval) {
  if_else(
    int_overlaps(UnitCapacity$AvailableWindow, interval),
    (as.numeric(
      difftime(
        if_else(
          ymd(UnitCapacity$InventoryEndAdjust) <=  int_end(interval),
          as.POSIXct(UnitCapacity$InventoryEndAdjust),
          int_end(interval)
        ),
        if_else(
          ymd(UnitCapacity$InventoryStartAdjust) >= int_start(interval),
          as.POSIXct(UnitCapacity$InventoryStartAdjust),
          int_start(interval)
        ),
        units = "days"
      )
    ) + 1) * UnitCapacity$UnitCount,
    NULL
  )
}

UnitCapacity <- UnitCapacity %>%
  mutate(
    FilePeriod = unit_capacity(FilePeriod),
    Month1 = unit_capacity(FirstMonth),
    Month2 = unit_capacity(SecondMonth),
    Month3 = unit_capacity(ThirdMonth),
    Month4 = unit_capacity(FourthMonth),
    Month5 = unit_capacity(FifthMonth),
    Month6 = unit_capacity(SixthMonth),
    Month7 = unit_capacity(SeventhMonth),
    Month8 = unit_capacity(EighthMonth),
    Month9 = unit_capacity(NinthMonth),
    Month10 = unit_capacity(TenthMonth),
    Month11 = unit_capacity(EleventhMonth),
    Month12 = unit_capacity(TwelfthMonth),
    Month13 = unit_capacity(ThirteenthMonth),
    Month14 = unit_capacity(FourteenthMonth),
    Month15 = unit_capacity(FifteenthMonth),
    Month16 = unit_capacity(SixteenthMonth),
    Month17 = unit_capacity(SeventeenthMonth),
    Month18 = unit_capacity(EighteenthMonth),
    Month19 = unit_capacity(NineteenthMonth),
    Month20 = unit_capacity(TwentiethMonth),
    Month21 = unit_capacity(TwentyfirstMonth),
    Month22 = unit_capacity(TwentysecondMonth),
    Month23 = unit_capacity(TwentythirdMonth),
    Month24 = unit_capacity(TwentyfourthMonth))

UnitCapacity <- UnitCapacity %>%
  group_by(ProjectID, ProjectName, ProjectType) %>%
  summarise(
    UCY = sum(FilePeriod, na.rm = TRUE),
    UC1 = sum(Month1, na.rm = TRUE),
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
    UC12 = sum(Month12, na.rm = TRUE),
    UC13 = sum(Month13, na.rm = TRUE),
    UC14 = sum(Month14, na.rm = TRUE),
    UC15 = sum(Month15, na.rm = TRUE),
    UC16 = sum(Month16, na.rm = TRUE),
    UC17 = sum(Month17, na.rm = TRUE),
    UC18 = sum(Month18, na.rm = TRUE),
    UC19 = sum(Month19, na.rm = TRUE),
    UC20 = sum(Month20, na.rm = TRUE),
    UC21 = sum(Month21, na.rm = TRUE),
    UC22 = sum(Month22, na.rm = TRUE),
    UC23 = sum(Month23, na.rm = TRUE),
    UC24 = sum(Month24, na.rm = TRUE)
  )

# Unit Utilization --------------------------------------------------------

UnitUtilization <- left_join(UnitCapacity,
                            HHNights,
                            by = c("ProjectID", "ProjectName", "ProjectType")) %>%
  mutate(
    FilePeriod = HNY / UCY, accuracy = .1,
    Month1 = HN1 / UC1, accuracy = .1,
    Month2 = HN2 / UC2, accuracy = .1,
    Month3 = HN3 / UC3, accuracy = .1,
    Month4 = HN4 / UC4, accuracy = .1,
    Month5 = HN5 / UC5, accuracy = .1,
    Month6 = HN6 / UC6, accuracy = .1,
    Month7 = HN7 / UC7, accuracy = .1,
    Month8 = HN8 / UC8, accuracy = .1,
    Month9 = HN9 / UC9, accuracy = .1,
    Month10 = HN10 / UC10, accuracy = .1,
    Month11 = HN11 / UC11, accuracy = .1,
    Month12 = HN12 / UC12, accuracy = .1,
    Month13 = HN13 / UC13, accuracy = .1,
    Month14 = HN14 / UC14, accuracy = .1,
    Month15 = HN15 / UC15, accuracy = .1,
    Month16 = HN16 / UC16, accuracy = .1,
    Month17 = HN17 / UC17, accuracy = .1,
    Month18 = HN18 / UC18, accuracy = .1,
    Month19 = HN19 / UC19, accuracy = .1,
    Month20 = HN20 / UC20, accuracy = .1,
    Month21 = HN21 / UC21, accuracy = .1,
    Month22 = HN22 / UC22, accuracy = .1,
    Month23 = HN23 / UC23, accuracy = .1,
    Month24 = HN24 / UC24, accuracy = .1
  ) %>%
  select(ProjectID, ProjectName, ProjectType, FilePeriod, starts_with("Month"))
rm(UnitCapacity, HHNights, Beds, Utilizers)

names(UnitUtilization) <- 
  c("ProjectID", "ProjectName", "ProjectType", "FilePeriod",
    format.Date(int_start(FirstMonth), "%m%d%Y"),
    format.Date(int_start(SecondMonth), "%m%d%Y"),
    format.Date(int_start(ThirdMonth), "%m%d%Y"),
    format.Date(int_start(FourthMonth), "%m%d%Y"),
    format.Date(int_start(FifthMonth), "%m%d%Y"),
    format.Date(int_start(SixthMonth), "%m%d%Y"),
    format.Date(int_start(SeventhMonth), "%m%d%Y"),
    format.Date(int_start(EighthMonth), "%m%d%Y"),
    format.Date(int_start(NinthMonth), "%m%d%Y"),
    format.Date(int_start(TenthMonth), "%m%d%Y"),
    format.Date(int_start(EleventhMonth), "%m%d%Y"),
    format.Date(int_start(TwelfthMonth), "%m%d%Y"),
    format.Date(int_start(ThirteenthMonth), "%m%d%Y"),
    format.Date(int_start(FourteenthMonth), "%m%d%Y"),
    format.Date(int_start(FifteenthMonth), "%m%d%Y"),
    format.Date(int_start(SixteenthMonth), "%m%d%Y"),
    format.Date(int_start(SeventeenthMonth), "%m%d%Y"),
    format.Date(int_start(EighteenthMonth), "%m%d%Y"),
    format.Date(int_start(NineteenthMonth), "%m%d%Y"),
    format.Date(int_start(TwentiethMonth), "%m%d%Y"),
    format.Date(int_start(TwentyfirstMonth), "%m%d%Y"),
    format.Date(int_start(TwentysecondMonth), "%m%d%Y"),
    format.Date(int_start(TwentythirdMonth), "%m%d%Y"),
    format.Date(int_start(TwentyfourthMonth), "%m%d%Y"))

rm(bed_capacity, bed_nights_per_ee, unit_capacity)

SmallProject <- Project %>%
  filter(ProjectType %in% c(1, 2, 3, 8, 9) &
           ymd(OperatingStartDate) <= today() &
           (is.na(OperatingEndDate) | OperatingEndDate >= today()) &
           is.na(Project$GrantType)) %>%
  select(ProjectID,
         ProjectName,
         ProjectType, 
         OrganizationName)

# Current Bed Utilization -------------------------------------------------

SmallInventory <- Inventory %>%
  filter((ymd(InventoryStartDate) <= today() &
            (
              ymd(InventoryEndDate) >= today() |
                is.na(InventoryEndDate)
            )) &
           !is.na(HMISParticipatingBeds) &
           Inventory$CoCCode == "OH-507") %>%
  select(
    ProjectID,
    HouseholdType,
    UnitInventory,
    BedInventory,
    InventoryStartDate,
    InventoryEndDate,
    HMISParticipatingBeds
  )

SmallInventory <- inner_join(SmallProject, SmallInventory, by = "ProjectID")

Capacity <- SmallInventory %>%
  select(ProjectID,
         ProjectName,
         ProjectType,
         OrganizationName,
         HouseholdType,
         UnitInventory,
         BedInventory,
         InventoryStartDate,
         InventoryEndDate) %>%
  mutate(UnitCount = if_else(HouseholdType == 3,
                             UnitInventory, BedInventory)) %>%
  group_by(ProjectID, ProjectName, ProjectType, OrganizationName) %>%
  summarise(UnitCount = sum(UnitCount),
            BedCount = sum(BedInventory)) %>%
  ungroup()

providerids <- Capacity %>% 
  select(ProjectID, ProjectName, OrganizationName, ProjectType) %>%
  arrange(ProjectName) 
#here is where you could add a left join to the Regions object and add in Region

Clients <- Enrollment %>%
  left_join(., providerids, by = "ProjectID") %>%
  filter(is.na(ExitDate)) %>%
  group_by(ProjectID, ProjectName) %>%
  summarise(Clients = n_distinct(PersonalID)) %>%
  ungroup()

Households <- Enrollment %>%
  left_join(., providerids, by = "ProjectID") %>%
  filter(is.na(ExitDate)) %>%
  group_by(ProjectID, ProjectName) %>%
  summarise(Households = n_distinct(HouseholdID)) %>%
  ungroup()

Utilization <-
  left_join(Capacity, Clients,
            by = c("ProjectID", "ProjectName")) %>%
  left_join(., Households, 
            by = c("ProjectID", "ProjectName")) %>%
  filter(ProjectType %in% c(1, 2, 3, 8, 9)) %>%
  mutate(BedUtilization = percent(Clients/BedCount),
         UnitUtilization = percent(Households/UnitCount))

rm(Households, Clients, Capacity, Enrollment, Project, Inventory, 
   SmallInventory, SmallProject)

save.image("images/Utilization.RData")