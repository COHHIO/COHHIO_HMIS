library(tidyverse)
library(lubridate)
library(scales)
load("data/COHHIOHMIS.RData")

ReportStart <- "01012018"
ReportEnd <- "12312018"
ReportingPeriod <- interval(mdy(ReportStart), mdy(ReportEnd))

operating_between <- function(table, start, end) {
  operating <- ymd(table$OperatingStartDate) <= mdy(end) &
    (is.na(table$OperatingEndDate) | table$OperatingEndDate >= mdy(start))
  operating
}
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
         HouseholdID,
         MoveInDate)
SmallExit <- Exit %>%
  select(EnrollmentID, ExitDate)
Utilizers <- semi_join(SmallEnrollment, Beds, by = "ProjectID") %>%
  left_join(., SmallExit, by = "EnrollmentID")
# adding in the Project Type column here to calculate differently based on 
# MoveInDate
Utilizers <- left_join(Utilizers, SmallProject, by = "ProjectID") %>%
  select(
    PersonalID,
    EnrollmentID,
    ProjectID,
    ProjectName,
    ProjectType,
    HouseholdID,
    EntryDate,
    MoveInDate,
    ExitDate
  )

# Cleaning up the house ---------------------------------------------------

rm(Affiliation, Client, Disabilities, EmploymentEducation, Enrollment,
   EnrollmentCoC, Exit, Export, Funder, Geography, HealthAndDV, IncomeBenefits,
   Inventory, Organization, Project, ProjectCoC, Scores, Services, SmallEnrollment,
   SmallExit, SmallInventory, SmallProject, Users)

# Bed Nights Utilized -----------------------------------------------------

# filtering out any PSH or RRH records without a proper Move-In Date plus the 
# fake training providers
Utilizers <- Utilizers %>%
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
        ymd(MoveInDate) < ymd(EntryDate) &
        ymd(MoveInDate) > ymd(ExitAdjust)
    ) |
      ProjectType %in% c(1, 2, 8)
  ) &
    !ProjectID %in% c(1775, 1695, 1849, 1032, 1030, 1031, 1317)) %>%
  select(-EntryDate, -MoveInDate)


# Adding Month Intervals --------------------------------------------------

FirstMonth  <-  interval(mdy(ReportStart),
                         seq(as.Date(mdy(ReportStart) %m+% months(1)),
                             length = 1, by = "1 month") - 1) 
SecondMonth  <-  interval(mdy(ReportStart) %m+% months(1),
                          seq(as.Date(mdy(ReportStart) %m+% months(2)),
                              length=1, by="1 month") -1)
ThirdMonth <- interval(mdy(ReportStart) %m+% months(2),
                       seq(as.Date(mdy(ReportStart) %m+% months(3)),
                           length=1, by="1 month") -1)
FourthMonth <- interval(mdy(ReportStart) %m+% months(3),
                        seq(as.Date(mdy(ReportStart) %m+% months(4)),
                            length=1, by="1 month") -1)
FifthMonth <- interval(mdy(ReportStart) %m+% months(4),
                       seq(as.Date(mdy(ReportStart) %m+% months(5)),
                           length=1, by="1 month") -1)
SixthMonth <- interval(mdy(ReportStart) %m+% months(5),
                       seq(as.Date(mdy(ReportStart) %m+% months(6)),
                           length=1, by="1 month") -1)
SeventhMonth <- interval(mdy(ReportStart) %m+% months(6),
                         seq(as.Date(mdy(ReportStart) %m+% months(7)),
                             length=1, by="1 month") -1)
EighthMonth <- interval(mdy(ReportStart) %m+% months(7),
                        seq(as.Date(mdy(ReportStart) %m+% months(8)),
                            length=1, by="1 month") -1)
NinthMonth <- interval(mdy(ReportStart) %m+% months(8),
                       seq(as.Date(mdy(ReportStart) %m+% months(9)),
                           length=1, by="1 month") -1)
TenthMonth <- interval(mdy(ReportStart) %m+% months(9),
                       seq(as.Date(mdy(ReportStart) %m+% months(10)),
                           length=1, by="1 month") -1)
EleventhMonth <- interval(mdy(ReportStart) %m+% months(10),
                          seq(as.Date(mdy(ReportStart) %m+% months(11)),
                              length=1, by="1 month") -1)
TwelfthMonth <- interval(mdy(ReportStart) %m+% months(11),
                         seq(as.Date(mdy(ReportStart) %m+% months(12)),
                             length=1, by="1 month") -1)

# adding in month columns with utilization numbers

Utilizers <- Utilizers %>%
  mutate(
    Month1 = if_else(int_overlaps(StayWindow, FirstMonth),
                     as.numeric(difftime(
                       if_else(
                         ymd(ExitAdjust) <=  int_end(FirstMonth),
                         as.POSIXct(ExitAdjust),
                         int_end(FirstMonth)
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
                         as.POSIXct(ExitAdjust),
                         int_end(SecondMonth)
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
                         as.POSIXct(ExitAdjust),
                         int_end(ThirdMonth)
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
                         as.POSIXct(ExitAdjust),
                         int_end(FourthMonth)
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
                         as.POSIXct(ExitAdjust),
                         int_end(FifthMonth)
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
                         as.POSIXct(ExitAdjust),
                         int_end(SixthMonth)
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
                         as.POSIXct(ExitAdjust),
                         int_end(SeventhMonth)
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
                         as.POSIXct(ExitAdjust),
                         int_end(EighthMonth)
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
                         as.POSIXct(ExitAdjust),
                         int_end(NinthMonth)
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
                          as.POSIXct(ExitAdjust),
                          int_end(TenthMonth)
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
                          as.POSIXct(ExitAdjust),
                          int_end(EleventhMonth)
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
                          as.POSIXct(ExitAdjust),
                          int_end(TwelfthMonth)
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
Utilizers <- as.data.frame(Utilizers)

# grouping and summarising
BedNights <- Utilizers %>%
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
rm(Utilizers)
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
         InventoryStartAdjust = if_else(InventoryStartDate >= mdy(ReportStart),
                                        ymd(InventoryStartDate),
                                        mdy(ReportStart)),
         AvailableWindow = interval(ymd(InventoryStartAdjust),
                                    ymd(InventoryEndAdjust))) 
BedCapacity <- BedCapacity %>%
  mutate(
    Month1 = if_else(int_overlaps(AvailableWindow, FirstMonth),
                     as.numeric(difftime(
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
                     )) * BedInventory, NULL
    ),
    Month2 = if_else(int_overlaps(AvailableWindow, SecondMonth),
                     as.numeric(difftime(
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
                     )) * BedInventory, NULL
    ),
    Month3 = if_else(int_overlaps(AvailableWindow, ThirdMonth),
                     as.numeric(difftime(
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
                     )) * BedInventory, NULL
    ),
    Month4 = if_else(int_overlaps(AvailableWindow, FourthMonth),
                     as.numeric(difftime(
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
                     )) * BedInventory, NULL
    ),
    Month5 = if_else(int_overlaps(AvailableWindow, FifthMonth),
                     as.numeric(difftime(
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
                     )) * BedInventory, NULL
    ),
    Month6 = if_else(int_overlaps(AvailableWindow, SixthMonth),
                     as.numeric(difftime(
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
                     )) * BedInventory, NULL
    ),
    Month7 = if_else(int_overlaps(AvailableWindow, SeventhMonth),
                     as.numeric(difftime(
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
                     )) * BedInventory, NULL
    ),
    Month8 = if_else(int_overlaps(AvailableWindow, EighthMonth),
                     as.numeric(difftime(
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
                     )) * BedInventory, NULL
    ),
    Month9 = if_else(int_overlaps(AvailableWindow, NinthMonth),
                     as.numeric(difftime(
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
                     )) * BedInventory, NULL
    ),
    Month10 = if_else(int_overlaps(AvailableWindow, TenthMonth),
                     as.numeric(difftime(
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
                     )) * BedInventory, NULL
    ),
    Month11 = if_else(int_overlaps(AvailableWindow, EleventhMonth),
                     as.numeric(difftime(
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
                     )) * BedInventory, NULL
    ),
    Month12 = if_else(int_overlaps(AvailableWindow, TwelfthMonth),
                     as.numeric(difftime(
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
                     )) * BedInventory, NULL
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

Experiment <- left_join(BedCapacity,
                        BedNights,
                        by = c("ProjectID", "ProjectName", "ProjectType")) %>%
  mutate(Month1 = BN1/BC1,
         BN1 = NULL,
         BC1 = NULL,
         Month2 = BN2/BC2,
         BN2 = NULL,
         BC2 = NULL,
         Month3 = BN3/BC3,
         BN3 = NULL,
         BC3 = NULL,
         Month4 = BN4/BC4,
         BN4 = NULL,
         BC4 = NULL,
         Month5 = BN5/BC5,
         BN5 = NULL,
         BC5 = NULL,
         Month6 = BN6/BC6,
         BN6 = NULL,
         BC6 = NULL,
         Month7 = BN7/BC7,
         BN7 = NULL,
         BC7 = NULL,
         Month8 = BN8/BC8,
         BN8 = NULL,
         BC8 = NULL,
         Month9 = BN9/BC9,
         BN9 = NULL,
         BC9 = NULL,
         Month10 = BN10/BC10,
         BN10 = NULL,
         BC10 = NULL,
         Month11 = BN11/BC11,
         BN11 = NULL,
         BC11 = NULL,
         Month12 = BN12/BC12,
         BN12 = NULL,
         BC12 = NULL)
  