library(tidyverse)
library(lubridate)
load("data/COHHIOHMIS.RData")

ReportStart <- "01012018"
ReportEnd <- "12312018"
ReportingPeriod <- interval(mdy(ReportStart), mdy(ReportEnd))

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
         ExitAdjust = if_else(is.na(ExitDate), today(), ExitDate),
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
  summarise(Month1 = sum(Month1, na.rm = TRUE),
            Month2 = sum(Month2, na.rm = TRUE),
            Month3 = sum(Month3, na.rm = TRUE),
            Month4 = sum(Month4, na.rm = TRUE),
            Month5 = sum(Month5, na.rm = TRUE),
            Month6 = sum(Month6, na.rm = TRUE),
            Month7 = sum(Month7, na.rm = TRUE),
            Month8 = sum(Month8, na.rm = TRUE),
            Month9 = sum(Month9, na.rm = TRUE),
            Month10 = sum(Month10, na.rm = TRUE),
            Month11 = sum(Month11, na.rm = TRUE),
            Month12 = sum(Month12, na.rm = TRUE))
rm(Utilizers)
# Possible Bed Nights -----------------------------------------------------

BedCapacity <- Beds %>%
  select(ProjectID, 
         ProjectName, 
         BedInventory, 
         InventoryStartDate, 
         InventoryEndDate) %>%
  mutate(InventoryEndAdjust = if_else(is.na(InventoryEndDate), today(),
                                      InventoryEndDate),
         AvailableWindow = interval(ymd(InventoryStartDate), 
                                    ymd(InventoryEndAdjust))
         ) %>%
  select(-InventoryStartDate, -InventoryEndDate, -InventoryEndAdjust)
