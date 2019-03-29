library(tidyverse)
library(lubridate)
load("data/COHHIOHMIS.RData")



# Creating Beds table -----------------------------------------------------

SmallProject <- Project %>%
  filter(ProjectType %in% c(1, 2, 3, 8, 9) &
           (OperatingEndDate > today() |
              is.na(OperatingEndDate)) &
           is.na(GrantType)) %>%
  select(ProjectID,
         ProjectName,
         OperatingStartDate,
         OperatingEndDate,
         ProjectType)
SmallInventory <- Inventory %>%
  filter(!is.na(HMISParticipatingBeds) & 
           CoCCode == "OH-507" &
           (InventoryEndDate > today() |
              is.na(InventoryEndDate))) %>%
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
    ProjectType,
    HouseholdID,
    EntryDate,
    MoveInDate,
    ExitDate
  )

# Cleaning up the house ---------------------------------------------------

save(Beds, Utilizers, file = "data/BedUtilization.Rdata")
rm(list = ls())
load("data/BedUtilization.Rdata")
ReportStart <- "01012019"
ReportEnd <- "04302019"
# Bed Nights Utilized -----------------------------------------------------

# filtering out any PSH or RRH records without a proper Move-In Date plus the 
# fake training providers
Utilizers <- Utilizers %>%
  mutate(ExitAdjust = if_else(is.na(ExitDate), today(), ExitDate),
         ExitDate = NULL,
         EntryAdjust = case_when(
           ProjectType %in% c(1, 2, 8) ~ EntryDate,
           ProjectType %in% c(3, 9, 13) ~ MoveInDate)
           ) %>%
  filter((
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

# adding in calculated columns to help get to Bed Nights

Utilizers <- Utilizers %>%
  mutate(
    BedNights = 
        difftime(ymd(ExitAdjust), ymd(EntryAdjust),
                 units = "days"),
    StayWindow = interval(ymd(EntryAdjust), ymd(ExitAdjust))
  ) %>%
  filter(BedNights > 0) 

# grouping and summarising
Utilizers <- Utilizers %>%
  group_by(ProjectID, ProjectType) 


# LEFT OFF HERE -----------------------------------------------------------

#- need to work out how to group this by whether the intervals overlap

# bare bones list of bed nights per provider (no date ranges)
Utilization <- Utilizers %>% select(ProjectID, BedNights) %>% 
  group_by(ProjectID) %>% 
  summarise(BedNights = sum(BedNights))

# Possible Bed Nights -----------------------------------------------------

BedNights <- Beds %>%
  select(ProjectID, BedInventory) %>%
  group_by(ProjectID) %>%
  summarise(BedInventory =
              sum(BedInventory) * 30)
