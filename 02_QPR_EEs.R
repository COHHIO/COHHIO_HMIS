# this script uses the COHHIOHMIS data to populate the QPR.

library(tidyverse)
library(lubridate)
library(janitor)
library(plotly)

load("images/COHHIOHMIS.RData")

rm(Affiliation, Disabilities, EmploymentEducation, EnrollmentCoC, Exit,
   Export, Funder, Geography, HealthAndDV, IncomeBenefits, Offers,
   Organization, ProjectCoC, Scores, Services, VeteranCE, Users)
# Successful Placement ----------------------------------------------------

smallProject <- Project %>%
  select(ProjectID,
         OrganizationName,
         ProjectName,
         ProjectType,
         County,
         Region) 


hmisbeds <- Inventory %>%
  filter(HMIS_participating_between(Inventory, FileStart, FileEnd)) %>%
  select(ProjectID) %>% unique()

hpOutreach <- Project %>% 
  filter(ProjectType %in% c(4, 12) &
           operating_between(., FileStart, FileEnd)) %>% 
  select(ProjectID)
rm(Project, Inventory)
allHMISParticipating <- rbind(hmisbeds, hpOutreach)
rm(hmisbeds, hpOutreach)
smallProject <- smallProject %>% 
  semi_join(allHMISParticipating, by = "ProjectID") 
rm(allHMISParticipating)
smallProject <- as.data.frame(smallProject)

smallEnrollment <- Enrollment %>%
  select(
    EnrollmentID,
    PersonalID,
    HouseholdID,
    ProjectID,
    RelationshipToHoH,
    CountyServed,
    EntryDate,
    MoveInDate,
    ExitDate,
    ExitAdjust,
    Destination
  ) %>%
  filter(str_detect(HouseholdID, fixed("s_")) |
           (str_detect(HouseholdID, fixed("h_")) &
              RelationshipToHoH == 1))

smallEnrollment <- as.data.frame(smallEnrollment)

# captures all leavers PLUS all ee's in either HP or PSH
# also limits records to singles and HoHs only
QPR_EEs <- smallProject %>%
  left_join(smallEnrollment, by = "ProjectID") %>%
  filter((!is.na(ExitDate) | ProjectType %in% c(3, 9, 12)) &
           served_between(., FileStart, FileEnd)) %>%
  mutate(
    DestinationGroup = case_when(
      Destination %in% c(1, 2, 12, 13, 14, 16, 18, 27) ~ "Temporary",
      Destination %in% c(3, 10, 11, 19:23, 28, 31) ~ "Permanent",
      Destination %in% c(4:7, 15, 25:27, 29) ~ "Institutional",
      Destination %in% c(8, 9, 17, 24, 30, 99) ~ "Other"
    ),
    MoveInDateAdjust = if_else(
      ymd(EntryDate) <= ymd(MoveInDate) &
        ymd(MoveInDate) <= ExitAdjust &
        ProjectType %in% c(3, 9, 13),
      MoveInDate,
      NULL
    ),
    EntryAdjust = case_when(
      ProjectType %in% c(1, 2, 4, 8, 12) ~ EntryDate,
      ProjectType %in% c(3, 9, 13) & !is.na(MoveInDateAdjust) ~ MoveInDateAdjust,
      ProjectType %in% c(3, 9, 13) & is.na(MoveInDateAdjust) ~ EntryDate),
    DaysinProject = difftime(ExitAdjust, EntryAdjust, units = "days")
  )
rm(Client, Enrollment, smallEnrollment, smallProject, Regions)

save.image("images/QPR_EEs.RData")

PermAndRetention <- QPR_EEs %>%
  filter((DestinationGroup == "Permanent" | is.na(ExitDate)) &
           ProjectType %in% c(3, 9, 12))

PermLeavers <- QPR_EEs %>%
  filter(DestinationGroup == "Permanent" &
           ProjectType %in% c(1, 2, 4, 8, 13))

# this is useless without dates- should be moved into the app
# for all project types except PSH and HP
TotalHHLeavers <- QPR_EEs %>%
  filter(!is.na(ExitDate)) %>%
  group_by(ProjectName) %>%
  summarise(Leavers = n())
# for PSH and HP only
TotalHHLeaversAndStayers <- QPR_EEs %>%
  group_by(ProjectName) %>%
  summarise(LeaversAndStayers = n())

#also useless without dates, should be moved into app
PermAndRetentionByProject <- PermAndRetention %>%
  group_by(ProjectName) %>%
  summarise(PermanentDestOrStayer = n())


# Length of Stay ----------------------------------------------------------

LoSDetail <- QPR_EEs %>% 
  filter(ProjectType %in% c(1, 2, 4, 8, 12) &
           !is.na(ExitDate)) 

LoSSummary <- LoSDetail %>%
  mutate(
    AbbProjectType = case_when(
      ProjectType == 1 ~ "ES",
      ProjectType == 2 ~ "TH",
      ProjectType %in% c(3, 9) ~ "PSH",
      ProjectType == 4 ~ "OUTREACH",
      ProjectType == 8 ~ "Safe Haven",
      ProjectType == 12 ~ "Prevention",
      ProjectType == 13 ~ "RRH"
    ),
    Provider = paste(OrganizationName, AbbProjectType),
    ProjectType = case_when(
      ProjectType == 1 ~ "Emergency Shelter",
      ProjectType == 2 ~ "Transitional Housing",
      ProjectType %in% c(3, 9) ~ "Permanent Supportive Housing",
      ProjectType == 4 ~ "Street Outreach",
      ProjectType == 8 ~ "Safe Haven",
      ProjectType == 12 ~ "Prevention",
      ProjectType == 13 ~ "Rapid Rehousing"
    )
  ) %>%
  group_by(ProjectName, Provider, Region, County, ProjectType) %>%
  summarise(avg = as.numeric(mean(DaysinProject, na.rm = TRUE)),
            median = as.numeric(median(DaysinProject, na.rm = TRUE))) %>%
  arrange(ProjectType)

ggplot(LoSSummary %>% filter(Region == 1), 
       aes(x = OrganizationName)) +
  geom_col(aes(y = avg, fill = ProjectType)) +
  coord_flip() +
  xlab("Project Name") + 
  ylab("Average")

plot_ly(LoSSummary %>% filter(Region == 1), 
        x = ~ProjectName, 
        y = ~avg, type = "bar",
        name = "Average") %>%
  layout(yaxis = list(title = "Days"), 
         xaxis = list(title = "Provider"))

# Rapid Placement RRH -----------------------------------------------------

RapidPlacement <- QPR_EEs %>%
  filter(ProjectType == 13) %>%
  mutate(DaysToHouse = difftime(MoveInDateAdjust, EntryDate, units = "days")) %>%
  select(
    EnrollmentID,
    PersonalID,
    ProjectID,
    EntryDate,
    MoveInDate,
    MoveInDateAdjust,
    ExitDate,
    DaysToHouse
  ) 

DataQualityRapidPlacement <- QPR_EEs %>%
  filter(ProjectType == 13) %>%
  mutate(DaysToHouse = difftime(MoveInDate, EntryDate, units = "days")) %>%
  filter(DaysToHouse < 0 | DaysToHouse > 120) %>%
  select(
    EnrollmentID,
    PersonalID,
    ProjectID,
    EntryDate,
    MoveInDate,
    MoveInDateAdjust,
    DaysToHouse,
    ExitDate
  ) 



