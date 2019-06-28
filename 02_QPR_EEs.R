# this script uses the COHHIOHMIS data to populate the QPR.

library(tidyverse)
library(lubridate)
library(plotly)
library(readxl)

load("images/COHHIOHMIS.RData")

rm(Affiliation, Disabilities, EmploymentEducation, EnrollmentCoC, Exit,
   Export, Funder, Geography, HealthAndDV, IncomeBenefits, Offers,
   Organization, ProjectCoC, Scores, Services, VeteranCE, Users)

goals <- read_xlsx("data/Goals.xlsx")

goals <- goals %>%
  gather(key = "ProjectType", 
         value = "Goal", 
         -SummaryMeasure, -Measure, -Operator) %>%
  mutate(ProjectType = as.numeric(ProjectType))


# Building QPR_EEs ----------------------------------------------------

smallProject <- Project %>%
  select(ProjectID,
         OrganizationName,
         ProjectAKA,
         ProjectName,
         ProjectType,
         GrantType,
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

# the problem with this is you're pulling in all the projects that participated
# during the fileperiod, which would include those who are no longer 
# participating, but have a year's worth of PIT data entered. Like 126 for 
# example. I am not sure how to remedy this except to literally move ALL of this
# to the app, which I think is too much.

smallProject <- smallProject %>% 
  semi_join(allHMISParticipating, by = "ProjectID") 

rm(allHMISParticipating)

smallProject <- smallProject %>%
  filter(!is.na(Region)) %>%
  mutate(
    FriendlyProjectName = if_else(is.na(ProjectAKA), ProjectName, ProjectAKA))

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
    EntryAdjust,
    MoveInDateAdjust,
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

# Successful Placement ----------------------------------------------------

SuccessfullyPlaced <- QPR_EEs %>%
  filter(
    exited_between(., FileStart, FileEnd) &
      ((ProjectType %in% c(3, 9, 13) & !is.na(MoveInDateAdjust)) |
         ProjectType %in% c(1, 2, 4, 8, 12)) & # excluding non-mover-inners
      ((DestinationGroup == "Permanent" | #exited to ph or still in PSH/HP
          is.na(ExitDate)) &
         ProjectType %in% c(3, 9, 12) # PSH & HP
      ) | 
      (DestinationGroup == "Permanent" & # exited to ph
         ProjectType %in% c(1, 2, 4, 8, 13)) # ES, TH, SH, RRH, OUT
  ) %>%
  group_by(FriendlyProjectName, ProjectType, County, Region) %>%
  summarise(SuccessfullyPlacedHHs = n())

# calculating the total households to compare successful placements to
TotalHHsSuccessfulPlacement <- QPR_EEs %>%
  filter((
    served_between(., FileStart, FileEnd) &
      ProjectType %in% c(3, 9, 12) # PSH & HP
  ) |
    (
      exited_between(., FileStart, FileEnd) &
        ProjectType %in% c(1, 2, 4, 8, 13) # ES, TH, SH, OUT, RRH
    )) %>%
  group_by(FriendlyProjectName, ProjectType, County, Region) %>%
  summarise(TotalHHs = n()) # For PSH & HP, it's total hhs served; 
# otherwise, it's total hhs *exited* during the reporting period

SuccessfulPlacement <- TotalHHsSuccessfulPlacement %>%
  left_join(SuccessfullyPlaced,
            by = c("FriendlyProjectName", "ProjectType", "County", "Region")) %>%
  mutate(Percent = SuccessfullyPlacedHHs / TotalHHs)

SuccessfulPlacement[is.na(SuccessfulPlacement)] <- 0

# Length of Stay ----------------------------------------------------------
library(tidyverse)
library(lubridate)
library(patchwork)

load("images/QPR_EEs.RData")

LoSGoals <- goals %>%
  select(-Measure) %>% 
  filter(SummaryMeasure == "Length of Stay" & !is.na(Goal)) %>%
  unique()

LoSDetail <- QPR_EEs %>%
  filter((((!is.na(MoveInDateAdjust) &
              ProjectType %in% c(13)) |
             (ProjectType %in% c(1, 2, 8)) &
             !is.na(ExitDate)
  )) &
    exited_between(., "01012019", "05012019")) %>%
  left_join(LoSGoals, by = "ProjectType") %>% 
  mutate(
    ProjectType = case_when(
      ProjectType == 1 ~ "Emergency Shelter",
      ProjectType == 2 ~ "Transitional Housing",
      ProjectType %in% c(3, 9) ~ "Permanent Supportive Housing",
      ProjectType == 4 ~ "Street Outreach",
      ProjectType == 8 ~ "Safe Haven",
      ProjectType == 12 ~ "Homelessness Prevention",
      ProjectType == 13 ~ "Rapid Rehousing"
    ),
    Region = paste("Homeless Planning Region", Region)
  ) %>%
  filter(Region == "Homeless Planning Region 5") # this filter needs
  # to be here so the selection text matches the mutated data
  

LoSSummary <- LoSDetail %>%
  group_by(brokenProjectNames,
           ProjectName,
           Region,
           County,
           ProjectType,
           Operator,
           Goal) %>%
  summarise(avg = as.numeric(mean(DaysinProject, na.rm = TRUE)),
            median = as.numeric(median(DaysinProject, na.rm = TRUE)))

th <- ggplot(LoSSummary, aes(x = brokenProjectNames)) +
  ylab("Average Length of Stay") +
  xlab("") +
  ggtitle("Transitional Housing", subtitle = "date range") +
  geom_col(aes(y = as.numeric(avg)), fill = "#56B4E9") +
  geom_hline(yintercept = as.integer(LoSGoals %>% 
               filter(ProjectType == 2) %>% 
               select(Goal))) +
  annotate(
    "text",
    x = 0.65,
    y = as.integer(LoSGoals %>% 
                     filter(ProjectType == 2) %>% 
                     select(Goal)) + 1,
    label = "CoC Goal") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
th
es <- ggplot(LoSSummary, aes(x = brokenProjectNames)) +
  ylab("Average Length of Stay") +
  xlab("") +
  ggtitle("Emergency Shelters", subtitle = "date range") +
  geom_col(aes(y = as.numeric(avg)), fill = "#56B4E9") +
  geom_hline(yintercept = as.integer(LoSGoals %>% 
                                       filter(ProjectType == 1) %>% 
                                       select(Goal))) +
  annotate(
    "text",
    x = 0.65,
    y = as.integer(LoSGoals %>% 
                     filter(ProjectType == 1) %>% 
                     select(Goal)) + 1,
    label = "CoC Goal") +
  theme_light() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_grid(es, th)


somecolors <- c("#7156e9", "#56B4E9", "#56e98c", "#e98756", "#e9d056", "#ba56e9",
                "#e95684")
somemorecolors <- c('#f0f9e8','#ccebc5','#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe',
                    '#08589e')

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



