# this script uses the COHHIOHMIS data to populate the QPR.

library(tidyverse)
library(lubridate)
library(plotly)
library(readxl)
library(patchwork)

load("images/COHHIOHMIS.RData")

rm(Affiliation, Disabilities, EmploymentEducation, EnrollmentCoC, Exit,
   Export, Funder, Geography, HealthAndDV, IncomeBenefits, Offers,
   Organization, ProjectCoC, Scores, Services, VeteranCE, Users)

goals <- read_xlsx("data/Goals.xlsx")

goals <- goals %>%
  gather(key = "ProjectType", 
         value = "Goal", 
         -SummaryMeasure, -Measure, -Operator)


# Successful Placement ----------------------------------------------------

smallProject <- Project %>%
  select(ProjectID,
         OrganizationName,
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

smallProject <- smallProject %>% 
  semi_join(allHMISParticipating, by = "ProjectID") 

rm(allHMISParticipating)

smallProject <- smallProject %>%
  filter(!is.na(Region)) %>%
  mutate(
    brokenProjectNames = trimmer(ProjectName, 30))

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

LoSGoals <- goals %>%
  select(-Measure) %>% 
  filter(SummaryMeasure == "Length of Stay") %>%
  unique()

LoSDetail <- QPR_EEs %>%
  filter(ProjectType %in% c(1, 2, 8) &
           !is.na(ExitDate)) %>%
  mutate(
    ProjectType = case_when(
      ProjectType == 1 ~ "ES",
      ProjectType == 2 ~ "TH",
      ProjectType %in% c(3, 9) ~ "PSH",
      ProjectType == 4 ~ "OUT",
      ProjectType == 8 ~ "SH",
      ProjectType == 12 ~ "HP",
      ProjectType == 13 ~ "RRH"
    )
  ) %>%
  left_join(LoSGoals, by = "ProjectType")

LoSSummary <- LoSDetail %>%
  group_by(brokenProjectNames, ProjectName, Region, County, ProjectType, Operator, Goal) %>%
  summarise(avg = as.numeric(mean(DaysinProject, na.rm = TRUE)),
            median = as.numeric(median(DaysinProject, na.rm = TRUE))) %>%
  arrange(ProjectType)

es <-
  ggplotly(
    ggplot(
      LoSSummary %>% filter(Region == 5, ProjectType == "ES"),
      aes(x = brokenProjectNames)
    ) +
      geom_col(aes(y = as.numeric(avg))) +
      geom_hline(yintercept = LoSSummary$Goal[LoSSummary$ProjectType == "ES"]) +
      theme(axis.text.x = element_text(angle = 45))
  )
es
th <- ggplot(LoSSummary %>% filter(Region == 5, ProjectType == "TH"),
             aes(x = brokenProjectNames)) +
  ylab("Average Length of Stay") +
  xlab("") +
  ggtitle("Transitional Housing", subtitle = "date range") +
  geom_col(aes(y = as.numeric(avg)), fill = "#56B4E9") +
  geom_hline(yintercept = LoSSummary$Goal[LoSSummary$ProjectType == "TH"]) +
  annotate("text", x = 0.6, y = LoSSummary$Goal[LoSSummary$ProjectType == "TH"] + 1,
           label = "CoC Goal") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
th

somecolors <- c("#7156e9", "#56B4E9", "#56e98c", "#e98756", "#e9d056", "#ba56e9",
                "#e95684")
somemorecolors <- c('#f0f9e8','#ccebc5','#a8ddb5','#7bccc4','#4eb3d3','#2b8cbe','#08589e')

plot_ly(LoSSummary %>% filter(Region == 6), type = "bar") %>%
  add_trace(
    x = ~ ProjectName,
    y = ~ avg,
    type = "bar",
    color = ~ ProjectType
  ) %>%
  add_trace(x = ~ ProjectName,
            y = ~ Goal,
            type = "bar",
            opacity =.5,
            marker = list(color = "gray"),
            name = "Goal") %>%
  layout(
    yaxis = list(title = "Days"),
    xaxis = list(
      title = "Provider",
      categoryorder = "array",
      categoryarray = ~ ProjectType,
      barmode = "group"
    )
  )

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



