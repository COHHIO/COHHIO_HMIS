# COHHIO_HMIS
# Copyright (C) 2019  Coalition on Homelessness and Housing in Ohio (COHHIO)
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details at 
#<https://www.gnu.org/licenses/>.

library(tidyverse)
library(lubridate)
library(janitor)
library(HMIS)

load("images/COHHIOHMIS.RData")
rm(Affiliation, Client, EnrollmentCoC, EmploymentEducation, Export, Exit, 
   Funder, HealthAndDV, Disabilities, IncomeBenefits, Geography, Inventory, 
   Offers, Organization, ProjectCoC, Services, VeteranCE)

smallEnrollment <- Enrollment %>%
  left_join(Project[c("ProjectID", 
                      "OperatingStartDate",
                      "OperatingEndDate")], by = "ProjectID") %>%
  select(
    EnrollmentID,
    PersonalID,
    ProjectID,
    ProjectType,
    ProjectName,
    OperatingStartDate,
    OperatingEndDate,
    EntryDate,
    ExitDate,
    RelationshipToHoH,
    CountyServed
  )

Entries <- smallEnrollment %>%
  filter(ProjectType %in% c(3, 9, 13))

rm(Enrollment, Project)

hhsServedInCounty <- "The horizontal lines represent the average scores of Heads 
of Household who were served in the County in a ES, TH, SH, or Outreach project 
during the reporting period and who were scored. If a Head of Household was 
served in a County outside the Balance of State or if that data was missing, 
they are not being counted. When there are multiple project entries for the same 
client, this only counts the most recent entry. When there are multiple scores, 
this only counts the most recent score. There should not be more than 1 score on 
the same day, but if there are it is counting the highest score."

CountyData <-
  left_join(smallEnrollment, Scores, by = "PersonalID") %>%
  filter(
    ProjectType %in% c(1, 2, 4, 8) &
      RelationshipToHoH == 1 & 
      ymd(ScoreDate) <= ymd(EntryDate) & 
      !CountyServed %in% c("Montgomery", 
                           "Cuyahoga", 
                           "Mahoning", 
                           "Lucas", 
                           "Stark", 
                           "Summit", 
                           "Hamilton", 
                           "Franklin",
                           "--Outside of Ohio--") &
      !is.na(CountyServed)) %>%
  select(
    EnrollmentID,
    PersonalID,
    ProjectID,
    EntryDate,
    ExitDate,
    CountyServed,
    ScoreDate,
    Score
  ) %>%
  group_by(PersonalID) %>%
  mutate(MaxEntry = max(ymd(EntryDate))) %>% # most recent EE
  filter(ymd(MaxEntry) == ymd(EntryDate)) %>%  
  mutate(MaxScoreDate = max(ymd(ScoreDate))) %>% # most recent score
  filter(ymd(ScoreDate) == ymd(MaxScoreDate)) %>%
  mutate(MaxScore = max(Score)) %>% # highest score
  filter(Score == MaxScore) %>%
  ungroup() %>%
  select(PersonalID, CountyServed, Score, EntryDate, ExitDate) 

ReportStart <- "01012019"
ReportEnd <- "03312019"
# you might have to leave things here so the data can be filtered by date 
# in the app, moving the following smushings into the app.

ClientScoresInCounty <- CountyData %>%
  filter(served_between(CountyData, ReportStart, ReportEnd)) %>%
  select(CountyServed, PersonalID, Score) %>%
  distinct()

CountyAverageScores <- ClientScoresInCounty %>%
  group_by(CountyServed) %>%
  summarise(AverageScore = round(mean(Score), 1), 
            HHsLHinCounty = n())

hhsHousedInCounty <- "The triangle represents the average score of each 
household entering into a permanent housing project in a County during the 
reporting period. This will necessarily leave out households coming from 
Domestic Violence shelters since they are not scored. Any Heads of Household 
who entered a permanent housing project without a score will be counted as 
having a score of 0."

noteToUsers <- "It is very important that your Duplicate Entry Exits and your 
Household Data Quality tabs are totally clear for this report to be accurate. 
It is also important that your VI-SPDAT scores are ON THE HEAD OF HOUSEHOLD'S 
RECORD. Any scores recorded on non-HoHs will not be counted here.  Also if a 
HoH is missing their County data or they were served in a County outside the 
Ohio Balance of State, they will also not show here."

SPDATsByProject <- left_join(Entries, Scores, by = "PersonalID") %>%
  select(-ProjectType,
         -OperatingStartDate,
         -OperatingEndDate) %>%
  filter(
    RelationshipToHoH == 1 &
      (ymd(ScoreDate) <= ymd(EntryDate) | is.na(ScoreDate)) &
      !CountyServed %in% c(
        "Montgomery",
        "Cuyahoga",
        "Mahoning",
        "Lucas",
        "Stark",
        "Summit",
        "Hamilton",
        "Franklin",
        "--Outside of Ohio--"
      ) &
      !is.na(CountyServed)
  ) %>%
  group_by(EnrollmentID) %>%
  mutate(MaxScoreDate = max(ymd(ScoreDate))) %>%
  filter(ymd(ScoreDate) == ymd(MaxScoreDate) | is.na(ScoreDate)) %>%
  mutate(MaxScore = max(ScoreDate)) %>%
  filter(Score == MaxScore | is.na(ScoreDate)) %>%
  distinct() %>%
  ungroup() %>%
  select(-MaxScoreDate, -MaxScore) %>%
  mutate(ScoreAdjusted = if_else(is.na(Score), 0, Score)) %>%
  filter(!is.na(ScoreAdjusted))

# Also send these smushings over to the app as they're using Report Date Range

ProviderAverages <- SPDATsByProject %>%
  filter(served_between(SPDATsByProject, ReportStart, ReportEnd)) %>%
  select(EnrollmentID, ProjectName, ScoreAdjusted) %>%
  group_by(ProjectName) %>%
  summarise(AverageScore = round(mean(ScoreAdjusted), 1),
            EnrollmentCount = n())

CountyHousedAverageScores <- SPDATsByProject %>%
  filter(served_between(SPDATsByProject, ReportStart, ReportEnd)) %>%
  group_by(CountyServed) %>%
  summarise(HousedAverageScore = round(mean(ScoreAdjusted), 1),
            HHsHousedInCounty = n())

# If you have clients here, you should either verify the scores saved here are 
# valid or the correct client is marked as the Head of Household.

SPDATsOnNonHoHs <- left_join(Entries, Scores, by = "PersonalID") %>%
  filter(RelationshipToHoH != 1 & 
           !is.na(Score) & 
           served_between(., ReportStart, ReportEnd)) %>%
  select(ProjectName, PersonalID, EntryDate, ExitDate, Score) %>%
  arrange(ProjectName)

rm(Entries, Scores, smallEnrollment)

# Each county is represented based on whether they are serving higher than the 
# average SPDAT score in their county.



# The following compares the Average VI-SPDAT Score of those who have entered a 
# project that would mean they are literally homeless within a County to the 
# Average VI-SPDAT Score of those who have an Entry into a Permanent Housing 
# project type (RRH or PSH).

Regions <- read_csv("public_data/Regions.csv") %>%
  mutate(RegionName = paste0("Homeless Planning Region ", Region))

Compare <- 
  full_join(CountyAverageScores, 
            CountyHousedAverageScores, 
            by = "CountyServed") %>%
  arrange(CountyServed) %>%
  left_join(., Regions, by = c("CountyServed" = "County"))

mapdata <- Compare %>%
  mutate(Difference = HousedAverageScore - AverageScore,
         COUNTY_CD = toupper(
           case_when(
             !CountyServed %in% c(
               "Morrow",
               "Morgan",
               "Ashland",
               "Ashtabula",
               "Champaign",
               "Meigs",
               "Monroe",
               "Harrison"
             ) ~ substr(CountyServed, 1, 3),
             CountyServed == "Morrow" ~ "MRW",
             CountyServed == "Morgan" ~ "MRG",
             CountyServed == "Ashland" ~ "ASD",
             CountyServed == "Ashtabula" ~ "ATB",
             CountyServed == "Champaign" ~ "CHP",
             CountyServed == "Meigs" ~ "MEG",
             CountyServed == "Monroe" ~ "MOE",
             CountyServed == "Harrison" ~ "HAS"
           )
         )) %>%
  select(COUNTY_CD, Difference)

library(tmap)
library(tmaptools)
oh507 <- read_shape("Ohio/OH_507/OH_507.shp")
ohio <- read_shape("Ohio/ODOT_County_Boundaries.shp")

tm_shape(ohio) +
  tm_polygons("white") +
  tm_shape(oh507) +
  tm_polygons(lwd = 2, alpha = 0.2)

library(leaflet)
oh507leaf <- readOGR(dsn = "Ohio/OH_507", layer = "OH_507")
oh507map <- leaflet(oh507leaf) %>% addTiles() %>% setView(-83, 40.2, 7) %>% addPolygons()

ohioleaf <- readOGR(dsn = "Ohio", layer = "ODOT_County_Boundaries")
ohiomap <- leaflet(ohioleaf) %>% addTiles() %>% setView(-83, 40.2, 7) %>% addPolygons()

# spdatPlot <- ggplot(Compare, 
#        aes(x = CountyServed, y = AverageScore)) + 
#   geom_point(aes(x = CountyServed, y = AverageScore), size = 10, shape = 95) +
#   theme(axis.text.x = element_text(size = 10)) +
#   geom_point(aes(x = CountyServed, y = HousedAverageScore), 
#              size = 4, 
#              shape = 17) +
#   xlab(Compare$RegionName)

rm(CountyAverageScores, CountyHousedAverageScores, EighthMonth, EleventhMonth, FifthMonth, FirstMonth, FourthMonth, NinthMonth,
   ReportEnd, ReportingPeriod, ReportStart, SecondMonth, SeventhMonth, SixthMonth,
   TenthMonth, ThirdMonth, TwelfthMonth)

save.image("data/QPR_SPDATs.RData")
