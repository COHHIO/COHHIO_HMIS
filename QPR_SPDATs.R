library(tidyverse)
library(lubridate)
library(janitor)

load("data/COHHIOHMIS.RData")
rm(Affiliation, Client, EnrollmentCoC, EmploymentEducation, Export, Exit, 
   Funder, HealthAndDV, Disabilities, IncomeBenefits, Geography, Inventory, 
   Offers, Organization, ProjectCoC, Services, VeteranCE)

smallEnrollment <- Enrollment %>%
  left_join(Project, by = "ProjectID") %>%
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
      ymd(StartDate) <= ymd(EntryDate) & 
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
  group_by(PersonalID) %>%
  mutate(MaxEntry = max(ymd(EntryDate))) %>% # most recent EE
  filter(ymd(MaxEntry) == ymd(EntryDate)) %>%  
  mutate(MaxScoreDate = max(ymd(StartDate))) %>% # most recent score
  filter(ymd(StartDate) == ymd(MaxScoreDate)) %>%
  mutate(MaxScore = max(Score)) %>% # highest score
  filter(Score == MaxScore) %>%
  ungroup() %>%
  select(PersonalID, CountyServed, Score, EntryDate, ExitDate) %>%
  mutate(ExitAdjust = if_else(is.na(ExitDate), today(), ExitDate))

# you might have to leave things here so the data can be filtered by date 
# in the app, moving the following smushings into the app.

SPDATsByCounty <- CountyData %>%
  select(CountyServed, PersonalID, Score) %>%
  distinct()

CountyAverageScores <- SPDATsByCounty %>%
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
         -OperatingEndDate,
         -SPDATRecordID,
         -SPDATProvider) %>%
  filter(
    RelationshipToHoH == 1 &
      (ymd(StartDate) <= ymd(EntryDate) | is.na(StartDate)) &
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
  mutate(MaxScoreDate = max(ymd(StartDate))) %>%
  filter(ymd(StartDate) == ymd(MaxScoreDate) | is.na(StartDate)) %>%
  mutate(MaxScore = max(Score)) %>%
  filter(Score == MaxScore | is.na(StartDate)) %>%
  distinct() %>%
  ungroup() %>%
  select(-MaxScoreDate, -MaxScore) %>%
  mutate(ScoreAdjusted = if_else(is.na(Score), 0, Score)) %>%
  filter(!is.na(ScoreAdjusted))

ProviderAverages <- SPDATsByProject %>%
  select(EnrollmentID, ProjectName, ScoreAdjusted) %>%
  group_by(ProjectName) %>%
  summarise(AverageScore = round(mean(ScoreAdjusted), 1),
            EnrollmentCount = n())

CountyHousedAverageScores <- SPDATsByProject %>%
  group_by(CountyServed) %>%
  summarise(HousedAverageScore = round(mean(ScoreAdjusted), 1),
            HHsHousedInCounty = n())

# If you have clients here, you should either verify the scores saved here are 
# valid or the correct client is marked as the Head of Household.

SPDATsOnNonHoHs <- left_join(Entries, Scores, by = "PersonalID") %>%
  filter(RelationshipToHoH != 1 & !is.na(Score)) %>%
  select(ProjectName, PersonalID, EntryDate, Score) %>%
  arrange(ProjectName)

rm(Entries, Scores, smallEnrollment)

# Each county is represented based on whether they are serving higher than the 
# average SPDAT score in their county.



# The following compares the Average VI-SPDAT Score of those who have entered a 
# project that would mean they are literally homeless within a County to the 
# Average VI-SPDAT Score of those who have an Entry into a Permanent Housing 
# project type (RRH or PSH).

Regions <- read_csv("data/Regions.csv") %>%
  mutate(RegionName = paste0("Homeless Planning Region ", Region))

Compare <- 
  full_join(CountyAverageScores, 
            CountyHousedAverageScores, 
            by = "CountyServed") %>%
  arrange(CountyServed) %>%
  left_join(., Regions, by = c("CountyServed" = "County"))

rm(CountyAverageScores, CountyHousedAverageScores)

# spdatPlot <- ggplot(Compare, 
#        aes(x = CountyServed, y = AverageScore)) + 
#   geom_point(aes(x = CountyServed, y = AverageScore), size = 10, shape = 95) +
#   theme(axis.text.x = element_text(size = 10)) +
#   geom_point(aes(x = CountyServed, y = HousedAverageScore), 
#              size = 4, 
#              shape = 17) +
#   xlab(Compare$RegionName)

rm(EighthMonth, EleventhMonth, FifthMonth, FirstMonth, FourthMonth, NinthMonth,
   ReportEnd, ReportingPeriod, ReportStart, SecondMonth, SeventhMonth, SixthMonth,
   TenthMonth, ThirdMonth, TwelfthMonth)

save.image("data/QPR_SPDATs.RData")
