library(tidyverse)
library(shiny)
library(shinyWidgets)
library(lubridate)
library(shinydashboard)
library(scales)

updatedate <- file.info("data/COHHIOHMIS.RData")$mtime

load("data/Utilization.RData")

load("data/QPR_SPDATs.RData")

# CountyAverageScores <- CountyData %>%
#   filter(served_between(CountyData, ReportStart, ReportEnd)) %>%
#   select(CountyServed, PersonalID, Score) %>%
#   distinct() %>%
#   group_by(CountyServed) %>%
#   summarise(AverageScore = round(mean(Score), 1), 
#             HHsLHinCounty = n())
# 
# # ProviderAverages <- SPDATsByProject %>%
# #   filter(served_between(SPDATsByProject, ReportStart, ReportEnd)) %>%
# #   select(EnrollmentID, ProjectName, ScoreAdjusted) %>%
# #   group_by(ProjectName) %>%
# #   summarise(AverageScore = round(mean(ScoreAdjusted), 1),
# #             EnrollmentCount = n())
# 
# CountyHousedAverageScores <- SPDATsByProject %>%
#   filter(served_between(SPDATsByProject, ReportStart, ReportEnd)) %>%
#   group_by(CountyServed) %>%
#   summarise(HousedAverageScore = round(mean(ScoreAdjusted), 1),
#             HHsHousedInCounty = n())
# 
# Compare <- 
#   full_join(CountyAverageScores, 
#             CountyHousedAverageScores, 
#             by = "CountyServed") %>%
#   arrange(CountyServed) %>%
#   left_join(., Regions, by = c("CountyServed" = "County"))
