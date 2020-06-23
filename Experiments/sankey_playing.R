# Libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)
library(networkD3)

source("07_SPMs.R")
source("02_QPR_EEs.R")

destinations <- validation %>%
  filter(stayed_between(., format.Date(spm_current_start_date, "%m%d%Y"), 
                        format.Date(spm_current_end_date, "%m%d%Y")) &
           !is.na(ExitDate) &
           ProjectType %in% c(1:4, 8:9, 12:13)) %>%
  mutate(DestinationGroup = case_when(
    Destination %in% c(1, 18) ~ "Emergency Shelter/ Safe Haven",
    Destination == 16 ~ "Unsheltered",
    Destination %in% c(2, 27) ~ "TH",
    Destination == 3 ~ "PSH",
    Destination == 31 ~ "RRH",
    Destination %in% c(12:14, 32, 27) ~ "Other Temporary",
    Destination %in% c(10:11, 19:21, 28, 31, 33:34) ~ "Household's Own Housing",
    Destination %in% c(22:23) ~ "Shared Housing",
    Destination %in% c(4:7, 15, 25:26, 29) ~ "Institutional",
    Destination %in% c(8, 9, 17, 24, 30, 99) ~ "Other"
  ),
  ProjectType = case_when(
    ProjectType == 1 ~ "Emergency Shelter",
    ProjectType == 2 ~ "Transitional Housing",
    ProjectType %in% c(3, 9) ~ "Permanent Supportive Housing",
    ProjectType == 4 ~ "Street Outreach",
    ProjectType == 8 ~ "Safe Haven",
    ProjectType == 13 ~ "Rapid Rehousing",
    ProjectType == 12 ~ "Homelessness Prevention"
  )) %>%
  group_by(ProjectType, DestinationGroup) %>%
  summarise(Clients = n()) %>%
  select("source" = ProjectType,
         "target" = DestinationGroup,
         "value" = Clients)

# From these flows we need to create a node data frame: it lists every entities 
# involved in the flow
nodes <- data.frame(name = c(as.character(destinations$source),
                             as.character(destinations$target)) %>%
                      unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
destinations$IDsource <- match(destinations$source, nodes$name) - 1
destinations$IDtarget <- match(destinations$target, nodes$name) - 1

# prepare colour scale
ColourScal = 'd3.scaleOrdinal() .range(["#FDE725FF",
"#B4DE2CFF",
"#6DCD59FF",
"#35B779FF",
"#1F9E89FF",
"#26828EFF",
"#31688EFF",
"#3E4A89FF",
"#482878FF",
"#440154FF"])'

# Make the Network
sankeyNetwork(
  Links = destinations,
  Nodes = nodes,
  Source = "IDsource",
  Target = "IDtarget",
  Value = "value",
  NodeID = "name",
  sinksRight = FALSE,
  colourScale = ColourScal,
  nodeWidth = 40,
  fontSize = 13,
  fontFamily = "Arial",
  nodePadding = 20
)
