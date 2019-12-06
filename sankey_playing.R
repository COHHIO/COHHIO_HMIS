# Libraries
library(tidyverse)
library(viridis)
library(patchwork)
library(hrbrthemes)
library(circlize)

# A connection data frame is a list of flows with intensity for each flow
links <- data.frame(
  source = c("PSH", "PSH", "PSH", "PSH",
             "RRH", "RRH", "RRH", 
             "ES/SH/TH", "ES/SH/TH", "ES/SH/TH"), 
  target = c("Permanent Exit",
           "Remains in PSH", 
           "Temporary Destination", 
           "Other Destination",
           "Permanent Exit",
           "Temporary Destination", 
           "Other Destination",
           "Permanent Exit",
           "Temporary Destination", 
           "Other Destination"), 
  value = c(2, # PSH to Perm
            exits_ph$CurrentYear[2], # Remained in PSH
            2, # PSH to Temp
            3, # PSH to Other
            1, # RRH to Perm
            5, # RRH to Temp
            7, # RRH to Other
            5, # ESSHTH to Perm
            2, # ESSHTH to Temp
            3) # ESSHTH to Other
)

# From these flows we need to create a node data frame: it lists every entities 
# involved in the flow
nodes <- data.frame(name = c(as.character(links$source),
                             as.character(links$target)) %>%
                      unique())

# With networkD3, connection must be provided using id, not using real name like in the links dataframe.. So we need to reformat it.
links$IDsource <- match(links$source, nodes$name) - 1
links$IDtarget <- match(links$target, nodes$name) - 1

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
  Links = links,
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
