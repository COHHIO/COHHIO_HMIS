library(tidyverse)
library(lubridate)
project <- read_csv("data/hudcsvoneday/Project.csv")
write_csv(project, "data/hudcsvoneday/Project.csv", 
          na = "",  
          quote_escape = "backslash")
organization <- read_csv("data/hudcsvoneday/Organization.csv")
write_csv(organization, "data/hudcsvoneday/Organization.csv", 
          na = "",  
          quote_escape = "backslash")
inventory <- read_csv("data/hudcsvoneday/Inventory.csv")
write_csv(inventory, "data/hudcsvoneday/Inventory.csv", 
          na = "",  
          quote_escape = "backslash")
geography <- read_csv("data/hudcsvoneday/Geography.csv")
write_csv(geography, "data/hudcsvoneday/Geography.csv", 
          na = "",  
          quote_escape = "backslash")
funder <- read_csv("data/hudcsvoneday/Funder.csv")
write_csv(funder, "data/hudcsvoneday/Funder.csv", 
          na = "",  
          quote_escape = "backslash")
