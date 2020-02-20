
# Case 807291

library(tidyverse)
library(lubridate)

DateOpened <- mdy("03152019")

difftime(today(), DateOpened, units = "days")

# Importance between 1 and 10

Importance <- 3


# The Problem -------------------------------------------------------------

# The files in the CSV Export are all double quoted, but HDX doesn't accept
# files like this, so I have to unquote them. CW marked it as a defect and said
# they'd fix it.

# Test that it's corrected ------------------------------------------------

# Open a file in the export in NotePad++ to see if there are quotes around 
# everything. If there are not, then it's fixed!


