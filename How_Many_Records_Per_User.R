library("tidyverse")
library("readxl")
x <- read.delim("data/users.txt")

factor(x, levels = "User.Creating")
