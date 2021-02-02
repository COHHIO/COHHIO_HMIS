# COHHIO_HMIS
# Copyright (C) 2020  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.

library(tidyverse)
library(lubridate)
library(readxl)
library(HMIS)
library(writexl)

coc <- "OH-507: Balance of State"

how_many_worksheets_in_the_701 <- 5
how_many_worksheets_in_the_703 <- 6
what_sheet_is_701_data_on <- 2


# Renaming all the files to reasonable things -----------------------------

if(file.exists(paste0("SPM_data/Current/",
                      list.files("./SPM_data/Current", pattern = "(0700 -)")))) {
  file.rename(paste0("SPM_data/Current/",
                     list.files("./SPM_data/Current", pattern = "(0700 -)")),
              "SPM_data/Current/0700a.xls")
}

if(file.exists(paste0("SPM_data/Current/",
                      list.files("./SPM_data/Current", pattern = "(0700.1b)")))) {
  file.rename(paste0("SPM_data/Current/",
                     list.files("./SPM_data/Current", pattern = "(0700.1b)")),
              "SPM_data/Current/0700b.xls")
}

if(file.exists(paste0("SPM_data/Current/",
                      list.files("./SPM_data/Current", pattern = "(0701 -)")))) {
  file.rename(paste0("SPM_data/Current/",
                     list.files("./SPM_data/Current", pattern = "(0701 -)")),
              "SPM_data/Current/0701.xls")
}

if(file.exists(paste0("SPM_data/Current/",
                      list.files("./SPM_data/Current", pattern = "(0702 -)")))) {
  file.rename(paste0("SPM_data/Current/",
                     list.files("./SPM_data/Current", pattern = "(0702 -)")),
              "SPM_data/Current/0702.xls")
}

if(file.exists(paste0("SPM_data/Current/",
                      list.files("./SPM_data/Current", pattern = "(0703 -)")))) {
  file.rename(paste0("SPM_data/Current/",
                     list.files("./SPM_data/Current", pattern = "(0703 -)")),
              "SPM_data/Current/0703.xls")
}

if(file.exists(paste0("SPM_data/Current/",
                      list.files("./SPM_data/Current", pattern = "(0704 -)")))) {
  file.rename(paste0("SPM_data/Current/",
                     list.files("./SPM_data/Current", pattern = "(0704 -)")),
              "SPM_data/Current/0704.xls")
}

if(file.exists(paste0("SPM_data/Current/",
                      list.files("./SPM_data/Current", pattern = "(0706 -)")))) {
  file.rename(paste0("SPM_data/Current/",
                     list.files("./SPM_data/Current", pattern = "(0706 -)")),
              "SPM_data/Current/0706.xls")
}

# RENAMING PRIOR FILES

if(file.exists(paste0("SPM_data/Prior/",
                      list.files("./SPM_data/Prior", pattern = "(0700 -)")))) {
  file.rename(paste0("SPM_data/Prior/",
                     list.files("./SPM_data/Prior", pattern = "(0700 -)")),
              "SPM_data/Prior/0700a.xls")
}

if(file.exists(paste0("SPM_data/Prior/",
                      list.files("./SPM_data/Prior", pattern = "(0700.1b)")))) {
  file.rename(paste0("SPM_data/Prior/",
                     list.files("./SPM_data/Prior", pattern = "(0700.1b)")),
              "SPM_data/Prior/0700b.xls")
}

if(file.exists(paste0("SPM_data/Prior/",
                      list.files("./SPM_data/Prior", pattern = "(0701 -)")))) {
  file.rename(paste0("SPM_data/Prior/",
                     list.files("./SPM_data/Prior", pattern = "(0701 -)")),
              "SPM_data/Prior/0701.xls")
}

if(file.exists(paste0("SPM_data/Prior/",
                      list.files("./SPM_data/Prior", pattern = "(0702 -)")))) {
  file.rename(paste0("SPM_data/Prior/",
                     list.files("./SPM_data/Prior", pattern = "(0702 -)")),
              "SPM_data/Prior/0702.xls")
}

if(file.exists(paste0("SPM_data/Prior/",
                      list.files("./SPM_data/Prior", pattern = "(0703 -)")))) {
  file.rename(paste0("SPM_data/Prior/",
                     list.files("./SPM_data/Prior", pattern = "(0703 -)")),
              "SPM_data/Prior/0703.xls")
}

if(file.exists(paste0("SPM_data/Prior/",
                      list.files("./SPM_data/Prior", pattern = "(0704 -)")))) {
  file.rename(paste0("SPM_data/Prior/",
                     list.files("./SPM_data/Prior", pattern = "(0704 -)")),
              "SPM_data/Prior/0704.xls")
}

if(file.exists(paste0("SPM_data/Prior/",
                      list.files("./SPM_data/Prior", pattern = "(0706 -)")))) {
  file.rename(paste0("SPM_data/Prior/",
                     list.files("./SPM_data/Prior", pattern = "(0706 -)")),
              "SPM_data/Prior/0706.xls")
}

# OPEN ALL YOUR EXCEL FILES AND PRESS ENABLE EDITING BEFORE PROCEEDING
# Checking that the 0700a was run correctly -CURRENT-----------------------

check_loth_a <- read_xls("SPM_data/Current/0700a.xls",
         sheet = 3,
         range = cell_cols("B2:C9")) %>%
  select("Prompt" = 2, "Selection" = 3) %>%
  head(n = 7L) %>%
  spread(Prompt, Selection) %>%
  select("EDA" = 1,
         "EffectiveDate" = 2,
         "ReportEnd" = 3,
         "PriorYear" = 4,
         "ReportStart" = 5,
         "CoC" = 6,
         "Providers" = 7) %>%
  mutate(ReportStart = as.integer(ReportStart),
         ReportEnd = as.integer(ReportEnd),
         PriorYear = as.integer(PriorYear),
         EffectiveDate = as.integer(EffectiveDate),
         ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
         ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
         EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
         PriorYear = as.Date(PriorYear, origin = "1899-12-30"))

if(nrow(
  read_xls("SPM_data/Current/0700a.xls",
           sheet = 3,
           range = cell_cols("B2:C9")) %>%
  select("Prompt" = 2, "Selection" = 3) %>%
  head(n = 7L) %>%
  spread(Prompt, Selection) %>%
  select("EDA" = 1,
         "EffectiveDate" = 2,
         "ReportEnd" = 3,
         "PriorYear" = 4,
         "ReportStart" = 5,
         "CoC" = 6,
         "Providers" = 7) %>%
  mutate(ReportStart = as.integer(ReportStart),
         ReportEnd = as.integer(ReportEnd),
         PriorYear = as.integer(PriorYear),
         EffectiveDate = as.integer(EffectiveDate),
         ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
         ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
         EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
         PriorYear = as.Date(PriorYear, origin = "1899-12-30")) %>%
  filter(
    EDA != "-Default Provider-" |
    EffectiveDate != ReportEnd |
    CoC != coc |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
  print("the 0700a report was run incorrectly")


# checking that 700b was run correctly -CURRENT----------------------------

check_loth_b <- read_xls("SPM_data/Current/0700b.xls",
                         sheet = 3,
                         range = cell_cols("B2:C9")) %>%
  select("Prompt" = 2, "Selection" = 3) %>%
  head(n = 7L) %>%
  spread(Prompt, Selection) %>%
  select("EDA" = 1,
         "EffectiveDate" = 2,
         "ReportEnd" = 3,
         "PriorYear" = 4,
         "ReportStart" = 5,
         "CoC" = 6,
         "Providers" = 7) %>%
  mutate(ReportStart = as.integer(ReportStart),
         ReportEnd = as.integer(ReportEnd),
         PriorYear = as.integer(PriorYear),
         EffectiveDate = as.integer(EffectiveDate),
         ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
         ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
         EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
         PriorYear = as.Date(PriorYear, origin = "1899-12-30"))

if(nrow(
  check_loth_b %>%
  filter(
    EDA != "-Default Provider-" |
    EffectiveDate != ReportEnd |
    CoC != coc |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
  print("the 0700b report was run incorrectly")


# Checking that the 701 was run correctly -CURRENT-------------------------

check_recurrence <- read_xls("SPM_data/Current/0701.xls",
                         sheet = how_many_worksheets_in_the_701,
                         range = cell_cols("B2:C9")) %>%
  select("Prompt" = 2, "Selection" = 3) %>%
  head(n = 7L) %>%
  spread(Prompt, Selection) %>%
  select("EDA" = 1,
         "ReportEnd" = 2,
         "EffectiveDate" = 3,
         "PriorYear" = 4,
         "Prior2Year" = 5,
         "CoC" = 6,
         "Providers" = 7) %>%
  mutate(Prior2Year = as.integer(Prior2Year),
         ReportEnd = as.integer(ReportEnd),
         PriorYear = as.integer(PriorYear),
         EffectiveDate = as.integer(EffectiveDate),
         Prior2Year = as.Date(Prior2Year, origin = "1899-12-30"),
         ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
         EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
         PriorYear = as.Date(PriorYear, origin = "1899-12-30"))

if(nrow(
  check_recurrence %>%
  filter(
    EDA != "-Default Provider-" |
    EffectiveDate != ReportEnd |
    CoC != coc |
    ReportEnd - years(2) != PriorYear |
    ReportEnd - years(3) != Prior2Year
  )
) > 0)
  print("the 701 report was run incorrectly")

# checking the homeless count report was run correctly -CURRENT------------

check_homeless_count <- read_xls("SPM_data/Current/0702.xls",
                         sheet = 3,
                         range = cell_cols("B2:C9")) %>%
  select("Prompt" = 2, "Selection" = 3) %>%
  head(n = 7L) %>%
  spread(Prompt, Selection) %>%
  select("EDA" = 1,
         "ReportEnd" = 2,
         "ReportStart" = 3,
         "EffectiveDate" = 4,
         "PriorYear" = 5,
         "CoC" = 6,
         "Providers" = 7) %>%
  mutate(ReportStart = as.integer(ReportStart),
         ReportEnd = as.integer(ReportEnd),
         PriorYear = as.integer(PriorYear),
         EffectiveDate = as.integer(EffectiveDate),
         ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
         ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
         EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
         PriorYear = as.Date(PriorYear, origin = "1899-12-30"))

if(nrow(
  check_homeless_count %>%
  filter(
    EDA != "-Default Provider-" |
    EffectiveDate != ReportEnd |
    CoC != coc |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
  print("the 0702 report was run incorrectly")


# Checking the 703 report was run correctly -CURRENT-----------------------

check_income <- read_xls("SPM_data/Current/0703.xls",
                         sheet = how_many_worksheets_in_the_703,
                         range = cell_cols("B2:e9")) %>%
  select("Prompt" = 2, "Selection" = 5) %>%
  head(n = 7L) %>%
  spread(Prompt, Selection) %>%
  select("EDA" = 1,
         "ReportEnd" = 2,
         "ReportStart" = 3,
         "EffectiveDate" = 4,
         "PriorYear" = 5,
         "CoC" = 6,
         "Providers" = 7) %>%
  mutate(ReportStart = as.integer(ReportStart),
         ReportEnd = as.integer(ReportEnd),
         PriorYear = as.integer(PriorYear),
         EffectiveDate = as.integer(EffectiveDate),
         ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
         ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
         EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
         PriorYear = as.Date(PriorYear, origin = "1899-12-30"))

if(nrow(
  check_income %>%
  filter(
    EDA != "-Default Provider-" |
    EffectiveDate != ReportEnd |
    CoC != coc |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
  print("the 0703 report was run incorrectly")

# Checking the 704 report was run correctly -CURRENT-----------------------

check_first_timers <- read_xls("SPM_data/Current/0704.xls",
                         sheet = 4,
                         range = cell_cols("B2:C9")) %>%
  select("Prompt" = 2, "Selection" = 3) %>%
  head(n = 7L) %>%
  spread(Prompt, Selection) %>%
  select("EDA" = 1,
         "ReportEnd" = 2,
         "ReportStart" = 3,
         "EffectiveDate" = 4,
         "PriorYear" = 5,
         "CoC" = 6,
         "Providers" = 7) %>%
  mutate(ReportStart = as.integer(ReportStart),
         ReportEnd = as.integer(ReportEnd),
         PriorYear = as.integer(PriorYear),
         EffectiveDate = as.integer(EffectiveDate),
         ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
         ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
         EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
         PriorYear = as.Date(PriorYear, origin = "1899-12-30"))

if(nrow(
  check_first_timers %>%
  filter(
    EDA != "-Default Provider-" |
    EffectiveDate != ReportEnd |
    CoC != coc |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
  print("the 0704 report was run incorrectly")

# Checking the 706 report was run correctly -CURRENT-----------------------

check_exits_to_ph <- read_xls("SPM_data/Current/0706.xls",
                         sheet = 3,
                         range = cell_cols("B2:C9")) %>%
  select("Prompt" = 2, "Selection" = 3) %>%
  head(n = 7L) %>%
  spread(Prompt, Selection) %>%
  select("EDA" = 1,
         "ReportEnd" = 2,
         "ReportStart" = 3,
         "EffectiveDate" = 4,
         "PriorYear" = 5,
         "CoC" = 6,
         "Providers" = 7) %>%
  mutate(ReportStart = as.integer(ReportStart),
         ReportEnd = as.integer(ReportEnd),
         PriorYear = as.integer(PriorYear),
         EffectiveDate = as.integer(EffectiveDate),
         ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
         ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
         EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
         PriorYear = as.Date(PriorYear, origin = "1899-12-30"))

if(nrow(
  check_exits_to_ph %>%
  filter(
    EDA != "-Default Provider-" |
    EffectiveDate != ReportEnd |
    CoC != coc |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
  print("the 0706 report was run incorrectly")

# Check that ALL SPMs were run on the same date range -CURRENT-------------

starts <- c(check_exits_to_ph$ReportStart, 
            check_first_timers$ReportStart, 
            check_homeless_count$ReportStart, 
            check_income$ReportStart, 
            check_loth_a$ReportStart, 
            check_loth_b$ReportStart, 
            check_recurrence$ReportEnd - years(1))

ends <- c(check_exits_to_ph$ReportEnd, 
          check_first_timers$ReportEnd, 
          check_homeless_count$ReportEnd, 
          check_income$ReportEnd, 
          check_loth_a$ReportEnd, 
          check_loth_b$ReportEnd, 
          check_recurrence$ReportEnd)

priors <- c(check_exits_to_ph$PriorYear, 
            check_first_timers$PriorYear, 
            check_homeless_count$PriorYear, 
            check_income$PriorYear, 
            check_loth_a$PriorYear, 
            check_loth_b$PriorYear, 
            check_recurrence$PriorYear)

if_else(min(priors) == max(priors), 
        paste("Year Prior =", min(priors)),
        "Your Prior Dates do not all match.")
if_else(min(starts) == max(starts), 
   paste("Report Starts =", min(starts)),
   "Your Start Dates do not all match.")
if_else(min(ends) == max(ends), 
        paste("Report Ends =", min(ends)),
        "Your End Dates do not all match.")

rm(list = ls(pattern = "check_"))

spm_current_start_date <- min(starts)
spm_current_end_date <- min(ends)

# Checking that the 0700a was run correctly -PRIOR-------------------------

check_loth_a <- read_xls("SPM_data/Prior/0700a.xls",
                         sheet = 3,
                         range = cell_cols("B2:C9")) %>%
  select("Prompt" = 2, "Selection" = 3) %>%
  head(n = 7L) %>%
  spread(Prompt, Selection) %>%
  select("EDA" = 1,
         "EffectiveDate" = 2,
         "ReportEnd" = 3,
         "PriorYear" = 4,
         "ReportStart" = 5,
         "CoC" = 6,
         "Providers" = 7) %>%
  mutate(ReportStart = as.integer(ReportStart),
         ReportEnd = as.integer(ReportEnd),
         PriorYear = as.integer(PriorYear),
         EffectiveDate = as.integer(EffectiveDate),
         ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
         ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
         EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
         PriorYear = as.Date(PriorYear, origin = "1899-12-30"))

if(nrow(
  read_xls("SPM_data/Prior/0700a.xls",
           sheet = 3,
           range = cell_cols("B2:C9")) %>%
  select("Prompt" = 2, "Selection" = 3) %>%
  head(n = 7L) %>%
  spread(Prompt, Selection) %>%
  select("EDA" = 1,
         "EffectiveDate" = 2,
         "ReportEnd" = 3,
         "PriorYear" = 4,
         "ReportStart" = 5,
         "CoC" = 6,
         "Providers" = 7) %>%
  mutate(ReportStart = as.integer(ReportStart),
         ReportEnd = as.integer(ReportEnd),
         PriorYear = as.integer(PriorYear),
         EffectiveDate = as.integer(EffectiveDate),
         ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
         ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
         EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
         PriorYear = as.Date(PriorYear, origin = "1899-12-30")) %>%
  filter(
    EDA != "-Default Provider-" |
    EffectiveDate != ReportEnd |
    CoC != coc |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
print("the 0700a report was run incorrectly")


# checking that 700b was run correctly -PRIOR------------------------------

check_loth_b <- read_xls("SPM_data/Prior/0700b.xls",
                         sheet = 3,
                         range = cell_cols("B2:C9")) %>%
  select("Prompt" = 2, "Selection" = 3) %>%
  head(n = 7L) %>%
  spread(Prompt, Selection) %>%
  select("EDA" = 1,
         "EffectiveDate" = 2,
         "ReportEnd" = 3,
         "PriorYear" = 4,
         "ReportStart" = 5,
         "CoC" = 6,
         "Providers" = 7) %>%
  mutate(ReportStart = as.integer(ReportStart),
         ReportEnd = as.integer(ReportEnd),
         PriorYear = as.integer(PriorYear),
         EffectiveDate = as.integer(EffectiveDate),
         ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
         ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
         EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
         PriorYear = as.Date(PriorYear, origin = "1899-12-30"))

if(nrow(
  check_loth_b %>%
  filter(
    EDA != "-Default Provider-" |
    EffectiveDate != ReportEnd |
    CoC != coc |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
  print("the 0700b report was run incorrectly")


# Checking that the 701 was run correctly -PRIOR---------------------------

check_recurrence <- read_xls("SPM_data/Prior/0701.xls",
                             sheet = how_many_worksheets_in_the_701,
                             range = cell_cols("B2:C9")) %>%
  select("Prompt" = 2, "Selection" = 3) %>%
  head(n = 7L) %>%
  spread(Prompt, Selection) %>%
  select("EDA" = 1,
         "ReportEnd" = 2,
         "EffectiveDate" = 3,
         "PriorYear" = 4,
         "Prior2Year" = 5,
         "CoC" = 6,
         "Providers" = 7) %>%
  mutate(Prior2Year = as.integer(Prior2Year),
         ReportEnd = as.integer(ReportEnd),
         PriorYear = as.integer(PriorYear),
         EffectiveDate = as.integer(EffectiveDate),
         Prior2Year = as.Date(Prior2Year, origin = "1899-12-30"),
         ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
         EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
         PriorYear = as.Date(PriorYear, origin = "1899-12-30"))

if(nrow(
  check_recurrence %>%
  filter(
    EDA != "-Default Provider-" |
    EffectiveDate != ReportEnd |
    CoC != coc |
    ReportEnd - years(2) != PriorYear |
    ReportEnd - years(3) != Prior2Year
  )
) > 0)
  print("the 701 report was run incorrectly")

# checking the homeless count report was run correctly -PRIOR--------------

check_homeless_count <- read_xls("SPM_data/Prior/0702.xls",
                                 sheet = 3,
                                 range = cell_cols("B2:C9")) %>%
  select("Prompt" = 2, "Selection" = 3) %>%
  head(n = 7L) %>%
  spread(Prompt, Selection) %>%
  select("EDA" = 1,
         "ReportEnd" = 2,
         "ReportStart" = 3,
         "EffectiveDate" = 4,
         "PriorYear" = 5,
         "CoC" = 6,
         "Providers" = 7) %>%
  mutate(ReportStart = as.integer(ReportStart),
         ReportEnd = as.integer(ReportEnd),
         PriorYear = as.integer(PriorYear),
         EffectiveDate = as.integer(EffectiveDate),
         ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
         ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
         EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
         PriorYear = as.Date(PriorYear, origin = "1899-12-30"))

if(nrow(
  check_homeless_count %>%
  filter(
    EDA != "-Default Provider-" |
    EffectiveDate != ReportEnd |
    CoC != coc |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
  print("the 0702 report was run incorrectly")


# Checking the 703 report was run correctly -PRIOR-------------------------

check_income <- read_xls("SPM_data/Prior/0703.xls",
                         sheet = how_many_worksheets_in_the_703,
                         range = cell_cols("B2:e9")) %>%
  select("Prompt" = 2, "Selection" = 5) %>%
  head(n = 7L) %>%
  spread(Prompt, Selection) %>%
  select("EDA" = 1,
         "ReportEnd" = 2,
         "ReportStart" = 3,
         "EffectiveDate" = 4,
         "PriorYear" = 5,
         "CoC" = 6,
         "Providers" = 7) %>%
  mutate(ReportStart = as.integer(ReportStart),
         ReportEnd = as.integer(ReportEnd),
         PriorYear = as.integer(PriorYear),
         EffectiveDate = as.integer(EffectiveDate),
         ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
         ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
         EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
         PriorYear = as.Date(PriorYear, origin = "1899-12-30"))

if(nrow(
  check_income %>%
  filter(
    EDA != "-Default Provider-" |
    EffectiveDate != ReportEnd |
    CoC != coc |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
  print("the 0703 report was run incorrectly")

# Checking the 704 report was run correctly -PRIOR-------------------------

check_first_timers <- read_xls("SPM_data/Prior/0704.xls",
                               sheet = 4,
                               range = cell_cols("B2:C9")) %>%
  select("Prompt" = 2, "Selection" = 3) %>%
  head(n = 7L) %>%
  spread(Prompt, Selection) %>%
  select("EDA" = 1,
         "ReportEnd" = 2,
         "ReportStart" = 3,
         "EffectiveDate" = 4,
         "PriorYear" = 5,
         "CoC" = 6,
         "Providers" = 7) %>%
  mutate(ReportStart = as.integer(ReportStart),
         ReportEnd = as.integer(ReportEnd),
         PriorYear = as.integer(PriorYear),
         EffectiveDate = as.integer(EffectiveDate),
         ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
         ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
         EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
         PriorYear = as.Date(PriorYear, origin = "1899-12-30"))

if(nrow(
  check_first_timers %>%
  filter(
    EDA != "-Default Provider-" |
    EffectiveDate != ReportEnd |
    CoC != coc |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
  print("the 0704 report was run incorrectly")

# Checking the 706 report was run correctly -PRIOR-------------------------

check_exits_to_ph <- read_xls("SPM_data/Prior/0706.xls",
                              sheet = 3,
                              range = cell_cols("B2:C9")) %>%
  select("Prompt" = 2, "Selection" = 3) %>%
  head(n = 7L) %>%
  spread(Prompt, Selection) %>%
  select("EDA" = 1,
         "ReportEnd" = 2,
         "ReportStart" = 3,
         "EffectiveDate" = 4,
         "PriorYear" = 5,
         "CoC" = 6,
         "Providers" = 7) %>%
  mutate(ReportStart = as.integer(ReportStart),
         ReportEnd = as.integer(ReportEnd),
         PriorYear = as.integer(PriorYear),
         EffectiveDate = as.integer(EffectiveDate),
         ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
         ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
         EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
         PriorYear = as.Date(PriorYear, origin = "1899-12-30"))

if(nrow(
  check_exits_to_ph %>%
  filter(
    EDA != "-Default Provider-" |
    EffectiveDate != ReportEnd |
    CoC != coc |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
  print("the 0706 report was run incorrectly")

# Check that ALL SPMs were run on the same date range -PRIOR---------------

starts <- c(check_exits_to_ph$ReportStart, 
            check_first_timers$ReportStart, 
            check_homeless_count$ReportStart, 
            check_income$ReportStart, 
            check_loth_a$ReportStart, 
            check_loth_b$ReportStart, 
            check_recurrence$ReportEnd - years(1))

ends <- c(check_exits_to_ph$ReportEnd, 
          check_first_timers$ReportEnd, 
          check_homeless_count$ReportEnd, 
          check_income$ReportEnd, 
          check_loth_a$ReportEnd, 
          check_loth_b$ReportEnd, 
          check_recurrence$ReportEnd)

priors <- c(check_exits_to_ph$PriorYear, 
            check_first_timers$PriorYear, 
            check_homeless_count$PriorYear, 
            check_income$PriorYear, 
            check_loth_a$PriorYear, 
            check_loth_b$PriorYear, 
            check_recurrence$PriorYear)

if_else(min(priors) == max(priors), 
        paste("Year Prior = ", min(priors)),
        "Your Prior Dates do not all match.")
if_else(min(starts) == max(starts), 
        paste("Report Starts = ", min(starts)),
        "Your Start Dates do not all match.")
if_else(min(ends) == max(ends), 
        paste("Report Ends = ", min(ends)),
        "Your End Dates do not all match.")

rm(list = ls(pattern = "check_"))

spm_prior_start_date <- min(starts)
spm_prior_end_date <- min(ends)

# Reading in the data we need -CURRENT-------------------------------------

loth_ees_current <- read_xls("SPM_data/Current/0700a.xls",
                                 sheet = 1,
                                 range = cell_cols("B2:E4"))

loth_ees_current <-
  'colnames<-'(loth_ees_current,
               c(
                 "Metric1a",
                 "ClientCount",
                 "AvgLoT",
                 "MedLoT"
               ))
  
loth_self_report_current <- read_xls("SPM_data/Current/0700b.xls",
                             sheet = 1,
                             range = cell_cols("B2:E4"))

loth_self_report_current <-
  'colnames<-'(loth_self_report_current,
               c(
                 "Metric1b",
                 "ClientCount",
                 "AvgLoT",
                 "MedLoT"
               ))

# PLEASE NOTE SHEET 2 IS A CUSTOM MODIFICATION TO THE ART REPORT THAT BREAKS 
# OUT PSH AND RRH. CONTACT GD FOR INFO ON HOW TO MAKE THIS MODIFICATION.
recurrence_current <- read_xls("SPM_data/Current/0701.xls",
                             sheet = what_sheet_is_701_data_on,
                             range = cell_cols("B4:F10"))

recurrence_current <- recurrence_current[-1, ]

recurrence_current <- 'colnames<-'(recurrence_current, c("ProjectType", "ExitedToPHPast2Yrs", 
                           "LessThan6mo", "SixTo12mo", "ThirteenTo24mo"))

homeless_count_current <- read_xls("SPM_data/Current/0702.xls",
                           sheet = 1,
                           range = cell_cols("B2:D6"))

homeless_count_current <- 'colnames<-'(homeless_count_current, c("Type", "Prior", "Current"))

homeless_count_current <- homeless_count_current[, c(1, 3)]

income_current <- read_xls("SPM_data/Current/0703.xls",
                   sheet = 1)

income_empl_stayers_current <- income_current[-c(1, 5:34), ] %>%
  select("Metric4.1" = 1, "CurrentYear" = 3)

income_non_empl_stayers_current <- income_current[-c(1:7, 11:34), ] %>%
  select("Metric4.2" = 1, "CurrentYear" = 3)

income_total_stayers_current <- income_current[-c(1:13, 17:34), ] %>%
  select("Metric4.3" = 1, "CurrentYear" = 3)

income_empl_leavers_current <- income_current[-c(1:19, 23:34), ] %>%
  select("Metric4.4" = 1, "CurrentYear" = 3)

income_non_empl_leavers_current <- income_current[-c(1:25, 29:34), ] %>%
  select("Metric4.5" = 1, "CurrentYear" = 3)

income_total_leavers_current <- income_current[-c(1:31), ] %>%
  select("Metric4.6" = 1, "CurrentYear" = 3)

first_timers_current <- read_xls("SPM_data/Current/0704.xls",
                         sheet = 1)

first_timers_lh_current <- first_timers_current[-c(1, 5:10), ] %>%
  select("Metric5.1" = 1, "CurrentYear" = 3)

first_timers_all_current <- first_timers_current[-c(1:7), ] %>%
  select("Metric5.2" = 1, "CurrentYear" = 3)

exits_to_ph_current <- read_xls("SPM_data/Current/0706.xls",
                        sheet = 1)

exits_out_to_ph_current <- exits_to_ph_current[-c(1, 6:17), ] %>%
  select("Metric7a" = 1, "CurrentYear" = 3)

exits_lh_current <- exits_to_ph_current[-c(1:8, 12:17), ] %>%
  select("Metric7b1" = 1, "CurrentYear" = 3)

exits_ph_current <- exits_to_ph_current[-c(1:14), ] %>%
  select("Metric7b2" = 1, "CurrentYear" = 3)


# Reading in the data we need -PRIOR---------------------------------------

loth_ees_prior <- read_xls("SPM_data/Prior/0700a.xls",
                             sheet = 1,
                             range = cell_cols("B2:E4"))
loth_ees_prior <-
  'colnames<-'(loth_ees_prior,
               c(
                 "Metric1a",
                 "Prior_ClientCount",
                 "Prior_AvgLoT",
                 "Prior_MedLoT"
               ))

spm_1a_loth_ees <- loth_ees_prior %>%
  left_join(loth_ees_current, by = "Metric1a")

loth_self_report_prior <- read_xls("SPM_data/Prior/0700b.xls",
                                     sheet = 1,
                                     range = cell_cols("B2:E4"))

loth_self_report_prior <-
  'colnames<-'(loth_self_report_prior,
               c(
                 "Metric1b",
                 "Prior_ClientCount",
                 "Prior_AvgLoT",
                 "Prior_MedLoT"
               ))

spm_1b_loth_self_report <- loth_self_report_current %>%
  left_join(loth_self_report_prior, by = "Metric1b")

# PLEASE NOTE SHEET 2 IS A CUSTOM MODIFICATION TO THE ART REPORT THAT BREAKS 
# OUT PSH AND RRH. CONTACT GD FOR INFO ON HOW TO MAKE THIS MODIFICATION.
recurrence_prior <- read_xls("SPM_data/Prior/0701.xls",
                               sheet = what_sheet_is_701_data_on,
                               range = cell_cols("B4:F10"))

recurrence_prior <- recurrence_prior[-1, ]

recurrence_prior <-
  'colnames<-'(
    recurrence_prior,
    c(
      "ProjectType",
      "Prior_ExitedToPHPast2Yrs",
      "Prior_LessThan6mo",
      "Prior_SixTo12mo",
      "Prior_ThirteenTo24mo"
    )
  )

spm_2_recurrence <- recurrence_current %>%
  left_join(recurrence_prior, by = "ProjectType")

homeless_count_prior <- read_xls("SPM_data/Prior/0702.xls",
                                   sheet = 1,
                                   range = cell_cols("B2:D6"))

homeless_count_prior <-
  'colnames<-'(homeless_count_prior, c("Type", "Nothing", "Prior"))

homeless_count_prior <- homeless_count_prior[, c(1, 3)]

spm_3_homeless_count <- homeless_count_prior %>%
  left_join(homeless_count_current, by = "Type")

income_prior <- read_xls("SPM_data/Prior/0703.xls",
                           sheet = 1)

spm_4_1_empl_stayers <- income_prior[-c(1, 5:34), ] %>%
  select("Metric4.1" = 1, "PriorYear" = 3) %>%
  left_join(income_empl_stayers_current, by = "Metric4.1")

spm_4_2_income_non_empl_stayers <- income_prior[-c(1:7, 11:34), ] %>%
  select("Metric4.2" = 1, "PriorYear" = 3) %>%
  left_join(income_non_empl_stayers_current, by = "Metric4.2")

spm_4_3_income_total_stayers <- income_prior[-c(1:13, 17:34), ] %>%
  select("Metric4.3" = 1, "PriorYear" = 3) %>%
  left_join(income_total_stayers_current, by = "Metric4.3")

spm_4_4_income_empl_leavers <- income_prior[-c(1:19, 23:34), ] %>%
  select("Metric4.4" = 1, "PriorYear" = 3) %>%
  left_join(income_empl_leavers_current, by = "Metric4.4")

spm_4_5_income_non_empl_leavers <- income_prior[-c(1:25, 29:34), ] %>%
  select("Metric4.5" = 1, "PriorYear" = 3) %>%
  left_join(income_non_empl_leavers_current, by = "Metric4.5")

spm_4_6_income_total_leavers <- income_prior[-c(1:31), ] %>%
  select("Metric4.6" = 1, "PriorYear" = 3) %>%
  left_join(income_total_leavers_current, by = "Metric4.6")

first_timers_prior <- read_xls("SPM_data/Prior/0704.xls",
                                 sheet = 1)

spm_5_1_first_timers_lh <- first_timers_prior[-c(1, 5:10), ] %>%
  select("Metric5.1" = 1, "PriorYear" = 3) %>%
  left_join(first_timers_lh_current, by = "Metric5.1")

spm_5_2_first_timers_all <- first_timers_prior[-c(1:7), ] %>%
  select("Metric5.2" = 1, "PriorYear" = 3) %>%
  left_join(first_timers_all_current, by = "Metric5.2")

exits_to_ph_prior <- read_xls("SPM_data/Prior/0706.xls",
                                sheet = 1)

spm_7a_exits_out_to_ph <- exits_to_ph_prior[-c(1, 6:17), ] %>%
  select("Metric7a" = 1, "PriorYear" = 3) %>%
  left_join(exits_out_to_ph_current, by = "Metric7a")

spm_7b1_exits_lh <- exits_to_ph_prior[-c(1:8, 12:17), ] %>%
  select("Metric7b1" = 1, "PriorYear" = 3) %>%
  left_join(exits_lh_current, by = "Metric7b1")

spm_7b2_exits_ph <- exits_to_ph_prior[-c(1:14), ] %>%
  select("Metric7b2" = 1, "PriorYear" = 3) %>%
  left_join(exits_ph_current, by = "Metric7b2")

rm(list = setdiff(ls(), ls(pattern = "spm_")))

save.image("images/SPM_data.RData")
.bos_path <- "random_data/bos_spms.xlsx"
if (!dir.exists(dirname(.bos_path))) dir.create(dirname(.bos_path))
write_xlsx(
  x = list(
    measure1a = spm_1a_loth_ees,
    measure1b = spm_1b_loth_self_report,
    measure2 = spm_2_recurrence,
    measure3 = spm_3_homeless_count,
    measure4.1 = spm_4_1_empl_stayers,
    measure4.2 = spm_4_2_income_non_empl_stayers,
    measure4.3 = spm_4_3_income_total_stayers,
    measure4.4 = spm_4_4_income_empl_leavers,
    measure4.5 = spm_4_5_income_non_empl_leavers,
    measure4.6 = spm_4_6_income_total_leavers,
    measure5.1 = spm_5_1_first_timers_lh,
    measure5.2 = spm_5_2_first_timers_all,
    measure7a = spm_7a_exits_out_to_ph,
    measure7b1 = spm_7b1_exits_lh,
    measure7b2 = spm_7b2_exits_ph
  ),
  "random_data/bos_spms.xlsx"
)





