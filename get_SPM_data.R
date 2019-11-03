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
library(readxl)


# Renaming all the files to reasonable things -----------------------------

if(file.exists(paste0("SPM_data/",
                      list.files("./SPM_data", pattern = "(0700 -)")))) {
  file.rename(paste0("SPM_data/",
                     list.files("./SPM_data", pattern = "(0700 -)")),
              "SPM_data/0700a.xls")
}

if(file.exists(paste0("SPM_data/",
                      list.files("./SPM_data", pattern = "(0700.1b)")))) {
  file.rename(paste0("SPM_data/",
                     list.files("./SPM_data", pattern = "(0700.1b)")),
              "SPM_data/0700b.xls")
}

if(file.exists(paste0("SPM_data/",
                      list.files("./SPM_data", pattern = "(0701 -)")))) {
  file.rename(paste0("SPM_data/",
                     list.files("./SPM_data", pattern = "(0701 -)")),
              "SPM_data/0701.xls")
}

if(file.exists(paste0("SPM_data/",
                      list.files("./SPM_data", pattern = "(0702 -)")))) {
  file.rename(paste0("SPM_data/",
                     list.files("./SPM_data", pattern = "(0702 -)")),
              "SPM_data/0702.xls")
}

if(file.exists(paste0("SPM_data/",
                      list.files("./SPM_data", pattern = "(0703 -)")))) {
  file.rename(paste0("SPM_data/",
                     list.files("./SPM_data", pattern = "(0703 -)")),
              "SPM_data/0703.xls")
}

if(file.exists(paste0("SPM_data/",
                      list.files("./SPM_data", pattern = "(0704 -)")))) {
  file.rename(paste0("SPM_data/",
                     list.files("./SPM_data", pattern = "(0704 -)")),
              "SPM_data/0704.xls")
}

if(file.exists(paste0("SPM_data/",
                      list.files("./SPM_data", pattern = "(0706 -)")))) {
  file.rename(paste0("SPM_data/",
                     list.files("./SPM_data", pattern = "(0706 -)")),
              "SPM_data/0706.xls")
}


# Checking that the 0700a was run correctly ------------------------------

check_loth_a <- read_xls("SPM_data/0700a.xls",
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
  read_xls("SPM_data/0700a.xls",
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
    CoC != "OH-507: Balance of State" |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
  print("the 0700a report was run incorrectly")


# checking that 700b was run correctly ------------------------------------

check_loth_b <- read_xls("SPM_data/0700b.xls",
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
    CoC != "OH-507: Balance of State" |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
  print("the 0700b report was run incorrectly")


# Checking that the 701 was run correctly ---------------------------------

check_recurrence <- read_xls("SPM_data/0701.xls",
                         sheet = 5,
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
    CoC != "OH-507: Balance of State" |
    ReportEnd - years(2) != PriorYear |
    ReportEnd - years(3) != Prior2Year
  )
) > 0)
  print("the 701 report was run incorrectly")

# checking the homeless count report was run correctly --------------------

check_homeless_count <- read_xls("SPM_data/0702.xls",
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
    CoC != "OH-507: Balance of State" |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
  print("the 0702 report was run incorrectly")


# Checking the 703 report was run correctly -------------------------------

check_income <- read_xls("SPM_data/0703.xls",
                         sheet = 6,
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
    CoC != "OH-507: Balance of State" |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
  print("the 0703 report was run incorrectly")

# Checking the 704 report was run correctly -------------------------------

check_first_timers <- read_xls("SPM_data/0704.xls",
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
    CoC != "OH-507: Balance of State" |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
  print("the 0704 report was run incorrectly")

# Checking the 706 report was run correctly -------------------------------

check_exits_to_ph <- read_xls("SPM_data/0706.xls",
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
    CoC != "OH-507: Balance of State" |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
  print("the 0706 report was run incorrectly")

# Check that ALL SPMs were run on the same date range ---------------------

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

# Reading in the data we need ---------------------------------------------

loth_ees <- read_xls("SPM_data/0700a.xls",
                                 sheet = 1,
                                 range = cell_cols("B2:E4"))
  
loth_self_report <- read_xls("SPM_data/0700b.xls",
                             sheet = 1,
                             range = cell_cols("B2:E4"))

# PLEASE NOTE SHEET 2 IS A CUSTOM MODIFICATION TO THE ART REPORT THAT BREAKS 
# OUT PSH AND RRH. CONTACT GD FOR INFO ON HOW TO MAKE THIS MODIFICATION.
recurrence <- read_xls("SPM_data/0701.xls",
                             sheet = 2,
                             range = cell_cols("B4:F10"))

recurrence <- recurrence[-1, ]

recurrence <- 'colnames<-'(recurrence, c("ProjectType", "ExitedToPHPast2Yrs", 
                           "LessThan6mo", "SixTo12mo", "ThirteenTo24mo"))

homeless_count <- read_xls("SPM_data/0702.xls",
                           sheet = 1,
                           range = cell_cols("B2:D6"))

homeless_count <- 'colnames<-'(homeless_count, c("Type", "Prior", "Current"))

homeless_count <- homeless_count[, c(1, 3)]

income <- read_xls("SPM_data/0703.xls",
                   sheet = 1)

income_empl_stayers <- income[-c(1, 5:34), ] %>%
  select("Metric4.1" = 1, "CurrentYear" = 3)

income_non_empl_stayers <- income[-c(1:7, 11:34), ] %>%
  select("Metric4.2" = 1, "CurrentYear" = 3)

income_total_stayers <- income[-c(1:13, 17:34), ] %>%
  select("Metric4.3" = 1, "CurrentYear" = 3)

income_empl_leavers <- income[-c(1:19, 23:34), ] %>%
  select("Metric4.4" = 1, "CurrentYear" = 3)

income_non_empl_leavers <- income[-c(1:25, 29:34), ] %>%
  select("Metric4.5" = 1, "CurrentYear" = 3)

income_total_leavers <- income[-c(1:31), ] %>%
  select("Metric4.6" = 1, "CurrentYear" = 3)

first_timers <- read_xls("SPM_data/0704.xls",
                         sheet = 1)

first_timers_lh <- first_timers[-c(1, 5:10), ] %>%
  select("Metric5.1" = 1, "CurrentYear" = 3)

first_timers_all <- first_timers[-c(1:7), ] %>%
  select("Metric5.2" = 1, "CurrentYear" = 3)

exits_to_ph <- read_xls("SPM_data/0706.xls",
                        sheet = 1)

exits_out_to_ph <- exits_to_ph[-c(1, 6:17), ] %>%
  select("Metric7a" = 1, "CurrentYear" = 3)

exits_lh <- exits_to_ph[-c(1:8, 12:17), ] %>%
  select("Metric7b1" = 1, "CurrentYear" = 3)

exits_ph <- exits_to_ph[-c(1:14), ] %>%
  select("Metric7b2" = 1, "CurrentYear" = 3)

rm(income, first_timers, exits_to_ph)
