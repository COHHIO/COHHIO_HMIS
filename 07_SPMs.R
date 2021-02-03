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

what_sheet_is_701_data_on <- 2

rename_file <-
  function(CoC = "OH-507",
           subdirectory = "Current",
           pattern = "(0700 -)") {
    directory <- case_when(CoC == "OH-507" ~ "SPM_data_BoS",
                           CoC == "OH-504" ~ "SPM_data_YO")
    filename <- case_when(
      pattern == "(0700 -)" ~ "0700a",
      pattern == "(0700.1b)" ~ "0700b",
      pattern == "(0701 -)" ~ "0701",
      pattern == "(0702 -)" ~ "0702",
      pattern == "(0703 -)" ~ "0703",
      pattern == "(0704 -)" ~ "0704",
      pattern == "(0706 -)" ~ "0706"
    )
    
    if (file.exists(paste0(
      directory,
      "/",
      subdirectory,
      "/",
      list.files(paste0("./", directory, "/", subdirectory),
                 pattern = pattern)
    ))) {
      file.rename(
        paste0(
          directory,
          "/",
          subdirectory,
          "/",
          list.files(paste0("./", directory, "/", subdirectory),
                     pattern = pattern)
        ),
        paste0(directory, "/", subdirectory, "/", filename, ".xls")
      )
    }
  }


# Renaming all the files to reasonable things -----------------------------

rename_file(CoC = "OH-507", subdirectory = "Current", pattern = "(0700 -)")
rename_file(CoC = "OH-507", subdirectory = "Current", pattern = "(0700.1b)")
rename_file(CoC = "OH-507", subdirectory = "Current", pattern = "(0701 -)")
rename_file(CoC = "OH-507", subdirectory = "Current", pattern = "(0702 -)")
rename_file(CoC = "OH-507", subdirectory = "Current", pattern = "(0703 -)")
rename_file(CoC = "OH-507", subdirectory = "Current", pattern = "(0704 -)")
rename_file(CoC = "OH-507", subdirectory = "Current", pattern = "(0706 -)")
rename_file(CoC = "OH-507", subdirectory = "Prior", pattern = "(0700 -)")
rename_file(CoC = "OH-507", subdirectory = "Prior", pattern = "(0700.1b)")
rename_file(CoC = "OH-507", subdirectory = "Prior", pattern = "(0701 -)")
rename_file(CoC = "OH-507", subdirectory = "Prior", pattern = "(0702 -)")
rename_file(CoC = "OH-507", subdirectory = "Prior", pattern = "(0703 -)")
rename_file(CoC = "OH-507", subdirectory = "Prior", pattern = "(0704 -)")
rename_file(CoC = "OH-507", subdirectory = "Prior", pattern = "(0706 -)")

rename_file(CoC = "OH-504", subdirectory = "Current", pattern = "(0700 -)")
rename_file(CoC = "OH-504", subdirectory = "Current", pattern = "(0700.1b)")
rename_file(CoC = "OH-504", subdirectory = "Current", pattern = "(0701 -)")
rename_file(CoC = "OH-504", subdirectory = "Current", pattern = "(0702 -)")
rename_file(CoC = "OH-504", subdirectory = "Current", pattern = "(0703 -)")
rename_file(CoC = "OH-504", subdirectory = "Current", pattern = "(0704 -)")
rename_file(CoC = "OH-504", subdirectory = "Current", pattern = "(0706 -)")
rename_file(CoC = "OH-504", subdirectory = "Prior", pattern = "(0700 -)")
rename_file(CoC = "OH-504", subdirectory = "Prior", pattern = "(0700.1b)")
rename_file(CoC = "OH-504", subdirectory = "Prior", pattern = "(0701 -)")
rename_file(CoC = "OH-504", subdirectory = "Prior", pattern = "(0702 -)")
rename_file(CoC = "OH-504", subdirectory = "Prior", pattern = "(0703 -)")
rename_file(CoC = "OH-504", subdirectory = "Prior", pattern = "(0704 -)")
rename_file(CoC = "OH-504", subdirectory = "Prior", pattern = "(0706 -)")

# OPEN ALL YOUR EXCEL FILES AND PRESS ENABLE EDITING BEFORE PROCEEDING
# Checking that the 0700a was run correctly -------------------------------

check_loth_a <- function(CoC = "OH-507", subdirectory = "Current") {
  
  directory <- case_when(CoC == "OH-507" ~ "SPM_data_BoS",
                         CoC == "OH-504" ~ "SPM_data_YO")
  
  loth_a <-
    read_xls(
      paste0(directory, "/", subdirectory, "/0700a.xls"),
      sheet = 3,
      range = cell_cols("B2:C9")
    ) %>%
    select("Prompt" = 2, "Selection" = 3) %>%
    head(n = 7L) %>%
    spread(Prompt, Selection) %>%
    select(
      "EDA" = 1,
      "EffectiveDate" = 2,
      "ReportEnd" = 3,
      "PriorYear" = 4,
      "ReportStart" = 5,
      "CoCInfo" = 6,
      "Providers" = 7
    ) %>%
    mutate(
      ReportStart = as.integer(ReportStart),
      ReportEnd = as.integer(ReportEnd),
      PriorYear = as.integer(PriorYear),
      EffectiveDate = as.integer(EffectiveDate),
      ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
      ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
      EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
      PriorYear = as.Date(PriorYear, origin = "1899-12-30")
    )
  
  rpt_0700a <- loth_a %>%
    select(ReportStart, ReportEnd, PriorYear, EffectiveDate) %>%
    mutate(CoCName = CoC,
           CurrentOrPrior = subdirectory,
           ReportName = "0700a")
  
  assign(paste0(str_remove(CoC, "-"), "_", subdirectory, "_rpt_0700a"), 
         rpt_0700a, 
         envir = .GlobalEnv)
  
  loth_ees <- read_xls(
    paste0(directory,
           "/",
           subdirectory,
           "/0700a.xls"),
    sheet = 1,
    range = cell_cols("B2:E4")
  ) %>%
    select("Metric1a" = 1, "ClientCount" = 2, "AvgLoT" = 3, "MedLoT" = 4) %>%
    mutate(CoCName = CoC,
           CurrentPrior = subdirectory)
  
  assign(paste0("Metric_1a_", str_remove(CoC, "-"), "_", subdirectory), 
         loth_ees, 
         envir = .GlobalEnv)

  if (nrow(
    loth_a %>%
    filter(
      EDA != "-Default Provider-" |
      EffectiveDate != ReportEnd |
      str_sub(CoCInfo, 1, 6) != CoC |
      ReportEnd - years(1) != ReportStart |
      ReportStart - years(1) != PriorYear
    )
  ) > 0)
    print(paste("the", CoC, subdirectory, "0700a report was run incorrectly"))
  else{
    print(paste(CoC, subdirectory, "0700a ok"))
  }
}

check_loth_a(CoC = "OH-507", subdirectory = "Current")
check_loth_a(CoC = "OH-507", subdirectory = "Prior")

check_loth_a(CoC = "OH-504", subdirectory = "Current")
check_loth_a(CoC = "OH-504", subdirectory = "Prior")


# checking that 700b was run correctly ------------------------------------

check_loth_b <- function(CoC = "OH-507", subdirectory = "Current"){
  
  directory <- case_when(CoC == "OH-507" ~ "SPM_data_BoS",
                         CoC == "OH-504" ~ "SPM_data_YO")
  
  loth_b <- read_xls(paste0(directory,
                            "/",
                            subdirectory,
                            "/0700b.xls"),
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
         "CoCInfo" = 6,
         "Providers" = 7) %>%
  mutate(ReportStart = as.integer(ReportStart),
         ReportEnd = as.integer(ReportEnd),
         PriorYear = as.integer(PriorYear),
         EffectiveDate = as.integer(EffectiveDate),
         ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
         ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
         EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
         PriorYear = as.Date(PriorYear, origin = "1899-12-30"))

  rpt_0700b <- loth_b %>%
    select(ReportStart, ReportEnd, PriorYear, EffectiveDate) %>%
    mutate(CoCName = CoC,
           CurrentOrPrior = subdirectory,
           ReportName = "0700b")
  
  assign(paste0(str_remove(CoC, "-"), "_", subdirectory, "_rpt_0700b"), 
         rpt_0700b, 
         envir = .GlobalEnv)
  
  loth_self_report <- read_xls(
    paste0(directory,
           "/",
           subdirectory,
           "/0700b.xls"),
    sheet = 1,
    range = cell_cols("B2:E4")
  ) %>%
    select(
      "Metric1b" = 1,
      "ClientCount" = 2,
      "AvgLoT" = 3,
      "MedLoT" = 4
    ) %>%
    mutate(CoCName = CoC,
           CurrentPrior = subdirectory)  
  
  assign(paste0("Metric_1b_", str_remove(CoC, "-"), "_", subdirectory), 
         loth_self_report, 
         envir = .GlobalEnv)
  
if(nrow(
  loth_b %>%
  filter(
    EDA != "-Default Provider-" |
    EffectiveDate != ReportEnd |
    str_sub(CoCInfo, 1, 6) != CoC |
    ReportEnd - years(1) != ReportStart |
    ReportStart - years(1) != PriorYear
  )
) > 0)
  print(paste("the", CoC, subdirectory,"0700b report was run incorrectly"))
  else{
    print(paste(CoC, subdirectory, "0700b ok"))
  }
  
  }

check_loth_b(CoC = "OH-507", subdirectory = "Current")
check_loth_b(CoC = "OH-507", subdirectory = "Prior")

check_loth_b(CoC = "OH-504", subdirectory = "Current")
check_loth_b(CoC = "OH-504", subdirectory = "Prior")

# Checking that the 701 was run correctly ---------------------------------

check_recurrence <-
  function(CoC = "OH-507", subdirectory = "Current") {
    directory <- case_when(CoC == "OH-507" ~ "SPM_data_BoS",
                           CoC == "OH-504" ~ "SPM_data_YO")
    
    worksheet_count <- length(excel_sheets(paste0(
      directory,
      "/",
      subdirectory,
      "/0701.xls"
    )))
    
    recurrence <- read_xls(
      paste0(directory,
             "/",
             subdirectory,
             "/0701.xls"),
      sheet = worksheet_count,
      range = cell_cols("B2:C9")
    ) %>%
      select("Prompt" = 2, "Selection" = 3) %>%
      head(n = 7L) %>%
      spread(Prompt, Selection) %>%
      select(
        "EDA" = 1,
        "ReportEnd" = 2,
        "EffectiveDate" = 3,
        "PriorYear" = 4,
        "Prior2Year" = 5,
        "CoCInfo" = 6,
        "Providers" = 7
      ) %>%
      mutate(
        Prior2Year = as.integer(Prior2Year),
        ReportEnd = as.integer(ReportEnd),
        PriorYear = as.integer(PriorYear),
        EffectiveDate = as.integer(EffectiveDate),
        Prior2Year = as.Date(Prior2Year, origin = "1899-12-30"),
        ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
        EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
        PriorYear = as.Date(PriorYear, origin = "1899-12-30")
      )
    
    rpt_0701 <- recurrence %>%
      select(PriorYear, Prior2Year, ReportEnd, EffectiveDate) %>%
      mutate(CoCName = CoC,
             CurrentOrPrior = subdirectory,
             ReportName = "0701")
    
    assign(paste0(str_remove(CoC, "-"), "_", subdirectory, "_rpt_0701"), 
           rpt_0701, 
           envir = .GlobalEnv)
    
    recurrence_data <- read_xls(
      paste0(directory,
             "/",
             subdirectory,
             "/0701.xls"),
      sheet = what_sheet_is_701_data_on,
      range = cell_rows(3:10)
    ) %>%
      select(
        "ProjectType" = 1,
        "ExitedToPHPast2Yrs" = 2,
        "LessThan6mo" = 3,
        "SixTo12mo" = 4,
        "ThirteenTo24mo" = 5
      ) %>%
      mutate(CoCName = CoC,
             CurrentPrior = subdirectory)  
    
    assign(paste0("Metric_2_", str_remove(CoC, "-"), "_", subdirectory), 
           recurrence_data, 
           envir = .GlobalEnv)

    if (nrow(
      recurrence %>%
      filter(
        EDA != "-Default Provider-" |
        EffectiveDate != ReportEnd |
        str_sub(CoCInfo, 1, 6) != CoC |
        ReportEnd - years(2) != PriorYear |
        ReportEnd - years(3) != Prior2Year
      )
    ) > 0)
      print(paste("the", CoC, subdirectory, "0701 report was run incorrectly"))
    else{
      print(paste(CoC, subdirectory, "0701 ok"))
    }
  }

check_recurrence(CoC = "OH-507", subdirectory = "Current")
check_recurrence(CoC = "OH-507", subdirectory = "Prior")

check_recurrence(CoC = "OH-504", subdirectory = "Current")
check_recurrence(CoC = "OH-504", subdirectory = "Prior")

# checking the homeless count report was run correctly --------------------

check_homeless_count <-
  function(CoC = "OH-507", subdirectory = "Current") {
    directory <- case_when(CoC == "OH-507" ~ "SPM_data_BoS",
                           CoC == "OH-504" ~ "SPM_data_YO")
    
    homeless_count <- read_xls(
      paste0(directory,
             "/",
             subdirectory,
             "/0702.xls"),
      sheet = 3,
      range = cell_cols("B2:C9")
    ) %>%
      select("Prompt" = 2, "Selection" = 3) %>%
      head(n = 7L) %>%
      spread(Prompt, Selection) %>%
      select(
        "EDA" = 1,
        "ReportEnd" = 2,
        "ReportStart" = 3,
        "EffectiveDate" = 4,
        "PriorYear" = 5,
        "CoCInfo" = 6,
        "Providers" = 7
      ) %>%
      mutate(
        ReportStart = as.integer(ReportStart),
        ReportEnd = as.integer(ReportEnd),
        PriorYear = as.integer(PriorYear),
        EffectiveDate = as.integer(EffectiveDate),
        ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
        ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
        EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
        PriorYear = as.Date(PriorYear, origin = "1899-12-30")
      )
    
    rpt_0702 <- homeless_count %>%
      select(ReportStart, ReportEnd, PriorYear, EffectiveDate) %>%
      mutate(CoCName = CoC,
             CurrentOrPrior = subdirectory,
             ReportName = "0702")
    
    assign(paste0(str_remove(CoC, "-"), "_", subdirectory, "_rpt_0702"),
           rpt_0702,
           envir = .GlobalEnv)
    
    homeless_count_data <- read_xls(
      paste0(directory,
             "/",
             subdirectory,
             "/0702.xls"),
      sheet = 1,
      range = cell_cols("B2:D6")
    ) %>%
      select("Type" = 1, "Count" = 3)
    
    assign(paste0("Metric_3_", str_remove(CoC, "-"), "_", subdirectory),
           homeless_count_data,
           envir = .GlobalEnv)
    
    if (nrow(
      homeless_count %>%
      filter(
        EDA != "-Default Provider-" |
        EffectiveDate != ReportEnd |
        str_sub(CoCInfo, 1, 6) != CoC |
        ReportEnd - years(1) != ReportStart |
        ReportStart - years(1) != PriorYear
      )
    ) > 0)
      print(paste("the", CoC, subdirectory, "0702 report was run incorrectly"))
    else{
      print(paste(CoC, subdirectory, "0702 ok"))
    }
  }

check_homeless_count(CoC = "OH-507", subdirectory = "Current")
check_homeless_count(CoC = "OH-507", subdirectory = "Prior")

check_homeless_count(CoC = "OH-504", subdirectory = "Current")
check_homeless_count(CoC = "OH-504", subdirectory = "Prior")

# Checking the 703 report was run correctly -------------------------------

check_income <- function(CoC = "OH-507", subdirectory = "Current") {
  directory <- case_when(CoC == "OH-507" ~ "SPM_data_BoS",
                         CoC == "OH-504" ~ "SPM_data_YO")
  
  worksheet_count <- length(excel_sheets(paste0(
    directory,
    "/",
    subdirectory,
    "/0703.xls"
  )))
  
  income <- read_xls(
    paste0(directory,
           "/",
           subdirectory,
           "/0703.xls"),
    sheet = worksheet_count,
    range = cell_cols("B2:e9")
  ) %>%
    select("Prompt" = 2, "Selection" = 5) %>%
    head(n = 7L) %>%
    spread(Prompt, Selection) %>%
    select(
      "EDA" = 1,
      "ReportEnd" = 2,
      "ReportStart" = 3,
      "EffectiveDate" = 4,
      "PriorYear" = 5,
      "CoCInfo" = 6,
      "Providers" = 7
    ) %>%
    mutate(
      ReportStart = as.integer(ReportStart),
      ReportEnd = as.integer(ReportEnd),
      PriorYear = as.integer(PriorYear),
      EffectiveDate = as.integer(EffectiveDate),
      ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
      ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
      EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
      PriorYear = as.Date(PriorYear, origin = "1899-12-30")
    )
  
  
  rpt_0703 <- income %>%
    select(ReportStart, ReportEnd, PriorYear, EffectiveDate) %>%
    mutate(CoCName = CoC,
           CurrentOrPrior = subdirectory,
           ReportName = "0703")
  
  assign(paste0(str_remove(CoC, "-"), "_", subdirectory, "_rpt_0703"), 
         rpt_0703, 
         envir = .GlobalEnv)
  
  income_data <- read_xls(paste0(directory,
                                 "/",
                                 subdirectory,
                                 "/0703.xls"),
                             sheet = 1) %>%
    select("ClientsCounted" = 1, "Counts" = 3) %>%
    filter(!is.na(ClientsCounted))
  
  income_empl_stayers <- income_data[c(1:3),] %>%
    mutate(CoCName = CoC,
           CurrentPrior = subdirectory,
           Metric = "4.1")  
  
  income_non_empl_stayers <- income_data[c(5:7),] %>%
    mutate(CoCName = CoC,
           CurrentPrior = subdirectory,
           Metric = "4.2") 
  
  income_total_stayers <- income_data[c(9:11),] %>%
    mutate(CoCName = CoC,
           CurrentPrior = subdirectory,
           Metric = "4.3") 
  
  income_empl_leavers <- income_data[c(13:15),] %>%
    mutate(CoCName = CoC,
           CurrentPrior = subdirectory,
           Metric = "4.4") 
  
  income_non_empl_leavers <- income_data[c(17:19),] %>%
    mutate(CoCName = CoC,
           CurrentPrior = subdirectory,
           Metric = "4.5") 
  
  income_total_leavers <- income_data[c(21:23),] %>%
    mutate(CoCName = CoC,
           CurrentPrior = subdirectory,
           Metric = "4.6") 
  
  income_data <- rbind(
    income_empl_leavers,
    income_empl_stayers,
    income_non_empl_leavers,
    income_non_empl_stayers,
    income_total_leavers,
    income_total_stayers
  )
  
  assign(paste0("Metric_4_", str_remove(CoC, "-"), "_", subdirectory),
         income_data,
         envir = .GlobalEnv)
  
  if (nrow(
    income %>%
    filter(
      EDA != "-Default Provider-" |
      EffectiveDate != ReportEnd |
      str_sub(CoCInfo, 1, 6) != CoC |
      ReportEnd - years(1) != ReportStart |
      ReportStart - years(1) != PriorYear
    )
  ) > 0)
    print(paste("the", CoC, subdirectory, "0703 report was run incorrectly"))
  else{
    print(paste(CoC, subdirectory, "0703 ok"))
  }
}

check_income(CoC = "OH-507", subdirectory = "Current")
check_income(CoC = "OH-507", subdirectory = "Prior")

check_income(CoC = "OH-504", subdirectory = "Current")
check_income(CoC = "OH-504", subdirectory = "Prior")

# Checking the 704 report was run correctly -------------------------------

check_first_timers <-
  function(CoC = "OH-507", subdirectory = "Current") {
    
    directory <- case_when(CoC == "OH-507" ~ "SPM_data_BoS",
                           CoC == "OH-504" ~ "SPM_data_YO")
    
    first_timers <- read_xls(paste0(directory,
                                    "/",
                                    subdirectory,
                                    "/0704.xls"),
                             sheet = 4,
                             range = cell_cols("B2:C9")) %>%
      select("Prompt" = 2, "Selection" = 3) %>%
      head(n = 7L) %>%
      spread(Prompt, Selection) %>%
      select(
        "EDA" = 1,
        "ReportEnd" = 2,
        "ReportStart" = 3,
        "EffectiveDate" = 4,
        "PriorYear" = 5,
        "CoCInfo" = 6,
        "Providers" = 7
      ) %>%
      mutate(
        ReportStart = as.integer(ReportStart),
        ReportEnd = as.integer(ReportEnd),
        PriorYear = as.integer(PriorYear),
        EffectiveDate = as.integer(EffectiveDate),
        ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
        ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
        EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
        PriorYear = as.Date(PriorYear, origin = "1899-12-30")
      )
    
    rpt_0704 <- first_timers %>%
      select(ReportStart, ReportEnd, PriorYear, EffectiveDate) %>%
      mutate(CoCName = CoC,
             CurrentOrPrior = subdirectory,
             ReportName = "0704")
    
    assign(paste0(str_remove(CoC, "-"), "_", subdirectory, "_rpt_0704"), 
           rpt_0704, 
           envir = .GlobalEnv)
    
    if (nrow(
      first_timers %>%
      filter(
        EDA != "-Default Provider-" |
        EffectiveDate != ReportEnd |
        str_sub(CoCInfo, 1, 6) != CoC |
        ReportEnd - years(1) != ReportStart |
        ReportStart - years(1) != PriorYear
      )
    ) > 0)
      print(paste("the", CoC, subdirectory, "0704 report was run incorrectly"))
    else{
      print(paste(CoC, subdirectory, "0704 ok"))
    }
  }

check_first_timers(CoC = "OH-507", subdirectory = "Current")
check_first_timers(CoC = "OH-507", subdirectory = "Prior")

check_first_timers(CoC = "OH-504", subdirectory = "Current")
check_first_timers(CoC = "OH-504", subdirectory = "Prior")


# Checking the 706 report was run correctly -------------------------------


check_exits_to_ph <-
  function(CoC = "OH-507", subdirectory = "Current") {
    directory <- case_when(CoC == "OH-507" ~ "SPM_data_BoS",
                           CoC == "OH-504" ~ "SPM_data_YO")
    
    exits_to_ph <- read_xls(
      paste0(directory,
             "/",
             subdirectory,
             "/0706.xls"),
      sheet = 3,
      range = cell_cols("B2:C9")
    ) %>%
      select("Prompt" = 2, "Selection" = 3) %>%
      head(n = 7L) %>%
      spread(Prompt, Selection) %>%
      select(
        "EDA" = 1,
        "ReportEnd" = 2,
        "ReportStart" = 3,
        "EffectiveDate" = 4,
        "PriorYear" = 5,
        "CoCInfo" = 6,
        "Providers" = 7
      ) %>%
      mutate(
        ReportStart = as.integer(ReportStart),
        ReportEnd = as.integer(ReportEnd),
        PriorYear = as.integer(PriorYear),
        EffectiveDate = as.integer(EffectiveDate),
        ReportStart = as.Date(ReportStart, origin = "1899-12-30"),
        ReportEnd = as.Date(ReportEnd, origin = "1899-12-30"),
        EffectiveDate = as.Date(EffectiveDate, origin = "1899-12-30"),
        PriorYear = as.Date(PriorYear, origin = "1899-12-30")
      )
    
    rpt_0706 <- exits_to_ph %>%
      select(ReportStart, ReportEnd, PriorYear, EffectiveDate) %>%
      mutate(CoCName = CoC,
             CurrentOrPrior = subdirectory,
             ReportName = "0706")
    
    assign(paste0(str_remove(CoC, "-"), "_", subdirectory, "_rpt_0706"), 
           rpt_0706, 
           envir = .GlobalEnv)
    
    if (nrow(
      exits_to_ph %>%
      filter(
        EDA != "-Default Provider-" |
        EffectiveDate != ReportEnd |
        str_sub(CoCInfo, 1, 6) != CoC |
        ReportEnd - years(1) != ReportStart |
        ReportStart - years(1) != PriorYear
      )
    ) > 0)
      print(paste("the", CoC, subdirectory, "0706 report was run incorrectly"))
    else{
      print(paste(CoC, subdirectory, "0706 ok"))
    }
  }

check_exits_to_ph(CoC = "OH-507", subdirectory = "Current")
check_exits_to_ph(CoC = "OH-507", subdirectory = "Prior")

check_exits_to_ph(CoC = "OH-504", subdirectory = "Current")
check_exits_to_ph(CoC = "OH-504", subdirectory = "Prior")

# Check that ALL SPMs were run on the same date range ---------------------

current_rpts <- OH507_Current_rpt_0700a %>%
  full_join(OH507_Current_rpt_0700b) %>%
  full_join(OH507_Current_rpt_0701) %>%
  full_join(OH507_Current_rpt_0702) %>%
  full_join(OH507_Current_rpt_0703) %>%
  full_join(OH507_Current_rpt_0704) %>%
  full_join(OH504_Current_rpt_0700a) %>%
  full_join(OH504_Current_rpt_0700b) %>%
  full_join(OH504_Current_rpt_0701) %>%
  full_join(OH504_Current_rpt_0702) %>%
  full_join(OH504_Current_rpt_0703) %>%
  full_join(OH504_Current_rpt_0704)

prior_rpts <- OH507_Prior_rpt_0700a %>%
  full_join(OH507_Prior_rpt_0700b) %>%
  full_join(OH507_Prior_rpt_0701) %>%
  full_join(OH507_Prior_rpt_0702) %>%
  full_join(OH507_Prior_rpt_0703) %>%
  full_join(OH507_Prior_rpt_0704) %>%
  full_join(OH504_Prior_rpt_0700a) %>%
  full_join(OH504_Prior_rpt_0700b) %>%
  full_join(OH504_Prior_rpt_0701) %>%
  full_join(OH504_Prior_rpt_0702) %>%
  full_join(OH504_Prior_rpt_0703) %>%
  full_join(OH504_Prior_rpt_0704)

mahoning_current <- current_rpts %>%
  filter(CoCName == "OH-504") %>%
  mutate(CheckStart = if_else(min(ReportStart, na.rm = TRUE) ==
                                max(ReportStart, na.rm = TRUE),
                              0, 1),
         CheckEnd = if_else(min(ReportEnd, na.rm = TRUE) ==
                              max(ReportEnd, na.rm = TRUE),
                            0, 1),
         CheckPrior = if_else(min(PriorYear, na.rm = TRUE) ==
                              max(PriorYear, na.rm = TRUE),
                              0, 1),
         CheckPrior2 = if_else(is.na(Prior2Year) | 
                                 Prior2Year + years(1) == PriorYear,
                               0, 1))

bos_current <- current_rpts %>%
  filter(CoCName == "OH-507") %>%
  mutate(CheckStart = if_else(min(ReportStart, na.rm = TRUE) ==
                                max(ReportStart, na.rm = TRUE),
                              0, 1),
         CheckEnd = if_else(min(ReportEnd, na.rm = TRUE) ==
                              max(ReportEnd, na.rm = TRUE),
                            0, 1),
         CheckPrior = if_else(min(PriorYear, na.rm = TRUE) ==
                                max(PriorYear, na.rm = TRUE),
                              0, 1),
         CheckPrior2 = if_else(is.na(Prior2Year) | 
                                 Prior2Year + years(1) == PriorYear,
                               0, 1))

mahoning_prior <- prior_rpts %>%
  filter(CoCName == "OH-504") %>%
  mutate(CheckStart = if_else(min(ReportStart, na.rm = TRUE) ==
                                max(ReportStart, na.rm = TRUE),
                              0, 1),
         CheckEnd = if_else(min(ReportEnd, na.rm = TRUE) ==
                              max(ReportEnd, na.rm = TRUE),
                            0, 1),
         CheckPrior = if_else(min(PriorYear, na.rm = TRUE) ==
                                max(PriorYear, na.rm = TRUE),
                              0, 1),
         CheckPrior2 = if_else(is.na(Prior2Year) | 
                                 Prior2Year + years(1) == PriorYear,
                               0, 1))

bos_prior <- prior_rpts %>%
  filter(CoCName == "OH-507") %>%
  mutate(CheckStart = if_else(min(ReportStart, na.rm = TRUE) ==
                                max(ReportStart, na.rm = TRUE),
                              0, 1),
         CheckEnd = if_else(min(ReportEnd, na.rm = TRUE) ==
                              max(ReportEnd, na.rm = TRUE),
                            0, 1),
         CheckPrior = if_else(min(PriorYear, na.rm = TRUE) ==
                                max(PriorYear, na.rm = TRUE),
                              0, 1),
         CheckPrior2 = if_else(is.na(Prior2Year) | 
                                 Prior2Year + years(1) == PriorYear,
                               0, 1))

if_else(
  sum(mahoning_current$CheckStart) == 0,
  paste(
    "Mahoning Current Report Start =", 
    min(mahoning_current$ReportStart, na.rm = TRUE)
  ),
  "Your Mahoning Current Start Dates do not all match."
)

if_else(
  sum(mahoning_current$CheckEnd) == 0,
  paste(
    "Mahoning Current Report End =", 
    min(mahoning_current$ReportEnd, na.rm = TRUE)
  ),
  "Your Mahoning Current End Dates do not all match."
)

if_else(
  sum(mahoning_current$CheckPrior) == 0,
  paste(
    "Mahoning Current Report Prior Start =", 
    min(mahoning_current$PriorYear, na.rm = TRUE)
  ),
  "Your Mahoning Current Prior Start Dates do not all match."
)

if_else(
  sum(bos_current$CheckStart) == 0,
  paste(
    "BoS Current Report Start =", 
    min(bos_current$ReportStart, na.rm = TRUE)
  ),
  "Your BOS Current Start Dates do not all match."
)

if_else(
  sum(bos_current$CheckEnd) == 0,
  paste(
    "BoS Current Report End =", 
    min(bos_current$ReportEnd, na.rm = TRUE)
  ),
  "Your BoS Current End Dates do not all match."
)

if_else(
  sum(bos_current$CheckPrior) == 0,
  paste(
    "BoS Current Report Prior Start =", 
    min(bos_current$PriorYear, na.rm = TRUE)
  ),
  "Your BoS Current Prior Start Dates do not all match."
)

rm(list = ls(pattern = "OH-"))
rm(prior_rpts, current_rpts)

# Reading in the data we need -CURRENT-------------------------------------

# loth_ees_current <- read_xls("SPM_data/Current/0700a.xls",
#                                  sheet = 1,
#                                  range = cell_cols("B2:E4"))
# 
# loth_ees_current <-
#   'colnames<-'(loth_ees_current,
#                c(
#                  "Metric1a",
#                  "ClientCount",
#                  "AvgLoT",
#                  "MedLoT"
#                ))
  
# loth_self_report_current <- read_xls("SPM_data/Current/0700b.xls",
#                              sheet = 1,
#                              range = cell_cols("B2:E4"))
# 
# loth_self_report_current <-
#   'colnames<-'(loth_self_report_current,
#                c(
#                  "Metric1b",
#                  "ClientCount",
#                  "AvgLoT",
#                  "MedLoT"
#                ))

# PLEASE NOTE SHEET 2 IS A CUSTOM MODIFICATION TO THE ART REPORT THAT BREAKS 
# OUT PSH AND RRH. CONTACT GD FOR INFO ON HOW TO MAKE THIS MODIFICATION.
# recurrence_current <- read_xls("SPM_data/Current/0701.xls",
#                                sheet = what_sheet_is_701_data_on,
#                                range = cell_cols("B4:F10"))
# 
# recurrence_current <- recurrence_current[-1,]
# 
# recurrence_current <-
#   'colnames<-'(
#     recurrence_current,
#     c(
#       "ProjectType",
#       "ExitedToPHPast2Yrs",
#       "LessThan6mo",
#       "SixTo12mo",
#       "ThirteenTo24mo"
#     )
#   )

# homeless_count_current <- read_xls("SPM_data/Current/0702.xls",
#                            sheet = 1,
#                            range = cell_cols("B2:D6"))
# 
# homeless_count_current <- 'colnames<-'(homeless_count_current, c("Type", "Prior", "Current"))
# 
# homeless_count_current <- homeless_count_current[, c(1, 3)]

# income_current <- read_xls("SPM_data/Current/0703.xls",
#                    sheet = 1)
# 
# income_empl_stayers_current <- income_current[-c(1, 5:34), ] %>%
#   select("Metric4.1" = 1, "CurrentYear" = 3)
# 
# income_non_empl_stayers_current <- income_current[-c(1:7, 11:34), ] %>%
#   select("Metric4.2" = 1, "CurrentYear" = 3)
# 
# income_total_stayers_current <- income_current[-c(1:13, 17:34), ] %>%
#   select("Metric4.3" = 1, "CurrentYear" = 3)
# 
# income_empl_leavers_current <- income_current[-c(1:19, 23:34), ] %>%
#   select("Metric4.4" = 1, "CurrentYear" = 3)
# 
# income_non_empl_leavers_current <- income_current[-c(1:25, 29:34), ] %>%
#   select("Metric4.5" = 1, "CurrentYear" = 3)
# 
# income_total_leavers_current <- income_current[-c(1:31), ] %>%
#   select("Metric4.6" = 1, "CurrentYear" = 3)

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





