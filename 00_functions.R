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

# Age Function ------------------------------------------------------------

age_years <- function(earlier, later)
{
  lt <- data.frame(earlier, later)
  age <- as.numeric(format(lt[,2],format="%Y")) - as.numeric(format(lt[,1],format="%Y"))
  
  dayOnLaterYear <- ifelse(
    format(lt[, 1], format = "%m-%d") != "02-29",
    as.Date(paste(
      format(lt[, 2], format = "%Y"), "-",
      format(lt[, 1], format = "%m-%d"),
      sep = ""
    )),
    ifelse(
      as.numeric(format(later, format = "%Y")) %%
        400 == 0 |
        as.numeric(format(later,
                          format =
                            "%Y")) %%
        100 != 0 &
        as.numeric(format(later, format = "%Y")) %%
        4 == 0,
      as.Date(paste(
        format(lt[, 2], format = "%Y"),
        "-",
        format(lt[, 1], format =
                 "%m-%d"),
        sep = ""
      )),
      as.Date(paste(
        format(lt[, 2], format = "%Y"),
        "-",
        "02-28",
        sep = ""
      ))
    )
  )
  
  age[which(dayOnLaterYear > lt$later)] <- age[which(dayOnLaterYear > lt$later)] - 1
  
  age
}

# # Client Entry Exits Between Date Range Functions -------------------------------------
# 
# served_between <- function(table, start, end){
#   served <- ymd(table$EntryDate) <= mdy(end) &
#     (is.na(table$ExitDate) | ymd(table$ExitDate) >= mdy(start))
#   served
# }
# 
# # should move to this but will require a LOT of edits!
# 
# # served_between <- function(., start, end) {
# #   . %>% filter(ymd(EntryDate) <= mdy(end) &
# #                  (is.na(ExitDate) | ymd(ExitDate) >= mdy(start)))
# # }
# 
# entered_between <- function(table, start, end){
#   entered <- between(ymd(table$EntryDate), mdy(start), mdy(end)) 
#   entered
# }
# 
# exited_between <- function(table, start, end){
#   exited <- between(ymd(table$ExitDate), mdy(start), mdy(end)) 
#   exited
# }
# 
# stayed_between <- function(table, start, end){
#   stayed <- ymd(table$EntryAdjust) <= mdy(end) &
#     (is.na(table$ExitDate) | ymd(table$ExitDate) > mdy(start))
#   stayed
# }
# 
# # Projects Operating Between Date Range Function --------------------------
# 
# operating_between <- function(table, start, end) {
#   operating <-  if_else(
#     is.na(table$OperatingStartDate) |
#       ymd(table$OperatingStartDate) > mdy(end) |
#       (!is.na(table$OperatingEndDate) &
#          ymd(table$OperatingEndDate) < mdy(start)),
#     FALSE,
#     TRUE
#   )
#   operating
# }
# 
# # Beds Available Between --------------------------------------------------
# 
# beds_available_between <- function(table, start, end) {
#   available <-  if_else(
#     is.na(table$InventoryStartDate) |
#       ymd(table$InventoryStartDate) > mdy(end) |
#       (!is.na(table$InventoryEndDate) &
#          ymd(table$InventoryEndDate) < mdy(start)),
#     FALSE,
#     TRUE
#   )
#   available
# }

#CHANGED New Between function
#' @title between_
#' @keywords Internal
#' @description Performs quick filtering of qpr_* data.frames with the input of the type of filtering
#' @param . \code{(data.frame/tibble)} Input to be filtered. In a `magrittr` pipe this will always be the first object
#' @param status \code{(unquoted name)} One of:
#' \itemize{
#'   \item{\code{`served/se`}}
#'   \item{\code{`stayed/st`}}
#'   \item{\code{`entered/en`}}
#'   \item{\code{`exited/ex`}}
#'   \item{\code{`operating/op`}}
#'   \item{\code{`beds_available/be/ba`}}
#' }
#' that specifies the type of function to be performed
#' @param start The ReportStart variable created from user input - will be automatically retrieved from parent environments if not specified. If start is named other than ReportStart, it must be specified.
#' @param end The ReportEnd variable created from user input - will be automatically retrieved from parent environments if not specified. If end is named other than ReportEnd, it must be specified.
#' @examples 
#' \dontrun{
#' ReportStart = Sys.Date() - lubridate::weeks(4)
#' ReportEnd = Sys.Date()
#' qpr_leavers %>% between_(served)
#' }
#TODO Test with additional qpr_*, test with operating_* and beds_available_* instances
between_ <- function(., status, start = ReportStart, end = ReportEnd) {
  # if no status supplied, throw error
  if (missing(status)) {
    rlang::abort("Please supply a status. See ?between_ for details.")
  } 
  # Add input dates to list
  .dates <- list(start = start, end = end)
  # Check if inputs are all Date or POSIXct
  .test_date <- purrr::map_lgl(.dates, ~{inherits(.x, c("Date", "POSIXct"))})
  # If not
  if (!all(.test_date)) {
    # map over the one's that arent
    list2env(purrr::imap(.dates[!.test_date], ~{
      # try these formats
      .out <- lubridate::parse_date_time(.x, c("Ymd", "Ymd", "mdY", "mdY"))
      if (!inherits(.out, c("POSIXct","Date")) {
        # if none of those formats worked throw error and inform user which argument was not able to be parsed
        rlang::abort(paste0(.y, " could not be parsed to a Datetime, please check argument."))
      }
      .out
    }), environment())
    # bind the coerced Date/Datetimes to the environment, overwriting the existing values
  }
  # Get the expression provided by the user contained in status
  .cn <- rlang::enexpr(status)
  # Convert that to a character for regex parsing
  .cn_chr <- tolower(substr(rlang::expr_deparse(.cn), 0, 2))
  # If it's one of served of stayed
  if (stringr::str_detect(.cn_chr, "se|st")) {
    if (stringr::str_detect(.cn_chr, "se")) {
      # if served use entrydate
      .col <- rlang::sym("EntryDate")
    } else if (stringr::str_detect(.cn_chr, "st")) {
      # if stayed used entryadjust
      .col <- rlang::sym("EntryAdjust")
    }
    #filter the appropriate columns
    .out <- dplyr::filter(., !!.col <= end & (is.na(ExitDate) | ExitDate >= start))
  } else if (stringr::str_detect(.cn_chr, "en|ex")) {
    # if its entered or exited
    if (stringr::str_detect(.cn_chr, "en")) {
      # if entered use entrydate
      .col <- rlang::sym("EntryDate")
    } else if (stringr::str_detect(.cn_chr, "ex")) {
      #if exited use exit date
      .col <- rlang::sym("ExitDate")
    }
    # Filter the appropriate column using between
    .out <- dplyr::filter(., dplyr::between(!!.col, start, end))
  } else if (stringr::str_detect(.cn_chr, "op|be|ba")) {
    if (stringr::str_detect(.cn_chr, "op")) {
      .prefix <- "Operating"
    } else if (stringr::str_detect(.cn_chr, "be|ba")) {
      .prefix <- "Inventory"
    }
    # Construct column names from prefixes
    .cols <- paste0(.prefix, c("StartDate", "EndDate"))
    .tbl <- .
    # Extract the appropriate columns
    .cols <- purrr::map(.cols, ~{.tbl[[.x]]})
    # Do the filtering
    .out <- dplyr::if_else(is.na(.cols[[1]]) | .cols[[1]] > end | (!is.na(.cols[[2]]) & .cols[[2]] < start), 
                           FALSE,
                           TRUE)
  }
  .out
}


# Client Entry Exits Between Date Range Functions -------------------------------------

served_between <- function(., start = ReportStart, end = ReportEnd) {
  between_(., served, start, end)
}

entered_between <- function(., start = ReportStart, end = ReportEnd) {
  between_(., entered, start, end)
}

exited_between <- function(., start = ReportStart, end = ReportEnd){
  between_(., exited, start, end)
}

stayed_between <- function(., start = ReportStart, end = ReportEnd){
  between_(., stayed, start, end)
}

# Projects Operating Between Date Range Function --------------------------

operating_between <- function(., start = ReportStart, end = ReportEnd){
  between_(., operating, start, end)
}

beds_available_between <- function(., start = ReportStart, end = ReportEnd){
  between_(., ba, start, end)
}

living_situation <- function(ReferenceNo) {
  case_when(
    ReferenceNo == 1 ~ "Emergency shelter/ h/motel paid for by a third party/Host Home shelter",
    ReferenceNo == 2 ~ "Transitional housing",
    ReferenceNo == 3 ~ "Permanent housing (other than RRH) for formerly homeless persons",
    ReferenceNo == 4 ~ "Psychiatric hospital/ other psychiatric facility",
    ReferenceNo == 5 ~ "Substance abuse treatment facility or detox center",
    ReferenceNo == 6 ~ "Hospital or other residential non-psychiatric medical facility",
    ReferenceNo == 7 ~ "Jail/prison/juvenile detention",
    ReferenceNo == 8 ~ "Client doesn't know",
    ReferenceNo == 9 ~ "Client refused",
    ReferenceNo == 32 ~ "Host Home (non-crisis)",
    ReferenceNo == 13 ~ "Staying or living with friends, temporary tenure",
    ReferenceNo == 36 ~ "Staying or living in a friend's room, apartment or house",
    ReferenceNo == 18 ~ "Safe Haven",
    ReferenceNo == 15 ~ "Foster care home of foster care group home",
    ReferenceNo == 12 ~ "Staying or living with family, temporary tenure",
    ReferenceNo == 25 ~ "Long-term care facility or nursing home",
    ReferenceNo == 22 ~ "Staying or living with family, permanent tenure",
    ReferenceNo == 35 ~ "Staying or living in a family member's room, apartment, or house",
    ReferenceNo == 16 ~ "Place not meant for habitation",
    ReferenceNo == 23 ~ "Staying or living with friends, permanent tenure",
    ReferenceNo == 29 ~ "Residential project or halfway house with no homeless criteria",
    ReferenceNo == 14 ~ "H/Motel paid for by household",
    ReferenceNo == 26 ~ "Moved from one HOPWA funded project to HOPWA PH",
    ReferenceNo == 27 ~ "Moved from HOPWA funded project to HOPWA TH",
    ReferenceNo == 28 ~ "Rental by client, with GPD TIP housing subsidy",
    ReferenceNo == 19 ~ "Rental by client, with VASH housing subsidy",
    ReferenceNo == 31 ~ "Rental by client, with RRH or equivalent subsidy",
    ReferenceNo == 33 ~ "Rental by client, with HCV voucher",
    ReferenceNo == 34 ~ "Rental by client in a public housing unit",
    ReferenceNo == 10 ~ "Rental by client, no ongoing housing subsidy",
    ReferenceNo == 20 ~ "Rental by client, with other ongoing housing subsidy",
    ReferenceNo == 21 ~ "Owned by client, with ongoing housing subsidy",
    ReferenceNo == 11 ~ "Owned by client, no ongoing housing subsidy",
    ReferenceNo == 30 ~ "No exit interview completed",
    ReferenceNo == 17 ~ "Other",
    ReferenceNo == 24 ~ "Deceased",
    ReferenceNo == 37 ~ "Worker unable to determine",
    ReferenceNo == 99 ~ "Data not collected"
  )
}

project_type <- function(ReferenceNo){
  case_when(
    ReferenceNo == 1 ~ "Emergency Shelter",
    ReferenceNo == 2 ~ "Transitional Housing",
    ReferenceNo == 3 ~ "Permanent Supportive Housing",
    ReferenceNo == 4 ~ "Street Outreach",
    ReferenceNo == 6 ~ "Services Only",
    ReferenceNo == 8 ~ "Safe Haven",
    ReferenceNo == 12 ~ "Prevention",
    ReferenceNo == 13 ~ "Rapid Rehousing",
    ReferenceNo == 14 ~ "Coordinated Entry"
  )
}

replace_yes_no <- function(column_name) {
  if_else(column_name == "No" | is.na(column_name), 0, 1)
}

# Experimental ------------------------------------------------------------

# HUD_value_to_description <-
#   function(table, element_name, element_column) {
#     element_name <- sym(element_name)
#     element_column <- enquo(element_column)
#     
#     a <- HUD_specs %>%
#       filter(DataElement == element_name) %>%
#       select("ReferenceNo", "Description")
#     
#     table$element_column <- with(a,
#                                  Description[match(table$element_column,
#                                                    HUD_specs$ReferenceNo)])
#   }
# 
# a <- subset(HUD_specs,
#             DataElement == "HouseholdType",
#             select = c("ReferenceNo", "Description"))
# Inventory$HouseholdType <- with(a,
#                                 Description[match(Inventory$HouseholdType,
#                                                   ReferenceNo)])


 
# HMIS_participating_between <- function(table, start, end) {
#   HMISParticipating <-  if_else(
#     (table$HMISParticipatingBeds == 0 | is.na(table$HMISParticipatingBeds)) |
#     (is.na(table$InventoryStartDate) |
#       ymd(table$InventoryStartDate) > mdy(end)) |
#       (!is.na(table$InventoryEndDate) &
#          ymd(table$InventoryEndDate) < mdy(start)),
#     FALSE,
#     TRUE
#   )
#   HMISParticipating
# }
# not sure what the heck to do about this. :( will have to pull based
# on UsesSP which is super clunky and will leave out providers
