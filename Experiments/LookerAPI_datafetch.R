library(dplyr)
`%||%` <- rlang::`%||%`
# Auth
.path <- path.expand("~/R/Contributor_Repos/COHHIO/COHHIO_HMIS")
setwd(.path)
sdk <- lookr::LookerSDK$new(configFile = file.path(.path, "Looker.ini"))
#' @title Quickly retrieve Look IDs from HTML
#' @description Get Looks IDs from the Folder browse table outerHTML in Looker
#' @param x \code{(character)} path to html file
#' @return \code{(code)} to generate the vector of Look IDs
look_ids_from_html <- function(x) {
  x %>%
    xml2::read_html() %>% 
    rvest::html_nodes(xpath = "//a[@ng-class = '{disabled: item.deletedAt}']") %>%
    rvest::html_attr("href") %>%
    stringr::str_extract("\\d{5}$") %>%
    as.numeric() %>%
    setNames(
      nm = htm %>%
        rvest::html_nodes(xpath = "//div[@ng-bind = 'item.title']") %>%
        rvest::html_text() %>%
        stringr::str_remove("HUD_Export\\.")
    ) %>% dput
}


# https://looker.clarityhs.com:9999/explore/cohhio_mig_connection_model/export
# Look mappings
# Note:
# ExportID omitted in all exports
# Leave Row Limit blank to retrieve all rows via the API

hud_export <- list(
  Affiliation = list(look = c(
    year2 = 65501,
    s2020 = 65869,
    daily = 65914
  )),
  # Assessments not in use
  Client = list(look = c(
    year2 = 65504,
    s2020 = 65870,
    daily = 65915
  )),
  CurrentLivingSituation = list(
    look = c(
      year2 = 65506,
      s2020 = 65871,
      daily = 65918
    ),
    api_nm = "Current Living Situation"
  ),
  Disabilities = list(look = c(
    year2 = 65508,
    s2020 = 65872,
    daily = 65919
  )),
  EmploymentEducation = list(
    look = c(
      year2 = 65509,
      s2020 = 65873,
      daily = 65920
    ),
    api_nm = "Employment Education"
  ),
  Enrollment = list(look = c(
    year2 = 65514,
    s2020 = 65875,
    daily = 65921
  )),
  EnrollmentCoC = list(
    look = c(
      year2 = 65515,
      s2020 = 65876,
      daily = 65922
    ),
    api_nm = "Enrollment CoC"
  ),
  Event = list(look = c(
    year2 = 65516,
    s2020 = 65877,
    daily = 65923
  )),
  Exit = list(look = c(
    year2 = 65512,
    s2020 = 65874,
    daily = 65924
  )),
  Export = list(look = c(
    year2 = 65961,
    s2020 = 65962,
    daily = 65963
  )),
  Funder = list(look = c(
    year2 = 65949,
    s2020 = 65948,
    daily = 65925
  )),
  HealthAndDV = list(
    look = c(
      year2 = 65517,
      s2020 = 65878,
      daily = 65926
    ),
    api_nm = "Health DV"
  ),
  IncomeBenefits = list(
    look = c(
      year2 = 65518,
      s2020 = 65879,
      daily = 65927
    ),
    api_nm = "Income Benefits"
  ),
  Inventory = list(look = c(
    year2 = 65519,
    s2020 = 65880,
    daily = 65928
  )),
  Organization = list(look = c(
    year2 = 65525,
    s2020 = 65881,
    daily = 65930
  )),
  Project = list(look = c(
    year2 = 65526,
    s2020 = 65882,
    daily = 65931
  )),
  ProjectCoC = list(look = c(
    year2 = 65527,
    s2020 = 65883,
    daily = 65932,
    api_nm = "Project CoC"
  )),
  Services = list(look = c(
    year2 = 65528,
    s2020 = 65884,
    daily = 65933
  )),
  User = list(look = c(
    year2 = 65529,
    s2020 = 65885,
    daily = 65934
  ))
)
 
# purrr::map2(hud_export, daily_looks, ~{
#   .x$look <- c(.x$look, daily = .y)
#   .x
# }) %>% dput


# Load migration data
# hud_export <- purrr::imap(hud_export, ~{
#   .x$mig <- try(readr::read_csv(file.path("data/API",paste0(.y, ".csv"))))
#   .x
# })




col_types <- function(col, type = c("fun", "chr")[1]) {
  fm <- list("integer" %in% . ~ readr::parse_integer,
  "numeric" %in% . ~ readr::parse_number,
  "logical" %in% . ~ readr::parse_logical,
  "factor" %in% . ~ readr::parse_factor,
  "Date" %in% . ~ readr::parse_date,
  "POSIXct" %in% . ~ readr::parse_datetime,
  "character" %in% . ~ readr::parse_character)
  if (type == "chr")
    fm <- purrr::map(fm, ~{
      .x[[3]] <- switch(stringr::str_extract(deparse(.x[[3]]), "(?<=\\_)[a-z]+$"), 
                        integer = "i",
                        number = "n",
                        logical = "l",
                        factor = "f",
                        date = "D",
                        datetime = "T",
                        character = "c")
      .x
    })
  rlang::exec(purrr::when, class(col), !!!fm)
}

hud_rename <- function(data, .nm) {
  if (is.null(data)) return(NULL)
  data %>% 
    dplyr::rename_with(.fn = ~{
      # All column names are prefixed with the HUD CSV Export BETA report name from Looker - with spaces between capitalized words. This is removed
      out <- trimws(stringr::str_remove(.x, stringr::fixed(paste0(.nm, " ")))) %>% 
        stringr::str_replace_all("(?<!a)[Ii][Dd]$", "ID") %>% 
        stringr::str_remove("^Enrollment ") %>% 
        stringr::str_replace_all("[Cc][Oo][Cc]", "CoC") %>% 
        stringr::str_replace_all("^[Zz][Ii][Pp]$", "ZIP") %>% 
        stringr::str_replace_all("(?<=rk)p(?=lace)", "P")
      
      if (all(is.na(out)))
        out <- .x
      out
    }) 
}


#' @title Retrieve data from disk or the API
#' @description Determines the appropriate location from which to retrieve HUD Export data
#' @param .x \code{(list/character)} If a list item (such as when using `map` on `hud_export`) the data will be retrieved from the following source in order of precedence: 
#' \itemize{
#'   \item{the `hud_export` item in the calling environment}
#'   \item{the csv file in data/API}
#'   \item{the API itself}
#' }
#' Unless `look_type == "daily"`, in which case daily looks will be retrieved from the API as is.
#' Must be a `list` with a `look` vector of LookIDs to retrieve data from the API.
#' @param .y \code{(character)} The object name (for `purrr:imap_*`)
#' @param look_type \code{(character)} The look type to retrieve, see `hud_export` for details
#' @param sdk \code{(LookerSDK)} Authed looker sdk. *Require to retrieve data from API*. If omitted, it is searched for in the calling environment.
#' @inheritParams readr::read_csv 
#' @param sp \code{(logical)} Whether to load the ServicePoint data as well
#' @param rename \code{(logical)} Whether to rename column names using `hud_rename`
#' @param write \coe{(logical)} Whether to write the raw data from the API and the renamed data to the data/API folder
#' @return \code{(list)} of HUD Export items requested. Automatically saves retrieved data to the `hud_export` item in the calling environment if present to avoid data loss on error.

fetch <- function(.x,
                  .y,
                  look_type,
                  sdk,
                  col_types,
                  sp = FALSE,
                  rename = FALSE,
                  write = FALSE
) {
  tictoc::tic()
  
  if (missing(look_type))
    look_type <- get0("look_type", rlang::caller_env())
  if (missing(sdk))
    sdk <- get0("sdk", rlang::caller_env())
  
  if (is.character(.x)) {
    .y <- .x
    sp <- write <- rename <- FALSE
  } else {
    .nm <- .x$api_nm %||% .y
  }
  ce <- rlang::caller_env()
  hud_export <- get0("hud_export", ce)
  
  if (sp) {
    message(.y, ": load_SP_data")
    csv_data <- hud_export[[.y]]$sp %||% readr::read_csv(file.path("data", paste0(.y, ".csv"))) %||% warning(.y , " not found")
    ce$hud_export[[.y]]$sp <<- .x$sp <- csv_data
  }
  
  if (missing(col_specs))
    col_specs <- get0("col_specs", rlang::caller_env())[[.y]] %||% source(file.path("Experiments", "col_specs.R"))$value[[.y]] %||% attr(get0("csv_data", inherits = FALSE), "spec")
  
  if (look_type != "daily")
    data <- hud_export[[.y]][[look_type]] %||% try(readr::read_csv(file.path("data", "API", paste0(.y, ".csv")), col_types = col_types))
  
  if ((inherits(data, "try-error") && !is.character(.x)) || look_type == "daily") {
    message(.y, ": fetch_data")
    if (is.null(.x$look[look_type])) return(NULL)
    data <- sdk$runLook(.x$look[look_type], "csv", as = "parsed", col_types = col_types)
    if (write) {
      readr::write_csv(data, file.path("data", "API", paste0(.y, "_raw.csv")))
    }
    if (nrow(data) %in% c(0, 500)) stop(.y, " row count is", nrow(data))
  }
  
  if (rename) {
    message(.y, ": rename")
    data <- hud_rename(data, .y)
  }
  
  if (write) {
    message(.y, ": write")
    readr::write_csv(data, file.path("data", "API", paste0(.y, ".csv")))
  }
  

  if (!is.character(.x) && look_type != "daily") {
    ce$hud_export[[.y]][[look_type]] <<- .x[[look_type]] <- data
  } else {
    return(data)
  }
  
  message(.y, " complete")
  tictoc::toc()
  .x
}

source(file.path("Experiments", paste0("col_specs.R")))
look_type = "year2"
hud_export <- purrr::imap(hud_export, fetch, sdk = sdk, look_type = look_type, col_specs = col_specs, sp = TRUE, rename = FALSE, write = FALSE)

cols_reconcile <- purrr::map(hud_export, ~{
  if (is.null(.x[[look_type]])) return(NULL)
  list(
    sp.mig = setdiff(names(.x$sp), names(.x[[look_type]])),
    mig.sp = setdiff(names(.x[[look_type]]), names(.x$sp)),
    col_types = purrr::map_chr(.x[[look_type]], col_types, type = "chr")
  )
})

# hud_export <- purrr::imap(hud_export, ~{
#   .x$mig <- hud_rename(.x$mig, .x$api_nm %||% .y)
#   .x
# })
# saveRDS(purrr::map(hud_export, ~{
#   .x$real
# }), file.path(.path, "data/hud_export_looker.rds"), compress = "xz")
# saveRDS(purrr::map(hud_export, ~{
#   .x$real
# }), file.path(.path, "data/hud_export_dropbox.rds"), compress = "xz")
# rdrop2::drop_auth(rdstoken = "dropbox_auth_token.rds")
# rdrop2::drop_upload(file.path(.path, "data/hud_export_dropbox.rds"), path = "shiny")

#RPushbullet::pbPost(body = "Looker API Datafetch complete")


