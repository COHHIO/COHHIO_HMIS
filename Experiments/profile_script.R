#' @title profile_script
#' @description This function will add profiling code to a script wherever the following flags are found in the first non-spacing characters on the line: 
#' \itemize{
#'   \item{\code{#<p}}{ Opening comment flag where `profile_open` will be inserted.}
#'   \item{\code{#>p}}{ Closing comment flag where `profile_close` will be inserted.}
#' }
#' @param .file path to script
#' @param profile_open \code{(expression)} The code that will be added preceding the opening comment flag. Consider using `glue` such that internal variables can be used for file naming conventions. In addition the function arguments, the following variable names can be used:
#' \itemize{
#'   \item{\code{.lo}}{ Short for line open, the line number of the opening profile code.}
#'   \item{\code{.lc}}{ Short for line close, the line number of the closing profile code.}
#' }
#' **Default: `utils::Rprof(fs::path(dir_profvis, basename(.x), glue::glue('{.lo}-{.lc}'), ext = "Rprof"), interval = .01, line.profiling = TRUE, gc.profiling = TRUE, memory.profiling = TRUE)`**. If `profile_script` were called on a script called `script.R` with opening flag at line 20 and closing flag at line 30, then line 20 would be changed as follows: `utils::Rprof("profvis/script.R/20-30.Rprof", interval = 0.01, line.profiling = TRUE, gc.profiling = TRUE, memory.profiling = TRUE) #<p`.
#' @param profile_close \code{(expression)} The code that will be added preceding the closing comment flag.  **Default: `utils::Rprof(NULL)`**
#' @param dir_profvis \code{(character)} The directory in which all `profvis` related files will be put. **Default: `"profvis"`**. Profile-ready script copies (if `new_scripts = TRUE`) and Rprof files/directories will be placed here. Set to `NULL` to use the working directory and overwrite existing files.
#' @param remove \code{(logical/character)} to indicate whether to remove profiling code. **Default: FALSE**. Change to `TRUE` to remove profile code but leave flags. Change to `"f"/"flags"` to remove code and flags. Remove modifies `.file` in place.
#' @param new_script \code{(logical)} to indicate whether to write new profile-ready script copies to `dir_profvis`. **Default: TRUE**, if `FALSE` the character vector output  of the profile-ready script lines from the function can be written to a file manually.




profile_script <- function(.file, profile_open = utils::Rprof(fs::path(dir_profvis, stringr::str_remove(basename(.file), "\\.[a-zA-Z0-9\\-]+$"), glue::glue('{.lo}-{.lc}'), ext = "Rprof"), interval = .01, line.profiling = TRUE, gc.profiling = TRUE, memory.profiling = TRUE), profile_close = utils::Rprof(NULL), dir_profvis = "profvis", remove = FALSE, new_script = TRUE) {
  stopifnot(inherits(.file, "character"))
  .lines <- readr::read_lines(.file)
  .po <- rlang::enexpr(profile_open)
  .pc <- rlang::enexpr(profile_close)
  # create dir if it doesn't exist
  if (!dir.exists(dir_profvis)) dir.create(dir_profvis)
  # data.frame of open/close flags (accounting for preceding tabs or spaces)
  if (isTRUE(remove) || inherits(remove, "character")) {
    .flags <- data.frame(
      .lo = stringr::str_which(.lines, "\\#\\<p[\\t\\s]*")
      , .lc = stringr::str_which(.lines, "\\#\\>p[\\t\\s]*$")
    )
  } else {
    .flags <- data.frame(
      .lo = stringr::str_which(.lines, "^[\\t\\s]*\\#\\<p[\\t\\s]*$")
      , .lc = stringr::str_which(.lines, "^[\\t\\s]*\\#\\>p[\\t\\s]*$")
    )
  }
  
  # if there aren't any flags in the script, warn
  if (nrow(.flags) == 0) {
    .write <- FALSE
    rlang::warn(glue::glue("No flags found in file, no changes will be made."))
  } else {
    .write <- TRUE
  }
  
  if (isTRUE(remove)) {
    slider::slide(.flags, ~{
      .lo <- .x[[1]]
      .lc <- .x[[2]]
      .lines[.lo] <<- "#<p"
      .lines[.lc] <<- "#>p"
    })
  } else if (inherits(remove, "character")) {
    .lines[c(.flags$.lo, .flags$.lc)] <- ""
  } else {
    slider::slide(.flags, ~{
      .lo <- .x[[1]]
      .lc <- .x[[2]]
      #browser()
      # evaluate the expression to create a filename for the Rprof file and coerce to character
      .po[[2]] <- as.character(eval(.po[[2]]))
      .dn <- dirname(.po[[2]])
      if (!dir.exists(.dn)) fs::dir_create(.dn, recurse = TRUE)
      #write the code to the appropriate lines
      .lines[.lo] <<- paste0(stringr::str_flatten(rlang::expr_deparse(.po)), " ", .lines[.lo])
      .lines[.lc] <<- paste0(stringr::str_flatten(rlang::expr_deparse(.pc)), " ", .lines[.lc])
    })
  }
  
  # write the new file
  if (new_script && .write && (isFALSE(remove) || !is.character(remove))) {
    readr::write_lines(.lines, fs::path(dir_profvis, basename(.file)), append = FALSE)
  } else if (isTRUE(remove) || is.character(remove)) {
    readr::write_lines(.lines, .file, append = FALSE)
  }
  return(.lines)
}

