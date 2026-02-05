#' Calculate the Area Under the Disease Progress Curve (AUDPC)
#'
#' This function is used to return the AUDPC in the output of [seir()].
#' Not to be used alone as it assumes that there is always only one day between
#' observations so it takes shortcuts in the calculation of the values.
#'
#' @param intensity A `vector` of disease intensity from [seir()].
#'
#' @returns  A `numeric` value as `double`.
#'
#' @examplesIf interactive()
#' # get weather for IRRI Zeigler Experiment Station in wet season 2000
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#' bb <- bacterial_blight(wth, emergence = "2000-07-01")
#' .calculate_audpc(intensity = bb$intensity)
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#'
#' @references
#' Shaner G., and R.E. Finney, 1977. The effect of nitrogen fertilization on the
#' expression of slow-mildewing resistance in Knox wheat. **Phytopathology**
#' 67: 1051–1056.
#'
#' Sparks, A.H., P.D. Esker, M. Bates, W. Dall' Acqua, Z. Guo, V. Segovia, S.D.
#' Silwal, S. Tolos, and K.A. Garrett, 2008. Ecology and Epidemiology in R:
#' Disease Progress over Time. **The Plant Health Instructor**.
#' \doi{10.1094/PHI-A-2008-0129-02}.
#'
#' Madden, L. V., G. Hughes, and F. van den Bosch. 2007. **The Study of Plant
#' Disease Epidemics**. American Phytopathological Society, St. Paul, MN.
#' \doi{doi.org/10.1094/9780890545058}.
#' @dev

.calculate_audpc <- function(intensity) {
  n <- length(intensity)

  if (n < 2L) {
    return(0L)
  }

  # Trapezoidal rule:  AUDPC = sum of (y[i] + y[i+1]) / 2 for all intervals
  sum((intensity[-1L] + intensity[-n]) / 2)
}

#' Get AUDPC from epicrop.sim Object
#'
#' Get the \acronym{AUDPC} value from an \pkg{epicrop} model run object's
#'  attributes for use in analysis, mapping, or reporting.
#' @param x An `epicrop.sim` object
#' @returns Numeric AUDPC value
#' @autoglobal
#' @export
get_audpc <- function(x) {
  if (!inherits(x, "epicrop.sim")) {
    cli::cli_abort(
      c(
        "{.arg x} must be an {.cls epicrop.sim} object",
        "i" = "Created by {.fun seir} or disease-specific helper functions"
      )
    )
  }

  audpc <- attr(x, "AUDPC")

  if (is.null(audpc)) {
    cli::cli_abort(
      c(
        "AUDPC attribute not found in {.arg x}",
        "i" = "This should be automatically calculated by {.fun seir}"
      )
    )
  }

  audpc
}

#' Extract AUDPC Summary as data.table
#'
#' Extracts Area Under Disease Progress Curve (AUDPC) values along with
#' location and emergence date information from an epicrop.sim object.
#'
#' @param x An `epicrop.sim` object from [seir()] or disease model functions.
#'
#' @returns A [data.table::data.table()] with columns:
#'   * `lat`: Latitude (if available)
#'   * `lon`: Longitude (if available)
#'   * `AUDPC`: Area under disease progress curve
#'   * `emergence`: Emergence date (as character)
#'
#' @autoglobal
#' @export
audpc_dt <- function(x) {
  # Validate input class
  if (!inherits(x, "epicrop.sim")) {
    cli::cli_abort(
      c(
        "{.arg x} must be an {.cls epicrop.sim} object",
        "i" = "Created by {.fun seir} or disease-specific helper functions"
      )
    )
  }

  # Get AUDPC from attribute
  audpc_val <- attr(x, "AUDPC")
  if (is.null(audpc_val)) {
    cli::cli_abort(
      c(
        "{.arg x} must contain AUDPC value as attribute",
        "i" = "This should be automatically added by {.fun seir}"
      )
    )
  }

  # Get emergence date
  if ("dates" %in% names(x)) {
    emergence_val <- as.character(x$dates[1])
  } else if ("emergence" %in% names(x)) {
    emergence_val <- as.character(x$emergence[1])
  } else {
    cli::cli_abort(
      c(
        "{.arg x} must contain {.field dates} or {.field emergence} column",
        "i" = "This should be automatically added by {.fun seir}"
      )
    )
  }

  # Build column list - only add columns that exist
  cols <- list()

  if ("lat" %in% names(x)) {
    cols$lat <- x$lat[1]
  }

  if ("lon" %in% names(x)) {
    cols$lon <- x$lon[1]
  }

  cols$AUDPC <- audpc_val
  cols$emergence <- emergence_val

  # Create data.table from list
  result <- data.table::as.data.table(cols)

  # Set column order: spatial cols, AUDPC, emergence
  desired_order <- c("lat", "lon", "AUDPC", "emergence")
  actual_order <- intersect(desired_order, names(result))
  data.table::setcolorder(result, actual_order)

  # Set key for sorting/indexing
  data.table::setkey(result, emergence)

  result[]
}
