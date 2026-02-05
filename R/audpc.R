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

#' Get the AUDPC from an seir Object
#'
#' Get the \acronym{AUDPC} value from an \pkg{epicrop} model run object's
#'  attributes for use in analysis, mapping, or reporting.
#'
#' @param x an object of class `epicrop.sim` as returned by any of
#'  the modelling functions in \pkg{epicrop}.
#'
#' @examples
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#' bb <- bacterial_blight(wth, emergence = "2000-07-01")
#' get_audpc(bb)
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @returns A scalar numeric double precision value of the AUCPC for the
#'   given object.
#' @export
get_audpc <- function(x) attr(x, "AUDPC")

#' Create an AUDPC data frame for locations and emergence dates
#' @param x A `data.table` created by `run_epicrop_model()`, `seir()` or
#'   any one of the disease specific helper functions.
#' @returns A [data.table::data.table()] suitable for mapping or other analysis.
#'  If `dt` is from `run_epicrop_model()` directly, the output will be a
#'  `data.table` with five columns: `lat`, `lon`, `AUDPC`, `location`, and
#'  `emergence`. If `dt` is from any of the helper functions or directly from
#'   `seir()`, the output will be a `data.table` with two (or) four
#'   columns: `lat`*, `lon`*, `AUDPC` and `emergence`. *`lat` and `lon` columns
#'   must be in the original weather dataset for them to appear.
#' @autoglobal
#' @export

audpc_dt <- function(x) {
  if (!rlang::inherits_any(x = x, class = "epicrop.sim")) {
    cli::cli_abort(
      "{.var x} must be an `epicrop.sim` object created by
      {.fun run_epicrop_model}, {.fun seir} or any of the other
      helper functions that create an `epicrop.sim` object."
    )
  }

  if (all(names(x) %in% c("lat", "lon", "AUDPC", "location", "emergence"))) {
    x <- x[, .SD, .SDcols = c("lat", "lon", "AUDPC", "location", "emergence")]
    data.table::setkey(x, location, emergence)
  } else {
    audpc <- get_audpc(x)
    if ("lat" %in% names(x) && "lon" %in% names(x)) {
      x <- x[1, .SD, .SDcols = c("lat", "lon", "dates")]
    } else {
      x <- x[1, "dates"]
    }
    x[, AUDPC := audpc]
    data.table::setnames(x, "dates", "emergence")
    data.table::setcolorder(x, "AUDPC", before = "emergence")
    x[, emergence := as.factor(emergence)]
    data.table::setkey(x, emergence)
  }
  return(unique(x[]))
}
