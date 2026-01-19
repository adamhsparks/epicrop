#' Calculate the Area Under the Disease Progress Curve (AUDPC)
#'
#' This function is used to return the AUDPC in the output of [epicrop_sim()].
#' Not to be used alone as it assumes that there is always only one day between
#' observations so it takes shortcuts in the calculation of the values.
#'
#' @param intensity A `vector` of disease intensity from [epicrop_sim()].
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

#' Get the AUDPC from an epicrop_sim Object
#'
#' Get the \acronym{AUDPC} value from an \pkg{epicrop} model run object's
#'  attributes for use in analysis, mapping, or reporting.
#'
#' @param epicrop_obj an object of class `epicrop.sim` as returned by any of
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
#' @returns A scalar numeric double precision value of the AUCPC for the
#'   given object.
#' @export
get_audpc <- function(result) attr(result, "AUDPC")
