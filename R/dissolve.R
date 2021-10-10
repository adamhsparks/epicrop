#' Generate summary statistics for epicrop output
#'
#' "Dissolves" the voluminous output from [SEIR()] into easy to interpret
#' outputs that are useful values for communicating the output of \pkg{epicrop}.
#'
#' @param x A [data.table] object that is the output of [SEIR()] or any of the
#' `predict` family of functions.  The values provided by this function are the
#' values used by Savary _et al._ 2012 to report the model outputs in a
#' \acronym{GIS} framework.
#'
#' @return A [data.table::data.table()] containing the following columns:
#'
#' \describe{
#'   \item{audpc}{Area under the disease progress curve (\acronym{AUDPC}).}
#'   \item{sd}{Standard deviation of the disease intensity.}
#'   \item{lat}{Latitude value if provided by the `x` object.}
#'   \item{lon}{Longitude value if provided by the `x` object.}
#' }
#'
#' @examplesIf interactive()
#' # get weather for IRRI Zeigler Experiment Station in wet season 2000
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#' bb <- predict_bacterial_blight(wth, emergence = "2000-07-01")
#' dissolve(x = bb)
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#'
#' @references
#' Madden, L. V., G. Hughes, and F. van den Bosch. 2007. The Study of Plant
#' Disease Epidemics. American Phytopathological Society, St. Paul, MN.
#' DOI:[10.1094/9780890545058](https://doi.org/10.1094/9780890545058).
#'
#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario,
#' J. Modeling and mapping potential epidemics of rice diseases globally. _Crop
#' Protection_, Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
#' [10.1016/j.cropro.2011.11.009](https://dx.doi.org/10.1016/j.cropro.2011.11.009).
#'
#' Sparks, A.H., P.D. Esker, M. Bates, W. Dall' Acqua, Z. Guo, V. Segovia, S.D.
#' Silwal, S. Tolos, and K.A. Garrett, 2008. Ecology and Epidemiology in R:
#' Disease Progress over Time. *The Plant Health Instructor*.
#' DOI:[10.1094/PHI-A-2008-0129-02](https://doi.org/10.1094/PHI-A-2008-0129-02).
#'
#' @export

dissolve <- function(x) {
  lon <- lat <- NULL

  audpc <- .calculate_audpc(x)
  sd <- sd(x$intensity)

  out <-
    setDT(list("audpc" = audpc,
               "sd" = sd))

  # Only add lat and lon values if they exist in `x`
  if (all(c("lat", "lon") %in% names(x)))
  {
    out[, lat := x$lat[1]]
    out[, lon := x$lon[1]]
  }

  return(out[])
}
