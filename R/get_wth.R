
#' Get weather data from NASA POWER
#'
#' @param lonlat A numeric vector of geographic coordinates for a cell or region
#'  entered as x, y coordinates.
#' @param dates A character vector of start and end dates in that order.
#'
#' @return A \code{\link[data.table]{data.table}} of weather data suitable for
#'  use in \pkg{epirice}.
#'
#' @details This function is just a wrapper for the \CRANpkg{nasapower}
#'  \code{\link[nasapower]{get_power}} function with predefined parameters
#'  suitable for use in EPIRICE.
#'
#' @examples
#' \donttest{
#' wth <- get_wth(lonlat = c(151.81, -27.48),
#'                dates = c("2015-01-15", "2015-05-15"))
#' }
#' @author Adam H. Sparks,
#' @export get_wth

get_wth <- function(lonlat, dates) {
  wth <- setDT(
    nasapower::get_power(
      lonlat = lonlat,
      dates = dates,
      community = "AG",
      pars = c("T2M",
               "T2M_MAX",
               "T2M_MIN",
               "T2MDEW",
               "RH2M",
               "PRECTOT"),
      temporal_average = "DAILY"
    )
  )

  wth[, c("YEAR", "MM", "DD") := NULL][]
  setnames(
    wth,
    old = c(
      "LON",
      "LAT",
      "DOY",
      "YYYYMMDD",
      "T2M",
      "T2M_MAX",
      "T2M_MIN",
      "T2MDEW",
      "RH2M",
      "PRECTOT"
    ),
    new = c(
      "LON",
      "LAT",
      "YYYYMMDD",
      "DOY",
      "TM",
      "TN",
      "TX",
      "TDEW",
      "RH",
      "RAIN"
    )
  )
  setcolorder(wth,
              c("YYYYMMDD", "DOY", "TM", "TN", "TX", "TDEW", "RH", "RAIN"))

  return(wth)
}
