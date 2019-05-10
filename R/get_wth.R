
#' Get Weather Data from NASA POWER
#'
#' @param lonlat A numeric vector of geographic coordinates for a cell or region
#'  entered as x, y coordinates.
#' @param dates A character vector of start and end dates in that order.
#'
#' @return A `data.frame` of weather data suitable for use in \pkg{epirice}.
#'
#' @examples
#' \donttest{
#' wth <- get_wth(lonlat = c(151.81, -27.48),
#'                dates = c("2015-01-15", "2015-05-15"))
#' }
#' @author Adam H. Sparks,
#' @export get_wth

get_wth <- function(lonlat, dates) {
  wth <- as.data.frame(
    nasapower::get_power(
      lonlat = lonlat,
      dates = dates,
      community = "AG",
      pars = c("T2M",
               "T2M_MAX",
               "T2M_MIN",
               "RH2M",
               "PRECTOT"),
      temporal_average = "DAILY")
  )

  wth <- wth[,c(7, 6, 8, 10, 9, 11, 12)]
  names(wth) <- c("YYYYMMDD", "DOY", "T2M", "T2MN", "T2MX", "RH2M", "RAIN")

  return(wth)
}
