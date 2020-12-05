
#' Get weather data from NASA POWER API
#'
#' @param lonlat A numeric vector of geographic coordinates for a cell or region
#'  entered as x, y coordinates.
#' @param dates A character vector of start and end dates in that order.
#'
#' @return A \code{\link[data.table]{data.table}} of weather data, dates and
#'  geolocation information (LAT/LON values) suitable for use in \pkg{epicrop}
#'  with the following columns:
#'   \tabular{rl}{
#'   **YYYYMMDD**:\tab Date as Year Month Day (ISO8601).\cr
#'   **DOY**:\tab  Consecutive day of year, commonly called "Julian date".\cr
#'   **TEMP**:\tab Mean daily temperature (°C).\cr
#'   **TMIN**:\tab Minimum daily temperature (°C).\cr
#'   **TMAX**:\tab Maximum daily temperature (°C).\cr
#'   **TDEW**:\tab Mean daily dew point temperature (°C).\cr
#'   **RHUM**:\tab Mean daily temperature (°C).\cr
#'   **RAIN**:\tab Mean daily rainfall (mm).\cr
#'   **LAT**:\tab Latitude of area of interest.\cr
#'   **LON**:\tab Longitude of area of interest.\cr
#'   }
#' @details This function is just a wrapper for the \CRANpkg{nasapower}
#'  \code{\link[nasapower]{get_power}} function with predefined parameters
#'  suitable for use in \pkg{epicrop}.
#'
#' @examples
#' \donttest{
#' # get weather for IRRI Zeigler Experiment Station in wet season 2000
#' (wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' ))
#' }
#' @author Adam H. Sparks
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
      "DOY",
      "YYYYMMDD",
      "T2M",
      "T2M_MAX",
      "T2M_MIN",
      "T2MDEW",
      "RH2M",
      "PRECTOT",
      "LAT",
      "LON"
    ),
    new = c(
      "DOY",
      "YYYYMMDD",
      "TEMP",
      "TMIN",
      "TMAX",
      "TDEW",
      "RHUM",
      "RAIN",
      "LAT",
      "LON"
    )
  )
  setcolorder(wth,
              c(
                "YYYYMMDD",
                "DOY",
                "TEMP",
                "TMIN",
                "TMAX",
                "TDEW",
                "RHUM",
                "RAIN",
                "LAT",
                "LON"
              ))

  return(wth)
}
