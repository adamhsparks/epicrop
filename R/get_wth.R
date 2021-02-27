

#' Get weather data from NASA POWER API
#'
#' @param lonlat A numeric vector of geographic coordinates for a cell or region
#'  entered as x, y coordinates.
#' @param dates A character vector of start and end dates in that order.
#' @param season_length A numeric value indicating how many days a single
#'  growing season being simulated is to be. If this is used, only the start
#'  date will be used and the end date will be ignored if supplied.
#'
#' @return A \code{\link[data.table]{data.table}} of weather data, dates and
#'  geolocation information (LAT/LON values) suitable for use in \pkg{epicrop}
#'  with the following columns:
#'   \tabular{rl}{
#'   **YYYYMMDD**:\tab Date as Year Month Day (ISO8601).\cr
#'   **DOY**:\tab  Consecutive day of year, commonly called "Julian date".\cr
#'   **TEMP**:\tab Mean daily temperature (°C).\cr
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
#' # get 120 days of weather for IRRI Zeigler Experiment Station in wet
#' # season 2000
#' (wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = "2000-06-30",
#'   season_length = 120
#' ))
#' }
#' @author Adam H. Sparks
#' @export get_wth

get_wth <- function(lonlat, dates, season_length) {

  if (!missing(season_length)) {
    dates[2] <-
      as.character(as.Date(as.Date(dates[1]) + (season_length)))
  }

  wth <- setDT(
    nasapower::get_power(
      lonlat = lonlat,
      dates = dates,
      community = "AG",
      pars = c("T2M",
               "RH2M",
               "PRECTOT"),
      temporal_average = "DAILY"
    )
  )

  wth[, c("YEAR", "MM", "DD") := NULL][]
  setnames(
    wth,
    old = c("DOY",
            "YYYYMMDD",
            "T2M",
            "PRECTOT",
            "RH2M",
            "LAT",
            "LON"),
    new = c("DOY",
            "YYYYMMDD",
            "TEMP",
            "RAIN",
            "RHUM",
            "LAT",
            "LON")
  )
  setcolorder(wth,
              c("YYYYMMDD",
                "DOY",
                "TEMP",
                "RHUM",
                "RAIN",
                "LAT",
                "LON"))

  .check_na(.wth = wth)

  return(wth)

}

#' @noRd
.check_na <- function(.wth) {
  if (anyNA(.wth[, c("TEMP", "RHUM", "RAIN")])) {
    message(
      "The POWER weather data have missing values in your request.\n",
      "This can cause errors when running the model.\n",
      "You should inspect the weather data carefully and either fill missing\n",
      "or try again to download to see if that provides a complete set of data."
    )
  }
}
