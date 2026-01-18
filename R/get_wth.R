#' Get Weather Data for Modelling Disease
#'
#' This function is a wrapper for the [nasapower::get_power()] with predefined
#'  parameters suitable for use in \pkg{epicrop}.
#'
#' @param lonlat A numeric vector of geographic coordinates for a cell or region
#'  entered as x, y coordinates.
#' @param dates A character vector of start and end dates in that order.
#' @param duration A numeric value indicating how many days a single
#'  growing season being simulated is to be.  If this is used, only the start
#'  date will be used and the end date will be ignored if supplied.  This must
#'  match the `duration` parameter value passed along to [epicrop_sim()] or any of the
#'  `predict` family of functions.
#'
#' @returns A `epicrop.wth`, a data frame based on a [data.table::data.table()],
#'  object suitable for use in \pkg{epicrop} containing weather data, dates and
#'  geolocation information (LAT/LON values) with the following columns:
#'
#'   **Field Name** | **Value**
#'   --------------:|:----------
#'   _YYYYMMDD_ | Date as Year Month Day (ISO8601)
#'   _DOY_      | Consecutive day of year, commonly called "Julian date"
#'   _TEMP_     | Mean daily temperature (°C)
#'   _TMIN_     | Daily minimum temperature (°C)
#'   _TMAX_     | Daily maximum temperature (°C)
#'   _RHUM_     | Mean daily relative humidity (%)
#'   _RAIN_     | Mean daily rainfall (mm)
#'   _LAT_      | Latitude of area of interest
#'   _LON_      | Longitude of area of interest.
#'
#' @examplesIf interactive()
#'
#' # get weather for IRRI Zeigler Experiment Station in wet season 2000 from the
#' # default NASA POWER data.
#' power <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#'
#' # get 120 days of weather for IRRI Zeigler Experiment Station in wet season
#' # 2000 by specifying the duration but not the end-date and specifying to use
#' # POWER data.
#' power <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = "2000-06-30",
#'   duration = 120
#' )
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @export get_wth

get_wth <- function(lonlat, dates, duration) {
  if (!missing(duration)) {
    dates[2L] <- as.character(as.Date(as.Date(dates[1L]) + (duration)))
  }

  wth <- setDT(
    nasapower::get_power(
      lonlat = lonlat,
      dates = dates,
      community = "AG",
      pars = c(
        "T2M",
        "T2M_MIN",
        "T2M_MAX",
        "RH2M",
        "PRECTOTCORR"
      ),
      temporal_api = "DAILY"
    )
  )
  wth[, c("YEAR", "MM", "DD") := NULL][]
  setnames(
    wth,
    old = c(
      "T2M",
      "T2M_MIN",
      "T2M_MAX",
      "PRECTOTCORR",
      "RH2M"
    ),
    new = c(
      "TEMP",
      "TMIN",
      "TMAX",
      "RAIN",
      "RHUM"
    )
  )
  setcolorder(
    wth,
    c(
      "YYYYMMDD",
      "DOY",
      "TEMP",
      "TMIN",
      "TMAX",
      "RHUM",
      "RAIN",
      "LAT",
      "LON"
    )
  )
  .check_na(.wth = wth)
  class(wth) <- union("epicrop.wth", class(wth))
  return(wth)
}

#' Check POWER data for any missing values
#' @param .wth A data.frame object from [nasapower::get_power] with weather
#'  data for checking
#' Checks the data returned from the POWER dataset and informs the user if
#' missing values are found. It does not do anything other than emitting a
#' message about missing data.
#' @returns Called for its side-effects, an invisible `NULL`.
#' @examples
#' wth <- setDT(
#'   nasapower::get_power(
#'     lonlat = c(121.25562, 14.6774),
#'     dates = "2000-06-30",
#'     community = "AG",
#'     pars = c(
#'       "T2M",
#'       "T2M_MIN",
#'       "T2M_MAX",
#'       "RH2M",
#'       "PRECTOTCORR"
#'     ),
#'     temporal_api = "DAILY"
#'   )
#' )
#'
#' wth[, c("YEAR", "MM", "DD") := NULL][]
#' setnames(
#'   wth,
#'   old = c(
#'     "T2M",
#'     "T2M_MIN",
#'     "T2M_MAX",
#'     "PRECTOTCORR",
#'     "RH2M"
#'   ),
#'   new = c(
#'     "TEMP",
#'     "TMIN",
#'     "TMAX",
#'     "RAIN",
#'     "RHUM"
#'   )
#' )
#' .check_na(wth)
#' @dev

.check_na <- function(.wth) {
  if (anyNA(.wth[, c("TEMP", "TMIN", "TMAX", "RHUM", "RAIN")])) {
    cli::cli_alert(
      c(
        i = "The weather data have missing values in your request.",
        x = "This can cause errors when running the model.
      You should inspect the weather data carefully and either fill missing
      or try again to download to see if that provides a complete set of data."
      )
    )
    return(invisible(NULL))
  }
}
