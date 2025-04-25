#' Format weather data for use
#'
#' Formats an existing [data.frame] of weather data for use in \pkg{cropsim}.
#'
#' @param wth A [data.frame] object containing the weather data for use.
#' @param date The column name indicating the field containing the date values.
#' @param temp The column name indicating the field containing the average daily
#'  temperature values.
#' @param tmin The column name indicating the field containing the minimum daily
#'  temperature (optional).
#' @param tmax The column name indicating the field containing the maximum daily
#'  temperature.
#' @param rhum The column name indicating the field containing the average daily
#'  relative humidity.
#' @param rain The column name indicating the field containing the total daily
#'  rainfall.
#' @param lat The column name indicating the field containing the latitude of
#'  the observation data (optional).
#' @param lon The column name indicating the field containing the longitude of
#'  the observasion data (optional).
#'
#' @details
#'
#' ## Required values
#'
#' Weather data must be a daily time-step and contain the following values,
#'
#' * full date including year, month and day,
#' * average daily temperature,
#' * average daily relative humidity,
#' * total daily rainfall,
#'
#' ## Optional values
#'
#' The following values are optional and the model can be run by default without
#'  them.
#'
#' * average minimum daily temperature (only used if `simple_wetness == FALSE`)
#' * average maximum daily temperature (only used if `simple_wetness == FALSE`)
#' * Latitude (only used if `simple_wetness == FALSE` and for mapping outputs)
#' * Longitude (only used for mapping outputs)
#'
#'   **Field Name** | **Value**
#'   --------------:|:----------
#'   _YYYYMMDD_ | Date as Year Month Day (ISO8601)
#'   _DOY_      | Consecutive day of year, commonly called "Julian date"
#'   _TEMP_     | Mean daily temperature (°C)
#'   _TMIN*_     | Daily minimum temperature (˚C)
#'   _TMAX*_     | Daily maximum temperature (˚C)
#'   _RHUM_     | Mean daily relative humidity (%)
#'   _RAIN_     | Mean daily rainfall (mm)
#'   _LAT*_      | Latitude of area of interest
#'   _LON*_      | Longitude of area of interest.
#'
#' *Values marked with "*" are optional and not necessary to run the model
#'   when using default settings.
#'
#'
#' @returns A data.frame suitable for use in \pkg{epicrop}.
#' @examples
#' format_wth(wth)
#' @export
format_wth <- function(
    wth,
    date,
    temp,
    tmin = NULL,
    tmax = NULL,
    rhum,
    rain,
    lat = NULL,
    lon = NULL) {
  vars <- c(wth, date, temp, rhum, rain)
  for (i in vars) {
    if (missing(i)) {
      cli::cli_abort("{.var i} must be supplied.")
    }
  }
  if (inherits(wth, "data.frame")) {
    setDT(wth)

    if (length(wth) == 8) {
      old_names <- c(date, temp, tmin, tmax, rhum, rain, lat, lon)
      new_names <- c(
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
      setnames(wth, old = old_names, new = new_names)
    } else {
      setcolorder(wth, c(date, temp, rhum, rain))
      setnames(
        wth,
        old = vars[-wth],
        new = c("YYYYMMDD", "TEMP", "RHUM", "RAIN")
      )
    }
  } else {
    cli::cli_abort("{.var wth} must be a data.frame.")
  }
  wth[, DOY := as.integer(as.POSIXlt(wth$YYYYMMDD)$yday)]
  setcolorder(
    wth,
    c("YYYYMMDD", "DOY", "TEMP", "TMIN", "TMAX", "RHUM", "RAIN"),
    skip_absent = TRUE
  )
}
