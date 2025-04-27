#' Format weather data for use
#'
#' Formats an existing [data.frame] of weather data for use in \pkg{cropsim}.
#'  Only the data necessary for use in the model is returned and the columns
#'  are renamed to values expected by the model.
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
#'  the observation data (optional).
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
#' @returns A `epicrop.wth`, a data frame based on a [data.table::data.table],'
#'  suitable for use in \pkg{epicrop}.
#' @examples
#' # Get weather data from the POWER API, using the `get_wth()` function,
#' # this is for example purposes only as it is NOT necessary to reformat
#' # the data provided by `get_wth()`.
#'
#' library(nasapower)
#' w <- get_power(
#'   lonlat = c(121.25562, 14.6674),
#'   dates = c("2000-06-30", "2000-12-31"),
#'   community = "AG",
#'   pars = c(
#'     "T2M",
#'     "T2M_MIN",
#'     "T2M_MAX",
#'     "RH2M",
#'     "PRECTOTCORR"
#'   ),
#'   temporal_api = "DAILY"
#' )
#'
#' format_wth(
#'   wth = w,
#'   date = "YYYYMMDD",
#'   temp = "T2M",
#'   rhum = "RH2M",
#'   rain = "PRECTOTCORR",
#'   tmin = "T2M_MIN",
#'   tmax = "T2M_MAX",
#'   lat = "LAT",
#'   lon = "LON"
#' )
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @export
format_wth <- function(
    wth,
    date,
    temp,
    rhum,
    rain,
    tmin,
    tmax,
    lat,
    lon) {
  rlang::check_required(c(wth, date, temp, rhum, rain))
  passed_args <- as.character(match.call())[-c(1:2)] # remove fn & `wth` fm args
  if (any(passed_args %notin% names(wth))) {
    not_in <- passed_args[passed_args %notin% names(wth)]
    cli::cli_abort(
      c(
        "The following columns that you have provided are not in {.var wth}:
        {.var {not_in}}."
      )
    )
  }

  if (inherits(wth, "data.frame")) {
    setDT(wth)

    # Add the Julian day of year
    wth[, DOY := as.integer(as.POSIXlt(wth$YYYYMMDD)$yday)]

    if (length(passed_args) == 8) {
      old_names <- c(date, temp, tmin, tmax, rhum, rain, lat, lon)
      new_names <- c(
        "YYYYMMDD",
        "TEMP",
        "TMIN",
        "TMAX",
        "RHUM",
        "RAIN",
        "LAT",
        "LON"
      )
      setnames(wth, old = old_names, new = new_names)
      # select only the columns that are needed
      wth <- wth[, new_names, with = FALSE]
    } else {
      old_names <- c(date, temp, rhum, rain)

      new_names <- c(
        "YYYYMMDD",
        "TEMP",
        "RHUM",
        "RAIN"
      )
      setnames(
        wth,
        old = old_names,
        new = new_names
      )
      # select only the columns that are needed
      wth <- wth[, new_names, with = FALSE]
    }
  } else {
    cli::cli_abort("{.var wth} must be a data.frame.")
  }

  setcolorder(
    wth,
    c("YYYYMMDD", "DOY", "TEMP", "TMIN", "TMAX", "RHUM", "RAIN", "LAT", "LON"),
    skip_absent = TRUE
  )[]

  class(wth) <- union("epicrop.wth", class(wth))
  return(wth)
}
