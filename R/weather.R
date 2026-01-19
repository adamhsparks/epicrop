#' Get Weather Data for Modelling Disease
#'
#' This function is a wrapper for the [nasapower:: get_power()] with predefined
#'  parameters suitable for use in \pkg{epicrop}.
#'
#' @param lonlat A numeric vector of geographic coordinates for a cell or region
#'  entered as x, y coordinates.
#' @param dates A character vector of start and end dates in that order.
#' @param duration A numeric value indicating how many days a single
#'  growing season being simulated is to be.   If this is used, only the start
#'  date will be used and the end date will be ignored if supplied.  This must
#'  match the `duration` parameter value passed along to [epicrop_sim()] or any of the
#'  disease model functions.
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
#' # 2000 by specifying the duration but not the end-date.
#' power <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = "2000-06-30",
#'   duration = 120
#' )
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @export
get_wth <- function(lonlat, dates, duration) {
  if (!missing(duration)) {
    dates[2L] <- as.character(as.Date(as.Date(dates[1L]) + (duration)))
  }

  wth <- data.table::setDT(
    nasapower::get_power(
      lonlat = lonlat,
      dates = dates,
      community = "AG",
      pars = c("T2M", "T2M_MIN", "T2M_MAX", "RH2M", "PRECTOTCORR"),
      temporal_api = "DAILY"
    )
  )

  wth[, c("YEAR", "MM", "DD") := NULL][]

  data.table::setnames(
    wth,
    old = c("T2M", "T2M_MIN", "T2M_MAX", "PRECTOTCORR", "RH2M"),
    new = c("TEMP", "TMIN", "TMAX", "RAIN", "RHUM")
  )

  data.table::setcolorder(
    wth,
    c("YYYYMMDD", "DOY", "TEMP", "TMIN", "TMAX", "RHUM", "RAIN", "LAT", "LON")
  )

  .check_na(.wth = wth)
  class(wth) <- union("epicrop.wth", class(wth))
  return(wth)
}

#' Format Weather Data for Use in epicrop
#'
#' Formats an existing [data.frame()] of weather data for use in \pkg{epicrop}.
#'  Only the data necessary for use in the model is returned and the columns
#'  are renamed to values expected by the model.  The data are assumed to be on
#'  a daily time-step.   For converting sub-daily weather data to daily, see
#'  [cropgrowdays::daily_mean()].
#'
#' @param wth A [data.frame()] object containing the weather data for
#' formatting.
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
#' * total daily rainfall
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
#'   _TMIN_     | Daily minimum temperature (°C)
#'   _TMAX_     | Daily maximum temperature (°C)
#'   _RHUM_     | Mean daily relative humidity (%)
#'   _RAIN_     | Mean daily rainfall (mm)
#'   _LAT_      | Latitude of area of interest
#'   _LON_      | Longitude of area of interest.
#'
#' @returns An `epicrop.wth` object, a data frame compatible with a
#'  [data.table::data.table()], suitable for use in \pkg{epicrop}.
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
#'   pars = c("T2M", "T2M_MIN", "T2M_MAX", "RH2M", "PRECTOTCORR"),
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
format_wth <- function(wth, date, temp, rhum, rain, tmin, tmax, lat, lon) {
  rlang::check_required(c(wth, date, temp, rhum, rain))

  # Get all provided arguments (excluding wth)
  passed_args <- as.character(match.call())[-c(1L:2L)]

  if (any(passed_args %notin% names(wth))) {
    not_in <- passed_args[passed_args %notin% names(wth)]
    cli::cli_abort(
      call = rlang::caller_env(),
      "The following columns that you have provided are not in {.var wth}:  
        {.var {not_in}}."
    )
  }

  wth <- data.table::data.table(wth)

  dates <- .check_date(wth[[date]])
  unique_dates <- length(unique(dates))
  diff_dates <- diff(dates)
  nrows <- nrow(wth)

  if ((unique_dates != nrows) || (any(as.numeric(diff_dates) != 1.0))) {
    cli::cli_abort(
      call = rlang::caller_env(),
      "Please check that the values in {.var wth} are on a daily time-step."
    )
  }

  # Build list of columns to rename based on what was provided
  old_names <- c(date, temp, rhum, rain)
  new_names <- c("YYYYMMDD", "TEMP", "RHUM", "RAIN")

  # Add optional columns if provided
  if (!missing(tmin)) {
    old_names <- c(old_names, tmin)
    new_names <- c(new_names, "TMIN")
  }
  if (!missing(tmax)) {
    old_names <- c(old_names, tmax)
    new_names <- c(new_names, "TMAX")
  }
  if (!missing(lat)) {
    old_names <- c(old_names, lat)
    new_names <- c(new_names, "LAT")
  }
  if (!missing(lon)) {
    old_names <- c(old_names, lon)
    new_names <- c(new_names, "LON")
  }

  data.table::setnames(wth, old = old_names, new = new_names)
  wth <- wth[, new_names, with = FALSE]

  # Add DOY
  wth[, DOY := as.integer(as.POSIXlt(wth$YYYYMMDD)$yday)]

  data.table::setcolorder(
    wth,
    c("YYYYMMDD", "DOY", "TEMP", "TMIN", "TMAX", "RHUM", "RAIN", "LAT", "LON"),
    skip_absent = TRUE
  )[]

  class(wth) <- union("epicrop.wth", class(wth))
  return(wth)
}

#' Check POWER data for any missing values
#' @param . wth A data.frame object from [nasapower::get_power] with weather
#'  data for checking
#' @returns Called for its side-effects, an invisible `NULL`.
#' @autoglobal
#' @noRd
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
