#' Format Weather Data for Use in seir() Simulations
#'
#' Formats an existing [data.frame()] of weather data for use in \pkg{epicrop}.
#'  Only the data necessary for use in the model is returned and the columns
#'  are renamed to values expected by the model.  The data are assumed to be on
#'  a daily time-step. For converting sub-daily weather data to daily, see
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
  lon
) {
  # Early return if already formatted
  if (inherits(wth, "epicrop.wth")) {
    cli::cli_warn(
      "{.var wth} is already an {.cls epicrop.wth} object. No formatting is necessary."
    )
    return(wth)
  }

  # Validate required arguments
  rlang::check_required(c(wth, date, temp, rhum, rain))

  # Convert to data.table (by reference if already data.table)
  if (!data.table::is.data.table(wth)) {
    w <- data.table::as.data.table(wth)
  } else {
    w <- data.table::copy(wth)
  }

  # Build column mapping for renaming
  col_map <- c(
    "YYYYMMDD" = date,
    "TEMP" = temp,
    "RHUM" = rhum,
    "RAIN" = rain
  )

  # Add optional columns if provided
  if (!missing(tmin)) {
    col_map["TMIN"] <- tmin
  }
  if (!missing(tmax)) {
    col_map["TMAX"] <- tmax
  }
  if (!missing(lat)) {
    col_map["LAT"] <- lat
  }
  if (!missing(lon)) {
    col_map["LON"] <- lon
  }

  # Validate all specified columns exist
  missing_cols <- col_map[!col_map %in% names(w)]
  if (length(missing_cols) > 0L) {
    cli::cli_abort(
      c(
        "The following columns are not in {.var wth}:",
        "x" = "{.var {missing_cols}}"
      )
    )
  }

  # Select and rename columns in one operation
  w <- w[, .SD, .SDcols = col_map]
  data.table::setnames(w, old = col_map, new = names(col_map))

  # Convert date to Date class
  w[, YYYYMMDD := as.Date(YYYYMMDD)]

  # Validate daily data - check for duplicates
  if (anyDuplicated(w$YYYYMMDD)) {
    dup_dates <- w[duplicated(YYYYMMDD), YYYYMMDD]
    cli::cli_abort(
      c(
        "Weather data contains duplicate dates",
        "x" = "Found {length(dup_dates)} duplicate date{?s}",
        "i" = "First duplicate: {dup_dates[1]}",
        "i" = "Each date must appear exactly once for daily data"
      )
    )
  }

  # Sort by date for validation and consistency
  data.table::setorder(w, YYYYMMDD)

  # Validate consecutive daily time-step (no gaps)
  if (nrow(w) > 1L) {
    date_diff <- as.integer(diff(w$YYYYMMDD))

    if (!all(date_diff == 1L)) {
      gaps <- which(date_diff != 1L)
      gap_sizes <- date_diff[gaps]

      cli::cli_abort(
        c(
          "Weather data is not on a daily time-step or has gaps",
          "x" = "Found {length(gaps)} gap{?s} in date sequence",
          "i" = "First gap: {gap_sizes[1]} day{?s} between {w$YYYYMMDD[gaps[1]]} and {w$YYYYMMDD[gaps[1] + 1L]}",
          "i" = "Weather data must be continuous daily observations"
        )
      )
    }
  }

  # Add DOY (day of year)
  w[, DOY := data.table::yday(YYYYMMDD)]

  # Set column order
  col_order <- c(
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
  data.table::setcolorder(w, intersect(col_order, names(w)))

  # Set class
  data.table::setattr(w, "class", c("epicrop.wth", class(w)))

  w[]
}
