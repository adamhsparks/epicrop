#' Format Weather Data for Use in seir() Simulations
#'
#' Reformats an existing [data.frame()] of weather data into the standard
#' \pkg{epicrop} format for use in [seir()] and disease model functions.
#' Selects and renames columns to match model expectations, validates data
#' quality (no gaps, no duplicates), and adds derived fields (DOY).
#'
#' @section Input Requirements:
#'
#' Weather data must contain **daily observations** (one row per day) with these
#' required columns (names arbitrary):
#' * **Date** column with full date (year, month, day)
#' * **Temperature** column with mean daily temperature (degrees C)
#' * **Relative humidity** column with mean daily RH (%)
#' * **Rainfall** column with total daily precipitation (mm)
#'
#' Optional columns that enhance model functionality:
#' * **Minimum temperature** (degrees C; required if `simple_wetness = FALSE`)
#' * **Maximum temperature** (degrees C; required if `simple_wetness = FALSE`)
#' * **Latitude** (degrees; used for hourly wetness and mapping)
#' * **Longitude** (degrees; used for mapping and spatial analysis)
#'
#' @section Data Validation:
#'
#' This function validates that:
#' 1. All required columns exist in the input data.frame
#' 2. No duplicate dates are present
#' 3. Dates form a contiguous sequence with no gaps (true daily data)
#' 4. Date column is properly formatted as Date class
#'
#' If validation fails, detailed error messages pinpoint the issue.
#'
#' @section Output Format:
#'
#' Returns a [data.table::data.table()] with class `epicrop.wth` containing
#' the following columns (in order):
#'
#'   **Column** | **Description**
#'   -----------:|:----------
#'   YYYYMMDD | Date (ISO 8601 format, Date class)
#'   DOY | Day of year (1-365/366, integer)
#'   TEMP | Mean daily temperature (degrees C)
#'   TMIN | Minimum daily temperature (degrees C) \[optional\]
#'   TMAX | Maximum daily temperature (degrees C) \[optional\]
#'   RHUM | Mean daily relative humidity (%)
#'   RAIN | Total daily rainfall (mm)
#'   LAT | Latitude \[optional\]
#'   LON | Longitude \[optional\]
#'
#' @param wth A [data.frame()] or [data.table::data.table()] containing raw
#'   weather observations. Must have one row per day with no gaps.
#' @param date Character. Name of column in `wth` containing dates. Dates
#'   should be in ISO format (YYYY-MM-DD), integer (YYYYMMDD), or already
#'   Date class. Required.
#' @param temp Character. Name of column containing mean daily temperature
#'   (degrees C). Required.
#' @param rhum Character. Name of column containing mean daily relative
#'   humidity (%). Required.
#' @param rain Character. Name of column containing total daily rainfall (mm).
#'   Required.
#' @param tmin Character. Name of column containing minimum daily temperature
#'   (degrees C). Optional; required only if using `simple_wetness = FALSE`
#'   in [seir()].
#' @param tmax Character. Name of column containing maximum daily temperature
#'   (degrees C). Optional; required only if using `simple_wetness = FALSE`
#'   in [seir()].
#' @param lat Character. Name of column containing latitude (degrees).
#'   Optional; improves hourly wetness calculation and enables mapping.
#' @param lon Character. Name of column containing longitude (degrees).
#'   Optional; enables spatial mapping of results.
#'
#' @returns An `epicrop.wth` object ([data.table::data.table()] subclass)
#'   containing reformatted weather data. Compatible with [seir()] and all
#'   disease model functions ([bacterial_blight()], [leaf_blast()], etc.).
#'   Automatically includes DOY (day of year) derived from date.
#'
#' @examplesIf interactive()
#' # Example 1: Format weather from NASA POWER API
#' # (Requires nasapower package; this example shows typical usage)
#'
#' # Simulated POWER data (in practice from get_power())
#' wth_raw <- data.frame(
#'   DATE = seq(as.Date("2000-06-30"), as.Date("2000-12-31"), by = "day"),
#'   TAVG = rnorm(185, mean = 25, sd = 3),
#'   TMIN = rnorm(185, mean = 20, sd = 3),
#'   TMAX = rnorm(185, mean = 30, sd = 3),
#'   RH = rnorm(185, mean = 80, sd = 10),
#'   PRECIP = rgamma(185, shape = 2, rate = 0.5),
#'   LAT = 14.6774,
#'   LON = 121.25562
#' )
#'
#' # Format for epicrop
#' wth <- format_wth(
#'   wth = wth_raw,
#'   date = "DATE",
#'   temp = "TAVG",
#'   tmin = "TMIN",
#'   tmax = "TMAX",
#'   rhum = "RH",
#'   rain = "PRECIP",
#'   lat = "LAT",
#'   lon = "LON"
#' )
#'
#' # Check class and structure
#' class(wth)
#' head(wth)
#' # Ready for use in disease models
#' sim <- bacterial_blight(wth, emergence = "2000-07-01")
#'
#' # Example 2: Minimal data (no lat/lon, no TMIN/TMAX)
#' # Only requires temp, rhum, rain (simple_wetness = TRUE)
#' wth_minimal <- data.frame(
#'   obs_date = seq(as.Date("2000-06-30"), as.Date("2000-08-28"), by = "day"),
#'   mean_temp = rnorm(60, mean = 25, sd = 2),
#'   mean_rh = rnorm(60, mean = 85, sd = 5),
#'   daily_rain = rpois(60, lambda = 3)
#' )
#'
#' wth_fmt <- format_wth(
#'   wth = wth_minimal,
#'   date = "obs_date",
#'   temp = "mean_temp",
#'   rhum = "mean_rh",
#'   rain = "daily_rain"
#' )
#'
#' # Simulate with binary wetness (no hourly calculation)
#' sim <- leaf_blast(wth_fmt, emergence = "2000-07-01", simple_wetness = TRUE)
#'
#' # Example 3: Integer date format (YYYYMMDD)
#' wth_int_date <- data.frame(
#'   date_int = c(20000630L, 20000701L, 20000702L),
#'   temp = c(25, 26, 24),
#'   rhum = c(85, 80, 90),
#'   rain = c(5, 0, 8)
#' )
#'
#' # Convert to character first (data.table will handle conversion)
#' wth_int_date$date_int <- as.character(wth_int_date$date_int)
#'
#' wth_formatted <- format_wth(
#'   wth = wth_int_date,
#'   date = "date_int",
#'   temp = "temp",
#'   rhum = "rhum",
#'   rain = "rain"
#' )
#'
#' # Example 4: Subset and format existing epicrop.wth
#' # (e.g., use only part of a longer dataset)
#' wth_full <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#'
#' # Subset to June-July only
#' wth_subset <- wth_full[month(YYYYMMDD) %in% 6:7]
#'
#' # Re-validate formatting
#' wth_check <- format_wth(
#'   wth = wth_subset,
#'   date = "YYYYMMDD",
#'   temp = "TEMP",
#'   rhum = "RHUM",
#'   rain = "RAIN",
#'   tmin = "TMIN",
#'   tmax = "TMAX",
#'   lat = "LAT",
#'   lon = "LON"
#' )
#'
#' # Example 5: Error handling - demonstrating validation
#' # This will fail (missing column)
#' \dontrun{
#' bad_wth <- data.frame(
#'   date = seq(as.Date("2000-06-30"), as.Date("2000-07-05"), by = "day"),
#'   temp = c(25, 26, 24, 25, 27, 26),
#'   rhum = c(85, 80, 90, 85, 75, 80)
#'   # Missing 'rain' column!
#' )
#'
#' format_wth(
#'   wth = bad_wth,
#'   date = "date",
#'   temp = "temp",
#'   rhum = "rhum",
#'   rain = "rain"  # This column doesn't exist
#' )
#' # Error: The following columns are not in wth: rain
#' }
#'
#' # This will fail (duplicate dates)
#' \dontrun{
#' dup_wth <- data.frame(
#'   date = c(as.Date("2000-06-30"), as.Date("2000-06-30"), as.Date("2000-07-01")),
#'   temp = c(25, 26, 24),
#'   rhum = c(85, 80, 90),
#'   rain = c(5, 0, 8)
#' )
#'
#' format_wth(
#'   wth = dup_wth,
#'   date = "date",
#'   temp = "temp",
#'   rhum = "rhum",
#'   rain = "rain"
#' )
#' # Error: Weather data contains duplicate dates
#' }
#'
#' # This will fail (gaps in dates)
#' \dontrun{
#' gap_wth <- data.frame(
#'   date = c(
#'     as.Date("2000-06-30"),
#'     as.Date("2000-07-01"),  # Skip July 1
#'     as.Date("2000-07-03")   # Gap of 2 days!
#'   ),
#'   temp = c(25, 24, 26),
#'   rhum = c(85, 90, 80),
#'   rain = c(5, 8, 0)
#' )
#'
#' format_wth(
#'   wth = gap_wth,
#'   date = "date",
#'   temp = "temp",
#'   rhum = "rhum",
#'   rain = "rain"
#' )
#' # Error: Weather data is not on a daily time-step or has gaps
#' }
#'
#' @details
#'
#' ## Data Source Examples
#'
#' Common data sources and required column names:
#' * **NASA POWER** (via [nasapower::get_power()]): Use `date = "YYYYMMDD"`,
#'   `temp = "T2M"`, `rhum = "RH2M"`, `rain = "PRECTOTCORR"`, etc.
#' * **CHIRPS rainfall** (via [chirps::get_chirps()]): Combine with
#'   temperature data from another source
#' * **Local weather station data**: Map station file columns to parameter names
#'
#' ## Caveats
#'
#' * Sub-daily weather data must be aggregated to daily first. See
#'   [cropgrowdays::daily_mean()] for hourly data aggregation.
#' * Dates must be complete and continuous (no missing days). Interpolation
#'   is not performed.
#' * Temperature, RH, and rainfall must be numeric (integers or floats).
#'   Character/factor columns cause errors.
#' * For `simple_wetness = FALSE` in [seir()], both TMIN and TMAX are required.
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
    cli::cli_warn(c(
      "{.var wth} is already an {.cls epicrop.wth} object.",
      i = "No formatting is necessary; returning as-is."
    ))
    return(wth)
  }

  # Validate required arguments
  rlang::check_required(c(wth, date, temp, rhum, rain))

  # Convert to data.table (copy if already data.table to avoid side effects)
  if (!data.table::is.data.table(wth)) {
    w <- data.table::as.data.table(wth)
  } else {
    w <- data.table::copy(wth)
  }

  # Build column mapping for renaming
  # Structure: new_name = old_name
  col_map <- c(
    "YYYYMMDD" = date,
    "TEMP" = temp,
    "RHUM" = rhum,
    "RAIN" = rain
  )

  # Add optional columns if provided (check with missing())
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
    cli::cli_abort(c(
      "The following columns are not in {.var wth}:",
      x = "{.val {missing_cols}}"
    ))
  }

  # Select and rename columns in one data.table operation
  # Using .SD (subset of data) and .SDcols to select specific columns
  w <- w[, .SD, .SDcols = col_map]
  data.table::setnames(w, old = col_map, new = names(col_map))

  # Convert date to Date class (handles multiple input formats)
  w[, YYYYMMDD := as.Date(YYYYMMDD)]

  # Validate no duplicate dates
  if (anyDuplicated(w$YYYYMMDD)) {
    dup_dates <- w[duplicated(YYYYMMDD), unique(YYYYMMDD)]
    cli::cli_abort(c(
      "Weather data contains duplicate dates",
      x = "Found {length(dup_dates)} duplicate date{?s}",
      i = "First duplicate: {dup_dates[1L]}",
      i = "Each date must appear exactly once for daily observations"
    ))
  }

  # Sort by date for validation and consistency
  data.table::setorder(w, YYYYMMDD)

  # Validate contiguous daily time-step (no gaps)
  if (nrow(w) > 1L) {
    # Calculate day differences between consecutive dates
    date_diff <- as.integer(diff(w$YYYYMMDD))

    # All differences should be exactly 1 (one day apart)
    if (!all(date_diff == 1L)) {
      gaps <- which(date_diff != 1L)
      gap_sizes <- date_diff[gaps]

      cli::cli_abort(c(
        "Weather data is not on a daily time-step or has gaps",
        x = "Found {length(gaps)} gap{?s} in date sequence",
        i = "First gap: {gap_sizes[1L]} day{?s} between {w$YYYYMMDD[gaps[1L]]} and {w$YYYYMMDD[gaps[1L] + 1L]}",
        i = "Weather data must be continuous daily observations"
      ))
    }
  }

  # Add DOY (day of year) derived column using data.table
  w[, DOY := data.table::yday(YYYYMMDD)]

  # Set desired column order
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
  # Only reorder columns that exist
  data.table::setcolorder(w, intersect(col_order, names(w)))

  # Set class attribute to mark as epicrop.wth
  data.table::setattr(w, "class", c("epicrop.wth", class(w)))

  return(w[])
}
