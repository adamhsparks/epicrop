#' Get Weather Data for Disease Modelling
#'
#' Retrieves daily weather data from the NASA POWER database and formats it
#' for use in [seir()] and \pkg{epicrop} disease models. This function wraps
#' [nasapower::get_power()] with predefined parameters optimized for
#' epidemiological modelling.
#'
#' @section Data Source:
#'
#' Weather data are obtained from NASA's Prediction of Worldwide Energy Resource
#' (POWER) database, which provides global daily weather data from 1984 onwards
#' at 0.5-degree spatial resolution. See [nasapower::get_power()] for details.
#'
#' POWER data are derived from satellite and reanalysis sources and may differ
#' from local weather station observations. For critical analyses, consider
#' comparing with ground-truth data from your region.
#'
#' @section Retrieved Variables:
#'
#' This function automatically retrieves:
#' * **T2M**: Mean daily temperature (2-meter height)
#' * **T2M_MIN**: Daily minimum temperature
#' * **T2M_MAX**: Daily maximum temperature
#' * **RH2M**: Mean daily relative humidity (2-meter height)
#' * **PRECTOTCORR**: Daily precipitation (bias-corrected)
#'
#' All variables are returned in standard units: temperature in degrees C,
#' humidity in %, rainfall in mm.
#'
#' @section Date Handling:
#'
#' Two methods for specifying the weather window:
#' 1. **Start and end dates** (character vector): `dates = c("2000-06-30", "2000-12-31")`
#' 2. **Start date + duration** (integer): `dates = "2000-06-30", duration = 120`
#'
#' Duration is interpreted as inclusive days: `end = start + duration - 1`.
#' For example, `duration = 120` starting on June 30 ends on October 27 (120 days
#' total).
#'
#' @param lonlat Numeric vector of coordinates as `c(longitude, latitude)`.
#'   Longitude: -180 to 180 degrees (West/East)
#'   Latitude: -90 to 90 degrees (South/North)
#'   Example: `c(121.255669, 14.167425)` for IRRI, Philippines
#'
#' @param dates Character vector of start date and optional end date, or
#'   character scalar start date. Format: `"YYYY-MM-DD"` (ISO 8601).
#'   * Length 1: Use with `duration` parameter
#'   * Length 2+: First is start, second is end; `duration` parameter ignored
#'
#' @param duration Optional integer. Number of days to retrieve (inclusive).
#'   Only used if `dates` is a single start date. Ignored if `dates` has
#'   length >= 2. Must be positive (> 0).
#'   If not provided and `dates` has only one element, an error is raised.
#'
#' @returns An `epicrop.wth` object ([data.table::data.table()] subclass)
#'   containing daily weather data with columns:
#'
#'   **Column** | **Description** | **Units**
#'   -----------:|:---------|:----------
#'   YYYYMMDD | Date | ISO 8601 (YYYY-MM-DD)
#'   DOY | Day of year | Integer (1-365/366)
#'   TEMP | Mean daily temperature | Degrees C
#'   TMIN | Minimum daily temperature | Degrees C
#'   TMAX | Maximum daily temperature | Degrees C
#'   RHUM | Mean daily relative humidity | Percent (0-100)
#'   RAIN | Total daily rainfall | Millimeters
#'   LAT | Latitude of query location | Degrees (-90 to 90)
#'   LON | Longitude of query location | Degrees (-180 to 180)
#'
#'   Ready for direct use in [seir()], [bacterial_blight()], [leaf_blast()],
#'   and other disease models.
#'
#' @examplesIf interactive()
#' # Example 1: Get weather for a date range
#' # IRRI Zeigler Experiment Station, wet season 2000
#' wth_range <- get_wth(
#'   lonlat = c(121.255669, 14.167425),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#' head(wth_range)
#' nrow(wth_range)  # Number of days
#'
#' # Example 2: Get weather by start date and duration
#' # 120-day window from June 30, 2000
#' wth_duration <- get_wth(
#'   lonlat = c(121.255669, 14.167425),
#'   dates = "2000-06-30",
#'   duration = 120L
#' )
#' nrow(wth_duration)  # Should be 120 rows
#'
#' # Example 3: Verify both methods return equivalent data
#' # (within floating-point precision)
#' all.equal(wth_range[1:120], wth_duration)
#'
#' # Example 4: Use retrieved weather in disease model
#' wth <- get_wth(
#'   lonlat = c(121.255669, 14.167425),
#'   dates = "2000-06-30",
#'   duration = 120L
#' )
#'
#' # Run bacterial blight model
#' bb_sim <- bacterial_blight(wth, emergence = "2000-07-01")
#' head(bb_sim)
#' plot(bb_sim$simday, bb_sim$intensity, type = "l",
#'      xlab = "Days After Emergence", ylab = "Disease Intensity")
#'
#' # Example 5: Multiple locations - compare disease risk
#' locations <- data.frame(
#'   name = c("IRRI_Laguna", "BRRI_Gazipur", "CIMMYT_Mexico"),
#'   lon = c(121.255669, 90.2167, -99.1828),
#'   lat = c(14.167425, 24.0028, 19.1296)
#' )
#'
#' # Get weather for each location
#' wth_list <- lapply(1:nrow(locations), function(i) {
#'   get_wth(
#'     lonlat = c(locations$lon[i], locations$lat[i]),
#'     dates = "2000-06-30",
#'     duration = 120L
#'   )
#' })
#' names(wth_list) <- locations$name
#'
#' # Run leaf blast model at each location
#' lb_results <- lapply(wth_list, function(w) {
#'   leaf_blast(w, emergence = "2000-07-01")
#' })
#'
#' # Compare AUDPC (disease intensity summary)
#' audpc_comparison <- data.frame(
#'   location = names(lb_results),
#'   AUDPC = sapply(lb_results, function(x) attr(x, "AUDPC"))
#' )
#' audpc_comparison
#'
#' # Example 6: Cross-year comparison (same location, different years)
#' years <- 2000:2002
#' wth_by_year <- lapply(years, function(y) {
#'   get_wth(
#'     lonlat = c(121.255669, 14.167425),
#'     dates = paste0(y, "-06-30"),
#'     duration = 120L
#'   )
#' })
#' names(wth_by_year) <- as.character(years)
#'
#' # Run model for each year
#' bb_by_year <- lapply(wth_by_year, function(w) {
#'   bacterial_blight(w, emergence = unique(w$YYYYMMDD)[1])
#' })
#'
#' # Compare disease progression across years
#' plot_data <- data.table::rbindlist(
#'   lapply(seq_along(bb_by_year), function(i) {
#'     data.frame(
#'       year = names(bb_by_year)[i],
#'       simday = bb_by_year[[i]]$simday,
#'       intensity = bb_by_year[[i]]$intensity
#'     )
#'   })
#' )
#' plot(plot_data$simday, plot_data$intensity,
#'      col = as.factor(plot_data$year), pch = 16)
#' legend("topleft", legend = names(bb_by_year),
#'        col = seq_along(bb_by_year), pch = 16)
#'
#' # Example 7: Error handling
#' \dontrun{
#' # This will fail (invalid coordinates)
#' get_wth(
#'   lonlat = c(200, 95),  # Out of range
#'   dates = "2000-06-30",
#'   duration = 120
#' )
#' # Error: Longitude must be in [-180, 180], got 200
#'
#' # This will fail (invalid date format)
#' get_wth(
#'   lonlat = c(121.255669, 14.167425),
#'   dates = "30-06-2000",  # Wrong format
#'   duration = 120
#' )
#' # Error: dates[1] must be an ISO date ('YYYY-MM-DD')
#'
#' # This will fail (missing date specification)
#' get_wth(
#'   lonlat = c(121.255669, 14.167425),
#'   dates = "2000-06-30"  # No end date or duration
#' )
#' # Error: Provide dates as c(start, end) or supply duration
#' }
#'
#' @references
#' NASA POWER project (2019). Agroclimatology, Prediction Of Worldwide
#' Energy Resource. Funded by NASA's Applied Science Program.
#' https://power.larc.nasa.gov/
#'
#' Sparks, A. H., P. D. Esker, M. Bates, W. Dall'Acqua, Z. Guo, V. Segovia,
#' S. D. Silwal, S. Tolos, and K. A. Garrett. 2008. Ecology and Epidemiology
#' in R: Disease Progress over Time. *The Plant Health Instructor*.
#' \doi{10.1094/PHI-A-2008-0129-02}.
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @export
get_wth <- function(lonlat, dates, duration = NULL) {
  ll <- .validate_lonlat(lonlat)
  lon <- ll[1L]
  lat <- ll[2L]

  if (missing(dates) || length(dates) < 1L) {
    cli::cli_abort(
      "Provide {.arg dates} as 'YYYY-MM-DD' (start) or c(start, end)."
    )
  }

  start <- as.Date(dates[1L])
  if (is.na(start)) {
    cli::cli_abort("{.arg dates[1]} must be an ISO date ('YYYY-MM-DD').")
  }

  end <- .resolve_end_date(dates, start, duration)
  date_vec <- format(c(start, end), "%Y-%m-%d")

  wth <- data.table::as.data.table(
    nasapower::get_power(
      community = "AG",
      pars = c("T2M", "T2M_MIN", "T2M_MAX", "RH2M", "PRECTOTCORR"),
      temporal_api = "DAILY",
      lonlat = c(lon, lat),
      dates = date_vec
    )
  )

  .postprocess_wth(wth)
}

#' Validate Longitude and Latitude Coordinates
#'
#' Checks that coordinates are numeric, correct length, and within valid ranges.
#'
#' @param lonlat Numeric vector of c(longitude, latitude).
#'
#' @returns Numeric vector of c(longitude, latitude) as doubles.
#'
#' @details
#' Validates:
#' * Length is exactly 2
#' * Values are numeric (not character, factor, etc.)
#' * Longitude in \[-180, 180\] (East/West)
#' * Latitude in \[-90, 90\] (South/North)
#' * Neither value is NA or Inf
#'
#' @examplesIf interactive()
#' # Valid coordinates
#' .validate_lonlat(c(121.255669, 14.167425))  # IRRI, Philippines
#' .validate_lonlat(c(-99.1828, 19.1296))      # CIMMYT, Mexico
#'
#' # Invalid (will error)
#' \dontrun{
#' .validate_lonlat(c(200, 14))     # Longitude out of range
#' .validate_lonlat(c(121.25))      # Length not 2
#' .validate_lonlat(c("121", "14")) # Not numeric
#' }
#'
#' @autoglobal
#' @dev
.validate_lonlat <- function(lonlat) {
  if (!is.numeric(lonlat) || length(lonlat) != 2L) {
    cli::cli_abort(
      "{.arg lonlat} must be numeric length-2: {.code c(lon, lat)}."
    )
  }

  lon <- as.double(unname(lonlat[1L]))
  lat <- as.double(unname(lonlat[2L]))

  .check_coord(lon, "Longitude", -180, 180)
  .check_coord(lat, "Latitude", -90, 90)

  c(lon, lat)
}

#' Check Coordinate Value is Within Valid Range
#'
#' Validates a single coordinate value against min/max bounds.
#'
#' @param val Numeric value to check.
#' @param name Character name of coordinate (for error messages).
#' @param lo Numeric lower bound (inclusive).
#' @param hi Numeric upper bound (inclusive).
#'
#' @returns NULL (invisibly). Called for side effects (validation/abort).
#'
#' @details Aborts if value is NA, Inf, or outside \[lo, hi\].
#'
#' @autoglobal
#' @dev
.check_coord <- function(val, name, lo, hi) {
  if (is.na(val) || !is.finite(val) || val < lo || val > hi) {
    cli::cli_abort(
      "{name} must be in [{lo}, {hi}], got {val}."
    )
  }
}

#' Resolve End Date from Duration or Explicit End Date
#'
#' Determines the end date for weather retrieval using either:
#' 1. An explicit end date (from `dates` vector), or
#' 2. Duration calculation (start + duration)
#'
#' @section Logic:
#'
#' Priority order:
#' 1. If `duration` is provided (not NULL), use start + duration
#' 2. Else if `dates` has length >= 2, use `dates[2]` as end
#' 3. Else abort (neither duration nor end date provided)
#'
#' @param dates Character vector of dates from user input.
#' @param start Date start date (already parsed as Date class).
#' @param duration Integer duration (days) or NULL.
#'
#' @returns Date object representing the end date (inclusive).
#'
#' @details
#' Duration interpretation: `end = start + duration` means the window spans
#' exactly `duration` days.
#'
#' Example: start = 2000-06-30, duration = 3 gives end = 2000-07-02
#' (June 30, July 1, July 2 = 3 days total)
#'
#' @examplesIf interactive()
#' start <- as.Date("2000-06-30")
#'
#' # Using duration = 3
#' end1 <- .resolve_end_date(c("2000-06-30"), start, duration = 3L)
#' end1  # 2000-07-02
#' # Days: 06-30, 07-01, 07-02 (3 days)
#'
#' # Using explicit end date
#' end2 <- .resolve_end_date(c("2000-06-30", "2000-12-31"), start, NULL)
#' end2  # 2000-12-31
#'
#' @autoglobal
#' @dev
.resolve_end_date <- function(dates, start, duration) {
  if (!is.null(duration)) {
    if (!rlang::is_integerish(duration, finite = TRUE)) {
      cli::cli_abort(
        "{.arg duration} must be a positive integer or vector of positive integers."
      )
    }

    if (any(duration <= 0L)) {
      cli::cli_abort("{.arg duration} must be positive (> 0 days).")
    }

    # Duration: end = start + duration
    # Example: start=2000-06-30, duration=3 -> end=2000-07-02
    # Days spanned: 06-30, 07-01, 07-02 (3 days)
    return(start + as.integer(duration[1L]))
  }

  if (length(dates) >= 2L) {
    end <- as.Date(dates[2L])
    if (is.na(end)) {
      cli::cli_abort("{.arg dates[2]} must be an ISO date ('YYYY-MM-DD').")
    }
    return(end)
  }

  cli::cli_abort(
    "Provide {.arg dates} as c(start, end) or supply {.arg duration}."
  )
}

#' Post-Process NASA POWER Weather Data
#'
#' Reformats weather data retrieved from NASA POWER into the standard
#' \pkg{epicrop} format: renames columns, converts dates, orders columns,
#' and sets appropriate class attributes.
#'
#' @section Steps:
#'
#' 1. Remove temporary year/month/day columns (YEAR, MM, DD)
#' 2. Convert YYYYMMDD to Date class using [data.table::as.IDate()]
#' 3. Rename POWER columns to \pkg{epicrop} standards:
#'    - T2M → TEMP
#'    - T2M_MIN → TMIN
#'    - T2M_MAX → TMAX
#'    - RH2M → RHUM
#'    - PRECTOTCORR → RAIN
#' 4. Ensure DOY (day of year) is present
#' 5. Reorder columns to standard order (YYYYMMDD, DOY, TEMP, TMIN, ...)
#' 6. Set primary key on YYYYMMDD for efficient lookup
#' 7. Add `epicrop.wth` class attribute
#'
#' @param wth data.table from [nasapower::get_power()] result.
#'
#' @returns data.table of class `epicrop.wth`, ready for use in [seir()] and
#'   disease models.
#'
#' @autoglobal
#' @dev
.postprocess_wth <- function(wth) {
  # Remove temporary year/month/day columns
  wth[, c("YEAR", "MM", "DD") := NULL]

  # Convert YYYYMMDD string to Date class
  wth[, YYYYMMDD := data.table::as.IDate(wth$YYYYMMDD, format = "%Y%m%d")]

  # Rename POWER column names to epicrop standard
  rename_map <- c(
    T2M = "TEMP",
    T2M_MIN = "TMIN",
    T2M_MAX = "TMAX",
    PRECTOTCORR = "RAIN",
    RH2M = "RHUM"
  )

  for (old in names(rename_map)) {
    if (old %in% names(wth)) {
      data.table::setnames(wth, old, rename_map[[old]])
    }
  }

  # Define target column order
  target <- c(
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

  # Reorder: keep target columns first (in order), then any others
  keep <- intersect(target, names(wth))
  data.table::setcolorder(wth, c(keep, setdiff(names(wth), keep)))

  # Set key on date for efficient indexing
  data.table::setkey(wth, "YYYYMMDD")

  # Add epicrop.wth class (preserves data.table class)
  class(wth) <- union("epicrop.wth", class(wth))

  wth
}
