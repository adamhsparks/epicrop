#' Get Weather Data for Disease Modelling
#'
#' Retrieves daily weather data from either the NASA POWER database or ERA5
#' reanalysis (via the Open-Meteo Archive API) and formats it for use in
#' [seir()] and \pkg{epicrop} disease models.
#'
#' @section Data Source:
#'
#' * `"power"` (default): NASA POWER via [nasapower::get_power()], global daily
#'   data from 1984 onwards at 0.5-degree resolution.
#' * `"era5"`: ERA5 from 1940 onwards at about 0.25-degree resolution, roughly
#'   25 km, via the Open-Meteo Archive API
#'   (<https://open-meteo.com/en/docs/historical-weather-api>). Hourly values
#'   are aggregated to daily values (mean/min/max temperature, mean relative
#'   humidity, total precipitation) by local calendar day so that the output
#'   matches the POWER-derived columns. ERA5 data are typically available with
#'   a lag of about five days from 1940 onwards.
#'
#' Both sources are modelled/reanalysis products and may differ from local
#' weather station observations. Note that ERA5 precipitation is not the same
#' quantity as POWER's bias-corrected `PRECTOTCORR`, so rainfall totals will
#' differ between sources.
#'
#' @section Retrieved Variables:
#'
#' This function automatically provides:
#' * Mean daily temperature (2-meter height)
#' * Daily minimum temperature
#' * Daily maximum temperature
#' * Mean daily relative humidity (2-meter height)
#' * Total daily precipitation (bias-corrected for POWER)
#'
#' All variables are provided in standard units: temperature in degrees C,
#' humidity in %, rainfall in mm.
#'
#' @section Date Handling:
#'
#' Two methods for specifying the weather window:
#' 1. **Start and end dates** (character vector):
#'    `dates = c("2000-06-30", "2000-12-31")`
#' 2. **Start date + duration** (integer):
#'    `dates = "2000-06-30", duration = 120`
#'
#' Duration is interpreted as inclusive days: `end = start + duration - 1`. For
#' example, `duration = 120` starting on June 30 ends on October 27 (120 days
#' total).
#'
#' @param lonlat Numeric vector of coordinates as `c(longitude, latitude)`.
#'   Longitude: -180 to 180 degrees (West/East) Latitude: -90 to 90 degrees
#'   (South/North) Example: `c(121.255669, 14.167425)` for IRRI, Philippines
#'
#' @param dates Character vector of start date and optional end date, or
#'   character scalar start date. Format: `"YYYY-MM-DD"` (ISO 8601).
#'   * Length 1: Use with `duration` parameter
#'   * Length 2+: First is start, second is end (used only if `duration` is not
#'     supplied)
#' @param duration Optional integer. Number of days to retrieve (inclusive). If
#'   supplied, it takes precedence over any end date in `dates`. Must be
#'   positive (`> 0`). If not provided and `dates` has only one element, an
#'   error is raised.
#'
#' @param source Character. Weather data source, one of `"power"` (default) or
#'   `"era5"`.
#'
#' @returns
#' An `epicrop.wth` object ([data.table::data.table()] subclass) containing
#' daily weather data with columns:
#'
#' **Column** | **Description** | **Units**
#' -----------:|:---------|:----------
#' YYYYMMDD | Date | ISO 8601 (YYYY-MM-DD)
#' DOY | Day of year | Integer (1-365/366)
#' TEMP | Mean daily temperature | Degrees C
#' TMIN | Minimum daily temperature | Degrees C
#' TMAX | Maximum daily temperature | Degrees C
#' RHUM | Mean daily relative humidity | Percent (0-100)
#' RAIN | Total daily rainfall | Millimeters
#' LAT | Latitude of query location | Degrees (-90 to 90)
#' LON | Longitude of query location | Degrees (-180 to 180)
#'
#' Ready for direct use in [seir()], [bacterial_blight()], [leaf_blast()], and
#' other disease models.
#'
#' @examplesIf interactive()
#' # Example 1: Get weather for a date range
#' # IRRI Zeigler Experiment Station, wet season 2000
#' wth_range <- get_wth(
#'   lonlat = c(121.255669, 14.167425),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#' head(wth_range)
#' nrow(wth_range) # Number of days
#'
#' # Example 2: Get weather by start date and duration
#' # 120-day window from June 30, 2000
#' wth_duration <- get_wth(
#'   lonlat = c(121.255669, 14.167425),
#'   dates = "2000-06-30",
#'   duration = 120L
#' )
#' nrow(wth_duration) # Should be 120 rows
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
#' plot(
#'   bb_sim$simday,
#'   bb_sim$intensity,
#'   type = "l",
#'   xlab = "Days After Emergence",
#'   ylab = "Disease Intensity"
#' )
#'
#' # Example 5: Multiple locations - compare disease risk
#' locations <- data.frame(
#'   name = c("IRRI_Laguna", "BRRI_Gazipur", "CIMMYT_Mexico"),
#'   lon = c(121.255669, 90.2167, -99.1828),
#'   lat = c(14.167425, 24.0028, 19.1296)
#' )
#'
#' # Get weather for each location
#' wth_list <- lapply(
#'   1:nrow(locations),
#'   function(i) {
#'     get_wth(
#'       lonlat = c(locations$lon[i], locations$lat[i]),
#'       dates = "2000-06-30",
#'       duration = 120L
#'     )
#'   }
#' )
#' names(wth_list) <- locations$name
#'
#' # Run leaf blast model at each location
#' lb_results <- lapply(
#'   wth_list,
#'   function(w) leaf_blast(w, emergence = "2000-07-01")
#' )
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
#' wth_by_year <- lapply(
#'   years,
#'   function(y) {
#'     get_wth(
#'       lonlat = c(121.255669, 14.167425),
#'       dates = paste0(y, "-06-30"),
#'       duration = 120L
#'     )
#'   }
#' )
#' names(wth_by_year) <- as.character(years)
#'
#' # Run model for each year
#' bb_by_year <- lapply(
#'   wth_by_year,
#'   function(w) bacterial_blight(w, emergence = unique(w$YYYYMMDD)[1])
#' )
#'
#' # Compare disease progression across years
#' plot_data <- data.table::rbindlist(lapply(
#'   seq_along(bb_by_year),
#'   function(i) {
#'     data.frame(
#'       year = names(bb_by_year)[i],
#'       simday = bb_by_year[[i]]$simday,
#'       intensity = bb_by_year[[i]]$intensity
#'     )
#'   }
#' ))
#' plot(
#'   plot_data$simday,
#'   plot_data$intensity,
#'   col = as.factor(plot_data$year),
#'   pch = 16
#' )
#' legend(
#'   "topleft",
#'   legend = names(bb_by_year),
#'   col = seq_along(bb_by_year),
#'   pch = 16
#' )
#'
#' # Example 7: Error handling
#' \dontrun{
#' # This will fail (invalid coordinates)
#' get_wth(
#' lonlat = c(200, 95),  # Out of range
#' dates = "2000-06-30",
#' duration = 120
#' )
#' # Error: Longitude must be in [-180, 180], got 200
#'
#' # This will fail (invalid date format)
#' get_wth(
#' lonlat = c(121.255669, 14.167425),
#' dates = "30-06-2000",  # Wrong format
#' duration = 120
#' )
#' # Error: dates[1] must be an ISO date ('YYYY-MM-DD')
#'
#' # This will fail (missing date specification)
#' get_wth(
#' lonlat = c(121.255669, 14.167425),
#' dates = "2000-06-30"  # No end date or duration
#' )
#' # Error: Provide dates as c(start, end) or supply duration
#' }
#'
#' # Example 8: ERA5 data
#' # Get ERA5 data in the same format as POWER
#' wth_era5 <- get_wth(
#'   lonlat = c(121.255669, 14.167425),
#'   dates = "2000-06-30",
#'   duration = 120L,
#'   source = "era5"
#' )
#' head(wth_era5)
#'
#' @references
#' NASA POWER project (2019). Agroclimatology, Prediction Of Worldwide Energy
#' Resource. Funded by NASA's Applied Science Program.
#' https://power.larc.nasa.gov/.
#'
#' Hersbach, H. et al. (2020). The ERA5 global reanalysis. *Quarterly Journal
#' of the Royal Meteorological Society*, 146, 1999-2049.
#' \doi{10.1002/qj.3803}.
#'
#' Sparks, A. H., P. D. Esker, M. Bates, W. Dall'Acqua, Z. Guo, V. Segovia, S.
#' D. Silwal, S. Tolos, and K. A. Garrett. (2008). Ecology and Epidemiology in
#' R: Disease Progress over Time. *The Plant Health Instructor*.
#' \doi{10.1094/PHI-A-2008-0129-02}.
#'
#' Zippenfenig, P. (2023) Open-Meteo.com Weather API. Zenodo. doi:
#' \doi{10.5281/ZENODO.7970649}.
#'
#' @author
#' Adam H. Sparks, \email{adamhsparks@@gmail.com} with ERA5 support adapted
#' from code by Emerson M. Del Ponte \email{delponte@@ufv.br} from the
#' \CRANpkg{r4pde}.
#' @autoglobal
#' @export
get_wth <- function(
  lonlat,
  dates,
  duration = NULL,
  source = c("power", "era5")
) {
  source <- rlang::arg_match(source)

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

  switch(
    source,
    power = .get_power(lon, lat, start, end),
    era5 = .get_era5(lon, lat, start, end)
  )
}

#' Fetch Daily Weather from NASA POWER
#'
#' @param lon,lat Numeric longitude and latitude.
#' @param start,end Date start and end (inclusive).
#'
#' @returns data.table of class `epicrop.wth`.
#' @autoglobal
#' @dev
.get_power <- function(lon, lat, start, end) {
  wth <- data.table::as.data.table(nasapower::get_power(
    community = "AG",
    pars = c("T2M", "T2M_MIN", "T2M_MAX", "RH2M", "PRECTOTCORR"),
    temporal_api = "DAILY",
    lonlat = c(lon, lat),
    dates = format(c(start, end), "%Y-%m-%d")
  ))

  .postprocess_wth(wth)
}

#' Fetch Daily Weather from ERA5 via Open-Meteo
#'
#' Downloads hourly ERA5 data from the Open-Meteo Archive API and aggregates it
#' to daily values matching the columns of the POWER-derived output:
#' * `TEMP`: mean of hourly `temperature_2m`
#' * `TMIN`/`TMAX`: min/max of hourly `temperature_2m`
#' * `RHUM`: mean of hourly `relative_humidity_2m`
#' * `RAIN`: sum of hourly `precipitation`
#'
#' Hourly data are used because the Open-Meteo Archive API does not offer a
#' daily mean relative humidity variable.
#'
#' @section Time zones:
#' The request uses `timezone = "auto"`, so Open-Meteo resolves the location's
#' local time zone, interprets `start_date`/`end_date` as local calendar dates
#' and returns hourly timestamps in local time. Hours are grouped by their local
#' calendar date, so each daily value covers one local day.
#'
#' @param lon,lat Numeric longitude and latitude.
#' @param start,end Date start and end (inclusive).
#'
#' @returns data.table of class `epicrop.wth`.
#' @autoglobal
#' @dev
.get_era5 <- function(lon, lat, start, end) {
  req <- httr2::request("https://archive-api.open-meteo.com/v1/archive") |>
    httr2::req_url_query(
      latitude = lat,
      longitude = lon,
      start_date = format(start, "%Y-%m-%d"),
      end_date = format(end, "%Y-%m-%d"),
      hourly = "temperature_2m,relative_humidity_2m,precipitation",
      # local time zone of the location: dates are local calendar days and the
      # returned timestamps are local, so grouping by date gives local-day values
      timezone = "auto",
      models = "era5"
    ) |>
    httr2::req_timeout(60) |>
    httr2::req_retry(max_tries = 3) |>
    httr2::req_user_agent("epicrop") |>
    httr2::req_error(is_error = \(resp) FALSE) # handle HTTP errors ourselves

  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      cli::cli_abort("Failed to reach the Open-Meteo API.", parent = e)
    }
  )

  txt <- httr2::resp_body_string(resp)

  if (httr2::resp_is_error(resp)) {
    reason <- tryCatch(
      yyjsonr::read_json_str(txt)$reason,
      error = function(e) NULL
    )
    cli::cli_abort(c(
      "Open-Meteo API request failed (HTTP {httr2::resp_status(resp)}).",
      x = "{reason %||% 'Unknown error'}"
    ))
  }

  json <- yyjsonr::read_json_str(txt)

  if (is.null(json$hourly)) {
    cli::cli_abort(
      "Open-Meteo returned no hourly data for this location/period."
    )
  }

  hourly <- data.table::as.data.table(json$hourly)

  # yyjsonr gives whole-number arrays as integer and all-null arrays as a list;
  # coerce everything to double, with NA for null
  to_num <- function(x) {
    if (is.list(x)) {
      x <- vapply(
        x,
        function(v) if (is.null(v)) NA_real_ else as.numeric(v),
        numeric(1L)
      )
    }
    as.numeric(x)
  }

  vars <- c("temperature_2m", "relative_humidity_2m", "precipitation")
  hourly[, (vars) := lapply(.SD, to_num), .SDcols = vars]

  if (all(is.na(hourly$temperature_2m))) {
    cli::cli_abort(c(
      "Open-Meteo returned no ERA5 data for this location/period.",
      i = "ERA5 is typically available with a lag of about 5 days.",
      i = "Check that {.arg dates} are not too recent."
    ))
  }

  if (all(is.na(hourly$relative_humidity_2m))) {
    cli::cli_abort(c(
      "Open-Meteo returned no ERA5 relative humidity for this location/period.",
      i = "Disease models need {.field RHUM}; try again later."
    ))
  }

  if (all(is.na(hourly$precipitation))) {
    cli::cli_abort(c(
      "Open-Meteo returned no ERA5 precipitation for this location/period.",
      i = "Check that {.arg dates} are not too recent, or try again later."
    ))
  }

  # `time` is local ISO 8601 (e.g. "2000-06-30T13:00"), so its first ten
  # characters are the local calendar date
  hourly[, YYYYMMDD := data.table::as.IDate(substr(time, 1L, 10L))]

  # min/max that return NA rather than +/-Inf (and a warning) if all missing
  safe_stat <- function(x, f) {
    if (all(is.na(x))) NA_real_ else f(x, na.rm = TRUE)
  }

  wth <- hourly[,
    list(
      TEMP = mean(temperature_2m, na.rm = TRUE),
      TMIN = safe_stat(temperature_2m, min),
      TMAX = safe_stat(temperature_2m, max),
      RHUM = mean(relative_humidity_2m, na.rm = TRUE),
      RAIN = if (all(is.na(precipitation))) {
        NA_real_
      } else {
        sum(precipitation, na.rm = TRUE)
      }
    ),
    by = YYYYMMDD
  ]

  # mean() of an all-missing day is NaN; use NA throughout
  num_cols <- c("TEMP", "TMIN", "TMAX", "RHUM", "RAIN")
  wth[,
    (num_cols) := lapply(.SD, \(x) replace(x, is.nan(x), NA_real_)),
    .SDcols = num_cols
  ]

  wth[,
    `:=`(
      DOY = data.table::yday(YYYYMMDD),
      LAT = json$latitude,
      LON = json$longitude
    )
  ]

  .as_epicrop_wth(wth)
}

#' Finalise an epicrop.wth Object
#'
#' Orders columns, sets the key on `YYYYMMDD` and adds the `epicrop.wth` class.
#' Shared by the POWER and ERA5 code paths so both return identical structures.
#'
#' @param wth data.table with epicrop-standard column names.
#'
#' @returns data.table of class `epicrop.wth`.
#' @autoglobal
#' @dev
.as_epicrop_wth <- function(wth) {
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

  keep <- intersect(target, names(wth))
  data.table::setcolorder(wth, c(keep, setdiff(names(wth), keep)))
  data.table::setkey(wth, "YYYYMMDD")
  class(wth) <- union("epicrop.wth", class(wth))

  wth
}

#' Post-Process NASA POWER Weather Data
#'
#' Renames POWER columns to \pkg{epicrop} standards, converts dates and
#' finalises the object via `.as_epicrop_wth()`.
#'
#' @param wth data.table from [nasapower::get_power()].
#'
#' @returns data.table of class `epicrop.wth`.
#' @autoglobal
#' @dev
.postprocess_wth <- function(wth) {
  wth[, c("YEAR", "MM", "DD") := NULL]
  wth[, YYYYMMDD := data.table::as.IDate(YYYYMMDD, format = "%Y%m%d")]

  data.table::setnames(
    wth,
    old = c("T2M", "T2M_MIN", "T2M_MAX", "PRECTOTCORR", "RH2M"),
    new = c("TEMP", "TMIN", "TMAX", "RAIN", "RHUM"),
    skip_absent = TRUE
  )

  .as_epicrop_wth(wth)
}

#' Resolve End Date from Duration or Explicit End Date
#'
#' Determines the end date for weather retrieval using either:
#' 1. A `duration` (inclusive days from `start`), or
#' 2. An explicit end date (from the `dates` vector)
#'
#' @section Logic:
#'
#' Priority order:
#' 1. If `duration` is provided (not NULL), use `start + duration - 1`
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
#' Duration is inclusive: `end = start + duration - 1`, so the window spans
#' exactly `duration` days.
#'
#' Example: start = 2000-06-30, duration = 3 gives end = 2000-07-02 (June 30,
#' July 1, July 2 = 3 days total)
#'
#' @examplesIf interactive()
#' start <- as.Date("2000-06-30")
#'
#' # Using duration = 3
#' end1 <- .resolve_end_date(c("2000-06-30"), start, duration = 3L)
#' end1 # 2000-07-02
#' # Days: 06-30, 07-01, 07-02 (3 days)
#'
#' # Using explicit end date
#' end2 <- .resolve_end_date(c("2000-06-30", "2000-12-31"), start, NULL)
#' end2 # 2000-12-31
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

    # Inclusive: start + duration - 1 spans exactly `duration` days
    return(start + as.integer(duration[1L]) - 1L)
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
#' .validate_lonlat(c(121.255669, 14.167425)) # IRRI, Philippines
#' .validate_lonlat(c(-99.1828, 19.1296))     # CIMMYT, Mexico
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
    cli::cli_abort("{name} must be in [{lo}, {hi}], got {val}.")
  }
}
