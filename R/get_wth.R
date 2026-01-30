#' Get Weather Data for Modelling Disease
#'
#' This function is a wrapper for the [nasapower::get_power()] with predefined
#'  parameters suitable for use in \pkg{epicrop}.
#'
#' @param lonlat A numeric vector of geographic coordinates for a cell or region
#'  entered as lon (x), lat (y) coordinates.
#' @param dates A character vector of start and end dates in that order.
#' @param duration A numeric value indicating how many days a single
#'  growing season being simulated is to be.  If this is used, only the start
#'  date will be used and the end date will be ignored if supplied.  This must
#'  match the `duration` parameter value passed along to [epicrop_sim()] or any
#'  of the `predict` family of functions.
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
#' @export

get_wth <- function(lonlat, dates, duration = NULL) {
  # --- existing checks ---
  if (!is.numeric(lonlat) || length(lonlat) != 2L) {
    cli::cli_abort(
      "{.arg lonlat} must be numeric length-2: {.code c(lon, lat)}."
    )
  }

  lon <- as.double(unname(lonlat[1L]))
  lat <- as.double(unname(lonlat[2L]))

  if (is.na(lon) || lon < -180 || lon > 180) {
    cli::cli_abort("Longitude must be in [-180, 180], got {lon}.")
  }
  if (is.na(lat) || lat < -90 || lat > 90) {
    cli::cli_abort("Latitude must be in [-90, 90], got {lat}.")
  }

  if (missing(dates) || length(dates) < 1L) {
    cli::cli_abort(
      "Provide {.arg dates} as 'YYYY-MM-DD' (start) or c(start, end)."
    )
  }
  start <- as.Date(dates[1L])
  if (is.na(start)) {
    cli::cli_abort("{.arg dates[1]} must be an ISO date ('YYYY-MM-DD').")
  }

  if (!is.null(duration) && !missing(duration)) {
    if (
      !rlang::is_integerish(duration, n = 1, finite = TRUE) || duration <= 0L
    ) {
      cli::cli_abort(
        "{.arg duration} must be a single positive integer (days)."
      )
    }
    end <- start + (as.integer(duration)) # inclusive
  } else if (length(dates) >= 2L) {
    end <- as.Date(dates[2L])
    if (is.na(end)) {
      cli::cli_abort("{.arg dates[2]} must be an ISO date ('YYYY-MM-DD').")
    }
  }

  date_vec <- c(format(start, "%Y-%m-%d"), format(end, "%Y-%m-%d"))

  wth <- data.table::as.data.table(
    nasapower::get_power(
      community = "AG",
      pars = c("T2M", "T2M_MIN", "T2M_MAX", "RH2M", "PRECTOTCORR"),
      temporal_api = "DAILY",
      lonlat = c(lon, lat),
      dates = date_vec
    )
  )
  wth[, c("YEAR", "MM", "DD") := NULL]
  wth[,
    YYYYMMDD := data.table::as.IDate(
      wth$YYYYMMDD,
      format = "%Y%m%d"
    )
  ]

  rename_map <- c(
    "T2M" = "TEMP",
    "T2M_MIN" = "TMIN",
    "T2M_MAX" = "TMAX",
    "PRECTOTCORR" = "RAIN",
    "RH2M" = "RHUM"
  )
  for (old in names(rename_map)) {
    if (old %in% names(wth)) data.table::setnames(wth, old, rename_map[[old]])
  }

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
