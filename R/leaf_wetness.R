#' Calculate leaf wetness duration (wet-hours) from relative humidity and rainfall
#'
#' Calculates daily leaf wetness duration as wet-hours from 0 to 24 using
#' relative humidity and rainfall. The wet-hours output can then be mapped to an
#' infection-rate multiplier from 0 to 1 using an RcW curve (hours -> 0 to 1).
#'
#' Two modes are supported:
#' * Unweighted (default): counts the number of hourly RH values >= rhlim
#'   (0 to 24 wet-hours).
#' * Weighted (weighted_wetness = TRUE): converts each hourly RH value to a
#'   wetness weight from 0 to 1 (0 below 80%, 1 at/above rhlim) and sums weights
#'   across 24 hours (0 to 24 wet-hours).
#'
#' Rainfall contributes additional wet-hours proportional to RAIN / rainlim,
#' capped at 24, and combined without double-counting by scaling to the
#' remaining dry fraction of the day. If TMIN/TMAX are missing, the function
#' falls back to a binary wetness day (0 or 24 wet-hours) based on rhlim/rainlim
#' thresholds.
#'
#' @param wth Weather data in the proper format.
#' @param rhlim Relative humidity threshold (%) used to determine wetness.
#' @param rainlim Rainfall scaling threshold (mm per wet-hour equivalent).
#' @param weighted_wetness If TRUE, compute weighted hourly wetness (0 to 24);
#'   if FALSE, count hours where hourly RH >= rhlim (0 to 24).
#'
#' @returns A numeric vector of daily wet-hours from 0 to 24, length nrow(wth).
#'   Use with RcW (hours -> 0 to 1) to obtain the wetness multiplier for
#'   infection.
#'
#' @examples
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-07-31")
#' )
#'
#' wet_simple <- .calculate_leaf_wetness(
#'   wth = wth,
#'   rhlim = 90,
#'   rainlim = 5,
#'   weighted_wetness = FALSE
#' )
#' summary(wet_simple)
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @dev
.calculate_leaf_wetness <- function(
  wth,
  rhlim,
  rainlim,
  weighted_wetness = FALSE
) {
  n <- nrow(wth)

  # Validate required columns
  if (!rlang::has_name(wth, "RHUM")) {
    cli::cli_abort("{.field RHUM} column missing from weather data")
  }

  if (!rlang::has_name(wth, "DOY")) {
    cli::cli_abort("{.field DOY} column missing from weather data")
  }

  # Extract data
  rh <- wth$RHUM
  prec <- wth$RAIN %||% rep(0, n)
  prec[is.na(prec)] <- 0

  rainlim <- max(rainlim, .Machine$double.eps)

  # Fallback to binary wetness if TMIN/TMAX missing
  if (!rlang::has_name(wth, "TMIN") || !rlang::has_name(wth, "TMAX")) {
    return(data.table::fifelse(rh >= rhlim | prec >= rainlim, 24L, 0L))
  }

  # Get temperature data
  tmp <- wth$TEMP %||% wth$T2M %||% ((wth$TMIN + wth$TMAX)) / 2L

  if (is.null(tmp)) {
    cli::cli_abort("{.field TEMP} cannot be derived from available columns")
  }

  # Get latitude
  if (!rlang::has_name(wth, "LAT")) {
    cli::cli_abort("{.field LAT} required for hourly wetness calculation")
  }

  lat <- wth$LAT
  if (length(lat) == 1L) {
    lat <- rep(lat, n)
  }

  # Calculate hourly RH
  rh_hr <- .diurnal_rh(
    rh = rh,
    tmin = wth$TMIN,
    tmax = wth$TMAX,
    tmp = tmp,
    lat = lat,
    doy = wth$DOY
  )

  # Count wet hours
  if (weighted_wetness) {
    lw <- vapply(
      rh_hr,
      function(x) {
        sum(.calculate_weighted_hr_wet(x, rhlim))
      },
      numeric(1)
    )
  } else {
    lw <- vapply(rh_hr, function(x) sum(x >= rhlim), numeric(1))
  }

  # Add rain contribution (combine without double counting)
  wet_rain_hours <- pmin(24, prec / rainlim)
  wet_total <- lw + (1 - lw / 24) * wet_rain_hours

  # Clamp to [0, 24]
  pmin(24, pmax(0, wet_total))
}

#' Calculate hourly relative humidity for a given day
#'
#' @param rh Mean daily relative humidity.
#' @param tmin Minimum temperature.
#' @param tmax Maximum temperature.
#' @param tmp Mean daily temperature.
#' @param lat Latitude.
#' @param doy Day of year.
#'
#' @returns List of numeric vectors (one per day, 24 values each).
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @dev
.diurnal_rh <- function(rh, tmin, tmax, tmp, lat, doy) {
  n <- length(rh)
  rh_hr <- vector("list", n)

  for (i in seq_len(n)) {
    # Vapour pressure (constant through day)
    vp <- .saturated_vapour_pressure(tmp[i]) * rh[i] / 100

    # Hourly temperature
    hrtemp <- .diurnal_temp(lat[i], doy[i], tmin[i], tmax[i])

    # Hourly saturated vapour pressure
    es <- .saturated_vapour_pressure(hrtemp)

    # Hourly RH (clamped to [0, 100])
    rh_hr[[i]] <- pmin(100, pmax(0, 100 * vp / es))
  }

  rh_hr
}

#' Calculate hourly temperature for a given day or multiple days
#'
#' @param lat Latitude (scalar or vector).
#' @param doy Day of year (scalar or vector).
#' @param tmin Minimum temperature (scalar or vector).
#' @param tmax Maximum temperature (scalar or vector).
#'
#' @returns Numeric vector of 24 hourly temperatures for single day, or
#'   numeric vector of 24*n hourly temperatures for multiple days.
#'
#' @author Robert J. Hijmans with modifications by Adam H. Sparks
#' @autoglobal
#' @dev
.diurnal_temp <- function(lat, doy, tmin, tmax) {
  # Determine length and recycle vectors
  n <- max(length(lat), length(doy), length(tmin), length(tmax))

  if (n == 0L) {
    return(numeric(0))
  }

  # Recycle to common length
  lat <- rep_len(lat, n)
  doy <- rep_len(doy, n)
  tmin <- rep_len(tmin, n)
  tmax <- rep_len(tmax, n)

  # Calculate day length parameters (vectorized)
  dayl <- .day_length(lat, doy)
  nightl <- 24 - dayl
  cs_1 <- 0.5 * dayl
  sunris <- 12 - cs_1
  sunset <- 12 + cs_1

  # Sunset temperature
  tsunst <- tmin + (tmax - tmin) * sin(pi * dayl / (dayl + 3))

  # Pre-compute exponential terms
  exp_night <- exp(-nightl / 4)
  denom <- 1 - exp_night

  # Create expanded grid: n days × 24 hours
  # Repeat each day's parameters 24 times
  n_total <- n * 24L
  day_idx <- rep(seq_len(n), each = 24L)
  hr <- rep(1:24, times = n)

  # Vectorized parameter lookup
  tmin_v <- tmin[day_idx]
  tmax_v <- tmax[day_idx]
  dayl_v <- dayl[day_idx]
  sunris_v <- sunris[day_idx]
  sunset_v <- sunset[day_idx]
  tsunst_v <- tsunst[day_idx]
  exp_night_v <- exp_night[day_idx]
  denom_v <- denom[day_idx]

  # Initialize result
  hr_temp <- numeric(n_total)

  # Vectorized calculation for all three periods
  # Before sunrise
  idx_before <- hr < sunris_v
  hr_temp[idx_before] <- (tmin_v[idx_before] -
    tsunst_v[idx_before] * exp_night_v[idx_before] +
    (tsunst_v[idx_before] - tmin_v[idx_before]) *
      exp(-(hr[idx_before] + 24 - sunset_v[idx_before]) / 4)) /
    denom_v[idx_before]

  # Daytime (sunrise to sunset)
  idx_day <- hr >= sunris_v & hr < sunset_v
  hr_temp[idx_day] <- tmin_v[idx_day] +
    (tmax_v[idx_day] - tmin_v[idx_day]) *
      sin(pi * (hr[idx_day] - sunris_v[idx_day]) / (dayl_v[idx_day] + 3))

  # After sunset
  idx_after <- hr >= sunset_v
  hr_temp[idx_after] <- (tmin_v[idx_after] -
    tsunst_v[idx_after] * exp_night_v[idx_after] +
    (tsunst_v[idx_after] - tmin_v[idx_after]) *
      exp(-(hr[idx_after] - sunset_v[idx_after]) / 4)) /
    denom_v[idx_after]

  return(hr_temp)
}
# Sunset temperature

#' Calculate day length for a given latitude and date
#'
#' @param lat Latitude (degrees).
#' @param doy Day of year (1-365).
#'
#' @returns Numeric day length in hours.
#'
#' @references
#' Forsythe et al. (1995) Ecological Modeling 80:87-95.
#' \doi{10.1016/0304-3800(94)00034-F}
#'
#' @author Robert J. Hijmans, modifications by Adam H. Sparks
#' @autoglobal
#' @dev
.day_length <- function(lat, doy) {
  # Normalize DOY to [1, 365]
  doy <- (doy - 1) %% 365 + 1

  # Solar declination
  p <- asin(
    0.39795 * cos(0.2163108 + 2 * atan(0.9671396 * tan(0.00860 * (doy - 186))))
  )

  # Hour angle
  lat_rad <- lat * pi / 180
  a <- (sin(0.8333 * pi / 180) + sin(lat_rad) * sin(p)) /
    (cos(lat_rad) * cos(p))
  a <- pmin(pmax(a, -1), 1) # Clamp to [-1, 1]

  return(24 - 7.639437 * acos(a))
}

#' Calculate saturated vapour pressure
#'
#' @param tmp Temperature (°C).
#' @returns Saturated vapour pressure (kPa).
#'
#' @references Alduchov and Eskridge (1996) J. Appl. Meteor. 35:601-609.
#' \doi{10.1175/1520-0450(1996)035<0601:IMFAOS>2.0.CO;2}
#'
#' @author Adam H. Sparks
#' @dev
.saturated_vapour_pressure <- function(tmp) {
  0.61094 * exp((17.625 * tmp) / (tmp + 243.04))
}

#' Calculate weighted hourly leaf wetness
#'
#' @param rh Hourly RH values (%).
#' @param rhlim RH threshold for full wetness (%).
#'
#' @returns Weighted wetness values \[0, 1\].
#'
#' @author Adam H. Sparks
#' @autoglobal
#' @dev
.calculate_weighted_hr_wet <- function(rh, rhlim) {
  if (rhlim < 80) {
    cli::cli_abort(
      "{.arg rhlim} must be >= 80 for weighted wetness (got {rhlim})"
    )
  }

  # Vectorized calculation
  w <- (rh - 80) / (rhlim - 80)
  w[rh >= rhlim] <- 1
  w[rh < 80] <- 0

  return(w)
}
