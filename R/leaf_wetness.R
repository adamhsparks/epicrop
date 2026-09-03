#' Calculate Leaf Wetness Duration (Wet-Hours) from Relative Humidity and Rainfall
#'
#' Calculates daily leaf wetness duration as wet-hours (0 to 24) from relative
#' humidity, temperature, and rainfall. The output can be mapped to an
#' infection-rate multiplier (0 to 1) using an RcW curve or a simple linear
#' relationship.
#'
#' @section Calculation Methods:
#'
#' Two modes are supported:
#' * **Unweighted** (default): Counts hourly RH values >= `rhlim` (0-24 hours)
#' * **Weighted**: Converts hourly RH to a wetness weight (0 at 80%, 1 at
#'   `rhlim`%) and sums across 24 hours (0-24 wet-hours)
#'
#' Rainfall contributes additional wet-hours (`RAIN / rainlim`, capped at 24)
#' combined with humidity wet-hours without double-counting by scaling to the
#' remaining dry fraction.
#'
#' **Fallback behavior:** If TMIN/TMAX are missing, returns binary wetness
#' (0 or 24 hours) based on `rhlim` and `rainlim` thresholds.
#'
#' @section Temperature and RH Interpolation:
#'
#' Hourly RH is derived from daily mean RH and temperature via:
#' 1. Compute vapour pressure from mean RH and mean temperature
#' 2. Calculate hourly temperature using a sine/exponential diurnal model
#' 3. Compute hourly saturated vapour pressure
#' 4. Calculate hourly RH = 100 * (vapour pressure / saturated pressure)
#'
#' This approach accounts for the fact that RH varies inversely with temperature
#' during the day (peaks at dawn/dusk, drops at noon) even when vapour pressure
#' is constant.
#'
#' @param wth Weather data (data.frame or data.table) with columns:
#'   * `RHUM`: Mean daily relative humidity (%)
#'   * `RAIN`: Daily rainfall (mm)
#'   * `DOY`: Day of year (1-365)
#'   * `TEMP`: Mean daily temperature (degrees C); or both TMIN/TMAX
#'   * `TMIN`: Minimum daily temperature (degrees C)
#'      \[optional if TEMP present\]
#'   * `TMAX`: Maximum daily temperature (degrees C)
#'      \[optional if TEMP present\]
#'   * `LAT`: Latitude (degrees) \[required for hourly calculation\]
#' @param rhlim Relative humidity threshold (%, default 90) for determining
#'   leaf wetness. Values >= `rhlim` are considered wet.
#' @param rainlim Rainfall threshold (mm, default 5) for contributing to
#'   wet-hours. Each mm of rain = 1/rainlim hours of additional wetness.
#' @param weighted_wetness Logical. If TRUE, compute weighted hourly RH
#'   (0-1 scale from 80% to `rhlim`); if FALSE (default), count binary
#'   wet hours (RH >= `rhlim`).
#'
#' @returns Numeric vector of daily wet-hours (0-24), length = `nrow(wth)`.
#'   Can be passed to an RcW curve to obtain the infection-rate multiplier
#'   (0-1).
#'
#' @examplesIf interactive()
#' # Example 1: Basic leaf wetness calculation
#' wth <- get_wth(
#'   lonlat = c(121.255669, 14.16742),
#'   dates = "2000-06-30",
#'   duration = 10L
#' )
#'
#' # Unweighted wetness (count hours RH >= 90%)
#' wet_hours <- .calculate_leaf_wetness(
#'   wth = wth,
#'   rhlim = 90,
#'   rainlim = 5,
#'   weighted_wetness = FALSE
#' )
#' summary(wet_hours)
#' # Typical range: 0-24 hours per day
#'
#' # Example 2: Weighted wetness for more sensitive pathogen model
#' wet_weighted <- .calculate_leaf_wetness(
#'   wth = wth,
#'   rhlim = 90,
#'   rainlim = 5,
#'   weighted_wetness = TRUE
#' )
#' head(cbind(wth$YYYYMMDD, wet_hours, wet_weighted), 10)
#'
#' # Example 3: Using with RcW curve for infection rate
#' # Linear relationship: 0 hours = 0 modifier, 24 hours = 1 modifier
#' wet_hrs <- .calculate_leaf_wetness(wth, rhlim = 90, rainlim = 5)
#' Rc_wet <- pmin(1, wet_hrs / 24)  # Linear interpolation
#' plot(wth$DOY, wet_hrs, type = "l", xlab = "Day of Year", ylab = "Wet-Hours")
#'
#' # Example 4: Effect of rainfall
#' # Create synthetic weather with same RH but varying rain
#' wth_dry <- wth[RAIN < 2]
#' wth_wet <- wth[RAIN >= 2]
#' wet_dry <- .calculate_leaf_wetness(wth_dry, rhlim = 90, rainlim = 5)
#' wet_rain <- .calculate_leaf_wetness(wth_wet, rhlim = 90, rainlim = 5)
#' mean(wet_dry, na.rm = TRUE)  # Without rain
#' mean(wet_rain, na.rm = TRUE) # With rain
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

  # Extract data with fallbacks
  rh <- wth$RHUM
  prec <- if (rlang::has_name(wth, "RAIN")) wth$RAIN else rep(0.0, n)
  prec[is.na(prec)] <- 0.0

  rainlim <- max(rainlim, .Machine$double.eps)

  # Fallback to binary wetness if TMIN/TMAX missing
  if (!rlang::has_name(wth, "TMIN") || !rlang::has_name(wth, "TMAX")) {
    return(
      if (data.table::is.data.table(wth)) {
        data.table::fifelse(rh >= rhlim | prec >= rainlim, 24L, 0L)
      } else {
        ifelse(rh >= rhlim | prec >= rainlim, 24L, 0L)
      }
    )
  }

  # Get mean temperature (prefer TEMP, fall back to (TMIN+TMAX)/2)
  tmp <- if (rlang::has_name(wth, "TEMP")) {
    wth$TEMP
  } else if (rlang::has_name(wth, "T2M")) {
    wth$T2M
  } else {
    (wth$TMIN + wth$TMAX) / 2L
  }

  if (is.null(tmp)) {
    cli::cli_abort("{.field TEMP} or {.field TMIN}/{.field TMAX} required")
  }

  # Get latitude
  if (!rlang::has_name(wth, "LAT")) {
    cli::cli_abort("{.field LAT} required for hourly wetness calculation")
  }

  lat <- wth$LAT
  if (length(lat) == 1L) {
    lat <- rep(lat, n)
  }

  # Calculate hourly RH via diurnal interpolation
  rh_hr <- .diurnal_rh(
    rh = rh,
    tmin = wth$TMIN,
    tmax = wth$TMAX,
    tmp = tmp,
    lat = lat,
    doy = wth$DOY
  )

  # Count wet hours (unweighted or weighted)
  if (weighted_wetness) {
    lw <- vapply(
      rh_hr,
      function(x) sum(.calculate_weighted_hr_wet(x, rhlim)),
      numeric(1)
    )
  } else {
    lw <- vapply(rh_hr, function(x) sum(x >= rhlim), numeric(1))
  }

  # Add rain contribution (combine without double counting)
  wet_rain_hours <- pmin(24L, prec / rainlim)
  wet_total <- lw + (1L - lw / 24L) * wet_rain_hours

  # Clamp to [0, 24]
  pmin(24L, pmax(0L, wet_total))
  return(as.integer(wet_total))
}

#' Calculate Hourly Relative Humidity for a Given Day
#'
#' Derives hourly relative humidity from daily mean RH and hourly temperature
#' by maintaining constant vapour pressure (VP) throughout the day.
#'
#' @section Algorithm:
#'
#' 1. Compute vapour pressure from mean RH and mean temperature (constant all
#'    day)
#' 2. Compute hourly saturated vapour pressure using diurnal temperature
#' 3. Hourly RH = \eqn{100 * (VP / saturated VP)}, clamped to \[0, 100\]
#'
#' This accounts for the inverse relationship between temperature and RH even
#' when actual water vapour content is unchanged.
#'
#' @param rh Mean daily relative humidity (%).
#' @param tmin Minimum daily temperature (C).
#' @param tmax Maximum daily temperature (C).
#' @param tmp Mean daily temperature (C).
#' @param lat Latitude (degrees).
#' @param doy Day of year (1-365).
#'
#' @returns List of numeric vectors (one per day, 24 values each representing
#'   hourly RH %).
#'
#' @examplesIf interactive()
#' # Calculate hourly RH for 3 days
#' rh_list <- .diurnal_rh(
#'   rh = c(80, 85, 90),           # Mean daily RH (%)
#'   tmin = c(20, 19, 21),         # Min temp (C)
#'   tmax = c(30, 32, 29),         # Max temp (C)
#'   tmp = c(25, 25.5, 25),        # Mean temp (C)
#'   lat = c(14.16742, 14.16742, 14.16742),  # Latitude
#'   doy = c(182, 183, 184)        # Day of year
#' )
#'
#' # Each element is 24-hour RH profile
#' str(rh_list)
#' plot(1:24, rh_list[[1]], type = "l", ylim = c(0, 100),
#'      xlab = "Hour of Day", ylab = "Relative Humidity (%)")
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @dev
.diurnal_rh <- function(rh, tmin, tmax, tmp, lat, doy) {
  n <- length(rh)
  rh_hr <- vector("list", n)

  for (i in seq_len(n)) {
    # Vapor pressure (constant through day)
    vp <- .saturated_vapour_pressure(tmp[i]) * rh[i] / 100.0

    # Hourly temperature via sine/exponential model
    hrtemp <- .diurnal_temp(lat[i], doy[i], tmin[i], tmax[i])

    # Hourly saturated vapour pressure
    es <- .saturated_vapour_pressure(hrtemp)

    # Hourly RH (clamped to [0, 100])
    rh_hr[[i]] <- pmin(100.0, pmax(0.0, 100.0 * vp / es))
  }

  rh_hr
}

#' Calculate Hourly Temperature for a Given Day
#'
#' Computes a full diurnal temperature curve from minimum, maximum, and mean
#' temperatures using a sine wave (day) and exponential decay (night) model.
#'
#' @section Algorithm:
#'
#' Temperature varies sinusoidally during the day (sunrise to sunset) and
#' exponentially decays at night. Day length depends on latitude and day of year.
#' The model ensures continuity at sunrise and sunset.
#'
#' @param lat Latitude (degrees; scalar or vector).
#' @param doy Day of year (1-365; scalar or vector).
#' @param tmin Minimum daily temperature (degrees C; scalar or vector).
#' @param tmax Maximum daily temperature (degrees C; scalar or vector).
#'
#' @returns Numeric vector of 24 hourly temperatures (for single day or
#'   vectorised 24*n values for multiple days).
#'
#' @examplesIf interactive()
#' # Single day: latitude 14N, DOY 182 (Jul 1), Tmin=20, Tmax=30
#' hourly_temp <- .diurnal_temp(
#'   lat = 14.16742,
#'   doy = 182,
#'   tmin = 20,
#'   tmax = 30
#' )
#' plot(1:24, hourly_temp, type = "l", ylim = c(15, 35),
#'      xlab = "Hour of Day", ylab = "Temperature (degrees C)",
#'      main = "Diurnal Temperature Curve")
#' abline(h = c(20, 30), col = "red", lty = 2)
#'
#' # Multiple days (vectorised)
#' hourly_temps <- .diurnal_temp(
#'   lat = rep(14.16742, 3),
#'   doy = 182:184,
#'   tmin = c(20, 19, 21),
#'   tmax = c(30, 32, 29)
#' )
#' length(hourly_temps)  # Should be 24 * 3 = 72
#'
#' @author Robert J. Hijmans (original); Adam H. Sparks (modifications)
#' @autoglobal
#' @dev
.diurnal_temp <- function(lat, doy, tmin, tmax) {
  # Determine length and recycle vectors
  n <- max(length(lat), length(doy), length(tmin), length(tmax))

  if (n == 0L) {
    return(numeric(0))
  }

  # Recycle to common length using base R
  lat <- rep_len(lat, n)
  doy <- rep_len(doy, n)
  tmin <- rep_len(tmin, n)
  tmax <- rep_len(tmax, n)

  # Calculate day length parameters (vectorised)
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

  # Create expanded grid: n days * 24 hours
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

  # Before sunrise: exponential rise from Tmin toward Tsunst
  idx_before <- hr < sunris_v
  hr_temp[idx_before] <- (tmin_v[idx_before] -
    tsunst_v[idx_before] * exp_night_v[idx_before] +
    (tsunst_v[idx_before] - tmin_v[idx_before]) *
      exp(-(hr[idx_before] + 24 - sunset_v[idx_before]) / 4)) /
    denom_v[idx_before]

  # Daytime (sunrise to sunset): sine wave from Tmin to Tmax
  idx_day <- hr >= sunris_v & hr < sunset_v
  hr_temp[idx_day] <- tmin_v[idx_day] +
    (tmax_v[idx_day] - tmin_v[idx_day]) *
      sin(pi * (hr[idx_day] - sunris_v[idx_day]) / (dayl_v[idx_day] + 3))

  # After sunset: exponential decay from Tsunst toward Tmin
  idx_after <- hr >= sunset_v
  hr_temp[idx_after] <- (tmin_v[idx_after] -
    tsunst_v[idx_after] * exp_night_v[idx_after] +
    (tsunst_v[idx_after] - tmin_v[idx_after]) *
      exp(-(hr[idx_after] - sunset_v[idx_after]) / 4)) /
    denom_v[idx_after]

  return(hr_temp)
}

#' Calculate Day Length for a Given Latitude and Date
#'
#' Computes solar day length (sunrise to sunset) in hours using latitude and
#' day of year. Based on solar declination and hour angle.
#'
#' @section Formula:
#'
#' Uses solar declination via Forsythe et al. (1995):
#' ```
#' p = arcsin(0.39795 * cos(0.2163108 + 2*arctan(0.9671396*tan(0.008600*(doy-186)))))
#' a = (sin(0.8333 degrees) + sin(lat)*sin(p)) / (cos(lat)*cos(p))
#' day_length = 24 - 7.639437 * arccos(clamp(a, -1, 1))
#' ```
#'
#' @param lat Latitude (degrees; scalar or vector).
#' @param doy Day of year (1-365; scalar or vector).
#'
#' @returns Numeric day length (hours; scalar or vector).
#'
#' @examplesIf interactive()
#' # Day length at equator vs. poles across year
#' lats <- c(0, 30, 60, 90)
#' doy_range <- seq(1, 365, by = 30)
#'
#' plot(doy_range, rep(12, length(doy_range)), type = "n",
#'      ylim = c(0, 24), xlab = "Day of Year", ylab = "Day Length (hours)",
#'      main = "Seasonal Variation in Day Length")
#'
#' for (lat in lats) {
#'   dayl <- .day_length(lat, doy_range)
#'   lines(doy_range, dayl, label = paste("Lat", lat))
#' }
#' legend("right", paste("Lat", lats), lty = 1:4)
#'
#' # Extremes: minimum daylight at winter solstice (DOY 355)
#' .day_length(lat = 60, doy = 355)  # High latitude: ~5-6 hours
#' .day_length(lat = 0, doy = 355)   # Equator: ~12 hours (constant)
#'
#' @references
#' Forsythe, W. C., E. J. Rykiel Jr., R. S. Stahl, H. Wu, and R. M. Schoolfield
#' (1995). A model comparison for daylength as a function of latitude and day of
#' year. *Ecological Modeling*, 80:87-95.
#' \doi{10.1016/0304-3800(94)00034-F}
#'
#' @author Robert J. Hijmans, modifications by Adam H. Sparks
#' @autoglobal
#' @dev
.day_length <- function(lat, doy) {
  # Normalize DOY to [1, 365]
  doy <- (doy - 1) %% 365 + 1

  # Solar declination (radians)
  p <- asin(
    0.39795 * cos(0.2163108 + 2 * atan(0.9671396 * tan(0.008600 * (doy - 186))))
  )

  # Hour angle (radians)
  lat_rad <- lat * pi / 180
  a <- (sin(0.8333 * pi / 180) + sin(lat_rad) * sin(p)) /
    (cos(lat_rad) * cos(p))
  a <- pmin(pmax(a, -1), 1) # Clamp to [-1, 1] to avoid NaN from acos

  # Day length (hours)
  24 - 7.639437 * acos(a)
}

#' Calculate Saturated Vapor Pressure
#'
#' Computes saturated vapour pressure from temperature using the Alduchov and
#' Eskridge (1996) formula, valid from -40 to +50 degrees C.
#'
#' @section Formula:
#'
#' ```
#' es = 0.61094 * exp((17.625 * T) / (T + 243.04))
#' ```
#'
#' where T is temperature in degrees C and es is in kilopascals (kPa).
#'
#' @param tmp Temperature (degrees C; scalar or vector).
#'
#' @returns Saturated vapour pressure (kPa; scalar or vector).
#'
#' @examplesIf interactive()
#' # Vapor pressure curve over typical temperature range
#' temps <- seq(0, 40, by = 5)
#' es <- .saturated_vapour_pressure(temps)
#' plot(temps, es, type = "b", xlab = "Temperature (degrees C)",
#'      ylab = "es (kPa)",
#'      main = "Saturated Vapor Pressure")
#'
#' # Rapid increase at higher temps (why RH drops in afternoon)
#' es_20 <- .saturated_vapour_pressure(20)
#' es_30 <- .saturated_vapour_pressure(30)
#' es_30 / es_20  # ~1.5x increase for 10 degrees C rise
#'
#' @references
#' Alduchov, O. A., and R. E. Eskridge (1996). Improved Magnus form
#' approximation of saturation vapour pressure. *J. Appl. Meteor.*, 35:
#' 601-609. \doi{10.1175/1520-0450(1996)035<0601:IMFAOS>2.0.CO;2}
#'
#' @author Adam H. Sparks
#' @dev
.saturated_vapour_pressure <- function(tmp) {
  0.61094 * exp((17.625 * tmp) / (tmp + 243.04))
}

#' Calculate Weighted Hourly Leaf Wetness
#'
#' Converts hourly relative humidity to a weighted wetness value (0-1),
#' transitioning smoothly from 0 at 80% RH to 1 at `rhlim`% RH.
#'
#' @section Weighting Scheme:
#'
#' * RH < 80%: weight = 0 (no wetness)
#' * 80% <= RH < rhlim%: weight = (RH - 80) / (rhlim - 80) (linear transition)
#' * RH >= rhlim%: weight = 1 (fully wet)
#'
#' This creates a smooth response where partial wetness at intermediate RH
#' values contributes proportionally to the daily wet-hour total.
#'
#' @param rh Hourly relative humidity (%; scalar or vector, 0-100).
#' @param rhlim Relative humidity threshold for full wetness (%, >= 80).
#'
#' @returns Numeric vector of weights (0-1) for each RH value.
#'
#' @examplesIf interactive()
#' # Weighted response curve
#' rh_vals <- 0:100
#' weights <- .calculate_weighted_hr_wet(rh_vals, rhlim = 90)
#' plot(rh_vals, weights, type = "l", ylim = c(0, 1),
#'      xlab = "Hourly RH (%)", ylab = "Wetness Weight",
#'      main = "Weighted Wetness (rhlim = 90%)")
#' abline(v = c(80, 90), col = c("blue", "red"), lty = 2)
#' abline(h = c(0, 1), col = "gray", lty = 3)
#'
#' # Compare unweighted vs weighted
#' rh_test <- c(75, 85, 90, 95)
#' unweighted <- as.numeric(rh_test >= 90)  # 0, 0, 1, 1
#' weighted <- .calculate_weighted_hr_wet(rh_test, rhlim = 90)
#' cbind(RH = rh_test, unweighted, weighted)
#'
#' @author Adam H. Sparks
#' @autoglobal
#' @dev
.calculate_weighted_hr_wet <- function(rh, rhlim) {
  if (rhlim < 80.0) {
    cli::cli_abort(
      "{.arg rhlim} must be >= 80 for weighted wetness (got {rhlim})"
    )
  }

  w <- numeric(length(rh))
  w[rh < 80.0] <- 0.0
  w[rh >= rhlim] <- 1.0
  mid_idx <- rh >= 80.0 & rh < rhlim
  w[mid_idx] <- (rh[mid_idx] - 80.0) / (rhlim - 80.0)

  return(w)
}
