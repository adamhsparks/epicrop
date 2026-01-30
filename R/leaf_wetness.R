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
#' remaining dry fraction of the day.  If TMIN/TMAX are missing, the function
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
#' # Simple binary wetness
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

  rh <- wth$RHUM
  if (is.null(rh) || length(rh) != n) {
    cli::cli_abort(
      call = rlang::caller_env(),
      "RHUM is missing or wrong length."
    )
  }

  doy <- wth$DOY
  if (is.null(doy) || length(doy) != n) {
    cli::cli_abort(
      call = rlang::caller_env(),
      "DOY is missing or wrong length."
    )
  }

  # Always make rainfall a usable vector
  prec <- wth$RAIN
  if (is.null(prec)) {
    prec <- rep(0, n)
  }
  prec[is.na(prec)] <- 0

  rainlim <- max(rainlim, .Machine$double.eps)

  # If TMIN/TMAX are missing, fallback to binary wetness
  if (!all(c("TMIN", "TMAX") %in% names(wth))) {
    return(data.table::fifelse(rh >= rhlim | prec >= rainlim, 24L, 0L))
  }

  # Extract temperature data
  tmin <- wth$TMIN
  tmax <- wth$TMAX

  tmp <- wth$TEMP
  if (is.null(tmp)) {
    tmp <- wth$T2M
  }
  if (is.null(tmp)) {
    tmp <- (tmin + tmax) / 2
  }
  if (is.null(tmp) || length(tmp) != n) {
    cli::cli_abort(
      call = rlang::caller_env(),
      "TEMP is missing or wrong length (cannot derive from TMIN/TMAX)."
    )
  }

  # LAT required for diurnal calculation
  lat <- wth$LAT
  if (is.null(lat)) {
    cli::cli_abort(
      call = rlang::caller_env(),
      "LAT is required for hourly wetness calculation."
    )
  }
  if (length(lat) == 1) {
    lat <- rep(lat, n)
  }
  if (length(lat) != n) {
    cli::cli_abort(
      call = rlang::caller_env(),
      "LAT has wrong length."
    )
  }

  yyyymmdd <- wth$YYYYMMDD
  if (is.null(yyyymmdd)) {
    yyyymmdd <- seq_len(n)
  }

  # Calculate hourly RH
  rh_hr <- .diurnal_rh(rh, tmin, tmax, tmp, lat, doy, yyyymmdd)

  # Count wet hours
  if (isFALSE(weighted_wetness)) {
    lw <- unlist(lapply(rh_hr, .check_threshold, rhlim = rhlim))
  } else {
    w <- lapply(rh_hr, .calculate_weighted_hr_wet, rhlim = rhlim)
    lw <- unlist(lapply(w, sum))
  }

  # Add rain contribution
  wet_rain_hours <- pmin(24.0, prec / rainlim)

  # Combine without double counting
  wet_total <- lw + (1.0 - lw / 24.0) * wet_rain_hours

  # Enforce 0-24 range
  wet_total <- pmin(24.0, pmax(0.0, wet_total))
  return(wet_total)
}

#' Calculate hourly relative humidity for a given day
#'
#' Uses mean rh, doy, location (lat), mean tmp, tmin and tmax to calculate
#' hourly rh values.
#'
#' @param rh Mean daily relative humidity as provided by `wth` via [epicrop_sim()].
#' @param tmin Minimum temperature as provided by `wth` via [epicrop_sim()].
#' @param tmax Maximum temperature as provided by `wth` via [epicrop_sim()].
#' @param tmp Mean daily temperature as provided by `wth` via [epicrop_sim()].
#' @param lat Latitude provided by `wth` via [epicrop_sim()].
#' @param doy Sequential day of year as provided by `wth` via [epicrop_sim()].
#' @param yyyymmdd Date (YYYY-MM-DD) as provided by `wth` via [epicrop_sim()].
#'
#' @returns A numeric vector of relative humidity values over 24 hrs.
#'
#' @examples
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30")
#' )
#'
#' .diurnal_rh(
#'   rh = wth$RHUM,
#'   tmin = wth$TMIN,
#'   tmax = wth$TMAX,
#'   tmp = wth$TEMP,
#'   lat = wth$LAT,
#'   doy = wth$DOY,
#'   yyyymmdd = wth$YYYYMMDD
#' )
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @dev
.diurnal_rh <- function(rh, tmin, tmax, tmp, lat, doy, yyyymmdd) {
  rh_hr <- stats::setNames(vector("list", length(yyyymmdd)), yyyymmdd)

  for (i in seq_along(yyyymmdd)) {
    vp <- .saturated_vapour_pressure(tmp[[i]]) * rh[[i]] / 100.0
    hrtemp <- .diurnal_temp(
      lat = lat[[i]],
      doy = doy[[i]],
      tmin = tmin[[i]],
      tmax = tmax[[i]]
    )
    es <- .saturated_vapour_pressure(hrtemp)
    rh_hr[[i]] <- pmin(100.0, pmax(0.0, 100.0 * vp / es))
  }

  return(rh_hr)
}

#' Calculate hourly temperature for a given day
#'
#' Uses doy, location (lat) and TMIN and TMAX to calculate hourly temperatures.
#'
#' @param lat Latitude as provided by `wth` via [epicrop_sim()].
#' @param doy Sequential day of year as provided by `wth` via [epicrop_sim()].
#' @param tmin Minimum temperature as provided by `wth` via [epicrop_sim()].
#' @param tmax Maximum temperature as provided by `wth` via [epicrop_sim()].
#'
#' @returns A numeric vector of hourly temperature values for the duration of
#'  `wth` data.
#'
#' @examples
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#'
#' .diurnal_temp(
#'   lat = wth$LAT,
#'   doy = wth$DOY,
#'   tmin = wth$TMIN,
#'   tmax = wth$TMAX
#' )
#'
#' @author Robert J. Hijmans with modifications by Adam H. Sparks,
#'   \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @dev
.diurnal_temp <- function(lat, doy, tmin, tmax) {
  dayl <- mapply(FUN = .day_length, lat = lat, doy = doy)
  nightl <- 24.0 - dayl
  sandhya <- 0.5 * dayl
  sunris <- 12.0 - sandhya
  sunset <- 12.0 + sandhya
  daytime <- t(cbind(dayl, nightl, sunris, sunset, tmin, tmax))

  .hourly_t <- function(x) {
    hr_temp <- vector(mode = "numeric", length = 24.0)
    for (hr in 1.0:24.0) {
      if (hr < x["sunris"]) {
        tsunst <- x["tmin"] +
          (x["tmax"] - x["tmin"]) *
            sin(pi * (x["dayl"] / (x["dayl"] + 3.0)))
        hr_temp[[hr]] <- (x["tmin"] -
          tsunst * exp(-x["nightl"] / 4.0) +
          (tsunst - x["tmin"]) * exp(-(hr + 24 - x["sunset"]) / 4.0)) /
          (1.0 - exp(-x["nightl"] / 4.0))
      } else if (hr < x["sunset"]) {
        hr_temp[[hr]] <- x["tmin"] +
          (x["tmax"] - x["tmin"]) *
            sin(pi * (hr - x["sunris"]) / (x["dayl"] + 3.0))
      } else {
        tsunst <- x["tmin"] +
          (x["tmax"] - x["tmin"]) *
            sin(pi * (x["dayl"] / (x["dayl"] + 3.0)))
        hr_temp[[hr]] <- (x["tmin"] -
          tsunst * exp(-x["nightl"] / 4.0) +
          (tsunst - x["tmin"]) * exp(-(hr - x["sunset"]) / 4.0)) /
          (1.0 - exp(-x["nightl"] / 4.0))
      }
    }
    return(hr_temp)
  }

  x <- as.vector(apply(X = daytime, MARGIN = 2L, FUN = .hourly_t))
  return(x)
}

#' Calculate day length for a given latitude on a given date
#'
#' @param lat Latitude as provided by `wth` via [epicrop_sim()].
#' @param doy Sequential day of year (Julian date) as provided by `wth` via
#'  [epicrop_sim()].
#'
#' @returns A numeric vector of daylight hours based on latitude and date.
#'
#' @examples
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#' .day_length(lat = wth[, LAT], doy = wth[, DOY])
#'
#' @author Robert J. Hijmans, modifications by Adam H. Sparks,
#'   \email{adamhsparks@@gmail.com}
#'
#' @references
#' William C. Forsythe and Edward J. Rykiel and Randal S. Stahl and Hsin-i Wu
#' and Robert M. Schoolfield.  Ecological Modeling, Volume 80 (1995) pp.  87-95,
#' "A Model Comparison for Day Length as a Function of Latitude and Day of the
#' Year", \doi{10.1016/0304-3800(94)00034-F}
#'
#' @autoglobal
#' @dev
.day_length <- function(lat, doy) {
  doy <- (doy - 1.0) %% 365.0 + 1.0

  p <- asin(
    0.39795 *
      cos(0.2163108 + 2.0 * atan(0.9671396 * tan(0.00860 * (doy - 186.0))))
  )
  a <- (sin(0.8333 * pi / 180.0) + sin(lat * pi / 180.0) * sin(p)) /
    (cos(lat * pi / 180.0) * cos(p))
  a <- pmin(pmax(a, -1.0), 1.0)
  return(24 - 7.639437 * acos(a)) }

#' Calculate saturated vapour pressure, es
#'
#' @param tmp Mean temperature as provided by `wth` via [epicrop_sim()].
#'
#' @returns Single double precision value of es as kilopascals.
#'
#' @examples
#' .saturated_vapour_pressure(0)
#'
#' @references Alduchov and Eskridge 1995,
#'  \doi{10.1175/1520-0450(1996)035<0601:IMFAOS>2.0.CO;2}
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @dev
.saturated_vapour_pressure <- function(tmp) {
  0.61094 * exp((17.625 * (tmp)) / ((tmp) + 243.04))
}

#' Check if the relative humidity is above the threshold
#'
#' @param rh_hr A list of 24 relative humidity values for every DOY in the
#'  data.
#' @param rhlim The relative humidity threshold to check against.
#'
#' @returns Integer count of hours where RH >= rhlim.
#'
#' @examples
#' .check_threshold(rh_hr = rep(95, 24), rhlim = 95)
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @dev
.check_threshold <- function(rh_hr, rhlim) {
  return(sum(rh_hr >= rhlim))
}

#' Calculates the weighted hourly leaf wetness value
#'
#' @note This functionality is just a placeholder for future possible work.
#' This particular function is set up but not exposed to users yet.
#'
#' @param rh A numeric vector of relative humidity values.
#' @param rhlim The relative humidity threshold to check against between `80` &
#'  `100`.
#'
#' @returns A `vector` of weighted values between 0 and 1 depending on the
#' `rhlim`.
#'
#' @examples
#' .calculate_weighted_hr_wet(rh = 86, rhlim = 95)
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @dev
.calculate_weighted_hr_wet <- function(rh, rhlim) {
  if (rhlim < 80.0) {
    cli::cli_abort(
      "You have set your {.var rhlim} below 80%.  This value must be between 80
      and 100 to properly work with weighted hourly leaf wetness.",
      call = rlang::caller_env()
    )
  }
  w <- rh
  x <- (rh - 80.0) / (rhlim - 80.0)
  w[rh >= rhlim] <- 1.0
  cs_1 <- rh < rhlim
  w[cs_1] <- x[cs_1]
  w[rh < 80.0] <- 0.0
  return(w)
}
