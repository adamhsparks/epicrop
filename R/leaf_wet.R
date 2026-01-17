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
#' @returns A numeric vector of daily wet-hours from 0 to 24, length nrow(wth).
#'   Use with RcW (hours -> 0 to 1) to obtain the wetness multiplier for
#'   infection.
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
    cli::cli_abort("RHUM is missing or wrong length.")
  }

  doy <- wth$DOY
  if (is.null(doy) || length(doy) != n) {
    cli::cli_abort("DOY is missing or wrong length.")
  }

  # Always make rainfall a usable vector (RH-only runs supported)
  prec <- wth$RAIN
  if (is.null(prec)) {
    prec <- rep(0, n)
  }
  prec[is.na(prec)] <- 0

  rainlim <- max(rainlim, .Machine$double.eps)

  # If TMIN/TMAX are missing, we cannot do diurnal RH: strict-hours fallback
  if (!all(c("TMIN", "TMAX") %in% names(wth))) {
    return(data.table::fifelse(rh >= rhlim | prec >= rainlim, 24L, 0L))
  }

  # TEMP robust extraction (only needed for diurnal calc)
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
      "TEMP is missing or wrong length (cannot derive from TMIN/TMAX)."
    )
  }

  # LAT required only for diurnal calc
  lat <- wth$LAT
  if (is.null(lat)) {
    cli::cli_abort("LAT is required for hourly wetness calculation.")
  }
  if (length(lat) == 1) {
    lat <- rep(lat, n)
  }
  if (length(lat) != n) {
    cli::cli_abort("LAT has wrong length.")
  }

  yyyymmdd <- wth$YYYYMMDD
  if (is.null(yyyymmdd)) {
    yyyymmdd <- seq_len(n)
  }

  rh_hr <- .diurnal_rh(rh, tmin, tmax, tmp, lat, doy, yyyymmdd)

  if (isFALSE(weighted_wetness)) {
    lw <- unlist(lapply(rh_hr, .check_threshold, rhlim = rhlim))
  } else {
    w <- lapply(rh_hr, .calculate_weighted_hr_wet, rhlim = rhlim)
    lw <- unlist(lapply(w, sum))
  }

  # rain wet-hours (0..24), scaled by precip/rainlim
  wet_rain_hours <- pmin(24.0, prec / rainlim)

  # combine without double counting
  wet_total <- lw + (1.0 - lw / 24.0) * wet_rain_hours

  # strict contract enforcement: 0..24
  wet_total <- pmin(24.0, pmax(0.0, wet_total))
  wet_total
}
#' Calculate hourly relative humidity for a given day
#'
#' Uses mean rh, doy, location (lat), mean tmp, tmin and tmax to calculate
#'  hourly rh values.
#'
#' @param rh Mean daily relative humidity as provided by `wth` via [seir()].
#' @param tmin Minimum temperature as provided by `wth` via [seir()].
#' @param tmax Maximum temperature as provided by `wth` via [seir()].
#' @param tmp Mean daily temperature as provided by `wth` via [seir()].
#' @param doy Sequential day of year as provided by `wth` via [seir()].
#' @param lat Latitude provided by `wth` via [seir()].
#' @param yyyymmdd Date (YYYY-MM-DD) as provided by `wth` via [seir()].
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
#'   doy = wth$DOY,
#'   lat = wth$LAT
#'   yyyymmdd = wth$YYYYMMDD
#' )
#'
#' @returns A numeric vector of relative humidity values over 24 hrs.
#'
#' @dev
.diurnal_rh <- function(rh, tmin, tmax, tmp, lat, doy, yyyymmdd) {
  rh_hr <- setNames(vector("list", length(yyyymmdd)), yyyymmdd)

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
#' @param lat Latitude as provided by `wth` via `seir()`
#' @param doy Sequential day of year as provided by `wth` via `seir()`
#' @param tmin Minimum temperature as provided by `wth` via `seir()`
#' @param tmax Maximum temperature as provided by `wth` via `seir()`
#' @param dayl Daylight hours as provided by `wth` via `seir()`
#'
#' @returns A numeric vector of hourly temperature values for the duration of
#'  `wth` data
#' @author Robert J. Hijmans with modifications by Adam H. Sparks
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
      # period a: dhour between midnight and sunrise;
      if (hr < x["sunris"]) {
        tsunst <-
          x["tmin"] +
          (x["tmax"] - x["tmin"]) *
            sin(
              pi * (x["dayl"] / (x["dayl"] + 3.0))
            )
        hr_temp[[hr]] <-
          (x["tmin"] -
            tsunst * exp(-x["nightl"] / 4.0) +
            (tsunst - x["tmin"]) *
              exp(
                -(hr + 24 - x["sunset"]) / 4.0
              )) /
          (1.0 - exp(-x["nightl"] / 4.0))
      } else if (hr < x["sunset"]) {
        # period b: dhour between time of sunrise and sunset
        hr_temp[[hr]] <-
          x["tmin"] +
          (x["tmax"] - x["tmin"]) *
            sin(
              pi * (hr - x["sunris"]) / (x["dayl"] + 3.0)
            )
      } else {
        #  period c: dhour between sunset and midnight;
        tsunst <-
          x["tmin"] +
          (x["tmax"] - x["tmin"]) *
            sin(
              pi * (x["dayl"] / (x["dayl"] + 3.0))
            )
        hr_temp[[hr]] <-
          (x["tmin"] -
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
#' @param lat Latitude as provided by `wth` via `seir()`
#' @param doy Sequential day of year (Julian date) as provided by `wth` via
#'  `seir()`
#'
#' @examples
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#' .day_length(lat = wth[, LAT], doy = wth[, DOY])
#'
#' @author Robert J. Hijmans, modifications by Adam H. Sparks
#' @references
#' William C. Forsythe and Edward J. Rykiel and Randal S. Stahl and Hsin-i Wu
#' and Robert M. Schoolfield. Ecological Modeling, Volume 80 (1995) pp. 87-95,
#' "A Model Comparison for Day Length as a Function of Latitude and Day of the
#' Year", \doi{10.1016/0304-3800(94)00034-F}
#'
#' @returns A numeric vector of daylight hours based on latitude and date
#'
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
  return(24 - 7.639437 * acos(a)) # where 7.6397437 = 24 / pi
}

#' Calculate saturated vapour pressure, es
#'
#' @param tmp Mean temperature as provided by `wth` via `seir()`
#'
#' @returns Single double precision value of es as kilopascals
#'
#' @references Alduchov and Eskridge 1995,
#'  <doi:10.1175/1520-0450(1996)035<0601:IMFAOS>2.0.CO;2>
#'
#' @author Adam H. Sparks
#' @examples
#' .saturated_vapour_pressure(0)
#'
#' @dev
.saturated_vapour_pressure <- function(tmp) {
  0.61094 * exp((17.625 * (tmp)) / ((tmp) + 243.04))
}

#' Check if the relative humidity is above the threshold
#' @param rh_hr A list of 24 relative humidity values for every DOY in the
#'  data.
#' @param rhlim The relative humidity threshold to check against.
#'
#' @examples
#' .check_threshold(rh_hr = rep(95, 24), rhlim = 95)
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
#' @examples
#' .calculate_weighted_hr_wet(rh = 86, rhlim = 95)
#'
#' @returns A `vector` of weighted values between 0 and 1 depending on the
#' `rhlim`
#'
#' @dev
#'
.calculate_weighted_hr_wet <- function(rh, rhlim) {
  if (rhlim < 80.0) {
    cli::cli_abort(
      "You have set your {.var rhlim} below 80%. This value must be between `80`
      and `100` to properly work with weighted hourly leaf wetness.",
      call = rlang::caller_env()
    )
  }
  w <- rh
  x <- (rh - 80.0) / (rhlim - 80.0)
  w[rh >= rhlim] <- 1.0
  w[rh < rhlim] <- x[rh < rhlim]
  w[rh < 80.0] <- 0.0
  return(w)
}
