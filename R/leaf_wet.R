#' Calculate leaf wetness based on relative humidity
#'
#' Calculate a leaf wetness value between `0` and `1` based on the relative
#'  humidity and rainfall values in the weather data. The simple calculation
#'  uses only the relative humidity and rainfall threshold to set the value to 0
#'  or 1 based on the threshold values. While the complex calculation uses a
#'  formula to calculate a more nuanced value for leaf wetness between `0` and
#'  `1` using a scaled value of relative humidity where values > rhlim == 1,
#'  < 95% & > 80% == (rh - 80) / 15) or < 80%  == 0.
#'
#' @param weighted_wetness A `Boolean` value indicating whether to use simple
#'  leaf wetness calculation, `FALSE`, or a more complex calculation, `TRUE`,
#'  that weights the leaf wetness value based on the relative humidity for each
#'  hour between RH `rhlim` and 80%, \eqn{(rh - 80) / (rhlim - 80)}. Defaults to
#'  `FALSE`, using the hourly `rhlim` value to return a 0 or 1 value.
#' @param wth Weather data in the proper format.
#' @param rhlim Relative humidity threshold (%) to determine if there is free
#'  water on the leaf surface.
#' @param rainlim Rainfall threshold (mm) to determine if there is free water
#'  on the leaf surface.
#'
#'  @note For the complex calculation, the boundaries are included in the upper
#'   or lower boundaries, _i.e._, `rhlim` will be set to 1 and 80% will be set
#'   to 0. For the simple calculation, `rhlim` is included in the upper
#'   boundary, _i.e._, `rhlim` will be set to 1 and anything > `rhlim` will be
#'   set to 0. For the `rainlim`, anything > `rainlim` will be set to 1.
#'
#' @returns A numeric vector of leaf wetness values for use with `Rc_wet`.
#' @dev
#' @autoglobal
#' @author Robert J. Hijmans with modifications by Adam H. Sparks,
#'  \email{adamhsparks@@gmail.com}

.calculate_leaf_wetness <- function(
  weighted_wetness = FALSE,
  wth,
  rhlim,
  rainlim
) {
  rh <- wth$RHUM
  prec <- wth$RAIN
  tmin <- wth$TMIN
  tmax <- wth$TMAX
  tmp <- wth$TEMP
  lat <- wth$LAT
  doy <- wth$DOY

  rh <- .diurnal_rh(rh, tmin, tmax, tmp, lat, doy)

  if (isFALSE(weighted_wetness)) {
    lw <- unlist(lapply(X = rh_hr, FUN = .check_threshold, rhlim = rhlim))
  } else {
    w <- lapply(X = rh, FUN = .calc_weighted_hr_wet, rhlim = rhlim)
    lw <- unlist(lapply(X = w, FUN = sum))
  }

  prec[is.na(prec)] <- 0.0
  prhrs <- pmin(12.0, prec / rainlim)
  return(lw + (1.0 - lw / 24.0) * prhrs)
}

#' Calculate hourly relative humidity for a given day
#'
#' Uses mean rh, doy, location (lat), mean tmp, tmin and tmax to calculate
#'  hourly rh values.
#'
#' @param rh Mean daily relative humidity as provided by `wth` via `seir()`
#' @param tmin Minimum temperature as provided by `wth` via `seir()`
#' @param tmax Maximum temperature as provided by `wth` via `seir()`
#' @param tmp Mean daily temperature as provided by `wth` via `seir()`
#' @param doy Sequential day of year as provided by `wth` via `seir()`
#' @param lat Latitude provided by `wth` via `seir()`
#'
#' @examples
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30")
#' )
#'
#' .diurnal_rh(
#'   rh = wth[, RHUM],
#'   tmin = wth[, TMIN],
#'   tmax = wth[, TMAX],
#'   tmp = wth[, TEMP],
#'   doy = wth[, DOY],
#'   lat = wth[, LAT]
#' )
#'
#' @returns A numeric vector of relative humidity values over 24 hrs.
#'
#' @dev
.diurnal_rh <- function(rh, tmin, tmax, tmp, lat, doy) {
  rh_hr <- vector(mode = "list", length = length(doy))
  names(rh_hr) <- doy
  for (i in seq_along(doy)) {
    vp <- .saturated_vapour_pressure(tmp[[i]]) * rh[[i]] / 100.0
    hrtemp <- .diurnal_temp(
      lat = lat[[i]],
      doy = doy[[i]],
      tmin = tmin[[i]],
      tmax = tmax[[i]]
    )
    es <- .saturated_vapour_pressure(hrtemp)
    rh_hr[[i]] <- 100.0 * vp / es
    rh_hr[[i]] <- pmin(100.0, pmax(0.0, rh_hr[[i]]))
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
#'   lat = wth[, LAT],
#'   doy = wth[, DOY],
#'   tmin = wth[, TMIN],
#'   tmax = wth[, TMAX]
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
#' This particular function is set up but not exposed to users and thus not
#' documented in the {pkgdown} site via an `@dev` tag
#'
#' @param rh A numeric vector of relative humidity values.
#' @param rhlim The relative humidity threshold to check against.
#' @keywords Internal
#' @noRd
#'
.calc_weighted_hr_wet <- function(rh, rhlim) {
  w <- rh
  x <- (rh - 80.0) / (rhlim - 80.0)
  w[rh >= rhlim] <- 1.0
  w[rh < rhlim] <- x[rh < rhlim]
  w[rh < 80.0] <- 0.0
  return(w)
}
