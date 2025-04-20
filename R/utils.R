#' Calculate leaf wetness using NASAPOWER data
#'
#' @param wth Weather data in the proper format, usually obtained from
#'  [get_wth()] and passed through [seir()].
#' @param simple Logical value whether to use RH >= 90 to equal free moisture
#'  or perform more complex calculation to estimate leaf wetness that weights
#'  values > 95% (1), < 95 & > 80 ((rh - 80) / 15) or > 80 % (0)
#'
#' @examples
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#'
#' .leaf_wet(wth = wth, simple = TRUE)
#'
#' @author Robert J. Hijmans (original implementation) and Adam H. Sparks
#' @returns A numeric value for hours of leaf wetness based on RH
#' @autoglobal
#' @dev

.leaf_wet <-
  function(wth = wth, simple = TRUE) {
    rh <- .diurnal_rh(
      doy = wth[, DOY],
      rh = wth[, RHUM],
      tmin = wth[, TMIN],
      tmax = wth[, TMAX],
      tmp = wth[, TEMP],
      lat = wth[, LAT]
    )
    if (simple) {
      lw <- length(rh[rh >= 90])
    } else {
      w <- rh
      x <- (rh - 80) / 15
      w[rh > 95] <- 1
      cs_1 <- rh < 95
      w[cs_1] <- x[cs_1]
      w[rh < 80] <- 0
      lw <- sum(w)
    }
    return(lw)
  }

#' Calculate hourly relative humidity for a given day
#'
#' Uses mean RH, doy, location (lat), mean tmp, TMIN and TMAX to calculate
#'  hourly RH values.
#'
#' @param rh Mean daily relative humidity as provided by `wth` via [seir()].
#' @param tmin Minimum temperature as provided by `wth` via [seir()].
#' @param tmax Maximum temperature as provided by `wth` via [seir()].
#' @param tmp Mean daily temperature as provided by `wth` via [seir()].
#' @param doy Sequential day of year as provided by `wth` via [seir()].
#' @param lat Latitude provided by `wth` via [seir()].
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
#' @author Robert J. Hijmans (original implementation) and Adam H. Sparks
#' @returns A numeric vector of relative humidity values
#' @autoglobal
#' @dev
.diurnal_rh <- function(rh, tmin, tmax, tmp, doy, lat) {
  # CRAN NOTE avoidance
  vp <- .saturated_vapour_pressure(tmp) * rh / 100
  hrtemp <- .diurnal_temp(lat = lat, doy = doy, tmin = tmin, tmax = tmax)
  es <- .saturated_vapour_pressure(hrtemp)
  rh <- 100 * vp / es
  rh <- pmin(100, pmax(0, rh))
  return(rh)
}

#' Calculate hourly temperature for a given day
#'
#' Uses doy, location (lat) and TMIN and TMAX to calculate hourly temperatures.
#'
#' @param lat Latitude as provided by `wth` via [seir()].
#' @param doy Sequential day of year as provided by `wth` via [seir()].
#' @param tmin Minimum temperature as provided by `wth` via [seir()].
#' @param tmax Maximum temperature as provided by `wth` via [seir()].
#' @param dayl Daylight hours as provided by `wth` via [seir()].
#'
#' @author Robert J. Hijmans (original implementation) and Adam H. Sparks
#' @returns A numeric vector of hourly temperature values for the duration of
#'  `wth` data
#'
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
#' @autoglobal
#' @dev
.diurnal_temp <- function(lat, doy, tmin, tmax) {
  dayl <- mapply(FUN = .daylength, lat = lat, doy = doy)
  nightl <- 24 - dayl
  sandhya <- 0.5 * dayl
  sunris <- 12 - sandhya
  sunset <- 12 + sandhya
  dt <- t(cbind(dayl, nightl, sunris, sunset, tmin, tmax))

  .hourly_t <- function(x) {
    hr_temp <- vector(mode = "numeric", length = 24)
    for (hr in 1:24) {
      # period a: dhour between midnight and sunrise;
      if (hr < x["sunris"]) {
        tsunst <-
          x["tmin"] +
          (x["tmax"] - x["tmin"]) *
            sin(
              3.141593 * (x["dayl"] / (x["dayl"] + 3))
            )
        hr_temp[[hr]] <-
          (x["tmin"] -
            tsunst * exp(-x["nightl"] / 4L) +
            (tsunst - x["tmin"]) *
              exp(
                -(hr + 24 - x["sunset"]) / 4L
              )) /
            (1 - exp(-x["nightl"] / 4L))
      } else if (hr < x["sunset"]) {
        # period b: dhour between time of sunrise and sunset
        hr_temp[[hr]] <-
          x["tmin"] +
          (x["tmax"] - x["tmin"]) *
            sin(
              3.141593 * (hr - x["sunris"]) / (x["dayl"] + 3)
            )
      } else {
        #  period c: dhour between sunset and midnight;
        tsunst <-
          x["tmin"] +
          (x["tmax"] - x["tmin"]) *
            sin(
              3.141593 * (x["dayl"] / (x["dayl"] + 3))
            )
        hr_temp[[hr]] <-
          (x["tmin"] -
            tsunst * exp(-x["nightl"] / 4L) +
            (tsunst - x["tmin"]) * exp(-(hr - x["sunset"]) / 4L)) /
            (1 - exp(-x["nightl"] / 4L))
      }
    }
    return(hr_temp)
  }

  x <- as.vector(apply(X = dt, MARGIN = 2, FUN = .hourly_t))
  return(x)
}

#' Calculate day length for a given latitude on a given date
#'
#' @param lat Latitude as provided by `wth` via [seir()].
#' @param doy Sequential day of year (Julian date) as provided by `wth` via
#'  [seir()].
#'
#' @examples
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#' .daylength(lat = wth[, LAT], doy = wth[, DOY])
#'
#' @author Robert J. Hijmans (original implementation) and Adam H. Sparks
#' @returns A numeric vector of daylight hours based on latitude and date
#' @autoglobal
#' @dev
.daylength <- function(lat, doy) {
  if (lat > 90 || lat < -90) {
    cli::abort(
      "Latitude values must fall between -90 and 90 degrees"
    )
  }

  doy <- doy %% 366

  # William C. Forsythe and Edward J. Rykiel and Randal S. Stahl and Hsin-i Wu
  # and Robert M. Schoolfield. Ecological Modeling, Volume 80 (1995) pp. 87-95,
  # "A Model Comparison for Daylength as a Function of Latitude and Day of the
  # Year", <DOI: 10.1016/0304-3800(94)00034-F>
  p <- asin(
    0.39795 *
      cos(
        0.2163108 +
          2 *
            atan(
              0.9671396 *
                tan(0.00860 * (doy - 186))
            )
      )
  )
  a <- (0.01454 + sin(lat * 0.01745) * sin(p)) /
    (cos(lat * 0.01745) * cos(P))

  a <- pmin(pmax(a, -1), 1)
  dl <- 24 - (24 / pi) * acos(a)
  return(dl)
}

#' Calculate saturated vapour pressure, es
#'
#' @param tmp Mean temperature as provided by `wth` via [seir()].
#'
#' @author Adam H. Sparks
#' @return Single double precision value of es as kilopascals
#'
#' @references Alduchov and Eskridge 1995,
#'  <doi:10.1175/1520-0450(1996)035<0601:IMFAOS>2.0.CO;2>
#'
#' @examples
#' .saturated_vapour_pressure(0)
#' @autoglobal
#' @dev
.saturated_vapour_pressure <- function(tmp) {
  0.61094 * exp((17.625 * (tmp)) / ((tmp) + 243.04))
}
