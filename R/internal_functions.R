# Original author of these functions is Robert J. Hijmans
# Adapted from CRAN package meteor
# Adapted for epirice package package by Adam H. Sparks
# License GPL3

#' @noRd
afgen <- function(xy, x) {
  # it would be safe to first sort xy by x
  d <- dim(xy)
  if (x <= xy[1, 1]) {
    res <- xy[1, 2]
  } else if (x >= xy[d[1], 1]) {
    res <- xy[d[1], 2]
  } else {
    a <- xy[xy[, 1] <= x, ]
    b <- xy[xy[, 1] >= x, ]
    if (length(a) == 2) {
      int <- rbind(a, b[1, ])
    } else if (length(b) == 2) {
      int <- rbind(a[dim(a)[1], ], b)
    } else {
      int <- rbind(a[dim(a)[1], ], b[1, ])
    }
    if (x == int[1, 1]) {
      res <- int[1, 2]
    } else if (x == int[2, 1]) {
      res <- int[2, 2]
    } else {
      res <- int[1, 2] + (x - int[1, 1]) * ((int[2, 2] - int[1, 2]) /
                                              (int[2, 1] - int[1, 1]))
    }
  }
  return(res[[1]])
}

.leaf_wet <-
  function(wth = wth, simple = TRUE) {
    # CRAN NOTE Avoidance
    RH <- TMN <- TMX <- LAT <- NULL # nocov
    rh <- .diurnal_rh(
      rh = wth[, RH],
      tmin = wth[, TMN],
      tmax  = wth[, TMX],
      lat = wth[, LAT]
    )
    if (isTRUE(simple)) {
      lw <- length(rh[rh >= 90])
    } else {
      w <- rh
      x <- (rh - 80) / (95 - 80)
      w[rh > 95] <- 1
      w[rh < 95] <- x[rh < 95]
      w[rh < 80] <- 0
      lw <- sum(w)
    }
    return(lw)
  }

.diurnal_rh <- function(rh, tmin, tmax, lat, date) {
  tmin <- pmax(tmin, -5)
  tmax <- pmax(tmax, -5)
  tmp <- (tmin + tmax) / 2
  vp <- .saturated_vapor_pressure(tmp) * rh / 100

  hrtemp <- .diurnal_temp(lat, date, tmin, tmax)
  hr <- 1:24
  es <- .saturated_vapor_pressure(hrtemp[hr])
  rh <- 100 * vp / es
  rh <- pmin(100, pmax(0, rh))
  return(rh)
}

.diurnal_temp <- function(lat, doy, tmin, tmax) {
  TC <- 4.0
  P <- 1.5
  dayl <- geosphere::daylength(lat = lat, doy = doy)
  nigthl <- 24 - dayl
  sunris <- 12 - 0.5 * dayl
  sunset <- 12 + 0.5 * dayl
  hrtemp <- vector(length = 24)
  for (hr in 1:24) {
    #    period a: dhour between midnight and sunrise;
    if (hr < sunris)  {
      tsunst <- tmin + (tmax - tmin) * sin(pi * (dayl / (dayl + 2 * P)))
      hrtemp[hr] <-
        (tmin - tsunst * exp(-nigthl / TC) +
           (tsunst - tmin) * exp(-(hr + 24 - sunset) / TC)) /
        (1 - exp(-nigthl / TC))
    } else if (hr < (12 + P)) {
      # period b: dhour between sunrise and normal time that tmax is reached
      # (after noon)
      hrtemp[hr] <-
        tmin + (tmax - tmin) * sin(pi * (hr - sunris) / (dayl +
                                                           2 * P))
    } else if (hr < sunset) {
      #  period c: dhour between time of tmax and sunset;
      hrtemp[hr] <-
        tmin + (tmax - tmin) * sin(pi * (hr - sunris) / (dayl +
                                                           2 * P))
    } else {
      #  period d: dhour between sunset and midnight;
      tsunst <-
        tmin + (tmax - tmin) * sin(pi * (dayl / (dayl + 2 * P)))
      hrtemp[hr] <-
        (tmin - tsunst * exp(-nigthl / TC) +
           (tsunst - tmin) * exp(-(hr - sunset) / TC)) /
        (1 - exp(-nigthl / TC))
    }
  }
  return(hrtemp)
}

#' Calculate saturated vapour pressure, es
#'
#' @param TM Mean temperature as provided by `wth`
#'
#' @return Single double precision value of es as kilopascals

.saturated_vapor_pressure <- function(TM) {
  .611 * 10 ^ (7.5 * TM / (237.7 + TM))
}
