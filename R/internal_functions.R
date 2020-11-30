# Author: Robert J. Hijmans
# Taken from: CRAN package meteor
# License GPL3

.calculate_ea <- function(TDEW) {
  round(0.61094 * exp((17.625 * (TDEW)) /
                        ((TDEW) + 243.04)), 1)
}

# ES derived from average temperature
.calculate_es <- function(TM) {
  round(0.61094 * exp((17.625 * (TM)) /
                        ((TM) + 243.04)), 1)
}


.saturatedVaporPressure <- function(TM) {
  .611 * 10 ^ (7.5 * TM / (237.7 + TM))  #kpa
}


.vaporPressureDeficit <- function(TM, RH) {
  svp <- .saturatedVaporPressure(TM)
  (1 - (RH / 100)) * svp
}


.diurnalRH <- function(rh, tmin, tmax, lat, date) {
  tmin <- pmax(tmin, -5)
  tmax <- pmax(tmax, -5)
  tmp <- (tmin + tmax) / 2
  vp <- .saturatedVaporPressure(tmp) * rh / 100

  hrtemp <- ...diurnalTemp(lat, date, tmin, tmax)
  hr <- 1:24
  es <- .saturatedVaporPressure(hrtemp[hr])
  rh <- 100 * vp / es
  rh <- pmin(100, pmax(0, rh))
  return(rh)
}



.eLW <- function(rhmin, rhmax, tmin) {
  # emperical leaf wetness estimation according to Hijmans, Forbes and Walker, 2001
  ewhr <-
    exp(-8.093137318 + 0.11636662 * rhmax - 0.03715678 * rhmin + 0.000358713 *
          rhmin * rhmin)
  if (rhmin < 52) {
    ewhr52 <-
      exp(-8.093137318 + 0.11636662 * rhmax - 0.03715678 * 52 + 0.000358713 *
            52 * 52)

    ewhr <- ewhr52 - (ewhr - ewhr52)

  }
  ewhr <- max(0, min(ewhr, 24))
  if (tmin < 0) {
    ewhr <- 0
  }
  return(ewhr)
}

.leaf_wet <-
  function(rhmn, rhmx, tmin, tmax, lat, date, simple = TRUE) {
    rh <- (rhmn + rhmx) / 2
    rh <- .diurnalRH(rh, tmin, tmax, lat, date)
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

.diurnalRH <- function(rh, tmin, tmax, lat, date) {
  tmin <- pmax(tmin, -5)
  tmax <- pmax(tmax, -5)
  tmp <- (tmin + tmax) / 2
  vp <- .saturatedVaporPressure(tmp) * rh / 100

  hrtemp <- ...diurnalTemp(lat, date, tmin, tmax)
  hr <- 1:24
  es <- .saturatedVaporPressure(hrtemp[hr])
  rh <- 100 * vp / es
  rh <- pmin(100, pmax(0, rh))
  return(rh)
}

# can I source this from chillR instead?
...diurnalTemp <- function(lat, date, tmin, tmax) {
  TC <- 4.0
  P <- 1.5
  dayl <- .daylength(lat, doyFromDate(date))
  nigthl <- 24 - dayl
  sunris <- 12 - 0.5 * dayl
  sunset <- 12 + 0.5 * dayl
  hrtemp <- vector(length = 24)
  for (hr in 1:24) {
    #    period a: dhour between midnight and sunrise;
    if (hr < sunris)  {
      tsunst <- tmin + (tmax - tmin) * sin(pi * (dayl / (dayl + 2 * P)))
      hrtemp[hr] <-
        (tmin - tsunst * exp(-nigthl / TC) + (tsunst - tmin) * exp(-(hr + 24 -
                                                                       sunset) / TC)) / (1 - exp(-nigthl / TC))
    } else if (hr < (12 + P)) {
      #  period b: dhour between sunrise and normal time that tmax is reached (after noon)
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
        (tmin - tsunst * exp(-nigthl / TC) + (tsunst - tmin) * exp(-(hr - sunset) /
                                                                     TC)) / (1 - exp(-nigthl / TC))
    }
  }
  return(hrtemp)
}


.leafWetWithRain <- function(rhmn, rhmx, prec, simple = TRUE) {
  lw <- .leafWet(rhmn, rhmx, simple = simple)
  prec[is.na(prec)] <- 0
  prhrs <- pmin(12, prec / 5)
  return(lw + (1 - lw / 24) * prhrs)
}
