# Author: Robert J. Hijmans
# Taken from: CRAN package meteor
# License GPL3

# use ea/es calculations from GSODR
.saturatedVaporPressure <- function(tmp) {
  .611 * 10 ^ (7.5 * tmp / (237.7 + tmp))  #kpa
}


.vaporPressureDeficit <- function(tmp, rh) {
  svp <- .saturatedVaporPressure(tmp)
  (1 - (rh / 100)) * svp
}


.rhMinMax <- function(rh, tmin, tmax) {
  tmin <- pmax(tmin, -5)
  tmax <- pmax(tmax, -5)
  tmp <- (tmin + tmax) / 2

  es <- .saturatedVaporPressure(tmp)
  vp <- rh / 100 * es

  es <- .saturatedVaporPressure(tmax)
  rhmn <- 100 * vp / es

  rhmn <- pmax(0, pmin(100, rhmn))

  es <- .saturatedVaporPressure(tmin)
  rhmx <- 100 * vp / es

  rhmx <- pmax(0, pmin(100, rhmx))
  cbind(rhmn, rhmx)
}


.rhMinMax2 <- function(tmin, tmax, rhum) {
  tmin <- pmax(tmin, -5)
  tmax <- pmax(tmax, -5)
  tmp <- (tmin + tmax) / 2

  es <- .saturatedVaporPressure(tmp)
  vp <- rhum / 100 * es

  es <- .saturatedVaporPressure(tmax)
  rhmn <- 100 * vp / es

  rhmn <- pmax(0, pmin(100, rhmn))

  es <- .saturatedVaporPressure(tmin)
  rhmx <- 100 * vp / es

  rhmx <- pmax(0, pmin(100, rhmx))
  cbind(rhmn, rhmx)
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




.tDew <- function(temp, rh) {
  temp - (100 - rh) / 5
}


.FtoC <- function(x) {
  (5 / 9) * (x - 32)
}
.CtoF <- function(x) {
  x * 9 / 5 + 32
}

.atmp <- function(alt) {
  101.325 * (1 - 2.25577 * 10 ^ -5 * alt) ^ 5.25588   # kPa
}


.rel2abshum <- function(rh, t) {
  es <- .saturatedVaporPressure(t)
  ea <- rh * es / 100
  M <- 18.02 # g/mol
  R <- 8.314472 # Pa?m?/(mol?K)
  T <- t + 273.15  # C to K
  hum <- ea * M / (T * R)
  return(hum)
}


.abs2rhumum <- function(hum, t) {
  M <- 18.02 # g/mol
  R <- 8.314472 # Pa?m?/(mol?K)
  T <- t + 273.15  # C to K
  ea <- hum / (M / (T * R))
  es <- .saturatedVaporPressure(t)
  rh <- 100 * ea / es
  rh  <- pmin(rh, 100)
  return(rh)
}



.rel2spechum <- function(rh, t, alt) {
  es <- .saturatedVaporPressure(t)
  ea <- es * (rh / 100)
  p <- .atmp(0)
  0.62198 * ea / (p - ea)
}

.spec2rhumum <- function(spec, t, alt) {
  es <- .saturatedVaporPressure(t)
  100 * (spec * .atmp(alt)) / ((0.62198 + spec) * es)
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
