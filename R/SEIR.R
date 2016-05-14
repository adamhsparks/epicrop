#' @title SEIR
#'@description Adapted from cropsim package version 0.2.0-5 by Adam H.
#'Sparks - USQ CCH. Original model development: Serge Savary & Rene Pangga.
#'Original R implementation by Robert J. Hijmans, Rene Pangga, & Jorrel Aunario,
# International Rice Research Institute
#' @param wth
#' @param emergence
#' @details
#' @examples
#' @export
brown_spot <- function(wth, emergence = "2000-05-15", ...) {
  age_coef_rc <- cbind(0:6 * 20, c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0))
  temp_coef_rc <- cbind(15 + (0:5) * 5, c(0, 0.06, 1.0, 0.85, 0.16, 0))
  rh_coef_rc <- cbind(0:8 * 3,
                      c(0, 0.12, 0.20, 0.38, 0.46, 0.60, 0.73, 0.87, 1.0))
  return(SEIR(wth = wth, emergence = emergence, age_rc = age_coef_rc,
              tmp_rc = temp_coef_rc, rh_rc = rh_coef_rc, base_rc = 0.61,
              latrans = 6, inftrans = 19, init_sites = 600, aggr = 1,
              site_max = 100000, rr_physiol_senesc = 0.01, rrg = 0.1, ...))
}

leaf_blast <- function(wth, emergence = "2000-05-15", ...) {
  age_coef_rc <- cbind(0:24 * 5, c(1, 1, 1, 0.9, 0.8, 0.7, 0.64, 0.59, 0.53,
                                   0.43, 0.32, 0.22, 0.16, 0.09, 0.03, 0.02,
                                   0.02, 0.02, 0.01, 0.01, 0.01, 0.01, 0.01,
                                   0.01, 0.01))
  temp_coef_rc <- cbind(2:9 * 5, c(0, 0.5, 1, 0.6, 0.2, 0.05, 0.01, 0))
  rh_coef_rc <- cbind(4 + (0:10) * 2,
                      c(0, 0.02, 0.09, 0.19, 0.29, 0.43, 0.54, 0.63, 0.77,
                        0.88, 1.0))
  return(SEIR(wth = wth, emergence = emergence,
              age_rc = age_coef_rc, tmp_rc = temp_coef_rc, rh_rc = rh_coef_rc,
              base_rc = 1.14, latrans = 5, inftrans = 20, init_sites = 600,
              aggr = 1, site_max = 30000, rr_physiol_senesc = 0.01,
              rrg = 0.1, ...))
}

bacterial_blight <- function(wth, emergence = "2000-05-15", ...) {
  age_coef_rc <- cbind(0:12 * 10, c( 1, 1, 1, 0.9, 0.62, 0.43, 0.41, 0.42, 0.41,
                                    0.41, 0.41, 0.41, 0.41))
  temp_coef_rc <- cbind(16 + (0:8 * 3), c(0, 0.29, 0.44, 0.90, 0.90, 1.0, 0.88,
                                          0.01, 0))
  rh_coef_rc <- cbind(c(2, 1:8 * 3), c(0, 0.67, 0.81, 0.84, 0.87, 0.91, 0.94,
                                      0.97, 1.0))
  return(SEIR(wth = wth, emergence = emergence,
              age_rc = age_coef_rc, tmp_rc = temp_coef_rc, rh_rc = rh_coef_rc,
              base_rc = 0.87, latrans = 5, inftrans = 30,
              site_max = 3200, aggr = 4, init_sites = 100,
              rr_physiol_senesc = 0.01, rrg = 0.1, ...))
}

sheath_blight <- function(wth, emergence = "2000-05-15", ...) {
  age_coef_rc <- cbind(0:12 * 10, c(0.84, 0.84, 0.84, 0.84, 0.84, 0.84, 0.83,
                                    0.88, 0.88, 1.0, 1.0, 1.0, 1.0))
  rh_coef_rc <- cbind(c(8, 3:8 * 3), c(0, 0.24, 0.41, 0.68, 0.94, 0.97, 1.0))
  temp_coef_rc <- cbind(3:10 * 4, c(0, 0.42, 0.94, 0.94, 1.0, 0.85, 0.64, 0))
  return(SEIR(wth = wth, emergence = emergence,
              age_rc = age_coef_rc, tmp_rc = temp_coef_rc, rh_rc = rh_coef_rc,
              base_rc = 0.46, latrans = 3, inftrans = 120,
              site_max = 800, aggr = 2.8, init_sites = 25,
              rr_physiol_senesc = 0.005, rrg = 0.2, ...))
}

tungro <- function(wth, emergence = "2000-05-15", ...) {
  age_coef_rc <- cbind(0:8 * 15, c(1.0, 1.0, 0.98, 0.73, 0.51, 0.34, 0, 0, 0))
  temp_coef_rc <- cbind(c(9, 10 + (0:9 * 3.1111), 40),
                        c(0, 0.13, 0.65, 0.75, 0.83, 0.89, 0.93, 0.97, 1.0,
                          0.96, 0.93, 0))
  rh_coef_rc <- 1
  return(SEIR(wth = wth, emergence = emergence,
              age_rc = age_coef_rc, tmp_rc = temp_coef_rc, rh_rc = rh_coef_rc,
              base_rc = 0.18, latrans = 6, inftrans = 120, site_max = 100,
              aggr = 1, init_sites = 100, rr_physiol_senesc = 0.01,
              rrg = 0.1, ...))
}

SEIR <- function(wth, emergence, onset = 15, duration = 120, rhlim = 90,
                 rainlim = 5, wetness = 0, init_sites,
                 init_infection = 1, age_rc, tmp_rc, rh_rc, base_rc, latrans,
                 inftrans, site_max, aggr, rr_physiol_senesc, rrg,
                 senesc_type = 1) {
  infday <- NULL
  leaf_wet <- NULL

  emergence <- as.Date(emergence)
  wth@w <- subset(wth@w, wth@w$date >= emergence - 1)
  if (dim(wth@w)[1] < duration) {
    stop("Incomplete weather data")
    }
  wth@w <- wth@w[1:(duration + 1), ]

  if (wetness  ==  1) {
    W <- leaf_wet(wth, simple = TRUE)
  }

  # outputvars
  cofr <- rc <- RHCoef <- latency <- infectious <- severity <- rsenesced <-
    rgrowth <- rtransfer <- infection <- diseased <- senesced <- removed <-
    now_infectious <- now_latent <- sites <- total_sites <-
    rep(0, times = duration + 1)

  for (day in 0:duration) {
    # State calculations
    if (day == 0) {
      # start crop growth
      sites[day + 1] <- init_sites
      rsenesced[day + 1] <- rr_physiol_senesc * sites[day + 1]
    } else {
      if (day > inftrans) {
        removed_today <- infectious[infday + 2]
      } else {
        removed_today <- 0
      }

      sites[day + 1] <- sites[day] + rgrowth[day] - infection[day] -
        rsenesced[day]
      rsenesced[day + 1] <- removed_today * senesc_type +
        rr_physiol_senesc * sites[day + 1]
      senesced[day + 1] <- senesced[day] + rsenesced[day]

      latency[day + 1] <- infection[day]
      latday <- day - latrans + 1
      latday <- max(0, latday)
      now_latent[day + 1] <- sum(latency[latday:day + 1])

      infectious[day + 1] <- rtransfer[day]
      infday <- day - inftrans + 1
      infday <- max(0, infday)
      now_infectious[day + 1] <- sum(infectious[infday:day + 1])
    }

    if (sites[day + 1] < 0 ) {
      sites[day + 1] <- 0
      break
    }

    if (wetness == 0) {
      if (wth@w$rhmax[day + 1] == rhlim | wth@w$prec[day + 1] >= rainlim) {
        RHCoef[day + 1] <- 1
      }
    } else {
      RHCoef[day + 1] <- afgen(rh_rc, W[day + 1])
    }

    rc[day + 1] <- base_rc * afgen(age_rc, day) *
      afgen(tmp_rc, wth@w$tavg[day + 1]) * RHCoef[day + 1]
    diseased[day + 1] <- sum(infectious) +
      now_latent[day + 1] + removed[day + 1]
    removed[day + 1] <- sum(infectious) - now_infectious[day + 1]

    cofr[day + 1] <- 1 - (diseased[day + 1] /
                            (sites[day + 1] + diseased[day + 1]))

    if (day == onset) {
      # initialization of the disease
      infection[day + 1] <- init_infection
    } else if (day > onset) {
      infection[day + 1] <- now_infectious[day + 1] *
        rc[day + 1] * (cofr[day + 1] ^ aggr)
    } else {
      infection[day + 1] <- 0
    }

    if (day >=  latrans) {
      rtransfer[day + 1] <- latency[latday + 1]
    } else {
      rtransfer[day + 1] <- 0
    }

    total_sites[day + 1] <- diseased[day + 1] + sites[day + 1]
    rgrowth[day + 1] <- rrg * sites[day + 1] *
      (1 - (total_sites[day + 1] / site_max))
    severity[day + 1] <- (diseased[day + 1] - removed[day + 1]) /
      (total_sites[day + 1] - removed[day + 1]) * 100
  }

  res <- cbind(0:duration, sites, now_latent, now_infectious, removed,
               senesced, infection, rtransfer, rgrowth, rsenesced, diseased,
               severity)
  res <- as.data.frame(res[1:(day + 1), ])

  dates <- seq(emergence - 1, emergence + duration, 1)
  res <- cbind(dates[1:(day + 1)], res)
  colnames(res) <- c("date", "simday", "sites", "latent", "infectious",
                     "removed", "senesced", "rateinf", "rtransfer", "rgrowth",
                     "rsenesced", "diseased", "severity")

  result <- new("SEIR")
  result@d <- res
  return(result)
}
