# Adapted from cropsim package version 0.2.0-5 by Adam H. Sparks - USQ CCH
# Author: Serge Savary & Rene Pangga.
# R translation: Robert J. Hijmans , Rene Pangga & Jorrel Aunario, r.hijmans@gmail.com (translated from STELLA TungroMod v6PRI)
# International Rice Research Institute
# Date :  23 August 2010
# Version 0.2
# Licence GPL v3
#switch :  wetness (1) uses rhcoefrc = 1 vs wetness (0)  uses RH + rain threshold corresponding to  STELLA  TungroModv5T

# onset = 25 deleted as component of tungro function, as onset is not a constant
#component on nb of rice crop per year should be added in the function
tungro2 <- function(wth, emergence = "2000-05-15", duration = 120, rhlim = 0,
                    rainlim = 0, wetness = 1, pcisrice = 1) {
  emergence <- as.Date(emergence)

  wth@w <- subset(wth@w, wth@w$date >= emergence - 1)
  if (dim(wth@w)[1] < duration) {
    stop("Incomplete weather data")
  }
  wth@w <- wth@w[1:(duration + 1), ]
  rrg <- 0.1
  senesc_type <- 1
  aggr   <- 1
  base_rc <- 0.18
  site_max <- 100
  init_infection <- 1
  init_sites <- 100
  infectious_transit_time <- 120
  latency_transit_time <- 6
  extinct_pri <- -0.025
  init_prev_rainfall_index <- 100
  early <- 25
  late <- 40
  infday <- NULL

  if (pcisrice  ==  1) {
    pc <- 1
  } else {
    pc <- 150
  }

  # Output vars
  rainout <- rainin <- prev_rainfall_index <- cofr <- rc <- rhcoef <- latency <-
    infectious <- incidence <- rsenesced <- rgrowth <- rtransfer <-
    rinfection <- diseased <- senesced <- removed <- now_infectious <-
    now_latent <- sites <- total_sites <- rep(0, times = duration + 1)

  #parameters
  agerc <- cbind(0:8 * 15, c(1.0, 1.0, 0.98, 0.73, 0.51, 0.34, 0, 0, 0))
  tmprc <- cbind(c(9, 10 + (0:9 * 3.1111), 40),
                 c(0, 0.13, 0.65, 0.75, 0.83, 0.89, 0.93, 0.97, 1.0, 0.96,
                   0.93, 0))
  rhcoefrc <- 1

  for (day in 0:duration) {

    # State calculations
    if (day == 0) {
      # start crop growth and Previous Rainfall Index
      sites[day + 1] <- 100
      prev_rainfall_index[day + 1] <- init_prev_rainfall_index
    } else {
      if (day > infectious_transit_time) {
        removed_today <- infectious[infday + 1]
      } else {
        removed_today <- 0
      }

      sites[day + 1] <- sites[day] + rgrowth[day] - rinfection[day] -
        rsenesced[day]

      latency[day + 1] <- rinfection[day]
      latday <- day - latency_transit_time + 1
      latday <- max(0, latday)
      now_latent[day + 1] <- sum(latency[latday:day + 1])

      infectious[day + 1] <- rtransfer[day]
      infday <- day - infectious_transit_time + 1
      infday <- max(0, infday)
      now_infectious[day + 1] <- sum(infectious[infday:day + 1])

      prev_rainfall_index[day + 1] <- prev_rainfall_index[day] + rainin[day] -
        rainout[day]
    }

    if (sites[day + 1] < 0 ) {
      sites[day + 1] <- 0
      break
    }

    if (day < 21) {
      rainin[day + 1] <- wth@w$prec[day + 1] * exp(extinct_pri)
      rainout[day + 1] <- prev_rainfall_index[day + 1] * (1 - exp(extinct_pri))
    } else {
      rainin[day + 1] <- 0
      rainout[day + 1] <- 0
    }

    if (prev_rainfall_index[day + 1] > 100) {
      epidonset_endemic <- early
    } else {
      epidonset_endemic <- late
    }
    onset <- pc * epidonset_endemic

    if (wetness == 0) {
      if (rhx[day + 1] >=  rhlim | rain[day + 1] >= rainlim) {
        rhcoef[day] <- 1
      }
    } else {
      rhcoef[day + 1] <- rhcoefrc
    }

    rc[day + 1] <- base_rc * afgen(agerc, day) *
      afgen(tmprc, wth@w$tavg[day + 1]) * rhcoef[day + 1]

    diseased[day + 1] <- sum(infectious) + now_latent[day + 1] +
      removed[day + 1]
    removed[day + 1] <- sum(infectious) - now_infectious[day + 1]

    cofr[day + 1] <- 1 - (diseased[day + 1] /
                            (sites[day + 1] + diseased[day + 1]))

    if (day  ==  onset) {
      rinfection[day + 1] <- init_infection
    } else if (day > onset) {
      rinfection[day + 1] <- now_infectious[day + 1] * rc[day + 1] *
        (cofr[day + 1] ^ aggr)
    } else {
      rinfection[day + 1] <- 0
    }

    # Boxcar transfer to other state
    if (day >=  latency_transit_time ) {

    rtransfer[day + 1] <- latency[latday + 1]
  } else {
    rtransfer[day + 1] <- 0
  }

  total_sites[day + 1] <- diseased[day + 1] + sites[day + 1]
  rgrowth[day + 1] <- rrg * sites[day + 1] *
    (1 - (total_sites[day + 1] / site_max))

  # consider natural senescence
  incidence[day + 1] <- (diseased[day + 1] - removed[day + 1]) /
    (total_sites[day + 1] - removed[day + 1]) * 100

}

res <- cbind(0:duration, sites, now_latent, now_infectious, removed, senesced,
             rinfection, rtransfer, rgrowth, rsenesced, diseased, incidence,
             prev_rainfall_index)
res <- as.data.frame(res[1:(day + 1), ])

dates <- seq(emergence - 1, emergence + duration, 1)
res <- cbind(dates[1:(day + 1)], res)

colnames(res) <- c("date", "simday", "sites", "latent", "infectious",
                   "removed", "senesced", "rateinf", "rtransfer", "rgrowth",
                   "rsenesced", "diseased", "incidence", "prev_rainfall_index")

result <- new("SEIR")
result@d <- res
return(result)
}
