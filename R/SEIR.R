
#' Susceptible-Exposed-Infectious-Removed (SEIR) Model
#'
#' This function is used by specific disease models in EPIRICE to model disease
#' severity.
#'
#' @param wth a data frame of weather on a daily time-step containing data
#' with the following field names.
#'
#' \describe{
#'   \item{YYYYMMDD}{Date in YYYY-MM-DD format}
#'   \item{DOY}{Numeric day of year, e.g. 1 - 365}
#'   \item{T2M}{Mean daily temperature}
#'   \item{T2MN}{Minimum daily temperature}
#'   \item{T2MX}{Maximum daily temperature}
#'   \item{RH2M}{Relative humidity}
#'   \item{RAIN}{Precipitation}
#' }
#' @param emergence expected date of plant emergence entered in `YYYY-MM-DD`
#' format (usually supplied through the disease model function)
#' @param onset expected number of days until the onset of disease after
#' emergence date
#' @param duration simulation duration
#' @param rhlim threshold to decide whether leaves are wet or not (usually
#' 90\%)
#' @param rainlim threshold to decide whether leaves are wet or not
#' @param wetness simulate RHmax or rain threshold (0) or leaf wetness duration
#' (1)
#' @param init_sites -
#' @param init_infection -
#' @param age_rc -
#' @param tmp_rc -
#' @param rh_rc -
#' @param base_rc corrected basic infection rate
#' @param latrans latent period
#' @param inftrans infectious period
#' @param site_max -
#' @param aggr -
#' @param rr_physiol_senesc relative rate of physiological senescence
#' @param rrg -
#' @param senesc_type -
#'
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario,
#' Adam H Sparks, Aji Sukarta
#'
#' @details SEIR is called by the following specific disease models:
#' \code{\link{predict_leaf_blast}}, \code{\link{predict_bacterial_blight}},
#' \code{\link{predict_brown_spot}}, \code{\link{predict_sheath_blight}}
#'
SEIR <-
  function(wth,
           emergence,
           onset = 15,
           duration = 120,
           rhlim = 90,
           rainlim = 5,
           wetness = 0,
           init_sites,
           init_infection = 1,
           age_rc,
           tmp_rc,
           rh_rc,
           base_rc,
           latrans,
           inftrans,
           site_max,
           aggr,
           rr_physiol_senesc,
           rrg,
           senesc_type = 1) {
    infday <- NULL
    leaf_wet <- NULL

    emergence <- as.Date(emergence)
    wth$date

    # convert emergence date into Julian date, sequential day in year
    emergence_doy <- as.numeric(strftime(emergence, format = "%j"))

    # subset weather data where date is greater than emergence minus one
    wth <- wth[wth$YYYYMMDD >= emergence - 1, ]

    if (dim(wth)[1] < duration) {
      stop("Incomplete weather data")
    }
    wth <- wth[1:(duration + 1), ]

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

      if (sites[day + 1] < 0) {
        sites[day + 1] <- 0
        break
      }

      if (wetness == 0) {
        if (wth$rhmax[day + 1] == rhlim |
            wth$prec[day + 1] >= rainlim) {
          RHCoef[day + 1] <- 1
        }
      } else {
        RHCoef[day + 1] <- afgen(rh_rc, W[day + 1])
      }

      rc[day + 1] <- base_rc * afgen(age_rc, day) *
        afgen(tmp_rc, wth$tavg[day + 1]) * RHCoef[day + 1]
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

    res <-
      cbind(
        0:duration,
        sites,
        now_latent,
        now_infectious,
        removed,
        senesced,
        infection,
        rtransfer,
        rgrowth,
        rsenesced,
        diseased,
        severity
      )
    res <- as.data.frame(res[1:(day + 1), ])

    dates <- seq(emergence - 1, emergence + duration, 1)
    res <- cbind(dates[1:(day + 1)], res)
    colnames(res) <-
      c(
        "date",
        "simday",
        "sites",
        "latent",
        "infectious",
        "removed",
        "senesced",
        "rateinf",
        "rtransfer",
        "rgrowth",
        "rsenesced",
        "diseased",
        "severity"
      )

    result <- methods::new("SEIR")
    result@d <- res
    return(result)
  }
