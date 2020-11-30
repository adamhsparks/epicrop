
#' Susceptible-Exposed-Infectious-Removed (SEIR) model
#'
#' This function is used by specific disease models in EPIRICE to model disease
#' severity of several rice diseases.  It should be generic enough to port to
#' other pathosystems.
#'
#' @param wth a data frame of weather on a daily time-step containing data
#' with the following field names.
#'   * YYYYMMDD Date in YYYY-MM-DD format
#'   * DOY Numeric day of year, e.g. 1 - 365
#'   * T2M Mean daily temperature
#'   * T2MN Minimum daily temperature
#'   * T2MX Maximum daily temperature
#'   * RH2M Relative humidity
#'   * RAIN Precipitation
#' @param emergence expected date of plant emergence entered in `YYYY-MM-DD`
#' format (usually supplied through the disease model function)
#' @param onset expected number of days until the onset of disease after
#' emergence date
#' @param duration simulation duration
#' @param rhlim threshold to decide whether leaves are wet or not (usually
#' 90 pct)
#' @param rainlim threshold to decide whether leaves are wet or not
#' @param wetness simulate RHmax or rain threshold (0) or leaf wetness duration
#' (1)
#' @param init_sites Number of initial infection sites
#' @param init_infection NA
#' @param age_rc NA
#' @param tmp_rc NA
#' @param rh_rc NA
#' @param base_rc corrected basic infection rate
#' @param latrans latent period
#' @param inftrans infectious period
#' @param site_max NA
#' @param aggr NA
#' @param rr_physiol_senesc relative rate of physiological senescence
#' @param rrg NA
#' @param senesc_type NA
#'
#' @examples
#' \donttest{
#' # get weather for IRRI Zeigler Experiment Station in dry season 2000
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-01-15", "2000-05-31")
#' )
#'
#' # provide suitable values for brown spot severity
#' age_rc <-
#'   cbind(0:6 * 20, c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0))
#' tmp_rc <-
#'   cbind(15 + (0:5) * 5, c(0, 0.06, 1.0, 0.85, 0.16, 0))
#' rh_rc <- cbind(0:8 * 3,
#'              c(0, 0.12, 0.20, 0.38, 0.46, 0.60, 0.73, 0.87, 1.0))
#' emergence <- "2000-01-15"
#'
#' x <- SEIR(
#'   wth = wth,
#'   emergence = emergence,
#'   age_rc = age_rc,
#'   tmp_rc = tmp_rc,
#'   rh_rc = rh_rc,
#'   base_rc = 0.61,
#'   latrans = 6,
#'   inftrans = 19,
#'   init_sites = 600,
#'   aggr = 1,
#'   site_max = 100000,
#'   rr_physiol_senesc = 0.01,
#'   rrg = 0.1
#' )
#' }
#' @details \code{SEIR} is called by the following specific disease models:
#' \code{\link{predict_leaf_blast}}, \code{\link{predict_bacterial_blight}},
#' \code{\link{predict_brown_spot}}, \code{\link{predict_sheath_blight}}
#'
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario,
#' Adam H. Sparks, Aji Sukarta
#'
#' @return A \code{\link[data.table]{data.table}} containing the following
#'  columns
#'   \tabular{rl}{
#'   **simday**:\tab Zero indexed day of simulation that was run.\cr
#'   **dates**:\tab  Date of simulation.\cr
#'   **sites**:\tab Total number of sites present on day "x".\cr
#'   **latent**:\tab Number of latent sites present on day "x".\cr
#'   **infectious**:\tab Number of infectious sites present on day "x".\cr
#'   **removed**:\tab Number of removed sites present on day "x".\cr
#'   **senesced**:\tab Number of senesced sites present on day "x".\cr
#'   **rateinf**:\tab ...\cr
#'   **rtransfer**:\tab ...\cr
#'   **rgrowth**:\tab ...\cr
#'   **rsenesced**:\tab  ...\cr
#'   **diseased**:\tab  ...\cr
#'   **severity**:\tab  Disease severity or incidence (for tungro).\cr
#'   }
#'
#' @export
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
    # CRAN NOTE avoidance
    infday <- leaf_wet <- DOY <- NULL #nocov

    # set date formats
    emergence <- as.Date(emergence)

    # create vector of dates
    dates <- seq(emergence - 1, emergence + duration, 1)

    # convert emergence date into Julian date, sequential day in year
    emergence_doy <- as.numeric(strftime(emergence, format = "%j"))

    if (dim(wth)[1] < duration) {
      stop("Incomplete weather data")
    }

    # subset weather data where date is greater than emergence minus one
    wth[DOY >= emergence_doy - 1, ]

    if (wetness == 1) {
      W <- .leaf_wet(wth, simple = TRUE)
    }

    # outputvars
    cofr <-
      rc <-
      RHCoef <- latency <- infectious <- severity <- rsenesced <-
      rgrowth <-
      rtransfer <- infection <- diseased <- senesced <- removed <-
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

        sites[day + 1] <-
          sites[day] + rgrowth[day] - infection[day] -
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
        if (wth$RH[day + 1] == rhlim |
            wth$RAIN[day + 1] >= rainlim) {
          RHCoef[day + 1] <- 1
        }
      } else {
        RHCoef[day + 1] <- afgen(rh_rc, W[day + 1])
      }

      rc[day + 1] <- base_rc * afgen(age_rc, day) *
        afgen(tmp_rc, wth$TM[day + 1]) * RHCoef[day + 1]
      diseased[day + 1] <- sum(infectious) +
        now_latent[day + 1] + removed[day + 1]
      removed[day + 1] <- sum(infectious) - now_infectious[day + 1]

      cofr[day + 1] <- 1 - (diseased[day + 1] /
                              (sites[day + 1] + diseased[day + 1]))

      if (day == onset) {
        # initialisation of the disease
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
    } # end loop

    res <-
      data.table(cbind(
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
      ))

    res[, dates := dates[1:(day + 1)]][]

    setnames(
      res,
      c(
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
        "severity",
        "dates"
      )
    )

    setcolorder(res, c("simday", "dates"))

    return(res)
  }
