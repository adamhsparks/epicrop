
#' Susceptible-Exposed-Infectious-Removed (SEIR) model framework
#'
#' This function is originally used by specific disease models in EPIRICE to
#'  model disease severity of several rice diseases.  Given proper values it can
#'  be used with other pathosystems as well.
#'
#' @param wth a data frame of weather on a daily time-step containing data with
#'  the following field names.
#'   \tabular{rl}{
#'   **YYYYMMDD**:\tab Date as Year Month Day (ISO8601).\cr
#'   **DOY**:\tab  Consecutive day of year, commonly called "Julian date".\cr
#'   **TM**:\tab Mean daily temperature (°C).\cr
#'   **TN**:\tab Minimum daily temperature (°C).\cr
#'   **TX**:\tab Maximum daily temperature (°C).\cr
#'   **TDEW**:\tab Mean daily dew point temperature (°C).\cr
#'   **RH**:\tab Mean daily relative humidity (%).\cr
#'   **RAIN**:\tab Mean daily rainfall (mm).\cr
#'   }
#' @param emergence expected date of plant emergence entered in `YYYY-MM-DD`
#' format. From Table 1 Savary \emph{et al.} 2012.
#' @param onset expected number of days until the onset of disease after
#' emergence date. From Table 1 Savary \emph{et al.} 2012.
#' @param duration simulation duration (growing season length). From Table 1
#'  Savary \emph{et al.} 2012.
#' @param rhlim threshold to decide whether leaves are wet or not (usually
#' 90 %). From Table 1 Savary \emph{et al.} 2012.
#' @param rainlim threshold to decide whether leaves are wet or not. From Table
#'  1 Savary \emph{et al.} 2012.
#' @param wetness_type simulate RHmax or rain threshold (0) or leaf wetness
#'  duration (1). From Table 1 Savary \emph{et al.} 2012.
#' @param H0 initial number of plant's healthy sites. From Table 1 Savary
#'  \emph{et al.} 2012.
#' @param I0 initial number of infective sites. From Table 1 Savary
#'  \emph{et al.} 2012.
#' @param RcA crop age curve for pathogen optimum. From Table 1 Savary
#'  \emph{et al.} 2012.
#' @param RcT temperature curve for pathogen optimum. From Table 1 Savary
#'  \emph{et al.} 2012.
#' @param RcW relative curve for pathogen optimum. From Table 1 Savary
#'  \emph{et al.} 2012.
#' @param RcOpt potential basic infection rate corrected for removals. From
#'  Table 1 Savary \emph{et al.} 2012.
#' @param i duration of infectious period. From Table 1 Savary
#'  \emph{et al.} 2012.
#' @param p duration of latent period. From Table 1 Savary \emph{et al.} 2012.
#' @param Sx maximum number of sites. From Table 1 Savary \emph{et al.} 2012.
#' @param a aggregation coefficient. From Table 1 Savary \emph{et al.} 2012.
#' @param RRS relative rate of physiological senescence. From Table 1 Savary
#'  \emph{et al.} 2012.
#' @param RRG relative rate of growth. From Table 1 Savary \emph{et al.} 2012.
#'
#' @references Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario,
#' J. Modeling and mapping potential epidemics of rice diseases globally. Crop
#' Protection, Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
#' <http://dx.doi.org/10.1016/j.cropro.2011.11.009>.>
#'
#' @examples
#' \donttest{
#' # get weather for IRRI Zeigler Experiment Station in wet season 2000
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#'
#' # provide suitable values for brown spot severity
#' RcA <-
#'   cbind(0:6 * 20, c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0))
#' RcT <-
#'   cbind(15 + (0:5) * 5, c(0, 0.06, 1.0, 0.85, 0.16, 0))
#' RcW <- cbind(0:8 * 3,
#'              c(0, 0.12, 0.20, 0.38, 0.46, 0.60, 0.73, 0.87, 1.0))
#' emergence <- "2000-07-15"
#'
#' (SEIR(
#'   wth = wth,
#'   emergence = emergence,
#'   onset = 20,
#'   duration = 120,
#'   RcA = RcA,
#'   RcT = RcT,
#'   RcW = RcW,
#'   RcOpt = 0.61,
#'   p =  6,
#'   i = 19,
#'   H0 = 600,
#'   a = 1,
#'   Sx = 100000,
#'   RRS = 0.01,
#'   RRG = 0.1
#' ))
#' }
#' @details \code{SEIR} is called by the following specific disease modelling
#'  functions:
#' * \code{\link{predict_bacterial_blight}},
#' * \code{\link{predict_brown_spot}},
#' * \code{\link{predict_leaf_blast}},
#' * \code{\link{predict_sheath_blight}},
#' * \code{\link{predict_tungro}}
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
#'   **rateinf**:\tab Rate of infection. \cr
#'   **rtransfer**:\tab Rate of transfer from latent to infectious sites. \cr
#'   **rgrowth**:\tab Rate of growth of healthy sites. \cr
#'   **rsenesced**:\tab Rate of senescence of healthy sites. \cr
#'   **diseased**:\tab Number of diseased (latent + infectious + removed)
#'    sites. \cr
#'   **severity**:\tab Disease severity or incidence (for tungro).\cr
#'   **LAT**:\tab Latitude value as provided by `wth` object.\cr
#'   **LON**:\tab Longitude value as provided by `wth` object.\cr
#'   }
#'
#' @export
#'
SEIR <-
  function(wth,
           emergence,
           onset,
           duration,
           rhlim = 90,
           rainlim = 5,
           wetness_type = 0,
           H0,
           I0 = 1,
           RcA,
           RcT,
           RcW,
           RcOpt,
           p,
           i,
           Sx,
           a,
           RRS,
           RRG) {
    # CRAN NOTE avoidance
    infday <- leaf_wet <- DOY <- YYYYMMDD <- LAT <- LON <- NULL #nocov

    # set date formats
    emergence <- as.Date(emergence)

    # create vector of dates
    dates <- seq(emergence - 1, emergence + duration, 1)

    # convert emergence date into Julian date, sequential day in year
    emergence_doy <- as.numeric(strftime(emergence, format = "%j"))

    # check that the dates roughly align
    if (!(emergence >= wth[1, YYYYMMDD]) |
        !(emergence_doy <= max(wth[, DOY]) + duration)) {
      stop("Incomplete weather data or dates do not align")
    }

    # subset weather data where date is greater than emergence minus one
    wth[DOY >= emergence_doy - 1, ]

    if (wetness_type == 1) {
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
        sites[day + 1] <- H0
        rsenesced[day + 1] <- RRS * sites[day + 1]
      } else {
        if (day > i) {
          removed_today <- infectious[infday + 2]
        } else {
          removed_today <- 0
        }

        sites[day + 1] <-
          sites[day] + rgrowth[day] - infection[day] -
          rsenesced[day]
        rsenesced[day + 1] <- removed_today + RRS * sites[day + 1]
        senesced[day + 1] <- senesced[day] + rsenesced[day]

        latency[day + 1] <- infection[day]
        latday <- day - p + 1
        latday <- max(0, latday)
        now_latent[day + 1] <- sum(latency[latday:day + 1])

        infectious[day + 1] <- rtransfer[day]
        infday <- day - i + 1
        infday <- max(0, infday)
        now_infectious[day + 1] <- sum(infectious[infday:day + 1])
      }

      if (sites[day + 1] < 0) {
        sites[day + 1] <- 0
        break
      }

      if (wetness_type == 0) {
        if (wth$RH[day + 1] == rhlim |
            wth$RAIN[day + 1] >= rainlim) {
          RHCoef[day + 1] <- 1
        }
      } else {
        RHCoef[day + 1] <- afgen(RcW, W[day + 1])
      }

      rc[day + 1] <- RcOpt * afgen(RcA, day) *
        afgen(RcT, wth$TM[day + 1]) * RHCoef[day + 1]
      diseased[day + 1] <- sum(infectious) +
        now_latent[day + 1] + removed[day + 1]
      removed[day + 1] <- sum(infectious) - now_infectious[day + 1]

      cofr[day + 1] <- 1 - (diseased[day + 1] /
                              (sites[day + 1] + diseased[day + 1]))

      if (day == onset) {
        # initialisation of the disease
        infection[day + 1] <- I0
      } else if (day > onset) {
        infection[day + 1] <- now_infectious[day + 1] *
          rc[day + 1] * (cofr[day + 1] ^ a)
      } else {
        infection[day + 1] <- 0
      }

      if (day >=  p) {
        rtransfer[day + 1] <- latency[latday + 1]
      } else {
        rtransfer[day + 1] <- 0
      }

      total_sites[day + 1] <- diseased[day + 1] + sites[day + 1]
      rgrowth[day + 1] <- RRG * sites[day + 1] *
        (1 - (total_sites[day + 1] / Sx))
      severity[day + 1] <- (diseased[day + 1] - removed[day + 1]) /
        (total_sites[day + 1] - removed[day + 1]) * 100
    } # end loop

    res <-
      data.table(
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
      )

    res[, dates := dates[1:(day + 1)]]

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

    res[, LAT := rep_len(wth[, LAT], .N)]
    res[, LON := rep_len(wth[, LON], .N)]

    return(res[])
  }
