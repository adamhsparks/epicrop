
#' Susceptible-Exposed-Infectious-Removed (SEIR) model framework
#'
#' This function is originally used by specific disease models in
#'  \sQuote{EPIRICE} to model disease severity of several rice diseases.  Given
#'  proper values it can be used with other pathosystems as well.
#'
#' @param wth a data frame of weather on a daily time-step containing data with
#'  the following field names.
#'
#'   **Field Name** | **Value**
#'   --------------:|:----------
#'   _YYYYMMDD_ | Date as Year Month Day (ISO8601)
#'   _DOY_ | Consecutive day of year, commonly called "Julian date"
#'   _TEMP_ | Mean daily temperature (°C)
#'   _RHUM_ | Mean daily temperature (°C)
#'   _RAIN_ | Mean daily rainfall (mm)
#'
#' @param emergence expected date of plant emergence (or transplanting for rice)
#'  entered in `YYYY-MM-DD` format.  Described in Table 1 of Savary _et al._
#'  2012.
#' @param onset expected number of days until the onset of disease after
#'  emergence date.  Described in Table 1 of Savary _et al._ 2012.
#' @param duration simulation duration *i.e.*, growing season length (day).
#'  Described in Table 1 of Savary _et al._ 2012.
#' @param rhlim relative humidity value threshold to decide whether leaves are
#'  wet or not (numeric).  Savary _et al._ 2012 used 90%.
#' @param rainlim rainfall amount (mm) threshold to decide whether leaves are
#'  wet or not (numeric).  Savary _et al._ 2012 used 5mm.
#' @param H0 initial number of plant's healthy sites. Described in Table 1 of
#'  Savary _et al._ 2012.
#' @param I0 initial number of infective sites (numeric). Described in Table 1
#'  of Savary _et al._ 2012.
#' @param RcA modifier for _Rc_ (the basic infection rate corrected for
#'  removals) for crop age. Described in Table 1 of Savary _et al._ 2012.
#' @param RcT modifier for _Rc_ (the basic infection rate corrected for
#'  removals) for temperature. Described in Table 1 of Savary _et al._ 2012.
#' @param RcOpt potential basic infection rate corrected for removals. Derived
#'  from Table 1 of Savary _et al._ 2012.
#' @param i duration of infectious period (day).  Described in Table 1 of Savary
#'  _et al._ 2012.
#' @param p duration of latent period (day).  Described in  Table 1 of Savary
#'  _et al._ 2012.
#' @param Sx maximum number of sites.  Described in Table 1 of Savary _et al._
#'  2012.
#' @param a aggregation coefficient, values are from 1 to >1 (numeric).
#'  Described in Table 1 of Savary _et al._ 2012. See further details in **_a_ -
#'  Aggregation** section.
#' @param RRS relative rate of physiological senescence.  Described in Table 1
#'  of Savary _et al._ 2012.
#' @param RRG relative rate of growth.  Described in Table 1 of Savary _et al._
#'  2012.
#'
#' @references Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario,
#' J. Modeling and mapping potential epidemics of rice diseases globally. *Crop
#' Protection*, Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
#' <http://dx.doi.org/10.1016/j.cropro.2011.11.009>.
#'
#' @examplesIf interactive()
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
#' emergence <- "2000-07-15"
#'
#' (SEIR(
#'   wth = wth,
#'   emergence = emergence,
#'   onset = 20,
#'   duration = 120,
#'   RcA = RcA,
#'   RcT = RcT,
#'   RcOpt = 0.61,
#'   p =  6,
#'   i = 19,
#'   H0 = 600,
#'   a = 1,
#'   Sx = 100000,
#'   RRS = 0.01,
#'   RRG = 0.1
#' ))
#'
#' @details # _a_ - Aggregation
#' When _a_ is set to 1 the assumption is that that there is no disease
#' aggregation with new infections occurring at random among the healthy sites.
#' When _a_ is greater than 1 there is aggregation in the disease occurrence,
#' the pathogen is unable to access the entire population of healthy sites,
#' which results in disease aggregation. Refer to Savary _et al._ (2012) for
#' greater detail.
#'
#' @seealso
#' `SEIR()` is called by the following specific disease modelling functions:
#' * [predict_bacterial_blight()],
#' * [predict_brown_spot()],
#' * [predict_leaf_blast()],
#' * [predict_sheath_blight()],
#' * [predict_tungro()]
#'
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario,
#' Adam H. Sparks, Aji Sukarta
#'
#' @return A [data.table::data.table()] containing the following columns:
#'
#'   **Field Name** | **Value**
#'   --------------:|:----------
#'   _simday_| Zero indexed day of simulation that was run
#'   _dates_|  Date of simulation
#'   _sites_| Total number of sites present on day "x"
#'   _latent_| Number of latent sites present on day "x"
#'   _infectious_| Number of infectious sites present on day "x"
#'   _removed_| Number of removed sites present on day "x"
#'   _senesced_| Number of senesced sites present on day "x"
#'   _rateinf_| Rate of infection
#'   _rtransfer_| Rate of transfer from latent to infectious sites
#'   _rgrowth_| Rate of growth of healthy sites
#'   _rsenesced_| Rate of senescence of healthy sites
#'   _rlex_| Rate of lesion expansion
#'   _diseased_| Number of diseased (latent + infectious + removed) sites
#'   _severity_| Disease severity or incidence (for tungro)
#'   _lat_| Latitude value as provided by `wth` object
#'   _lon_| Longitude value as provided by `wth` object
#'
#' @export
#'
SEIR <-
  function(wth,
           emergence,
           onset,
           duration,
           rhlim,
           rainlim,
           H0,
           I0,
           RcA,
           RcT,
           RcOpt,
           p,
           i,
           Sx,
           a,
           RRS,
           RRG) {
    # CRAN NOTE avoidance
    infday <- YYYYMMDD <- lat <- lon <- LAT <- LON <- NULL #nocov

    if (a < 1) {
      stop(call. = FALSE,
           "`a` cannot be set to less than 1. Valid aggregation values, `a`,",
           " are from 1 to > 1.")
    }

    # set date formats
    emergence <- as.Date(emergence)

    # create vector of dates
    dates <- seq(emergence - 1, emergence + duration, 1)

    # check that the dates roughly align
    if (!(emergence >= wth[1, YYYYMMDD]) |
        (max(dates) > max(wth[, YYYYMMDD]))) {
      stop("Incomplete weather data or dates do not align")
    }

    # subset weather data where date is greater than emergence minus one and
    # less than duration
    wth <-
      wth[YYYYMMDD %between% c(emergence - 1, emergence + duration)]

    # outputvars
    cofr <-
      rc <-
      RHCoef <- latency <- infectious <- severity <- rsenesced <-
      rgrowth <-
      rtransfer <- infection <- diseased <- senesced <- removed <-
      now_infectious <-
      now_latent <- sites <- total_sites <- rrlex <-
      rep(0, times = duration + 1)

    for (day in 0:duration) {
      # State calculations for() loop -----
      cs_1 <- day + 1
      if (day == 0) {
        # start crop growth
        sites[cs_1] <- H0
        rsenesced[cs_1] <- RRS * sites[cs_1]
      } else {
        if (day > i) {
          removed_today <- infectious[infday + 2]
        } else {
          removed_today <- 0
        }

        sites[cs_1] <-
          sites[day] + rgrowth[day] - infection[day] -
          rsenesced[day]
        rsenesced[cs_1] <- removed_today + RRS * sites[cs_1]
        senesced[cs_1] <- senesced[day] + rsenesced[day]

        latency[cs_1] <- infection[day]
        latday <- day - p + 1
        latday <- max(0, latday)
        now_latent[day + 1] <- sum(latency[latday:day + 1])

        infectious[day + 1] <- rtransfer[day]
        infday <- day - i + 1
        infday <- max(0, infday)
        now_infectious[day + 1] <- sum(infectious[infday:day + 1])
      }

      cs_2 <- day + 1
      if (sites[cs_2] < 0) {
        sites[cs_2] <- 0
        break
      }

      if (wth$RHUM[cs_2] == rhlim |
          wth$RAIN[cs_2] >= rainlim) {
        RHCoef[cs_2] <- 1
      }

      cs_3 <- day + 1
      rc[cs_3] <- RcOpt * select_mod_val(xy = RcA, x = day) *
        select_mod_val(xy = RcT, x = wth$TEMP[day + 1]) * RHCoef[cs_3]

      cs_4 <- day + 1
      diseased[cs_3] <- sum(infectious) +
        now_latent[cs_4] + removed[cs_4]

      cs_5 <- day + 1
      removed[cs_4] <- sum(infectious) - now_infectious[cs_5]

      cofr[cs_5] <- 1 - (diseased[cs_5] / (sites[cs_5] + diseased[cs_5]))

      if (day == onset) {
        # initialisation of the disease
        infection[cs_5] <- I0
      } else if (day > onset) {
        infection[cs_5] <- now_infectious[cs_5] * rc[cs_5] * (cofr[cs_5] ^ a)
      } else {
        infection[cs_5] <- 0
      }

      if (day >=  p) {
        rtransfer[cs_5] <- latency[latday + 1]
      } else {
        rtransfer[cs_5] <- 0
      }

      total_sites[cs_5] <- diseased[cs_5] + sites[cs_5]
      rgrowth[cs_5] <- RRG * sites[cs_5] * (1 - (total_sites[cs_5] / Sx))
      severity[cs_5] <- (diseased[cs_5] - removed[cs_5]) /
        (total_sites[cs_5] - removed[cs_5]) * 100
    } # end loop

    res <-
      data.table(
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

    res[, dates := dates[1:(day + 1)]]
    res[, lat := rep_len(wth[, LAT], .N)]
    res[, lon := rep_len(wth[, LON], .N)]

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
        "dates",
        "lat",
        "lon"
      )
    )

    setcolorder(res, c("simday", "dates"))

    return(res[])
  }

#' Select a modifier value from a given curve
#'
#' Takes a matrix and numeric value for the _i_th day in the duration of a
#' `for()` loop and returns a value from the matrix corresponding to the value
#' along the corresponding correction curve, either `RcA` (removals corrected
#' for crop age) or `RcT` (removals corrected for temperature).
#'
#' @param xy a matrix of modifier values for the rate of removals corrected for
#'  crop age, `RcA`, or temperature, `RcT` representing a fitted curve.
#' @param x a numeric value indicating the _i_th day of the duration of the run
#'  for crop age, `RcA`, or the temperature value, `RcT`, on the _i_th day.
#'
#' @note The original author of afgen() function is Robert J. Hijmans
#'  This was adapted from the \R package \pkg{cropsim} for \pkg{epicrop}
#'  by Adam H. Sparks under the GPL3 License.
#'
#' @examples
#' day <- 1
#' RcA <-
#'   cbind(0:6 * 20, c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0))
#' select_mod_val(xy = RcA, x = day)
#'
#' @return A numeric value corresponding to the _i_th day's value
#'
#' @noRd
select_mod_val <- function(xy, x) {
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
      # calculate point on curve if no previous match in xy matrix
    } else {
      res <- int[1, 2] + (x - int[1, 1]) *
        ((int[2, 2] - int[1, 2]) /
           (int[2, 1] - int[1, 1]))
    }
  }
  return(res[[1]])
}
