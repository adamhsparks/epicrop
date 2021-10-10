
#' Susceptible-Exposed-Infectious-Removed (SEIR) model framework
#'
#' This function is originally used by specific disease models in
#'  \sQuote{EPIRICE} to model disease intensity of several rice diseases.  Given
#'  proper values it can be used with other pathosystems as well.
#'
#' @param wth a `data.frame` of weather on a daily time-step containing data
#' with the following field names.
#'
#' \describe{
#'   \item{YYYYMMDD}{Date as Year Month Day (ISO8601)}
#'   \item{DOY}{Consecutive day of year, commonly called "Julian date"}
#'   \item{TEMP}{Mean daily temperature (°C)}
#'   \item{RHUM}{Mean daily temperature (°C)}
#'   \item{RAIN}{Mean daily rainfall (mm)}
#'   \item{LAT}{Optional. Latitude of weather observation. See LAT/LON Section.}
#'   \item{LON}{Optional. Longitude of weather observation.
#'    See LAT/LON Section.}
#' }
#'
#' @param emergence expected date of plant emergence (or transplanting for rice)
#'  entered in `YYYY-MM-DD` format (character).  Described in Table 1 of Savary
#'  _et al._ 2012.
#' @param onset expected number of days until the onset of disease after
#'  emergence date (day, integer).  Described in Table 1 of Savary _et al._
#'  2012.
#' @param duration simulation duration *i.e.*, growing season length (day,
#'  integer).  Described in Table 1 of Savary _et al._ 2012.
#' @param rhlim relative humidity value threshold to decide whether leaves are
#'  wet or not (numeric).  Savary _et al._ 2012 used 90%.
#' @param rainlim rainfall amount (mm) threshold to decide whether leaves are
#'  wet or not (numeric).  Savary _et al._ 2012 used 5mm.
#' @param H0 initial number of plant's healthy sites (integer).  Described in
#'  Table 1 of Savary _et al._ 2012.
#' @param I0 initial number of infective sites (integer).  Described in Table 1
#'  of Savary _et al._ 2012.
#' @param RcA modifier for _Rc_ (the basic infection rate corrected for
#'  removals) for crop age (numeric vector).  Described in Table 1 of Savary
#'  _et al._ 2012.
#' @param RcT modifier for _Rc_ (the basic infection rate corrected for
#'  removals) for temperature (numeric vector).  Described in Table 1 of Savary
#'  _et al._ 2012.
#' @param RcOpt potential basic infection rate corrected for removals (numeric).
#'  Derived from Table 1 of Savary _et al._ 2012.
#' @param i duration of infectious period (day, integer).  Described in Table 1
#'  of Savary _et al._ 2012.
#' @param p duration of latent period (day, integer).  Described in  Table 1 of
#'  Savary _et al._ 2012.
#' @param Sx maximum number of sites (integer).  Described in Table 1 of Savary
#'  _et al._ 2012.
#' @param a aggregation coefficient, values are from 1 to >1 (numeric).
#'  Described in Table 1 of Savary _et al._ 2012.  See further details in
#'  **_a_ - Aggregation** section.
#' @param RRS relative rate of physiological senescence (numeric).  Described in
#'  Table 1 of Savary _et al._ 2012.
#' @param RRG relative rate of growth (numeric).  Described in Table 1 of Savary
#'  _et al._ 2012.
#'
#' @references
#' Sparks, A.H., P.D. Esker, M. Bates, W. Dall' Acqua, Z. Guo, V. Segovia, S.D.
#' Silwal, S. Tolos, and K.A. Garrett, 2008. Ecology and Epidemiology in R:
#' Disease Progress over Time. *The Plant Health Instructor*.
#' DOI:[10.1094/PHI-A-2008-0129-02]https://doi.org/10.1094/PHI-A-2008-0129-02).
#'
#' Madden, L. V., G. Hughes, and F. van den Bosch. 2007. The Study of Plant
#' Disease Epidemics. American Phytopathological Society, St. Paul, MN.
#' DOI:[10.1094/9780890545058](https://doi.org/10.1094/9780890545058).
#'
#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario,
#' J. Modeling and mapping potential epidemics of rice diseases globally. _Crop
#' Protection_, Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
#' [10.1016/j.cropro.2011.11.009](https://dx.doi.org/10.1016/j.cropro.2011.11.009).
#'
#' @examplesIf interactive()
#' # get weather for IRRI Zeigler Experiment Station in wet season 2000
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#'
#' # provide suitable values for brown spot intensity
#' RcA <-
#'   cbind(c(0L, 20L, 40L, 60L, 80L, 100L, 120L),
#'         c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0))
#' RcT <-
#'   cbind(c(15L, 20L, 25L, 30L, 35L, 40L),
#'         c(0, 0.06, 1.0, 0.85, 0.16, 0))
#' emergence <- "2000-07-15"
#'
#' (SEIR(
#'   wth = wth,
#'   emergence = emergence,
#'   onset = 20,
#'   duration = 120,
#'   rhlim = 90,
#'   rainlim = 5,
#'   RcA = RcA,
#'   RcT = RcT,
#'   RcOpt = 0.61,
#'   p =  6,
#'   i = 19,
#'   H0 = 600,
#'   I0 = 1,
#'   a = 1,
#'   Sx = 100000,
#'   RRS = 0.01,
#'   RRG = 0.1
#' ))
#'
#' @details # _a_ - Aggregation
#' When _a_ is set to `1` the assumption is that that there is no disease
#' aggregation with new infections occurring at random among the healthy sites.
#' When _a_ is greater than `1` there is aggregation in the disease occurrence,
#' the pathogen is unable to access the entire population of healthy sites,
#' which results in disease aggregation. Refer to Savary _et al._ (2012) for
#' greater detail.
#'
#' @details # _LAT_/_LON_
#' If the `wth` object provides _LAT_ and _LON_ columns, these will be included
#' in the output for mapping purposes. Both values must be present. These
#' columns are provided by default when using [get_wth()].
#'
#' @seealso
#' `SEIR()` is called by the following specific disease modelling functions:
#' * [predict_bacterial_blight()],
#' * [predict_brown_spot()],
#' * [predict_leaf_blast()],
#' * [predict_sheath_blight()],
#' * [predict_tungro()]
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#'
#' @return A [data.table()] containing the following columns:
#'
#' \describe{
#'   \item{simday}{Zero indexed day of simulation that was run}
#'   \item{dates}{Date of simulation}
#'   \item{sites}{Total number of sites present on day "x"}
#'   \item{latent}{Number of latent sites present on day "x"}
#'   \item{infectious}{Number of infectious sites present on day "x"}
#'   \item{removed}{Number of removed sites present on day "x"}
#'   \item{senesced}{Number of senesced sites present on day "x"}
#'   \item{ratinf}{Rate of infection}
#'   \item{rtransfer}{Rate of transfer from latent to infectious sites}
#'   \item{rgrowth}{Rate of growth of healthy sites}
#'   \item{rsenesced}{Rate of senescence of healthy sites}
#'   \item{rlex}{Rate of lesion expansion}
#'   \item{diseased}{Number of diseased (latent + infectious + removed) sites on
#'    day "x"}
#'   \item{intensity}{Proportion of diseased (latent + infectious + removed)
#'    sites per total sites not including removed sites on day "x"}
#'   \item{audpc}{Area under the disease progress curve \acronym{AUDPC} for the
#'    simulation}
#'   \item{lat}{Latitude value if provided by the `wth` object}
#'   \item{lon}{Longitude value if provided by the `wth` object}
#' }
#'
#' @export

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

    # set wth input as a data.table object if it's not already one, else this
    # function will fail on line 182
    if (!inherits(wth, "data.table")) {
      wth <- as.data.table(wth)
    }

    # check aggregation values
    if (a < 1) {
      stop(
        call. = FALSE,
        "`a` cannot be set to less than 1. Valid aggregation values, `a`,",
        " are from 1 to > 1."
      )
    }

    # check site values
    if (H0 < 0) {
      stop(
        call. = FALSE,
        "H0 cannot be < 0, check your initial number of healthy sites."
      )
    }

    if (I0 < 0) {
      stop(
        call. = FALSE,
        "I0 cannot be < 0, check your initial number of infective sites."
      )
    }

    # Set and check dates ----
    emergence <- as.Date(emergence)
    harvest <- emergence + sum(duration, -1)
    dates <- seq(from = emergence, to = harvest, by = "day")

    # check that the dates roughly align
    if (!(emergence >= wth[1, YYYYMMDD]) ||
        (max(dates) > max(wth[, YYYYMMDD]))) {
      stop(call. = FALSE,
           "Incomplete weather data or dates do not align")
    }

    # subset weather data where date is greater than emergence minus one and
    # less than duration
    if (nrow(wth) > duration) {
      wth <-
        wth[YYYYMMDD %between% c(emergence, emergence + sum(duration, -1))]
    }

    # Create vectors for referencing ----
    wth_rain <- wth$RAIN
    wth_rhum <- wth$RHUM
    Rc_temp <- .fn_Rc(.Rc = RcT, .xout = wth$TEMP)
    Rc_age <- .fn_Rc(.Rc = RcA, .xout = 0:duration)

    # Create output vars
    cofr <-
      rc <-
      RcW <- latency <- infectious <- intensity <- rsenesced <-
      rgrowth <-
      rtransfer <- infection <- diseased <- senesced <- removed <-
      now_infectious <- now_latent <- sites <- total_sites <-
      vector(mode = "double", length = duration)

    # Calculate state values -----
    for (d in 1:duration) {
      d_1 <- sum(d, -1)

      if (d == 1) {
        # start crop growth
        sites[d] <- H0
        rsenesced[d] <- RRS * sites[d]
      } else {
        if (d > i) {
          # as this never happens when `d > inftrans` so `infday` is assigned
          # a non-NULL value, when the loop comes back around it's then present,
          # so until `d > i`, `removed_today` will always be equal to "0" since
          # the disease has not had time to completely transit the infectious
          # period as specified by parameter `i` - AHS
          removed_today <- infectious[infday + 1]
        } else {
          removed_today <- 0
        }

        sites[d] <-
          sum(sites[d_1], rgrowth[d_1], -infection[d_1], -rsenesced[d_1])
        rsenesced[d] <- sum(removed_today, RRS * sites[d])
        senesced[d] <- sum(senesced[d_1], rsenesced[d_1])

        latency[d] <- infection[d_1]
        latday <- sum(d, -p)
        latday <- max(1, latday)
        now_latent[d] <- sum(latency[latday:d])

        infectious[d] <- rtransfer[d_1]
        infday <- sum(d, -i)
        infday <- max(1, infday)
        now_infectious[d] <- sum(infectious[infday:d])
      }

      if (wth_rhum[d] >= rhlim || wth_rain[d] >= rainlim) {
        RcW[d] <- 1
      }

      rc[d] <- RcOpt * Rc_age[d] * Rc_temp[d] * RcW[d]

      diseased[d] <- sum(sum(infectious), now_latent[d], removed[d])

      removed[d] <- sum(sum(infectious), -now_infectious[d])

      cofr[d] <- 1 - diseased[d] / sum(sites[d], diseased[d])

      if (d == onset) {
        # initialisation of the disease
        infection[d] <- I0
      } else if (d > onset) {
        infection[d] <- now_infectious[d] * rc[d] * (cofr[d] ^ a)
      } else {
        infection[d] <- 0
      }

      if (d >= p) {
        rtransfer[d] <- latency[latday]
      } else {
        rtransfer[d] <- 0
      }

      total_sites[d] <- sum(diseased[d], sites[d])

      rgrowth[d] <- RRG * sites[d] * sum(1, -(total_sites[d] / Sx))
      intensity[d] <- sum(diseased[d], -removed[d]) /
                          sum(total_sites[d], -removed[d])
    } # end loop

    audpc <- .calculate_audpc(intensity, simday)

    # Create output object ----
    out <-
      setDT(
        list(
          "simday" = 1:duration,
          "dates" = dates[1:d],
          "sites" = sites,
          "latent" = now_latent,
          "infectious" = now_infectious,
          "removed" = removed,
          "senesced" = senesced,
          "rateinf" = infection,
          "rtransfer" = rtransfer,
          "rgrowth" = rgrowth,
          "rsenesced" = rsenesced,
          "diseased" = diseased,
          "intensity" = intensity,
          "audpc" = audpc
        )
      )

    # Only add LAT and LON values if they exist in `wth`
    if (all(c("LAT", "LON") %in% names(wth)))
    {
      out[, lat := rep_len(wth[, LAT], .N)]
      out[, lon := rep_len(wth[, LON], .N)]
    }

    return(out[])
  }

#' Use approx() to return a modifier value from an RcA or RcT curve
#'
#' @param .Rc A matrix describing a growth curve for either temperature, `RcT`,
#'  or age, `RcA`.
#' @param .xout a value for `x`, either a temperature or age modifier value.
#'
#' @return A numeric value for modifying a growth curve in SEIR()
#'
#' @note This is a faster (and more simple) function that does what the original
#'  `afgen()` from \pkg{cropsim} does.
#'
#' @keywords internal
#'
#' @noRd

.fn_Rc <- function(.Rc, .xout)
  stats::approx(
    x = .Rc[, 1],
    y = .Rc[, 2],
    method = "linear",
    xout = .xout,
    yleft = 0,
    yright = 0
  )$y
