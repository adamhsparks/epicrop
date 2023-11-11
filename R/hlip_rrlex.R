
#' Healthy Latent Infectious Post-Infectious (HLIP) Model Framework With Lesion Expansion
#'
#' A model framework based on the medical idea of a Susceptible-Exposed-
#' Infectious-Removed (\acronym{SEIR}) model.  This framework is designed for
#' plant diseases. This function is originally used by specific disease models
#' in \sQuote{EPIWHEAT} to model disease intensity of two wheat diseases.  Given
#' proper values it can be used with other pathosystems as well.
#'
#' @section Differences between `hlip()` and `hlip_rrlex()`:
#' \describe{
#'   \item{`RRLEX`}{`hlip()`, does not include a parameter for the relative
#'   rate of lesion expansion whereas `hlip_rrlex()` does.}
#'   \item{`RcW`}{`hlip()` uses both relative humidity (RH) and rainfall to
#'   predict leaf wetness.  This differs from `hlip_rrlex()`, which uses only
#'   rainfall to predict leaf wetness.}
#' }
#'
#' @param wth a `data.frame` of weather on a daily time-step containing data
#' with the following field names.
#'
#'   **Field Name** | **Value**
#'   --------------:|:----------
#'   _YYYYMMDD_ | Date as Year Month Day (ISO8601).
#'   _DOY_ | Consecutive day of year, commonly called "Julian date".
#'   _TEMP_ | Mean daily temperature (°C).
#'   _RAIN_ | Mean daily rainfall (mm).
#'   _LAT_ | **Optional** latitude of weather observation. See LAT/LON Note.
#'   _LON_ | **Optional** longitude of weather observation. See LAT/LON Note.
#'
#' @param emergence expected date of plant emergence (or transplanting for rice)
#'  entered in `YYYY-MM-DD` format (character).  Described in Table 1 of Savary
#'  _et al._ 2012 and Table 1 of Savary _et al._ 2015.
#' @param onset expected number of days until the onset of disease after
#'  emergence date (day, integer).  Described in Table 1 of Savary _et al._
#'  2012 and Table 1 of Savary _et al._ 2015.
#' @param duration simulation duration _i.e._, growing season length (day,
#'  integer).  Described in Table 1 of Savary _et al._ 2012 and Table 1 of
#'  Savary _et al._ 2015.
#' @param rhlim relative humidity value threshold to decide whether leaves are
#'  wet or not (numeric).  Described in Table 1 of Savary _et al._ 2012. Savary
#'  _et al._ 2012 used 90%.
#' @param rainlim rainfall amount (mm) threshold to decide whether leaves are
#'  wet or not (numeric).  Described in Table 1 of Savary _et al._ 2012. Savary
#'  _et al._ 2012 used 5mm.
#' @param H0 initial number of plant's healthy sites (integer).  Described in
#'  Table 1 of Savary _et al._ 2012 and Table 1 of Savary _et al._ 2015.
#' @param I0 initial number of infective sites (integer).  Described in Table 1
#'  of Savary _et al._ 2012 and Table 1 of Savary _et al._ 2015.
#' @param RcA modifier for _Rc_ (the basic infection rate corrected for
#'  removals) for crop age (numeric vector).  Described in Table 1 of Savary
#'  _et al._ 2012 Table 1 of Savary _et al._ 2015.
#' @param RcT modifier for _Rc_ (the basic infection rate corrected for
#'  removals) for temperature (numeric vector).  Described in Table 1 of Savary
#'  _et al._ 2012 and Table 1 of Savary _et al._ 2015.
#' @param RcOpt potential basic infection rate corrected for removals (numeric).
#'  Derived from Table 1 of Savary _et al._ 2012 and Table 1 of Savary _et al._
#'  2015.
#' @param RRLEX relative rate of lesion expansion (numeric).  Described in Table
#'  1 of Savary _et al._ 2015.
#' @param i duration of infectious period (day, integer).  Described in Table 1
#'  of Savary _et al._ 2012 and Table 1 of Savary _et al._ 2015.
#' @param p duration of latent period (day, integer).  Described in  Table 1 of
#'  Savary _et al._ 2012 and Table 1 of Savary _et al._ 2015.
#' @param Sx maximum number of sites (integer).  Described in Table 1 of Savary
#'  _et al._ 2012 and Table 1 of Savary _et al._ 2015.
#' @param a aggregation coefficient, values are from 1 to >1 (numeric).
#'  Described in Table 1 of Savary _et al._ 2012 and Table 1 of Savary _et al._
#'  2015.  See further details in **_a_ - Aggregation** section.
#' @param RRS relative rate of physiological senescence (numeric).  Described in
#'  Table 1 of Savary _et al._ 2012 and Table 1 of Savary _et al._ 2015.
#' @param RRG relative rate of growth (numeric).  Described in Table 1 of Savary
#'  _et al._ 2012 and Table 1 of Savary _et al._ 2015.
#'
#' @references Savary, S., Stetkiewicz, S., Brun, R., and Willocquet, L.
#' Modelling and Mapping Potential Epidemics of Wheat Diseases—Examples on Leaf
#' Rust and Septoria Tritici Blotch Using EPIWHEAT. _European Journal of Plant
#' Pathology_ Volume 142, No. 4, 2015, Pages 771–90, DOI:
#' <https://doi.org/10.1007/s10658-015-0650-7>.
#'
#' @examplesIf interactive()
#' # get weather for Esperance, WA, AU for 2021 growing season
#' wth <- get_wth(
#'   lonlat = c(121.891945, -33.861111),
#'   dates = c("2021-01-01", "2021-12-31")
#' )
#'
#' # provide suitable values for leaf rust intensity
#' RcA <-
#'   cbind(c(0L, 20L, 40L, 60L, 80L, 100L, 120L),
#'         c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0))
#' RcT <-
#'   cbind(c(15L, 20L, 25L, 30L, 35L, 40L),
#'         c(0, 0.06, 1.0, 0.85, 0.16, 0))
#' emergence <- "2000-07-15"
#'
#' (hlip_rrlex(
#'   wth = wth,
#'   emergence = emergence,
#'   onset = 20,
#'   duration = 120,
#'   rhlim = 90,
#'   rainlim = 5,
#'   RcA = RcA,
#'   RcT = RcT,
#'   RcOpt = 1.17,
#'   RRLEX = 0.09,
#'   p =  11,
#'   i = 18,
#'   H0 = 250,
#'   I0 = 15,
#'   a = 1,
#'   Sx = 174000,
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
#' @note
#' If the `wth` object provides _LAT_ and _LON_ columns, these will be included
#' in the output for mapping purposes. Both values must be present. These
#' columns are provided by default when using [get_wth()].
#'
#' @seealso
#' `hlip_rrlex()` is called by the following specific disease modelling functions:
#' * [predict_leaf_rust()],
#' * [predict_septoria_tritici_blotch()]
#'
#' @author Adam H. Sparks
#'
#' @return A [data.table::data.table()] containing the following columns:
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
#'   \item{lat}{Latitude value if provided by `wth` object}
#'   \item{lon}{Longitude value if provided by `wth` object}
#' }
#'
#' @export

hlip_rrlex <-
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
           RRLEX,
           p,
           i,
           Sx,
           a,
           RRS,
           RRG) {
    # CRAN NOTE avoidance
    infday <- YYYYMMDD <- lat <- lon <- LAT <- LON <- NULL #nocov

    # set `wth` input as a data.table object if it's not already one, else this
    # function will fail on line 182
    if (!inherits(wth, "data.table")) {
      wth <- data.table::as.data.table(wth)
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

    # set date formats
    emergence <- as.Date(emergence)

    # create vector of dates
    dates <- seq(emergence, emergence + sum(duration, -1), 1)

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

    # create vectors for referencing
    wth_rain <- wth$RAIN
    wth_rhum <- wth$RHUM
    Rc_temp <- .fn_Rc(.Rc = RcT, .xout = wth$TEMP)
    Rc_age <- .fn_Rc(.Rc = RcA, .xout = 0:duration)

    # outputvars
    cofr <-
      rc <-
      RcW <- latency <- infectious <- intensity <- rsenesced <-
      rgrowth <-
      rtransfer <- infection <- diseased <- senesced <- removed <-
      now_infectious <- now_latent <- sites <- total_sites <-
      vector(mode = "double", length = duration)

    for (d in 1:duration) {
      # State calculations  -----
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
          "intensity" = intensity
        )
      )

    # Only add Lat and Lon values if they exist in WTH
    if (all(c("LAT", "LON") %in% names(wth))) {
      out[, lat := rep_len(wth[, LAT], .N)]
      out[, lon := rep_len(wth[, LON], .N)]
    }

    return(out[])
  }

#' Use approx() to Return a Modifier Value From an RcA or RcT Curve
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
#' @noRd

.fn_Rc <- function(.Rc, .xout) {
  stats::approx(
    return(
      x = .Rc[, 1],
      y = .Rc[, 2],
      method = "linear",
      xout = .xout,
      yleft = 0,
      yright = 0
    )$y
  )
}
