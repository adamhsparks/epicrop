
#' Susceptible-Exposed-Infectious-Removed (SEIR) model framework
#'
#' This function is originally used by specific disease models in
#'  \sQuote{EPIRICE} to model disease intensity of several rice diseases.  Given
#'  proper values it can be used with other pathosystems as well.
#'
#' @param wth a `data.frame` of weather on a daily time-step containing data
#' with the following field names.
#'
#'   **Field Name** | **Value**
#'   --------------:|:----------
#'   _YYYYMMDD_ | Date as Year Month Day (ISO8601)
#'   _DOY_ | Consecutive day of year, commonly called "Julian date"
#'   _TEMP_ | Mean daily temperature (°C)
#'   _RHUM_ | Mean daily temperature (°C)
#'   _RAIN_ | Mean daily rainfall (mm)
#'   _LAT_ | **Optional** latitude of weather observation. See LAT/LON Note.
#'   _LON_ | **Optional** longitude of weather observation. See LAT/LON Note.
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
#' @references Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario,
#' J. Modeling and mapping potential epidemics of rice diseases globally. _Crop
#' Protection_, Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
#' <http://dx.doi.org/10.1016/j.cropro.2011.11.009>.
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
#' @note
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
#' @importFrom data.table setnames
#' @importFrom data.table setcolorder
#' @importFrom data.table `:=`
#' @importFrom data.table data.table
#' @importFrom data.table `.N`
#' @importFrom data.table `%between%`
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

    # set date formats
    emergence <- as.Date(emergence)

    # create vector of dates
    dates <- seq(emergence, emergence + duration, 1)

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
        wth[YYYYMMDD %between% c(emergence, emergence + duration)]
    }

    # create vectors for referencing
    wth_rain <- wth$RAIN
    wth_rhum <- wth$RHUM
    wth_temp <- wth$TEMP

    # outputvars
    cofr <-
      rc <-
      RcW <- latency <- infectious <- intensity <- rsenesced <-
      rgrowth <-
      rtransfer <- infection <- diseased <- senesced <- removed <-
      now_infectious <- now_latent <- sites <- total_sites <-
      rep(0, times = duration + 1)

    for (d in 0:duration) {
      # State calculations for() loop -----
      d1 <- sum(d, 1)

      if (d == 0) {
        # start crop growth
        sites[d1] <- H0
        rsenesced[d1] <- RRS * sites[d1]
      } else {
        if (d > i) {
          # as this never happens when `d > inftrans` so `infday` is assigned
          # a non-NULL value, when the loop comes back around it's then present,
          # so until `d > i`, `removed_today` will always be equal to "0" since
          # the disease has not had time to completely transit the infectious
          # period as specified by parameter `i` - AHS
          removed_today <- infectious[infday + 2]
        } else {
          removed_today <- 0
        }

        sites[d1] <-
          sum(sites[d], rgrowth[d], -infection[d], -rsenesced[d])
        rsenesced[d1] <- sum(removed_today, RRS * sites[d1])
        senesced[d1] <- sum(senesced[d], rsenesced[d])

        latency[d1] <- infection[d]
        latday <- sum(d, -p, 1)
        latday <- max(0, latday)
        now_latent[d1] <- sum(latency[latday:d + 1])

        infectious[d1] <- rtransfer[d]
        infday <- sum(d, -i, 1)
        infday <- max(0, infday)
        now_infectious[d1] <- sum(infectious[infday:d + 1]) # why is this different than using `d1` here? h
      }

      if (sites[d1] < 0) {
        sites[d1] <- 0
        break
      }

      if (wth_rhum[d1] >= rhlim || wth_rain[d1] >= rainlim) {
        RcW[d1] <- 1
      }

      rc[d1] <- RcOpt * select_mod_val(xy = RcA, x = d) *
        select_mod_val(xy = RcT, x = wth_temp[d1]) * RcW[d1]

      diseased[d1] <- sum(sum(infectious), now_latent[d1], removed[d1])

      removed[d1] <- sum(sum(infectious), -now_infectious[d1])

      cofr[d1] <- 1 - diseased[d1] / sum(sites[d1], diseased[d1])

      if (d == onset) {
        # initialisation of the disease
        infection[d1] <- I0
      } else if (d > onset) {
        infection[d1] <- now_infectious[d1] * rc[d1] * (cofr[d1] ^ a)
      } else {
        infection[d1] <- 0
      }

      if (d >= p) {
        rtransfer[d1] <- latency[latday + 1]
      } else {
        rtransfer[d1] <- 0
      }

      total_sites[d1] <- sum(diseased[d1], sites[d1])

      rgrowth[d1] <- RRG * sites[d1] * sum(1, -(total_sites[d1] / Sx))
      intensity[d1] <- sum(diseased[d1], -removed[d1]) /
                          sum(total_sites[d1], -removed[d1])
    } # end loop

    out <-
      setDT(
        list(
          0:duration,
          dates[1:d1],
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
          intensity
        )
      )

    setnames(
      out,
      c(
        "simday",
        "dates",
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
        "intensity"
      )
    )

    # Only add Lat and Lon values if they exist in WTH
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
#' @param .out a value for `x`, either a temperature or age modifier value.
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
  approx(
    x = .Rc[, 1],
    y = .Rc[, 2],
    method = "linear",
    xout = .xout,
    yleft = 0,
    yright = 0
  )$y
