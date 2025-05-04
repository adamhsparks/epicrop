#' Susceptible-Exposed-Infectious-Removed (SEIR) Model Framework
#'
#' This function is originally used by specific disease models in
#'  \sQuote{EPIRICE} to model disease intensity of several rice diseases.  Given
#'  proper values it can be used with other pathosystems as well.
#'
#' @param wth a `data.frame` of class `epicrop.wth` from either [get_wth()] or
#'  [format_wth()] containing weather on a daily time-step with the following
#'  field names:
#'
#'   **Field Name** | **Value**
#'   --------------:|:----------
#'   _YYYYMMDD_ | Date as Year Month Day (ISO8601)
#'   _DOY_ | Consecutive day of year, commonly called "Julian date"
#'   _TEMP_ | Mean daily temperature (°C)
#'   _RHUM_ | Mean daily relative humidity (%)
#'   _RAIN_ | Mean daily rainfall (mm)
#'   _TMIN_ | **Optional** Minimum daily temperature (°C), see TMIN/TMAX Details
#'   _TMAX_ | **Optional** Maximum daily temperature (°C), see TMIN/TMAX Details
#'   _LAT_ | **Optional** latitude of weather observation, see LAT/LON Details
#'   _LON_ | **Optional** longitude of weather observation, see LAT/LON Details
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
#'  wet or not (numeric).  Described in Table 1 of Savary _et al._ 2012.  Savary
#'  _et al._ 2012 used `90`.
#' @param rainlim rainfall amount (mm) threshold to decide whether leaves are
#'  wet or not (numeric).  Described in Table 1 of Savary _et al._ 2012.  Savary
#'  _et al._ 2012 used `5`.
#' @param H0 initial number of plant's healthy sites (integer).  Described in
#'  Table 1 of Savary _et al._ 2012.
#' @param I0 initial number of infective sites (integer).  Described in Table 1
#'  of Savary _et al._ 2012.
#' @param RcA modifier for _Rc_ (the basic infection rate corrected for
#'  removals) for crop age as a numeric two-column matrix with the first column
#'  being the crop age in days (1 to `duration`) and the second being the
#'  modifier (bounded by 0 and 1).  Described in Tables 1 and 2 of Savary
#'  _et al._ 2012.
#' @param RcT modifier for _Rc_ (the basic infection rate corrected for
#'  removals) for temperature as a numeric two-column matrix with the first
#'  column being temperature in ˚C and the second the modifier (bounded by 0 and
#'  1).  Described in Tables 1 and 2 of Savary _et al._ 2012.
#' @param RcOpt potential basic infection rate corrected for removals (numeric).
#'  Described in Table 1 of Savary _et al._ 2012.  This value can be modified
#'  to reflect crop cultivar resistance ratings as well, see Kim _et al._ 2015
#'  for more details.
#' @param i duration of infectious period (day, integer).  Described in Table 1
#'  of Savary _et al._ 2012.
#' @param p duration of latent period (day, integer).  Described in Table 1 of
#'  Savary _et al._ 2012.
#' @param Sx maximum number of sites (integer).  Described in Table 1 of Savary
#'  _et al._ 2012.
#' @param a aggregation coefficient, values are from 1 to >1 (numeric).
#'  Described in Table 1 of Savary _et al._ 2012 and Table 1 of Savary _et al._
#'  2015.  See further details in **_a_ - Aggregation** section.
#' @param RRS relative rate of physiological senescence (numeric).  Described in
#'  Table 1 of Savary _et al._ 2012.
#' @param RRG relative rate of growth (numeric).  Described in Table 1 of Savary
#'  _et al._ 2012.
#' @param simple_wetness `Boolean` value, if `TRUE` the function will use a
#'  simple calculation of leaf wetness based on the daily `rhlim` and `rainlim`
#'  values. If `FALSE` the function will use a more complex calculation of leaf
#'  wetness based on the `wth` data and will calculate the leaf wetness value
#'  for each hour of the day and use `RcW` to determine the effect of leaf
#'  wetness on the infection rate.
#' @param RcW an optional modifier for _Rc_ to calculate leaf wetness effect
#'  from 1 if wet to 0 if dry.  This is a numeric two-column matrix with the
#'  first column being hours (1 to 24) and the second the modifier (bounded by 0
#'  and 1).  Only used when `simple_wetness == TRUE`, if it is provided and
#'  `simple_wetness == FALSE`, these values will be ignored.
#'
#' @references
#' Sparks, A.H., P.D. Esker, M. Bates, W. Dall' Acqua, Z. Guo, V. Segovia, S.D.
#' Silwal, S. Tolos, and K.A. Garrett, 2008. Ecology and Epidemiology in R:
#' Disease Progress over Time. *The Plant Health Instructor*.
#' \doi{10.1094/PHI-A-2008-0129-02}.
#'
#' Madden, L. V., G. Hughes, and F. van den Bosch. 2007. The Study of Plant
#' Disease Epidemics. American Phytopathological Society, St. Paul, MN.
#' \doi{10.1094/9780890545058}.
#'
#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario,
#' J. Modeling and mapping potential epidemics of rice diseases globally. _Crop
#' Protection_, Volume 34, 2012, Pages 6-17, ISSN 0261-2194
#' \doi{10.1016/j.cropro.2011.11.009}.
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
#'   cbind(
#'     c(0L, 20L, 40L, 60L, 80L, 100L, 120L),
#'     c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0)
#'   )
#' RcT <-
#'   cbind(
#'     c(15L, 20L, 25L, 30L, 35L, 40L),
#'     c(0, 0.06, 1.0, 0.85, 0.16, 0)
#'   )
#' emergence <- "2000-07-15"
#'
#' (seir(
#'   wth = wth,
#'   emergence = emergence,
#'   onset = 20,
#'   duration = 120,
#'   rhlim = 90,
#'   rainlim = 5,
#'   RcA = RcA,
#'   RcT = RcT,
#'   RcOpt = 0.61,
#'   p = 6,
#'   i = 19,
#'   H0 = 600,
#'   I0 = 1,
#'   a = 1,
#'   Sx = 100000,
#'   RRS = 0.01,
#'   RRG = 0.1
#'   simple_wetness = TRUE,
#'   RcW = NULL
#' ))
#'
#' @section _a_ -- Aggregation Details:
#'
#' When _a_ is set to `1` the assumption is that that there is no disease
#' aggregation with new infections occurring at random among the healthy sites.
#' When _a_ is greater than `1` there is aggregation in the disease occurrence,
#' the pathogen is unable to access the entire population of healthy sites,
#' which results in disease aggregation. Refer to Savary _et al._ (2012) for
#' greater detail.
#'
#' @section TMIN/TMAX Details:
#'
#' If `simple_wetness` is set to `FALSE`, the function will use the `TMIN` and
#'  TMAX` columns in the `wth` object to calculate the leaf wetness value for
#'  24 hours of the day using the `rhlim` and `rainlim` values and then uses
#'  `RcW` to calculate a value between 0 and 1 for the whole day.  When
#'  `simple_wetness` is set to `TRUE`, the function only sets the leaf wetness
#'  to 0 or 1 for the day based on the `rhlim` and `rainlim` values.
#'
#' @section LAT/LON Details:
#'
#' If the `wth` object provides `LAT` and `LON` columns, these will be included
#' in the output for mapping purposes. Both values must be present. These
#' columns are provided by default when using [get_wth()].
#'
#' @seealso
#' `seir()` is called by the following specific disease modelling functions:
#' * [bacterial_blight],
#' * [brown_spot],
#' * [leaf_blast],
#' * [sheath_blight],
#' * [tungro],
#' * [modified_kim_leaf_blast], and,
#' * [modified_kim_sheath_blight].
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com} from original by
#'   Robert J. Hijmans, Rene Pangga and Jorrel Aunario.
#'
#' @returns A [data.table::data.table] object containing the following columns:
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
#'   \item{diseased}{Number of diseased (latent + infectious + removed) sites on day "x"}
#'   \item{intensity}{Proportion of diseased (latent + infectious + removed) sites per total sites not including removed sites on day "x"}
#'   \item{AUDPC}{Area under the disease progress curve \acronym{AUDPC} for the simulation}
#'   \item{lat}{Latitude value if provided by the `wth` object}
#'   \item{lon}{Longitude value if provided by the `wth` object}
#' }
#'
#' @autoglobal
#' @export

seir <-
  function(
    wth,
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
    RRG,
    RcW,
    simple_wetness
  ) {
    rlang::check_required(c(
      wth,
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
      RRG
    ))

    passed_args <- as.list(match.call())[-1L]
    .check_inputs(passed_args)

    # Set and check dates ----
    emergence <- as.Date(emergence)
    harvest <- emergence + sum(duration, -1L)
    dates <- seq(from = emergence, to = harvest, by = "day")

    # check that the dates align
    if (
      (emergence < wth[1L, YYYYMMDD]) ||
        (max(dates) > max(wth[, YYYYMMDD]))
    ) {
      cli::cli_abort(
        "Incomplete weather data or dates do not align."
      )
    }

    # subset weather data where date is greater than emergence minus one and
    # less than duration
    if (dim(wth)[1L] > duration) {
      wth <-
        wth[YYYYMMDD %between% c(emergence, emergence + sum(duration, -1L))]
    }

    # Create vectors for referencing ----
    wth_rain <- wth$RAIN
    wth_rhum <- wth$RHUM

    # Create output vars
    cofr <-
      rc <-
        latency <- infectious <- intensity <- rsenesced <-
          rgrowth <-
            rtransfer <- infection <- diseased <- senesced <- removed <-
              now_infectious <- now_latent <- sites <- total_sites <-
                vector(mode = "double", length = duration)

    if (simple_wetness) {
      Rc_wet <- vector(mode = "double", length = length(wth_rhum))
      Rc_wet[wth_rhum >= rhlim | wth_rain >= rainlim] <- 1L
    } else {
      w <- .calculate_leaf_wetness(
        wth,
        rhlim = rhlim,
        rainlim = rainlim,
        weighted_wetness = FALSE
      )
      Rc_wet <- .fn_Rc(.Rc = RcW, .xout = w)
    }

    Rc_temp <- .fn_Rc(.Rc = RcT, .xout = wth$TEMP)
    Rc_age <- .fn_Rc(.Rc = RcA, .xout = 0L:duration)

    # Calculate state values -----
    for (d in 1L:duration) {
      d_1 <- sum(d, -1L)

      if (d == 1L) {
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
          removed_today <- infectious[infday + 1L]
        } else {
          removed_today <- 0L
        }

        sites[[d]] <-
          sum(sites[d_1], rgrowth[d_1], -infection[d_1], -rsenesced[d_1])
        rsenesced[[d]] <- sum(removed_today, RRS * sites[d])
        senesced[[d]] <- sum(senesced[d_1], rsenesced[d_1])

        latency[[d]] <- infection[d_1]
        latday <- sum(d, -p)
        latday <- max(1L, latday)
        now_latent[[d]] <- sum(latency[latday:d])

        infectious[[d]] <- rtransfer[d_1]
        infday <- sum(d, -i)
        infday <- max(1L, infday)
        now_infectious[[d]] <- sum(infectious[infday:d])
      }

      rc[[d]] <- RcOpt * Rc_age[d] * Rc_temp[d] * Rc_wet[d]

      diseased[[d]] <- sum(sum(infectious), now_latent[d], removed[d])

      removed[[d]] <- sum(sum(infectious), -now_infectious[d])

      cofr[[d]] <- 1L - diseased[d] / sum(sites[d], diseased[d])

      if (d == onset) {
        # initialisation of the disease
        infection[[d]] <- I0
      } else if (d > onset) {
        infection[[d]] <- now_infectious[d] * rc[d] * (cofr[d]^a)
      } else {
        infection[[d]] <- 0L
      }

      if (d >= p) {
        rtransfer[[d]] <- latency[latday]
      } else {
        rtransfer[[d]] <- 0L
      }

      total_sites[[d]] <- sum(diseased[d], sites[d])

      rgrowth[[d]] <- RRG * sites[d] * sum(1L, -(total_sites[d] / Sx))
      intensity[[d]] <- sum(diseased[d], -removed[d]) /
        sum(total_sites[d], -removed[d])
    } # end loop

    simday <- seq_len(duration)
    audpc <- .calculate_audpc(intensity)

    # Create output object ----
    out <-
      setDT(
        list(
          "simday" = simday,
          "dates" = dates[1L:d],
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
          "AUDPC" = rep_len(audpc, duration)
        )
      )

    # Only add LAT and LON values if they exist in `wth`
    if (all(c("LAT", "LON") %in% names(wth))) {
      out[, lat := rep_len(wth[, LAT], .N)]
      out[, lon := rep_len(wth[, LON], .N)]
    }

    out[]
  }

#' Use approx() to Return a Modifier Value From an RcA or RcT Curve
#'
#' @param .Rc A matrix describing a growth curve for either temperature, `RcT`,
#'  age, `RcA`, or RHUM, `RcRH`.
#' @param .xout a value for `x`, either a temperature, age or RHUM modifier
#'  value.
#'
#' @returns A numeric value for modifying a growth curve in [seir].
#'
#' @note This is a faster (and more simple) function that does what the original
#'  \pkg{cropsim} `afgen()` does.
#'
#' @dev

.fn_Rc <- function(.Rc, .xout) {
  stats::approx(
    x = .Rc[, 1L],
    y = .Rc[, 2L],
    method = "linear",
    xout = .xout,
    yleft = 0L,
    yright = 0L
  )$y
}
