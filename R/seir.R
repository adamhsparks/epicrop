#' Susceptible-Exposed-Infectious-Removed (SEIR) Model Framework
#'
#' This function is originally used by specific disease models in
#'  \sQuote{EPIRICE} to model disease intensity of several rice diseases.  Given
#'  proper values it can be used with other pathosystems as well.
#'
#' @param wth a `data.frame` of class `epicrop. wth` from either [get_wth()] or
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
#'  _et al. _ 2012.
#' @param onset expected number of days until the onset of disease after
#'  emergence date (day, integer).  Described in Table 1 of Savary _et al._
#'  2012.
#' @param duration simulation duration *i. e.*, growing season length (day,
#'  integer).  Described in Table 1 of Savary _et al. _ 2012.
#' @param rhlim relative humidity value threshold to decide whether leaves are
#'  wet or not (numeric).  Described in Table 1 of Savary _et al._ 2012.  Savary
#'  _et al._ 2012 used `90`.
#' @param rainlim rainfall amount (mm) threshold to decide whether leaves are
#'  wet or not (numeric).  Described in Table 1 of Savary _et al._ 2012.  Savary
#'  _et al._ 2012 used `5`.
#' @param H0 initial number of plant's healthy sites (integer).  Described in
#'  Table 1 of Savary _et al._ 2012.
#' @param I0 initial number of infective sites (integer).  Described in Table 1
#'  of Savary _et al._ 2012. I0 is injected as new infections on the onset
#'  (become infectious after `p` days).
#' @param RcA modifier for _Rc_ (the basic infection rate corrected for
#'  removals) for crop age as a numeric two-column matrix with the first column
#'  being the crop age in days (1 to `duration`) and the second being the
#'  modifier (bounded by 0 and 1).  Described in Tables 1 and 2 of Savary
#'  _et al._ 2012.
#' @param RcT modifier for _Rc_ (the basic infection rate corrected for
#'  removals) for temperature as a numeric two-column matrix with the first
#'  column being temperature in °C and the second the modifier (bounded by 0 and
#'  1).  Described in Tables 1 and 2 of Savary _et al._ 2012.
#' @param RcOpt potential basic infection rate corrected for removals (numeric).
#'  Described in Table 1 of Savary _et al._ 2012.  This value can be modified
#'  to reflect crop cultivar resistance ratings as well, see Kim _et al. _ 2015
#'  for more details.
#' @param p duration of latent period (day, integer).  Described in Table 1 of
#'  Savary _et al._ 2012.
#' @param i duration of infectious period (day, integer).  Described in Table 1
#'  of Savary _et al._ 2012.
#' @param Sx maximum number of sites (integer).  Described in Table 1 of Savary
#'  _et al._ 2012.
#' @param a aggregation coefficient, values are from 1 to >1 (numeric).
#'  Described in Table 1 of Savary _et al._ 2012 and Table 1 of Savary _et al._
#'  2015.  See further details in **_a_ - Aggregation** section.
#' @param RRS relative rate of physiological senescence (numeric).  Described in
#'  Table 1 of Savary _et al. _ 2012.
#' @param RRG relative rate of growth (numeric).  Described in Table 1 of Savary
#'  _et al._ 2012.
#' @param RcW Optional modifier for _Rc_ to calculate leaf wetness effect
#'  from 1 if wet to 0 if dry.   This is a numeric two-column matrix with the
#'  first column being hours (1 to 24) and the second the modifier (bounded by 0
#'  and 1).  Used when `simple_wetness = FALSE` (hourly/derived wetness). If
#'  `NULL` (default), a linear relationship is used where 0 hours wet = 0 and
#'  24 hours wet = 1. Ignored if `simple_wetness = TRUE`.
#' @param simple_wetness Logical. If `TRUE` (default), the function uses a
#'  simple calculation of leaf wetness based on daily `rhlim` and `rainlim`
#'  values (wetness is either 0 or 24 hours). If `FALSE`, the function uses a
#'  more complex calculation of leaf wetness based on hourly temperature and
#'  humidity patterns derived from daily TMIN/TMAX values, requiring additional
#'  weather data columns (TMIN, TMAX, LAT, DOY), and applies `RcW` to determine
#'  the effect of leaf wetness duration on the infection rate.
#'
#' @references
#' Sparks, A. H., P.D. Esker, M.  Bates, W.  Dall' Acqua, Z. Guo, V. Segovia, S.D.
#' Silwal, S. Tolos, and K. A. Garrett, 2008. Ecology and Epidemiology in R:
#' Disease Progress over Time.  *The Plant Health Instructor*.
#' \doi{10.1094/PHI-A-2008-0129-02}.
#'
#' Madden, L.  V., G. Hughes, and F. van den Bosch.  2007. The Study of Plant
#' Disease Epidemics.  American Phytopathological Society, St. Paul, MN.
#' \doi{10.1094/9780890545058}.
#'
#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario,
#' J.  Modeling and mapping potential epidemics of rice diseases globally.  _Crop
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
#' seir(
#'   wth = wth,
#'   emergence = emergence,
#'   onset = 20,
#'   duration = 120,
#'   rhlim = 90,
#'   rainlim = 5,
#'   H0 = 600,
#'   I0 = 1,
#'   RcA = RcA,
#'   RcT = RcT,
#'   RcOpt = 0.61,
#'   p = 6,
#'   i = 19,
#'   Sx = 100000,
#'   a = 1,
#'   RRS = 0.01,
#'   RRG = 0.1
#' )
#'
#' @section _a_ -- Aggregation Details:
#'
#' When _a_ is set to `1` the assumption is that that there is no disease
#' aggregation with new infections occurring at random among the healthy sites.
#' When _a_ is greater than `1` there is aggregation in the disease occurrence,
#' the pathogen is unable to access the entire population of healthy sites,
#' which results in disease aggregation.  Refer to Savary _et al. _ (2012) for
#' greater detail.
#'
#' @section TMIN/TMAX Details:
#'
#' If `simple_wetness` is set to `FALSE`, the function will use the `TMIN` and
#'  `TMAX` columns in the `wth` object to calculate the leaf wetness value for
#'  24 hours of the day using the `rhlim` and `rainlim` values and then uses
#'  `RcW` to calculate a value between 0 and 1 for the whole day.   When
#'  `simple_wetness` is set to `TRUE`, the function only sets the leaf wetness
#'  to 0 or 1 for the day based on the `rhlim` and `rainlim` values.
#'
#' @section LAT/LON Details:
#'
#' If the `wth` object provides `LAT` and `LON` columns, these will be included
#' in the output for mapping purposes.  Both values must be present.  These
#' columns are provided by default when using [get_wth()].
#'
#' @seealso
#' `seir()` is called by the following specific disease modelling functions:
#' * [bacterial_blight()],
#' * [brown_spot()],
#' * [leaf_blast()],
#' * [sheath_blight()],
#' * [tungro()],
#' * [modified_kim_leaf_blast()], and,
#' * [modified_kim_sheath_blight()].
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com} from original by
#'   Robert J. Hijmans, Rene Pangga and Jorrel Aunario.
#'
#' @returns A [data.table::data.table()] object containing the following
#' columns:
#'
#' \describe{
#'   \item{simday}{Day of simulation (1-indexed, where 1 is the emergence date)}
#'   \item{dates}{Date of simulation}
#'   \item{sites}{Total number of healthy sites present on day "x"}
#'   \item{latent}{Number of latent sites present on day "x"}
#'   \item{infectious}{Number of infectious sites present on day "x"}
#'   \item{removed}{Number of removed sites present on day "x"}
#'   \item{senesced}{Number of senesced sites present on day "x"}
#'   \item{rateinf}{Rate of infection (new infections on day "x")}
#'   \item{rtransfer}{Rate of transfer from latent to infectious sites}
#'   \item{rgrowth}{Rate of growth of healthy sites}
#'   \item{rsenesced}{Rate of senescence of healthy sites}
#'   \item{diseased}{Number of diseased (latent + infectious + removed) sites on day "x"}
#'   \item{intensity}{Proportion of diseased sites (latent + infectious) per total living sites (excludes removed and senesced) on day "x"}
#'   \item{AUDPC}{Area under the disease progress curve (AUDPC) for the simulation}
#'   \item{lat}{Latitude value if provided by the `wth` object}
#'   \item{lon}{Longitude value if provided by the `wth` object}
#' }
#'
#' @autoglobal
#' @export

seir <- function(
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
  RcW = NULL,
  simple_wetness = TRUE
) {
  # Check required arguments
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

  # Validate inputs
  called <- sys.call()
  called[[1L]] <- as.name("list")
  passed_args <- eval.parent(called)
  .check_inputs(seir_passed_args = passed_args)

  # Set and check dates
  emergence <- as.Date(emergence)
  harvest <- emergence + (duration - 1)
  dates <- seq(from = emergence, to = harvest, by = "day")

  # Validate date alignment
  if (emergence < wth[1L, YYYYMMDD] || max(dates) > max(wth[, YYYYMMDD])) {
    cli::cli_abort("Incomplete weather data or dates do not align.")
  }

  # Subset weather data to simulation period
  if (nrow(wth) > duration) {
    wth <- wth[YYYYMMDD %between% c(emergence, harvest)]
  }

  # Extract weather vectors
  wth_rhum <- wth$RHUM
  wth_rain <- wth$RAIN %||% rep(0, nrow(wth))

  # Calculate leaf wetness
  if (
    !simple_wetness && !all(c("TMIN", "TMAX", "LAT", "DOY") %in% names(wth))
  ) {
    cli::cli_warn(
      "Complex wetness requires TMIN, TMAX, LAT, DOY.  Using simple wetness."
    )
    simple_wetness <- TRUE
  }

  if (simple_wetness) {
    wet_hours <- data.table::fifelse(
      wth_rhum >= rhlim | wth_rain >= rainlim,
      24,
      0
    )
  } else {
    wet_hours <- .calculate_leaf_wetness(
      wth = wth,
      rhlim = rhlim,
      rainlim = rainlim,
      weighted_wetness = FALSE
    )
    wet_hours <- pmin(24, pmax(0, wet_hours))
  }

  # Map wet hours to RcW multiplier
  if (is.null(RcW)) {
    Rc_wet <- wet_hours / 24 # Linear 0-24 -> 0-1
  } else {
    Rc_wet <- .fn_Rc(.Rc = RcW, .xout = wet_hours)
  }

  # Prepare modifier vectors (all must be length = duration)
  Rc_temp <- .fn_Rc(.Rc = RcT, .xout = wth$TEMP)
  Rc_age <- .fn_Rc(.Rc = RcA, .xout = seq.int(0, duration - 1))

  # Handle RRG (can be scalar or matrix)
  if (length(RRG) == 1L) {
    RRG <- rep(RRG, duration)
  } else {
    RRG <- .fn_Rc(.Rc = RRG, .xout = seq_len(duration))
  }

  # Validate all modifier vectors have correct length
  stopifnot(
    "Modifier vectors must match duration" = length(Rc_temp) == duration &&
      length(Rc_age) == duration &&
      length(Rc_wet) == duration &&
      length(RRG) == duration
  )

  # Convert onset from 1-indexed (R) to 0-indexed (C++)
  # onset = 1 means disease starts on day 1 (emergence day)
  # In C++ this is day 0
  onset_cpp <- onset - 1L

  # Call C++ core function
  core <- seir_core_cpp(
    Rc_age = Rc_age,
    Rc_temp = Rc_temp,
    Rc_wet = Rc_wet,
    RRG = RRG,
    RcOpt = RcOpt,
    onset = onset_cpp,
    duration = duration,
    p = p,
    i = i,
    H0 = H0,
    I0 = I0,
    Sx = Sx,
    a = a,
    RRS = RRS
  )

  # Build output data.table
  out <- data.table::data.table(
    simday = seq_len(duration), # 1-indexed for R users
    dates = dates,
    sites = core$sites,
    latent = core$latency,
    infectious = core$infectious,
    removed = core$removed,
    senesced = core$senesced,
    rateinf = core$infection,
    rtransfer = core$rtransfer,
    rgrowth = core$rgrowth,
    rsenesced = core$rsenesced,
    diseased = core$diseased,
    intensity = core$intensity,
    AUDPC = .calculate_audpc(core$intensity)
  )

  # Add location if available
  if (all(c("LAT", "LON") %in% names(wth))) {
    out[, `:=`(
      lat = wth[["LAT"]][1],
      lon = wth[["LON"]][1]
    )]
  }

  return(out[])
}
#' Use approx() to Return a Modifier Value From an RcA or RcT Curve
#'
#' @param .Rc A matrix describing a growth curve for either temperature, `RcT`,
#'  age, `RcA`, or relative humidity, `RcRH`.
#' @param .xout a value for `x`, either a temperature, age or relative
#'  humidity modifier value.
#'
#' @returns A numeric value for modifying a growth curve in [seir].
#'
#' @note This is a faster (and more simple) function that does what the original
#'  \pkg{cropsim} `afgen()` does.
#'
#' @note
#' The original implementation of this function comes from \CRANpkg{ZeBook} and
#'  was adapted for use in \pkg{epicrop} by Adam H. Sparks.
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#'
#' @dev

.fn_Rc <- function(.Rc, .xout) {
  stats::approx(
    x = .Rc[, 1L],
    y = .Rc[, 2L],
    method = "linear",
    xout = .xout,
    yleft = .Rc[1L, 2L],
    yright = .Rc[nrow(.Rc), 2L]
  )$y
}

seir_cpp_core <- function(
  Rc_age,
  Rc_temp,
  Rc_wet,
  RRG,
  RcOpt,
  onset,
  duration,
  p,
  i,
  H0,
  I0,
  Sx,
  a,
  RRS
) {
  seir_core_cpp(
    Rc_age = Rc_age,
    Rc_temp = Rc_temp,
    Rc_wet = Rc_wet,
    RRG = RRG,
    RcOpt = RcOpt,
    onset = onset,
    duration = duration,
    p = p,
    i = i,
    H0 = H0,
    I0 = I0,
    Sx = Sx,
    a = a,
    RRS = RRS
  )
}
