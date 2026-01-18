#' Susceptible-Exposed-Infectious-Removed (SEIR) Model Framework with Lesion Expansion
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
#' @param RRLEX Rate of lesion expansion (scalar or vector).
#' @param age_driver Use "day" or "dvs" for age-based modifiers.
#' @param steps_per_day Sub-daily time steps for integration.
#' @param return_diagnostics Include extra diagnostic columns in output.
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
#' epicrop_sim(
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
#' `epicrop_sim()` is called by the following specific disease modelling functions:
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
#' @returns data.table with disease states, flows, and diagnostics
#' @autoglobal
#' @export
#' @export

epicrop_sim <- function(
  wth,
  start = list(
    mode = "emergence",
    emergence = NULL,
    base_temp = 0,
    heat_sum_threshold = 200
  ),
  run_days = 120L,
  inoculum = list(
    mode = "impulse",
    I0 = 10,
    start_offset = 20L,
    duration = 1L,
    series = NULL
  ),
  RcOpt,
  RcA,
  RcT,
  RcW = list(
    mode = "simple",
    rhlim = 90,
    rainlim = 5,
    weighted_wetness = FALSE,
    curve = NULL
  ),
  RRLEX = 0,
  p,
  i,
  SMax,
  a = 1,
  RRG,
  RRS,
  H0 = 250,
  thermal_time = list(base_temp = 0, gdd_to_maturity = 1100),
  age_driver = "dvs",
  steps_per_day = 1L,
  return_diagnostics = TRUE
) {
  # Validate and prepare weather data
  wth <- .validate_weather_data(wth, run_days)

  # Validate all parameters
  .validate_sim_parameters(
    run_days,
    RcOpt,
    SMax,
    H0,
    steps_per_day,
    start,
    inoculum,
    age_driver,
    RcW
  )

  # Determine simulation window
  run_info <- .determine_run_window(wth, start, run_days)
  w <- run_info$wth_subset

  # Calculate DVS
  dvs <- .calculate_dvs(w, thermal_time)

  # Prepare daily modifier inputs
  modifiers <- .prepare_modifiers(
    w,
    dvs,
    run_days,
    age_driver,
    RcA,
    RcT,
    RcW,
    RcOpt,
    p,
    i,
    RRS,
    a,
    RRLEX,
    RRG
  )

  # Build inoculum schedule
  Iprim <- .build_inoculum(inoculum, run_days)

  # Call C++ core
  core <- .run_cpp_core(
    modifiers,
    Iprim,
    H0,
    SMax,
    steps_per_day
  )

  # Assemble and return output
  .assemble_output(
    core,
    run_info$run_dates,
    run_days,
    modifiers,
    dvs,
    return_diagnostics
  )
}


#' Validate Weather Data
#' @autoglobal
#' @dev
.validate_weather_data <- function(wth, run_days) {
  if (!inherits(wth, "data.table")) {
    wth <- data.table::as.data.table(wth)
  }

  # Check required columns
  required_cols <- c("YYYYMMDD", "TEMP", "RAIN")
  missing_cols <- setdiff(required_cols, names(wth))

  if (length(missing_cols) > 0) {
    cli::cli_abort(c(
      "Missing required columns in {.var wth}:",
      "x" = "Need:  {.val {missing_cols}}"
    ))
  }

  # Add RHUM if missing
  if (!"RHUM" %in% names(wth)) {
    wth[, RHUM := NA_real_]
    cli::cli_inform(c(
      "i" = "{.var RHUM} column not found, using {.val NA}."
    ))
  }

  # Check for NAs
  .check_weather_nas(wth)

  data.table::setorderv(wth, "YYYYMMDD")
  wth
}

#' Check for NA Values in Weather Data
#' @autoglobal
#' @dev
.check_weather_nas <- function(wth) {
  if (anyNA(wth$TEMP)) {
    cli::cli_abort(c(
      "{.var TEMP} column contains {.val NA} values.",
      "x" = "Please impute or remove missing temperature data."
    ))
  }

  if (anyNA(wth$RAIN)) {
    cli::cli_warn(c(
      "!" = "{.var RAIN} column contains {.val NA} values.",
      "i" = "Treating as {.val 0} mm."
    ))
    wth[is.na(RAIN), RAIN := 0]
  }

  invisible(NULL)
}

#' Validate Simulation Parameters
#' @dev
.validate_sim_parameters <- function(
  run_days,
  RcOpt,
  SMax,
  H0,
  steps_per_day,
  start,
  inoculum,
  age_driver,
  RcW
) {
  # Numeric validations
  .validate_numeric_params(run_days, RcOpt, SMax, H0, steps_per_day)

  # Normalize string arguments
  start$mode <- match.arg(start$mode, c("emergence", "heat_sum"))
  age_driver <- match.arg(age_driver, c("day", "dvs"))
  inoculum$mode <- match.arg(
    inoculum$mode,
    c("impulse", "uniform", "triangle", "series")
  )
  RcW$mode <- match.arg(RcW$mode, c("simple", "complex", "custom"))

  invisible(NULL)
}

#' Validate Numeric Parameters
#' @dev
#' @autoglobal
.validate_numeric_params <- function(
  run_days,
  RcOpt,
  SMax,
  H0,
  steps_per_day
) {
  if (run_days <= 0) {
    cli::cli_abort(
      "{.var run_days} must be {.val > 0}, got {.val {run_days}}."
    )
  }
  if (RcOpt < 0) {
    cli::cli_abort(
      "{.var RcOpt} must be {.val >= 0}, got {.val {RcOpt}}."
    )
  }
  if (SMax <= 0) {
    cli::cli_abort(
      "{.var SMax} must be {.val > 0}, got {.val {SMax}}."
    )
  }
  if (H0 < 0) {
    cli::cli_abort(
      "{.var H0} must be {.val >= 0}, got {.val {H0}}."
    )
  }
  if (steps_per_day < 1) {
    cli::cli_abort(
      "{.var steps_per_day} must be {.val >= 1}, got {.val {steps_per_day}}."
    )
  }

  # Warn if not power of 2
  if ((steps_per_day & (steps_per_day - 1)) != 0) {
    cli::cli_warn(c(
      "!" = "{.var steps_per_day} should be a power of 2 for best accuracy.",
      "i" = "Got {.val {steps_per_day}}, consider {.val 1}, {.val 2}, {.val 4}, or {.val 8}."
    ))
  }

  invisible(NULL)
}

# ---- Run Window Functions (Complexity: 5-7) ----

#' Determine Simulation Run Window
#' @dev
#' @autoglobal
.determine_run_window <- function(wth, start, run_days) {
  if (start$mode == "heat_sum") {
    result <- .run_window_heat_sum(wth, start, run_days)
  } else {
    result <- .run_window_emergence(wth, start, run_days)
  }

  # Validate coverage
  .validate_run_window_coverage(result$wth_subset, run_days, result$run_dates)

  result
}

#' Run Window:  Heat Sum Mode
#' @keywords internal
.run_window_heat_sum <- function(wth, start, run_days) {
  wth[, gdd := cumsum(pmax(TEMP - start$base_temp, 0))]
  srow <- which(wth$gdd >= start$heat_sum_threshold)[1]

  if (is.na(srow)) {
    cli::cli_abort(c(
      "Heat sum threshold not reached in provided weather data.",
      "x" = "Need {.val {start$heat_sum_threshold}} °C·days, ",
      "maximum:  {.val {max(wth$gdd, na.rm = TRUE)}}."
    ))
  }

  run_dates <- wth$YYYYMMDD[srow + seq_len(run_days) - 1L]
  cli::cli_inform(c(
    "v" = "Heat sum mode:  starting row {.val {srow}} ({.val {wth$YYYYMMDD[srow]}})."
  ))

  list(
    run_dates = run_dates,
    wth_subset = wth[YYYYMMDD %in% run_dates]
  )
}

#' Run Window: Emergence Mode
#' @autoglobal
#' @dev
.run_window_emergence <- function(wth, start, run_days) {
  if (is.null(start$emergence)) {
    cli::cli_abort(
      "{.var start$emergence} required when {.field mode} = {. val emergence}."
    )
  }

  em <- as.Date(start$emergence)
  run_dates <- seq(em, by = "day", length.out = run_days)

  cli::cli_inform(c(
    "v" = "Emergence mode: starting {.val {em}}."
  ))

  list(
    run_dates = run_dates,
    wth_subset = wth[YYYYMMDD %in% run_dates]
  )
}

#' Validate Run Window Coverage
#' @autoglobal
#' @dev
.validate_run_window_coverage <- function(w, run_days, run_dates) {
  if (nrow(w) != run_days) {
    cli::cli_abort(c(
      "Weather records do not fully cover the run window.",
      "x" = "Need {.val {run_days}} days, have {.val {nrow(w)}}.",
      "i" = "Window: {.val {min(run_dates)}} to {.val {max(run_dates)}}."
    ))
  }
  invisible(NULL)
}

# ---- DVS Calculation (Complexity: 2) ----

#' Calculate Development Stage (DVS)
#' @dev
.calculate_dvs <- function(w, thermal_time) {
  gdd <- cumsum(pmax(w$TEMP - thermal_time$base_temp, 0))
  pmin(2, 2 * gdd / thermal_time$gdd_to_maturity)
}


# ---- Modifier Preparation (Complexity: 8) ----

#' Prepare All Daily Modifiers
#' @autoglobal
#' @dev
.prepare_modifiers <- function(
  w,
  dvs,
  run_days,
  age_driver,
  RcA,
  RcT,
  RcW,
  RcOpt,
  p,
  i,
  RRS,
  a,
  RRLEX,
  RRG
) {
  # Age-based modifier
  age_x <- if (age_driver == "dvs") dvs else seq.int(0, run_days - 1)
  RcA_day <- .safe_interp(RcA, age_x, "RcA")

  # Temperature modifier
  RcT_day <- .safe_interp(RcT, w$TEMP, "RcT")

  # Wetness modifier
  RcW_day <- .calculate_wetness_modifier(w, RcW, run_days)

  # Combined Rc
  Rc <- pmax(0, RcOpt * RcA_day * RcT_day * RcW_day)

  # Delays
  p_days <- .safe_interp(p, w$TEMP, "p")
  i_days <- .safe_interp(i, w$TEMP, "i")

  # Other modifiers
  RRS_day <- .safe_interp(RRS, dvs, "RRS")

  agg_x <- if (is.function(a)) dvs else seq_len(run_days)
  a_day <- pmax(1, .safe_interp(a, agg_x, "a"))

  RRLEX_day <- .safe_interp(RRLEX, seq_len(run_days), "RRLEX")
  RRG_day <- .safe_interp(RRG, seq_len(run_days), "RRG")

  list(
    Rc = Rc,
    RcA_day = RcA_day,
    RcT_day = RcT_day,
    RcW_day = RcW_day,
    p_days = p_days,
    i_days = i_days,
    RRS_day = RRS_day,
    a_day = a_day,
    RRLEX_day = RRLEX_day,
    RRG_day = RRG_day
  )
}

#' Safe Interpolation with Error Handling
#' @dev
.safe_interp <- function(def, x, name) {
  tryCatch(
    .interp(def, x),
    error = function(e) {
      cli::cli_abort(c(
        "Failed to interpolate {.var {name}}:",
        "x" = conditionMessage(e)
      ))
    }
  )
}


# ---- Wetness Calculation (Complexity: 6) ----

#' Calculate Wetness Modifier
#' @autoglobal
#' @dev
.calculate_wetness_modifier <- function(w, RcW, run_days) {
  if (RcW$mode == "simple") {
    return(.wetness_simple(w, RcW))
  }

  if (RcW$mode == "complex") {
    return(.wetness_complex(w, RcW))
  }

  if (RcW$mode == "custom") {
    return(.wetness_custom(w, RcW, run_days))
  }

  cli::cli_abort("Unknown wetness mode: {.val {RcW$mode}}")
}

#' Simple Wetness (Binary)
#' @dev
.wetness_simple <- function(w, RcW) {
  rhum_wet <- !is.na(w$RHUM) & (w$RHUM >= RcW$rhlim)
  rain_wet <- w$RAIN >= RcW$rainlim
  as.numeric(rhum_wet | rain_wet)
}

#' Complex Wetness (Hourly)
#' @autoglobal
#' @dev
.wetness_complex <- function(w, RcW) {
  # Check required columns
  .validate_complex_wetness_columns(w)

  # Calculate wet-hours
  wet_hours <- .calculate_leaf_wetness(
    wth = w,
    rhlim = RcW$rhlim,
    rainlim = RcW$rainlim,
    weighted_wetness = RcW$weighted_wetness
  )

  # Map to multiplier
  if (is.null(RcW$curve)) {
    return(wet_hours / 24.0)
  }

  .apply_wetness_curve(wet_hours, RcW$curve)
}

#' Validate Columns for Complex Wetness
#' @autoglobal
#' @dev
.validate_complex_wetness_columns <- function(w) {
  required <- c("TMIN", "TMAX", "LAT", "DOY")
  missing <- setdiff(required, names(w))

  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Complex wetness requires additional columns:",
      "x" = "Missing: {.val {missing}}.",
      "i" = "Use {.field mode} = {.val simple} or provide complete data."
    ))
  }

  invisible(NULL)
}

#' Apply Custom Wetness Curve
#' @dev
.apply_wetness_curve <- function(wet_hours, curve) {
  if (!is.matrix(curve) || ncol(curve) != 2) {
    cli::cli_abort(c(
      "{.var RcW$curve} must be a 2-column matrix:",
      "x" = "Column 1: wet-hours (0-24), Column 2: multiplier (0-1)."
    ))
  }

  stats::approx(
    x = curve[, 1],
    y = curve[, 2],
    xout = wet_hours,
    method = "linear",
    yleft = 0,
    yright = 1
  )$y
}

#' Custom Wetness Function
#' @autoglobal
#' @dev
.wetness_custom <- function(w, RcW, run_days) {
  if (!is.function(RcW$fun)) {
    cli::cli_abort(c(
      "Custom wetness requires {.var RcW$fun} to be a function.",
      "x" = "Got {.cls {class(RcW$fun)}}."
    ))
  }

  tryCatch(
    vapply(
      seq_len(run_days),
      function(d) RcW$fun(d, w$RAIN[d], w$RHUM[d], w),
      numeric(1)
    ),
    error = function(e) {
      cli::cli_abort(c(
        "Custom wetness function failed:",
        "x" = conditionMessage(e)
      ))
    }
  )
}


# ---- Interpolation (Complexity: 7) ----

#' Interpolate Parameter Definition
#' @dev
.interp <- function(def, x) {
  if (is.null(def)) {
    cli::cli_abort("{.var def} cannot be {.val NULL}.")
  }

  if (is.function(def)) {
    return(.interp_function(def, x))
  }

  if (is.numeric(def)) {
    return(.interp_numeric(def, x))
  }

  if (is.matrix(def) || is.data.frame(def)) {
    return(.interp_table(def, x))
  }

  cli::cli_abort(c(
    "Unsupported definition type: {.cls {class(def)}}.",
    "i" = "Expected:  scalar, vector, matrix, data.frame, or function."
  ))
}

#' Interpolate Function
#' @autoglobal
#' @dev
.interp_function <- function(def, x) {
  result <- tryCatch(
    def(x),
    error = function(e) {
      cli::cli_abort(c(
        "Function evaluation failed:",
        "x" = conditionMessage(e)
      ))
    }
  )
  pmax(0, as.numeric(result))
}

#' Interpolate Numeric
#' @autoglobal
#' @dev
.interp_numeric <- function(def, x) {
  if (length(def) == 1L) {
    return(rep(def, length(x)))
  }

  if (length(def) == length(x)) {
    return(as.numeric(def))
  }

  cli::cli_abort(c(
    "Numeric vector length mismatch:",
    "x" = "Vector length {.val {length(def)}}, expected {.val {length(x)}}."
  ))
}

#' Interpolate Table
#' @dev
.interp_table <- function(def, x) {
  if (ncol(def) != 2) {
    cli::cli_abort(c(
      "Table must have 2 columns:  {.val [x, value]}.",
      "x" = "Got {.val {ncol(def)}} columns."
    ))
  }

  stats::approx(
    x = def[, 1],
    y = def[, 2],
    xout = x,
    method = "linear",
    yleft = def[1, 2],
    yright = def[nrow(def), 2]
  )$y
}


#' Build Inoculum Schedule
#' @autoglobal
#' @dev
.build_inoculum <- function(inoculum, run_days) {
  so <- inoculum$start_offset

  # Validate offset
  if (so < 1L || so > run_days) {
    cli::cli_warn(c(
      "!" = "{.var start_offset} = {.val {so}} outside run window.",
      "i" = "Valid:  {.val 1} to {.val {run_days}}.  No inoculum applied."
    ))
    return(numeric(run_days))
  }

  # Dispatch by mode
  switch(
    inoculum$mode,
    impulse = .inoculum_impulse(inoculum, run_days, so),
    uniform = .inoculum_uniform(inoculum, run_days, so),
    triangle = .inoculum_triangle(inoculum, run_days, so),
    series = .inoculum_series(inoculum, run_days),
    cli::cli_abort("Unknown inoculum mode: {.val {inoculum$mode}}")
  )
}

#' Impulse Inoculum
#' @autoglobal
#' @dev
.inoculum_impulse <- function(inoculum, run_days, so) {
  Iprim <- numeric(run_days)
  Iprim[so] <- inoculum$I0

  cli::cli_inform(c(
    "i" = "Inoculum:  {.val {inoculum$I0}} at day {.val {so}} (impulse)."
  ))

  Iprim
}

#' Uniform Inoculum
#' @autoglobal
#' @dev
.inoculum_uniform <- function(inoculum, run_days, so) {
  Iprim <- numeric(run_days)
  L <- max(1L, inoculum$duration)
  idx <- so:(so + L - 1L)
  idx <- idx[idx >= 1L & idx <= run_days]

  if (length(idx) > 0L) {
    Iprim[idx] <- inoculum$I0 / length(idx)
    cli::cli_inform(c(
      "i" = "Inoculum: {.val {inoculum$I0}} uniform, days {.val {min(idx)}}-{.val {max(idx)}}."
    ))
  }

  Iprim
}

#' Triangle Inoculum
#' @autoglobal
#' @dev
.inoculum_triangle <- function(inoculum, run_days, so) {
  Iprim <- numeric(run_days)
  L <- max(3L, inoculum$duration)
  idx <- so:(so + L - 1L)
  idx <- idx[idx >= 1L & idx <= run_days]

  if (length(idx) > 0L) {
    half <- ceiling(length(idx) / 2)
    wts <- c(seq_len(half), rev(seq_len(length(idx) - half)))
    Iprim[idx] <- inoculum$I0 * (wts / sum(wts))

    cli::cli_inform(c(
      "i" = "Inoculum: {.val {inoculum$I0}} triangular, days {.val {min(idx)}}-{.val {max(idx)}}."
    ))
  }

  Iprim
}

#' Series Inoculum
#' @autoglobal
#' @dev
.inoculum_series <- function(inoculum, run_days) {
  if (is.null(inoculum$series)) {
    cli::cli_abort(c(
      "{.var inoculum$series} required for {.field mode} = {.val series}.",
      "x" = "Got {.val NULL}."
    ))
  }

  if (length(inoculum$series) != run_days) {
    cli::cli_abort(c(
      "{.var inoculum$series} length must equal {.var run_days}.",
      "x" = "Got {.val {length(inoculum$series)}}, expected {.val {run_days}}."
    ))
  }

  total <- sum(inoculum$series)
  cli::cli_inform(c(
    "i" = "Inoculum: custom series, total = {.val {round(total, 2)}}."
  ))

  inoculum$series
}


# ---- C++ Core Wrapper (Complexity: 2) ----

#' Run C++ Core with Error Handling
#' @autoglobal
#' @dev
.run_cpp_core <- function(modifiers, Iprim, H0, SMax, steps_per_day) {
  cli::cli_progress_step("Running C++ simulation core...")

  core <- tryCatch(
    epicrop_core_cpp(
      Rc = modifiers$Rc,
      RRLEX = modifiers$RRLEX_day,
      RRG = modifiers$RRG_day,
      RRS = modifiers$RRS_day,
      p_days = modifiers$p_days,
      i_days = modifiers$i_days,
      agg = modifiers$a_day,
      Iprim = Iprim,
      H0 = H0,
      SMax = SMax,
      steps_per_day = as.integer(steps_per_day)
    ),
    error = function(e) {
      cli::cli_abort(c(
        "C++ simulation core failed:",
        "x" = conditionMessage(e)
      ))
    }
  )

  cli::cli_progress_done()
  core
}

#' Assemble Simulation Output
#' @autoglobal
#' @dev
.assemble_output <- function(
  core,
  run_dates,
  run_days,
  modifiers,
  dvs,
  return_diagnostics
) {
  out <- data.table::data.table(
    simday = seq_len(run_days),
    dates = run_dates,
    sites = core$sites,
    latent = core$latent,
    infectious = core$infectious,
    removed = core$removed,
    senesced = core$senesced,
    rateinf = core$rateinf,
    rlex = core$rlex,
    rtransfer = core$rtransfer,
    rremoved = core$rremoved,
    rgrowth = core$rgrowth,
    rsenesced = core$rsenesced,
    diseased = core$diseased,
    intensity = core$intensity
  )

  if (return_diagnostics) {
    out[, `:=`(
      Rc = modifiers$Rc,
      RcA_today = modifiers$RcA_day,
      RcT_today = modifiers$RcT_day,
      RcW_today = modifiers$RcW_day,
      COFR = core$cofr,
      DVS = dvs,
      p_today = modifiers$p_days,
      i_today = modifiers$i_days
    )]
  }

  # Calculate AUDPC using existing function
  audpc <- .calculate_audpc(out$intensity)
  data.table::setattr(out, "AUDPC", audpc)

  cli::cli_inform(c(
    "v" = "Simulation complete:  {.val {run_days}} days.",
    "i" = "AUDPC = {.val {round(audpc, 2)}}."
  ))

  out[]
}
