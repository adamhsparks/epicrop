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
#' @param emergence Expected date of plant emergence (or transplanting for rice)
#'  entered in `YYYY-MM-DD` format (character) OR a list with:
#'   * `mode`: "emergence" or "heat_sum"
#'   * `emergence`: Date string (if mode = "emergence")
#'   * `base_temp`: Base temperature for GDD calculation (if mode = "heat_sum")
#'   * `heat_sum_threshold`: GDD threshold to trigger start (if mode = "heat_sum")
#'
#' @param onset expected number of days until the onset of disease after
#'  emergence date (day, integer).  Described in Table 1 of Savary _et al._
#'  2012.
#'
#' @param duration simulation duration *i.e.*, growing season length (day,
#'  integer).  Described in Table 1 of Savary _et al. _ 2012.
#'
#' @param rhlim relative humidity value threshold to decide whether leaves are
#'  wet or not (numeric).  Described in Table 1 of Savary _et al._ 2012.  Savary
#'  _et al._ 2012 used `90`.
#'
#' @param rainlim rainfall amount (mm) threshold to decide whether leaves are
#'  wet or not (numeric).  Described in Table 1 of Savary _et al._ 2012.  Savary
#'  _et al._ 2012 used `5`.
#'
#' @param H0 initial number of plant's healthy sites (integer).  Described in
#'  Table 1 of Savary _et al._ 2012.
#'
#' @param I0 initial number of infective sites (scalar integer) OR a list with:
#'   * `mode`: "impulse", "uniform", "triangle", or "series"
#'   * `I0`: Total inoculum amount
#'   * `start_offset`: Day to start inoculation (defaults to `onset`)
#'   * `duration`: Number of days over which to distribute inoculum
#'   * `series`: Custom daily inoculum vector (for mode = "series")
#'  Described in Table 1 of Savary _et al._ 2012. I0 is injected as new
#'  infections (become infectious after `p` days).
#'
#' @param RcA modifier for _Rc_ (the basic infection rate corrected for
#'  removals) for crop age as a numeric two-column matrix with the first column
#'  being the crop age in days (0 to `duration-1`) or DVS (0 to 2) and the
#'  second being the modifier (bounded by 0 and 1).  Described in Tables 1 and 2
#'  of Savary _et al._ 2012.
#'
#' @param RcT modifier for _Rc_ (the basic infection rate corrected for
#'  removals) for temperature as a numeric two-column matrix with the first
#'  column being temperature in °C and the second the modifier (bounded by 0 and
#'  1).  Described in Tables 1 and 2 of Savary _et al._ 2012.
#'
#' @param RcOpt potential basic infection rate corrected for removals (numeric).
#'  Described in Table 1 of Savary _et al._ 2012.  This value can be modified
#'  to reflect crop cultivar resistance ratings as well, see Kim _et al. _ 2015
#'  for more details.
#'
#' @param p duration of latent period (day, integer or function of temperature).
#'  Described in Table 1 of Savary _et al._ 2012.
#'
#' @param i duration of infectious period (day, integer or function of
#'  temperature).  Described in Table 1 of Savary _et al._ 2012.
#'
#' @param Sx maximum number of sites (integer).  Described in Table 1 of Savary
#'  _et al._ 2012.
#'
#' @param a aggregation coefficient, values are from 1 to >1 (numeric or
#'  function of DVS).  Described in Table 1 of Savary _et al._ 2012 and Table 1
#'  of Savary _et al._ 2015.  See further details in **_a_ - Aggregation**
#'  section.
#'
#' @param RRS relative rate of physiological senescence (numeric, vector, or
#'  function of DVS).  Described in Table 1 of Savary _et al._ 2012.
#'
#' @param RRG relative rate of growth (numeric, vector, or function of time).
#'  Described in Table 1 of Savary _et al._ 2012.
#'
#' @param RcW Optional modifier for _Rc_ to calculate leaf wetness effect
#'  from 1 if wet to 0 if dry.   This is a numeric two-column matrix with the
#'  first column being hours (0 to 24) and the second the modifier (bounded by 0
#'  and 1).  Used when `simple_wetness = FALSE` (hourly/derived wetness). If
#'  `NULL` (default), a linear relationship is used where 0 hours wet = 0 and
#'  24 hours wet = 1. Ignored if `simple_wetness = TRUE`.
#'
#' @param RRLEX Rate of lesion expansion (scalar, vector, or function). Allows
#'  healthy sites to be directly converted to infectious sites without passing
#'  through latent stage.  Set to 0 (default) for standard SEIR behavior.
#'
#' @param simple_wetness Logical.  If `TRUE` (default), uses binary wetness
#'  calculation (0 or 1) based on `rhlim` and `rainlim` thresholds.  If `FALSE`,
#'  uses hourly wetness duration calculation (requires TMIN, TMAX, LAT, DOY in
#'  `wth`) and applies `RcW` curve.
#'
#' @param age_driver Character. Use "day" (default, 0-indexed days from
#'  emergence) or "dvs" (development stage 0-2) for age-based modifiers in `RcA`.
#'  When "dvs", `thermal_time` parameter is required.
#'
#' @param thermal_time List with `base_temp` (°C) and `gdd_to_maturity` (°C·days).
#'  Only required when `age_driver = "dvs"`. Determines how growing degree days
#'  are accumulated and converted to development stage (DVS).
#'
#' @param steps_per_day Integer. Number of sub-daily time steps for numerical
#'  integration. Default is 1 (daily steps). Values of 2, 4, or 8 can improve
#'  accuracy for stiff systems at cost of computation time.  Should be a power of 2.
#'
#' @returns A [data.table::data.table()] object containing the following
#' columns:
#'
#' \describe{
#'   \item{simday}{Day of simulation (1-indexed, where 1 is the emergence date)}
#'   \item{dates}{Date of simulation (Date class)}
#'   \item{sites}{Total number of healthy sites present on day "x"}
#'   \item{latent}{Number of latent sites present on day "x"}
#'   \item{infectious}{Number of infectious sites present on day "x"}
#'   \item{removed}{Number of removed sites present on day "x"}
#'   \item{senesced}{Number of senesced sites present on day "x"}
#'   \item{rateinf}{Rate of infection (new infections on day "x")}
#'   \item{rlex}{Rate of lesion expansion (healthy sites converted to infectious via direct expansion on day "x")}
#'   \item{rtransfer}{Rate of transfer from latent to infectious sites on day "x"}
#'   \item{rremoved}{Rate of removal from infectious to removed sites on day "x"}
#'   \item{rgrowth}{Rate of growth of healthy sites on day "x"}
#'   \item{rsenesced}{Rate of senescence of healthy sites on day "x"}
#'   \item{diseased}{Number of diseased (latent + infectious + removed) sites on day "x"}
#'   \item{intensity}{Proportion of diseased sites (latent + infectious) per total living sites (excludes removed and senesced) on day "x"}
#'   \item{lat}{Latitude value if provided by the `wth` object}
#'   \item{lon}{Longitude value if provided by the `wth` object}
#' }
#'
#' The returned object also has an attribute `AUDPC` (accessed via
#' `attr(result, "AUDPC")`) containing the area under the disease progress
#' curve for the entire simulation, calculated using the trapezoidal rule.
#'
#' @section Output Structure:
#'
#' The output is organized into three categories:
#'
#' **State Variables** (stocks at end of each day):
#' * `sites`, `latent`, `infectious`, `removed`, `senesced`
#'
#' **Flow Variables** (rates/changes during each day):
#' * `rateinf`, `rlex`, `rtransfer`, `rremoved`, `rgrowth`, `rsenesced`
#'
#' **Derived Variables** (calculated from states):
#' * `diseased`, `intensity`
#'
#' **Mass Balance:**
#' At any day:  `sites + latent + infectious + removed + senesced` should equal
#' initial sites (H0) plus cumulative growth minus cumulative senescence
#' (accounting for numerical precision).
#'
#' @references
#' [...  keep all existing references ...]
#'
#' @examplesIf interactive()
#' [...  keep all existing examples ...]
#'
#' @section _a_ -- Aggregation Details:
#' [... keep existing section ...]
#'
#' @section TMIN/TMAX Details:
#' [... keep existing section ...]
#'
#' @section LAT/LON Details:
#' [... keep existing section ...]
#'
#' @seealso
#' `epicrop_sim()` is the core simulation engine called by disease-specific
#'  helper functions:
#' * [bacterial_blight()],
#' * [brown_spot()],
#' * [leaf_blast()],
#' * [sheath_blight()],
#' * [tungro()],
#' * [modified_kim_leaf_blast()],
#' * [modified_kim_sheath_blight()],
#' * [leaf_rust()], and
#' * [s_tritici_blotch()].
#'
#' @author Adam H.  Sparks, \email{adamhsparks@@gmail.com} from original by
#'   Robert J. Hijmans, Rene Pangga and Jorrel Aunario.
#'
#' @autoglobal
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
    return_diagnostics,
    wth
  )
}

# ==== Helper Functions ====

#' Prepare and Validate Weather Data
#'
#' Converts weather data to data.table format, validates required columns,
#' adds missing RHUM column if needed, checks for NA values, and orders by date.
#'
#' @param wth A data.frame or data.table containing weather data with at least
#'   YYYYMMDD, TEMP, and RAIN columns.
#'
#' @returns A data.table with validated and ordered weather data.
#'
#' @details
#' This function performs the following validations and transformations:
#' * Converts to data.table if not already
#' * Checks for required columns (YYYYMMDD, TEMP, RAIN)
#' * Adds RHUM column with NA if missing
#' * Checks TEMP for NA values (aborts if found)
#' * Checks RAIN for NA values (warns and replaces with 0)
#' * Orders data by YYYYMMDD
#'
#' @examplesIf interactive()
#' wth <- data.frame(
#'   YYYYMMDD = seq(as.Date("2000-01-01"), by = "day", length.out = 10),
#'   TEMP = rnorm(10, 25, 3),
#'   RAIN = rpois(10, 2)
#' )
#'
#' wth_clean <- epicrop:::.prep_weather(wth)
#' str(wth_clean)
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @dev
prep_weather <- function(wth) {
  if (!inherits(wth, "data.table")) {
    wth <- data.table::as.data.table(wth)
  }

  # Check required columns
  required <- c("YYYYMMDD", "TEMP", "RAIN")
  missing <- setdiff(required, names(wth))

  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Missing required columns in {.var wth}:",
      "x" = "{.val {missing}}"
    ))
  }

  # Add RHUM if missing
  if (!"RHUM" %in% names(wth)) {
    wth[, RHUM := NA_real_]
    cli::cli_inform("i" = "{.var RHUM} not found, using {.val NA}")
  }

  # Check for NAs
  if (anyNA(wth$TEMP)) {
    cli::cli_abort("{.var TEMP} contains {.val NA} values")
  }

  if (anyNA(wth$RAIN)) {
    cli::cli_warn("{.var RAIN} contains {.val NA}, treating as 0")
    wth[is.na(RAIN), RAIN := 0]
  }

  data.table::setorderv(wth, "YYYYMMDD")
  wth
}

#' Validate Simulation Parameters
#'
#' Checks that all numerical simulation parameters are within valid ranges.
#'
#' @param duration Simulation duration in days (must be > 0).
#' @param RcOpt Optimal infection rate (must be >= 0).
#' @param Sx Maximum number of sites (must be > 0).
#' @param H0 Initial healthy sites (must be >= 0).
#' @param steps_per_day Number of sub-daily time steps (must be >= 1).
#'
#' @returns Nothing, called for its side effects.  Aborts with cli error messages
#'   if any parameter is invalid.  Warns if steps_per_day is not a power of 2.
#'
#' @details
#' Valid parameter ranges:
#' * `duration`: Must be > 0
#' * `RcOpt`: Must be >= 0
#' * `Sx`: Must be > 0
#' * `H0`: Must be >= 0
#' * `steps_per_day`: Must be >= 1 (warns if not 1, 2, 4, or 8)
#'
#' @examplesIf interactive()
#' # Valid parameters
#' epicrop:: :.validate_params(
#'   duration = 120,
#'   RcOpt = 0.61,
#'   Sx = 100000,
#'   H0 = 600,
#'   steps_per_day = 1
#' )
#'
#' # This would abort
#' \dontrun{
#' epicrop:::.validate_params(
#'   duration = -10,  # Invalid:  must be > 0
#'   RcOpt = 0.61,
#'   Sx = 100000,
#'   H0 = 600,
#'   steps_per_day = 1
#' )
#' }
#'
#' @author Adam H.  Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @dev
.validate_params <- function(duration, RcOpt, Sx, H0, steps_per_day) {
  if (duration <= 0) {
    cli::cli_abort("{.var duration} must be {.val > 0}, got {.val {duration}}")
  }
  if (RcOpt < 0) {
    cli::cli_abort("{.var RcOpt} must be {.val >= 0}, got {.val {RcOpt}}")
  }
  if (Sx <= 0) {
    cli::cli_abort("{.var Sx} must be {.val > 0}, got {.val {Sx}}")
  }
  if (H0 < 0) {
    cli::cli_abort("{.var H0} must be {.val >= 0}, got {.val {H0}}")
  }
  if (steps_per_day < 1) {
    cli::cli_abort(
      "{.var steps_per_day} must be {.val >= 1}, got {.val {steps_per_day}}"
    )
  }

  # Warn if not power of 2
  if ((steps_per_day & (steps_per_day - 1)) != 0) {
    cli::cli_warn(c(
      "!" = "{.var steps_per_day} should be power of 2 for best accuracy",
      "i" = "Got {.val {steps_per_day}}, consider {.val 1}, {.val 2}, {.val 4}, or {.val 8}"
    ))
  }
}

#' Get Simulation Window from Weather Data
#'
#' Extracts the appropriate subset of weather data for the simulation based on
#' either a simple emergence date or advanced heat sum threshold.
#'
#' @param wth A data.table of weather data (already validated).
#' @param emergence Either a date string ("YYYY-MM-DD") or a list with:
#'   * `mode`: "emergence" or "heat_sum"
#'   * `emergence`: Date string (if mode = "emergence")
#'   * `base_temp`: Base temperature for GDD calculation (if mode = "heat_sum")
#'   * `heat_sum_threshold`: GDD threshold to trigger start (if mode = "heat_sum")
#' @param duration Number of days for the simulation.
#'
#' @returns A data.table subset of `wth` containing exactly `duration` rows
#'   starting from the determined emergence date.
#'
#' @details
#' Two modes are supported:
#'
#' **Simple mode** (emergence is a date string):
#' * Uses the provided date as simulation start
#' * Extracts `duration` consecutive days starting from that date
#'
#' **Heat sum mode** (emergence is a list with mode = "heat_sum"):
#' * Calculates cumulative growing degree days (GDD)
#' * Finds first day when GDD >= heat_sum_threshold
#' * Extracts `duration` consecutive days starting from that day
#'
#' @examplesIf interactive()
#' wth <- epicrop::get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-01-01", "2000-12-31")
#' )
#'
#' # Simple emergence mode
#' w_simple <- epicrop:::.get_simulation_window(
#'   wth = wth,
#'   emergence = "2000-07-15",
#'   duration = 120
#' )
#' nrow(w_simple)  # 120
#'
#' # Heat sum mode
#' w_heat <- epicrop:::.get_simulation_window(
#'   wth = wth,
#'   emergence = list(
#'     mode = "heat_sum",
#'     base_temp = 0,
#'     heat_sum_threshold = 200
#'   ),
#'   duration = 120
#' )
#' nrow(w_heat)  # 120
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @dev
.get_simulation_window <- function(wth, emergence, duration) {
  # Simple mode: emergence is a date string
  if (!is.list(emergence)) {
    em_date <- as.Date(emergence)
    dates <- seq(em_date, by = "day", length.out = duration)

    cli::cli_inform("v" = "Emergence mode: starting {.val {em_date}}")

    w <- wth[YYYYMMDD %in% dates]

    if (nrow(w) != duration) {
      cli::cli_abort(c(
        "Insufficient weather data for simulation window",
        "x" = "Need {.val {duration}} days, have {.val {nrow(w)}}",
        "i" = "Window:   {.val {min(dates)}} to {.val {max(dates)}}"
      ))
    }

    return(w)
  }

  # Advanced mode: emergence is a list
  mode <- match.arg(emergence$mode, c("emergence", "heat_sum"))

  if (mode == "emergence") {
    em_date <- as.Date(emergence$emergence)
    dates <- seq(em_date, by = "day", length.out = duration)

    cli::cli_inform("v" = "Emergence mode: starting {.val {em_date}}")
  } else {
    # Heat sum mode
    wth[, gdd := cumsum(pmax(TEMP - emergence$base_temp, 0))]
    srow <- which(wth$gdd >= emergence$heat_sum_threshold)[1]

    if (is.na(srow)) {
      cli::cli_abort(c(
        "Heat sum threshold not reached in weather data",
        "x" = "Need {.val {emergence$heat_sum_threshold}} °C·days",
        "i" = "Maximum available:   {.val {max(wth$gdd, na.rm = TRUE)}}"
      ))
    }

    dates <- wth$YYYYMMDD[srow + seq_len(duration) - 1L]

    cli::cli_inform(c(
      "v" = "Heat sum mode: starting row {.val {srow}}",
      "i" = "Date:  {.val {wth$YYYYMMDD[srow]}}"
    ))
  }

  w <- wth[YYYYMMDD %in% dates]

  if (nrow(w) != duration) {
    cli::cli_abort(c(
      "Insufficient weather data for simulation window",
      "x" = "Need {.val {duration}} days, have {.val {nrow(w)}}"
    ))
  }

  w
}

#' Calculate Development Stage (DVS)
#'
#' Calculates the development stage of the crop from 0 (emergence) to 2
#' (physiological maturity) based on cumulative growing degree days (GDD).
#'
#' @param w A data.table of weather data with a TEMP column.
#' @param thermal_time A list with:
#'   * `base_temp`: Base temperature for GDD calculation (°C)
#'   * `gdd_to_maturity`: Total GDD required to reach DVS = 2 (°C·days)
#'
#' @returns A numeric vector of DVS values from 0 to 2, one per day in `w`.
#'
#' @details
#' DVS (Development Stage) is calculated as:
#' * GDD = cumsum(max(0, TEMP - base_temp))
#' * DVS = min(2, 2 * GDD / gdd_to_maturity)
#'
#' This provides a physiologically realistic way to scale crop age effects,
#' as opposed to simple calendar days.  DVS values:
#' * 0 = Emergence
#' * 1 = Anthesis/flowering
#' * 2 = Physiological maturity
#'
#' @examplesIf interactive()
#' wth <- epicrop::get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-07-01", "2000-10-31")
#' )
#'
#' thermal_time <- list(
#'   base_temp = 10,
#'   gdd_to_maturity = 1100
#' )
#'
#' dvs <- epicrop:::.calculate_dvs(wth, thermal_time)
#'
#' # Plot DVS over time
#' plot(wth$YYYYMMDD, dvs, type = "l",
#'      xlab = "Date", ylab = "DVS",
#'      main = "Crop Development Stage")
#' abline(h = c(0, 1, 2), lty = 2, col = "gray")
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @dev
.calculate_dvs <- function(w, thermal_time) {
  gdd <- cumsum(pmax(w$TEMP - thermal_time$base_temp, 0))
  pmin(2, 2 * gdd / thermal_time$gdd_to_maturity)
}

#' Interpolate Parameter Values
#'
#' Interpolates or evaluates parameter definitions to produce a vector of daily
#' values for use in the simulation.  Handles scalars, vectors, matrices, data
#' frames, and functions.
#'
#' @param def Parameter definition.  Can be:
#'   * Scalar numeric:  Replicated to length of `x`
#'   * Numeric vector: Must have same length as `x`
#'   * Matrix/data. frame: 2-column table [x_values, y_values] for linear interpolation
#'   * Function:  Evaluated at each value of `x`
#' @param x Numeric vector of x-values at which to interpolate (e.g., days, temperatures, DVS).
#'
#' @returns A numeric vector of length `length(x)` with interpolated/evaluated values.
#'
#' @details
#' For tables/matrices:
#' * First column is x-values (e.g., days, °C)
#' * Second column is y-values (modifier from 0 to 1)
#' * Linear interpolation between points
#' * Values outside range use first/last y-value (no extrapolation)
#'
#' For functions:
#' * Called with vector `x` as argument
#' * Must return numeric vector of same length
#' * Negative values are set to 0
#'
#' @examplesIf interactive()
#' # Scalar
#' epicrop:::.interp(0.1, 1: 10)  # rep(0.1, 10)
#'
#' # Vector
#' epicrop:::.interp(seq(0, 1, length.out = 10), 1:10)
#'
#' # Matrix (temperature response)
#' RcT <- cbind(
#'   c(15, 20, 25, 30, 35, 40),
#'   c(0, 0.5, 1.0, 0.8, 0.3, 0)
#' )
#' temps <- seq(10, 45, by = 1)
#' response <- epicrop:::.interp(RcT, temps)
#' plot(temps, response, type = "l",
#'      xlab = "Temperature (°C)",
#'      ylab = "Modifier")
#'
#' # Function
#' quadratic <- function(x) x^2 / 100
#' epicrop:::.interp(quadratic, 1:10)
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @dev
.interp <- function(def, x) {
  # Handle functions
  if (is.function(def)) {
    result <- tryCatch(
      def(x),
      error = function(e) {
        cli::cli_abort(c(
          "Function evaluation failed:",
          "x" = conditionMessage(e)
        ))
      }
    )
    return(pmax(0, as.numeric(result)))
  }

  # Handle numeric scalars and vectors
  if (is.numeric(def)) {
    if (length(def) == 1) {
      return(rep(def, length(x)))
    }
    if (length(def) == length(x)) {
      return(as.numeric(def))
    }
    cli::cli_abort(c(
      "Vector length mismatch:",
      "x" = "Got {.val {length(def)}}, expected {.val {length(x)}}"
    ))
  }

  # Handle tables/matrices
  if (is.matrix(def) || is.data.frame(def)) {
    if (ncol(def) != 2) {
      cli::cli_abort(c(
        "Table must have 2 columns:   [x, value]",
        "x" = "Got {.val {ncol(def)}} columns"
      ))
    }

    return(
      stats::approx(
        x = def[, 1],
        y = def[, 2],
        xout = x,
        method = "linear",
        yleft = def[1, 2],
        yright = def[nrow(def), 2]
      )$y
    )
  }

  cli::cli_abort("Unsupported parameter type: {.cls {class(def)}}")
}

#' Calculate Wetness Modifier
#'
#' Calculates the daily wetness modifier (0 to 1) for infection rate based on
#' relative humidity and rainfall, using either simple binary or complex hourly
#' wetness calculation.
#'
#' @param w A data.table of weather data with RHUM, RAIN, and optionally
#'   TMIN, TMAX, LAT, DOY columns.
#' @param rhlim Relative humidity threshold (%) for considering leaves wet.
#' @param rainlim Rainfall threshold (mm) for considering leaves wet.
#' @param simple_wetness Logical.  If TRUE, uses binary wetness (0 or 1 per day).
#'   If FALSE, calculates hourly wetness duration (requires TMIN, TMAX, LAT, DOY).
#' @param RcW Optional 2-column matrix for mapping wet-hours to infection modifier.
#'   First column: wet-hours (0-24), second column: modifier (0-1).
#'   Only used when simple_wetness = FALSE.  If NULL, linear relationship is used.
#'
#' @returns A numeric vector of wetness modifiers (0 to 1), one per day in `w`.
#'
#' @details
#' **Simple mode** (simple_wetness = TRUE):
#' * Returns 1 if RHUM >= rhlim OR RAIN >= rainlim
#' * Returns 0 otherwise
#'
#' **Complex mode** (simple_wetness = FALSE):
#' * Calculates hourly relative humidity from diurnal temperature patterns
#' * Counts wet-hours (0 to 24) based on hourly RH and rainfall
#' * Maps wet-hours to infection modifier using RcW curve or linear relationship
#'
#' @examplesIf interactive()
#' wth <- epicrop::get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-07-01", "2000-07-31")
#' )
#'
#' # Simple binary wetness
#' wet_simple <- epicrop:::.calculate_wetness(
#'   w = wth,
#'   rhlim = 90,
#'   rainlim = 5,
#'   simple_wetness = TRUE,
#'   RcW = NULL
#' )
#' table(wet_simple)  # 0s and 1s
#'
#' # Complex hourly wetness
#' wet_complex <- epicrop:::.calculate_wetness(
#'   w = wth,
#'   rhlim = 90,
#'   rainlim = 5,
#'   simple_wetness = FALSE,
#'   RcW = NULL  # Linear:  0 hours = 0, 24 hours = 1
#' )
#' summary(wet_complex)  # Continuous 0 to 1
#'
#' # With custom RcW curve
#' RcW_curve <- cbind(
#'   c(0, 4, 8, 12, 16, 20, 24),
#'   c(0, 0, 0.2, 0.5, 0.8, 0.95, 1.0)
#' )
#' wet_custom <- epicrop:::.calculate_wetness(
#'   w = wth,
#'   rhlim = 90,
#'   rainlim = 5,
#'   simple_wetness = FALSE,
#'   RcW = RcW_curve
#' )
#'
#' @author Adam H.  Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @dev
.calculate_wetness <- function(w, rhlim, rainlim, simple_wetness, RcW) {
  if (simple_wetness) {
    # Binary wetness:   0 or 1
    rhum_wet <- !is.na(w$RHUM) & (w$RHUM >= rhlim)
    rain_wet <- w$RAIN >= rainlim
    return(as.numeric(rhum_wet | rain_wet))
  }

  # Complex wetness requires additional columns
  required <- c("TMIN", "TMAX", "LAT", "DOY")
  missing <- setdiff(required, names(w))

  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Complex wetness requires additional columns:",
      "x" = "Missing: {.val {missing}}",
      "i" = "Use {.code simple_wetness = TRUE} or provide complete data"
    ))
  }

  # Calculate wet-hours (0 to 24)
  wet_hours <- .calculate_leaf_wetness(
    wth = w,
    rhlim = rhlim,
    rainlim = rainlim,
    weighted_wetness = FALSE
  )

  # Map wet-hours to infection multiplier (0 to 1)
  if (is.null(RcW)) {
    # Linear:   0 hours = 0, 24 hours = 1
    return(wet_hours / 24.0)
  }

  # Apply custom curve
  if (!is.matrix(RcW) || ncol(RcW) != 2) {
    cli::cli_abort(c(
      "{.var RcW} must be a 2-column matrix:",
      "x" = "Column 1: wet-hours (0-24), Column 2: multiplier (0-1)"
    ))
  }

  stats::approx(
    x = RcW[, 1],
    y = RcW[, 2],
    xout = wet_hours,
    method = "linear",
    yleft = 0,
    yright = 1
  )$y
}

#' Build Inoculum Schedule
#'
#' Creates a vector of daily primary inoculum amounts for the simulation,
#' supporting both simple (single impulse) and advanced (distributed) modes.
#'
#' @param I0 Either:
#'   * A scalar numeric: Applied as impulse at `onset` day
#'   * A list with:
#'     - `mode`: "impulse", "uniform", "triangle", or "series"
#'     - `I0`: Total inoculum amount
#'     - `start_offset`: Day to start inoculation (defaults to `onset`)
#'     - `duration`: Number of days over which to distribute (for uniform/triangle)
#'     - `series`: Custom daily inoculum vector (for series mode, must be length `duration`)
#' @param onset Day of disease onset (used as default start_offset for simple mode).
#' @param duration Total number of simulation days.
#'
#' @returns A numeric vector of length `duration` with daily primary inoculum amounts.
#'
#' @details
#' **Inoculum modes:**
#'
#' * **impulse**: All inoculum applied on single day (start_offset)
#' * **uniform**: Equal amounts each day for `duration` days starting at start_offset
#' * **triangle**: Triangular distribution (ramping up then down) over `duration` days
#' * **series**: Custom daily amounts provided by user
#'
#' The inoculum represents new infections introduced from outside the system
#' (e.g., from neighboring fields, infected seed, or experimental inoculation).
#'
#' @examplesIf interactive()
#' # Simple scalar:   impulse at onset
#' inoc_simple <- epicrop:::.build_inoculum(
#'   I0 = 10,
#'   onset = 20,
#'   duration = 120
#' )
#' which(inoc_simple > 0)  # Day 20
#'
#' # Uniform distribution
#' inoc_uniform <- epicrop:::.build_inoculum(
#'   I0 = list(
#'     mode = "uniform",
#'     I0 = 100,
#'     start_offset = 20,
#'     duration = 5
#'   ),
#'   onset = 20,
#'   duration = 120
#' )
#' inoc_uniform[18: 25]  # 20 per day for days 20-24
#'
#' # Triangle distribution
#' inoc_tri <- epicrop:::.build_inoculum(
#'   I0 = list(
#'     mode = "triangle",
#'     I0 = 100,
#'     start_offset = 20,
#'     duration = 7
#'   ),
#'   onset = 20,
#'   duration = 120
#' )
#' plot(18:27, inoc_tri[18:27], type = "h",
#'      xlab = "Day", ylab = "Inoculum",
#'      main = "Triangular Inoculum")
#'
#' # Custom series
#' custom_inoc <- c(1, 2, 5, 10, 5, 2, 1)
#' inoc_series <- epicrop:::.build_inoculum(
#'   I0 = list(
#'     mode = "series",
#'     series = c(rep(0, 19), custom_inoc, rep(0, 94))
#'   ),
#'   onset = 20,
#'   duration = 120
#' )
#'
#' @author Adam H.  Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @dev
.build_inoculum <- function(I0, onset, duration) {
  # Simple mode: I0 is a scalar (impulse at onset)
  if (!is.list(I0)) {
    Iprim <- numeric(duration)

    if (onset >= 1 && onset <= duration) {
      Iprim[onset] <- I0
      cli::cli_inform(
        "i" = "Inoculum:  {.val {I0}} at day {.val {onset}} (impulse)"
      )
    } else {
      cli::cli_warn(c(
        "!" = "{.var onset} = {.val {onset}} outside simulation window",
        "i" = "No inoculum applied"
      ))
    }

    return(Iprim)
  }

  # Advanced mode: I0 is a list
  mode <- match.arg(I0$mode, c("impulse", "uniform", "triangle", "series"))

  # Use onset as default start_offset if not provided
  start_offset <- if (!is.null(I0$start_offset)) I0$start_offset else onset

  # Validate start_offset
  if (start_offset < 1 || start_offset > duration) {
    cli::cli_warn(c(
      "!" = "{.var start_offset} = {.val {start_offset}} outside simulation window",
      "i" = "No inoculum applied"
    ))
    return(numeric(duration))
  }

  Iprim <- numeric(duration)

  if (mode == "impulse") {
    Iprim[start_offset] <- I0$I0
    cli::cli_inform(
      "i" = "Inoculum: {.val {I0$I0}} at day {.val {start_offset}} (impulse)"
    )
  } else if (mode == "uniform") {
    L <- max(1, I0$duration)
    idx <- start_offset:(start_offset + L - 1)
    idx <- idx[idx >= 1 & idx <= duration]

    if (length(idx) > 0) {
      Iprim[idx] <- I0$I0 / length(idx)
      cli::cli_inform(
        "i" = "Inoculum: {.val {I0$I0}} uniform, days {.val {min(idx)}}-{.val {max(idx)}}"
      )
    }
  } else if (mode == "triangle") {
    L <- max(3, I0$duration)
    idx <- start_offset:(start_offset + L - 1)
    idx <- idx[idx >= 1 & idx <= duration]

    if (length(idx) > 0) {
      half <- ceiling(length(idx) / 2)
      wts <- c(seq_len(half), rev(seq_len(length(idx) - half)))
      Iprim[idx] <- I0$I0 * (wts / sum(wts))
      cli::cli_inform(
        "i" = "Inoculum: {.val {I0$I0}} triangular, days {.val {min(idx)}}-{.val {max(idx)}}"
      )
    }
  } else if (mode == "series") {
    if (is.null(I0$series)) {
      cli::cli_abort(c(
        "{.var I0$series} required for series mode",
        "x" = "Got {.val NULL}"
      ))
    }

    if (length(I0$series) != duration) {
      cli::cli_abort(c(
        "{.var I0$series} length must equal {.var duration}",
        "x" = "Got {.val {length(I0$series)}}, expected {.val {duration}}"
      ))
    }

    Iprim <- I0$series
    total <- sum(Iprim)
    cli::cli_inform(
      "i" = "Inoculum: custom series, total = {.val {round(total, 2)}}"
    )
  }

  Iprim
}
