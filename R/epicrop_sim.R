#' Susceptible-Exposed-Infectious-Removed (SEIR) Model Framework with Lesion Expansion
#'
#' This function is originally used by specific disease models in
#'  \sQuote{EPIRICE} and \sQuote{EPIWHEAT} to model disease intensity of
#'  several rice diseases and two wheat diseases respectively. Given proper
#'  values it can be used with other pathosystems as well.
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
#' @param emergence Expected date of plant emergence (or transplanting for rice)
#'  entered in `YYYY-MM-DD` format (character).
#'
#' @param onset expected number of days until the onset of disease after
#'  emergence date (day, integer). Described in Table 1 of Savary _et al._
#'  2012.
#'
#' @param duration simulation duration *i. e.*, growing season length (day,
#'  integer). Described in Table 1 of Savary _et al. _ 2012.
#'
#' @param rhlim relative humidity value threshold to decide whether leaves are
#'  wet or not (numeric). Described in Table 1 of Savary _et al._ 2012. Savary
#'  _et al._ 2012 used `90`.
#'
#' @param rainlim rainfall amount (mm) threshold to decide whether leaves are
#'  wet or not (numeric). Described in Table 1 of Savary _et al._ 2012. Savary
#'  _et al._ 2012 used `5`.
#'
#' @param H0 initial number of plant's healthy sites (integer). Described in
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
#'  second being the modifier (bounded by 0 and 1). Described in Tables 1 and 2
#'  of Savary _et al._ 2012.
#'
#' @param RcT modifier for _Rc_ (the basic infection rate corrected for
#'  removals) for temperature as a numeric two-column matrix with the first
#'  column being temperature in °C and the second the modifier (bounded by 0 and
#'  1). Described in Tables 1 and 2 of Savary _et al._ 2012.
#'
#' @param RcOpt potential basic infection rate corrected for removals (numeric).
#'  Described in Table 1 of Savary _et al._ 2012. This value can be modified
#'  to reflect crop cultivar resistance ratings as well, see Kim _et al. _ 2015
#'  for more details.
#'
#' @param p duration of latent period (day, integer or function of temperature).
#'  Described in Table 1 of Savary _et al._ 2012.
#'
#' @param i duration of infectious period (day, integer or function of
#'  temperature). Described in Table 1 of Savary _et al._ 2012.
#'
#' @param Sx maximum number of sites (integer). Described in Table 1 of Savary
#'  _et al._ 2012.
#'
#' @param a aggregation coefficient, values are from 1 to >1 (numeric or
#'  function of DVS). Described in Table 1 of Savary _et al._ 2012 and Table 1
#'  of Savary _et al._ 2015. See further details in **_a_ - Aggregation**
#'  section.
#'
#' @param RRS relative rate of physiological senescence (numeric, vector, or
#'  function of DVS). Described in Table 1 of Savary _et al._ 2012.
#'
#' @param RRG relative rate of growth (numeric, vector, or function of time).
#'  Described in Table 1 of Savary _et al._ 2012.
#'
#' @param RcW Optional modifier for _Rc_ to calculate leaf wetness effect
#'  from 1 if wet to 0 if dry.  This is a numeric two-column matrix with the
#'  first column being hours (0 to 24) and the second the modifier (bounded by 0
#'  and 1). Used when `simple_wetness = FALSE` (hourly/derived wetness). If
#'  `NULL` (default), a linear relationship is used where 0 hours wet = 0 and
#'  24 hours wet = 1. Ignored if `simple_wetness = TRUE`.
#'
#' @param RRLEX Rate of lesion expansion (scalar, vector, or function). Allows
#'  healthy sites to be directly converted to infectious sites without passing
#'  through latent stage. Set to 0 (default) for standard SEIR behavior.
#'
#' @param simple_wetness Logical.  If `TRUE` (default), uses binary wetness
#'  calculation (0 or 1) based on `rhlim` and `rainlim` thresholds. If `FALSE`,
#'  uses hourly wetness duration calculation (requires TMIN, TMAX, LAT, DOY in
#'  `wth`) and applies `RcW` curve.
#'
#' @param age_driver Character. Use "day" (default, 0-indexed days from
#'  emergence) or "dvs" (development stage 0-2) for age-based modifiers in
#'  `RcA`. When "dvs", `thermal_time` parameter is required.
#'
#' @param thermal_time List with `base_temp` (°C) and `gdd_to_maturity`
#'  (°C·days). Only required when `age_driver = "dvs"`. Determines how growing
#'  degree days are accumulated and converted to development stage (DVS).
#'
#' @param steps_per_day Integer. Number of sub-daily time steps for numerical
#'  integration. Default is 1 (daily steps). Values of 2, 4, or 8 can improve
#'  accuracy for stiff systems at cost of computation time.  Should be a power
#'  of 2.
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
#'   \item{rlex}{Rate of lesion expansion (healthy sites converted to infectious
#'    via direct expansion on day "x")}
#'   \item{rtransfer}{Rate of transfer from latent to infectious sites on day
#'    "x"}
#'   \item{rremoved}{Rate of removal from infectious to removed sites on day
#'    "x"}
#'   \item{rgrowth}{Rate of growth of healthy sites on day "x"}
#'   \item{rsenesced}{Rate of senescence of healthy sites on day "x"}
#'   \item{diseased}{Number of diseased (latent + infectious + removed) sites on
#'    day "x"}
#'   \item{intensity}{Proportion of diseased sites (latent + infectious) per
#'    total living sites (excludes removed and senesced) on day "x"}
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
#' Sparks, A. H., P. D. Esker, M. Bates, W. Dall' Acqua, Z. Guo, V. Segovia,
#' S.D. Silwal, S. Tolos, and K. A. Garrett, 2008. Ecology and Epidemiology in
#' R: Disease Progress over Time. *The Plant Health Instructor*.
#' \doi{10.1094/PHI-A-2008-0129-02}.
#'
#' Madden, L. V., G. Hughes, and F. van den Bosch. 2007. *The Study of Plant
#' Disease Epidemics*. American Phytopathological Society, St. Paul, MN.
#' \doi{10.1094/9780890545058}.
#'
#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario,
#' J. Modeling and mapping potential epidemics of rice diseases globally.
#' *Crop Protection*, Volume 34, 2012, Pages 6-17, ISSN 0261-2194
#' \doi{10.1016/j.cropro.2011.11.009}.
#'
#' @section _a_ -- Aggregation Details:
#'
#' When _a_ is set to `1` the assumption is that that there is no disease
#' aggregation with new infections occurring at random among the healthy sites.
#' When _a_ is greater than `1` there is aggregation in the disease occurrence,
#' the pathogen is unable to access the entire population of healthy sites,
#' which results in disease aggregation.  Refer to Savary _et al._ (2012) for
#' greater detail.
#'
#' @section TMIN/TMAX Details:
#'
#' If `simple_wetness` is set to `FALSE`, the function will use the `TMIN` and
#'  `TMAX` columns in the `wth` object to calculate the leaf wetness value for
#'  24 hours of the day using the `rhlim` and `rainlim` values and then uses
#'  `RcW` to calculate a value between 0 and 1 for the whole day. When
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
#' Disease-specific helper functions:
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
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com} from original by
#'   Robert J. Hijmans, Rene Pangga and Jorrel Aunario.
#'
#' @autoglobal
#' @export
epicrop_sim <- function(
  wth,
  emergence,
  onset,
  duration,
  rhlim = 90,
  rainlim = 5,
  H0,
  I0,
  RcA,
  RcT,
  RcOpt,
  p,
  i,
  Sx,
  a = 1,
  RRS,
  RRG,
  RcW = NULL,
  RRLEX = 0,
  simple_wetness = TRUE,
  age_driver = "day",
  thermal_time = list(base_temp = 0, gdd_to_maturity = 1100),
  steps_per_day = 1L
) {
  .validate_all_inputs(
    wth = wth,
    emergence = emergence,
    onset = onset,
    duration = duration,
    rhlim = rhlim,
    rainlim = rainlim,
    H0 = H0,
    I0 = I0,
    RcOpt = RcOpt,
    Sx = Sx,
    a = a,
    steps_per_day = steps_per_day,
    simple_wetness = simple_wetness,
    age_driver = age_driver,
    RcW = RcW
  )

  # Prepare weather data
  wth <- data.table::copy(wth)
  wth <- .prepare_weather_data(wth, duration)

  # Determine simulation window
  run_info <- .determine_run_window(wth, emergence, duration)
  w <- run_info$wth_subset

  # Calculate DVS if needed
  if (age_driver == "dvs") {
    dvs <- .calculate_dvs(w, thermal_time)
    age_vec <- dvs
  } else {
    dvs <- NULL
    age_vec <- seq(0, duration - 1)
  }

  # Prepare daily modifiers
  modifiers <- .prepare_modifiers(
    w = w,
    age_vec = age_vec,
    duration = duration,
    RcA = RcA,
    RcT = RcT,
    RcW = RcW,
    RcOpt = RcOpt,
    p = p,
    i = i,
    RRS = RRS,
    a = a,
    RRLEX = RRLEX,
    RRG = RRG,
    rhlim = rhlim,
    rainlim = rainlim,
    simple_wetness = simple_wetness
  )

  # Build inoculum schedule
  Iprim <- .build_inoculum(I0, onset, duration)

  # Call C++ core
  core <- epicrop_core_cpp(
    Rc = modifiers$Rc,
    RRLEX = modifiers$RRLEX,
    RRG = modifiers$RRG,
    RRS = modifiers$RRS,
    p_days = modifiers$p,
    i_days = modifiers$i,
    agg = modifiers$a,
    Iprim = Iprim,
    H0 = H0,
    SMax = Sx,
    steps_per_day = steps_per_day
  )

  result <- .assemble_output(
    core = core,
    run_dates = run_info$run_dates,
    duration = duration,
    wth = wth
  )

  return(result)
}

#' Prepare weather data for simulation
#'
#' @description
#' Validates and prepares weather data for use in disease simulation.  Ensures
#' required columns exist, handles missing values appropriately, and orders
#' data chronologically.
#'
#' @param wth A data.frame or data.table containing weather data with columns:
#'   * `YYYYMMDD` - Date in YYYYMMDD format
#'   * `TEMP` - Daily temperature (required, no NAs allowed)
#'   * `RAIN` - Daily rainfall (NAs treated as 0)
#'   * `RHUM` - Relative humidity (optional, NAs allowed)
#' @param duration Integer.  Number of days for the simulation period.
#'
#' @return A data.tablewith validated and ordered weather data, ensuring
#'   `RHUM` column exists (filled with NA if not provided).
#'
#' @details
#' The function performs the following operations:
#' * Converts input to data.table if necessary
#' * Adds `RHUM` column if missing (filled with NA)
#' * Validates that `TEMP` contains no missing values
#' * Replaces NA values in `RAIN` with 0 (assumes no rainfall)
#' * Orders data by `YYYYMMDD` in ascending order
#'
#' @dev
#' @autoglobal
#'
.prepare_weather_data <- function(wth, duration) {
  if (!rlang::inherits_any(wth, "data.table")) {
    wth <- data.table::as.data.table(wth)
  }

  # Add RHUM if missing
  if (!"RHUM" %in% names(wth)) {
    wth[, RHUM := NA_real_]
    cli::cli_inform(
      call = rlang::caller_env(),
      c("i" = "{.var RHUM} not found, using {.val NA}")
    )
  }

  # Check for NAs in temperature
  if (anyNA(wth$TEMP)) {
    cli::cli_abort(
      call = rlang::caller_env(),
      "{.var TEMP} contains {.val NA} values"
    )
  }

  # Handle NAs in rainfall
  if (anyNA(wth$RAIN)) {
    cli::cli_warn(
      call = rlang::caller_env(),
      "{.var RAIN} contains {.val NA}, treating as 0"
    )
    wth[is.na(RAIN), RAIN := 0]
  }

  data.table::setorderv(wth, "YYYYMMDD")
  return(wth)
}

#' Determine simulation window from weather data
#'
#' Extracts the subset of weather data needed for the simulation based on the
#' emergence date and simulation duration.  Validates that sufficient weather
#' data is available for the requested time window.
#'
#' @param wth A data.table containing weather data with daily time-step.   Must
#'   have a column `YYYYMMDD` of class `Date` containing the date for each
#'   observation.  This is typically an object of class `epicrop.wth` created
#'   by [get_wth()] or [format_wth()].
#' @param emergence Character string or Date object specifying the crop
#'   emergence date in `YYYY-MM-DD` format.  If a vector is provided, only the
#'   first element is used (with a warning).
#' @param duration Integer specifying the number of days to simulate from the
#'   emergence date.  Must be a positive integer, typically >= 50 days for
#'   meaningful disease progression.
#'
#' @returns A list with two elements:
#'   \describe{
#'     \item{wth_subset}{A data.table subset of the input weather data
#'       containing exactly `duration` rows, starting from the emergence date.
#'       This is the weather data that will be used for the simulation.}
#'     \item{run_dates}{A Date vector of length `duration` containing the
#'       sequence of dates for the simulation window, starting from the
#'       emergence date.}
#'   }
#'
#' @details
#' This function performs several operations:
#' \enumerate{
#'   \item Converts the emergence date to Date class using [.check_date()]
#'   \item Generates a sequence of dates from emergence to emergence +
#'      duration - 1
#'   \item Ensures the weather data's YYYYMMDD column is Date class for matching
#'   \item Extracts rows from the weather data that match the simulation dates
#'   \item Validates that the extracted data has exactly `duration` rows
#' }
#'
#' If multiple emergence dates are provided, only the first is used and a
#' warning is issued.
#'
#' If insufficient weather data is available (the extracted subset has fewer
#' than `duration` rows), an informative error is thrown showing:
#' \itemize{
#'   \item The requested number of days
#'   \item The actual number of days available
#'   \item The requested date window
#'   \item The available date range in the weather data
#' }
#'
#' @section Side Effects:
#' \itemize{
#'   \item Prints an informative message showing the emergence date
#'   \item May modify the input `wth` data.table in place to convert YYYYMMDD
#'     to Date class if it isn't already
#' }
#'
#' @examples
#' \dontrun{
#' # This is an internal function not meant to be called directly
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#'
#' # Extract 120-day window starting July 1
#' run_info <- epicrop:::.determine_run_window(
#'   wth = wth,
#'   emergence = "2000-07-01",
#'   duration = 120
#' )
#'
#' # Check the results
#' nrow(run_info$wth_subset)  # 120
#' length(run_info$run_dates)  # 120
#' range(run_info$run_dates)  # 2000-07-01 to 2000-10-28
#' }
#'
#' @seealso
#' \itemize{
#'   \item [.check_date()] for date parsing and validation
#'   \item [get_wth()] for fetching weather data from NASA POWER
#'   \item [format_wth()] for formatting user-supplied weather data
#'   \item [epicrop_sim()] which calls this function internally
#' }
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @keywords internal
#' @dev

.determine_run_window <- function(wth, emergence, duration) {
  em_date <- .check_date(emergence)
  if (length(em_date) > 1L) {
    # Use this function's environment (or omit `.envir` entirely)
    cli::cli_warn(
      "Only one day is used for emergence. Only the first value will be used."
    )
    em_date <- em_date[[1L]]
  }
  em_date <- as.Date(em_date)
  dates <- seq(from = em_date, by = "day", length.out = duration)

  # Convert a local view for matching without touching parent
  col <- wth[["YYYYMMDD"]]

  # Ensure YYYYMMDD is Date class for matching
  if (!inherits(col, "Date")) {
    col <- as.Date(col)
  }

  idx <- col %in% dates
  w <- wth[idx]

  if (nrow(w) != duration) {
    cli::cli_abort(c(
      "Insufficient weather data for simulation window",
      "x" = "Need {.val {duration}} days, have {.val {nrow(w)}}",
      "i" = "Window: {.val {min(dates)}} to {.val {max(dates)}}",
      "i" = "Available: {.val {min(col)}} to {.val {max(col)}}"
    ))
  }

  list(wth_subset = w, run_dates = dates)
}

#' Calculate development stage (DVS)
#'
#' @description
#' Computes the crop development stage based on accumulated thermal time
#' (growing degree days). DVS ranges from 0 (emergence) to 2 (physiological
#' maturity).
#'
#' @param w A data.table containing weather data with a `TEMP` column.
#' @param thermal_time A list containing:
#'   * `base_temp` - Base temperature for degree day calculation
#'   * `gdd_to_maturity` - Total growing degree days required to reach maturity
#'
#' @return Numeric vector of daily DVS values, ranging from 0 to 2, where:
#'   * 0 = emergence
#'   * 1 = flowering/anthesis
#'   * 2 = physiological maturity
#'
#' @details
#' Growing degree days (GDD) are calculated as the cumulative sum of daily
#' thermal time, where daily thermal time = max(TEMP - base_temp, 0).
#' DVS is then calculated as 2 * GDD / gdd_to_maturity, capped at 2.
#'
#' @dev
#' @autoglobal
#'
.calculate_dvs <- function(w, thermal_time) {
  gdd <- cumsum(pmax(w$TEMP - thermal_time$base_temp, 0))
  dvs <- pmin(2, 2 * gdd / thermal_time$gdd_to_maturity)
  return(dvs)
}

#' Prepare daily modifiers for simulation
#'
#' @description
#' Interpolates and calculates all daily modifier values needed for disease
#' progression simulation, including age, temperature, wetness, and other
#' crop-specific parameters.
#'
#' @param w A data.tablecontaining weather data with `TEMP`, `RHUM`, and
#'   `RAIN` columns.
#' @param age_vec Numeric vector of crop ages (in days or physiological time).
#' @param duration Integer. Total number of simulation days.
#' @param RcA Numeric vector or data.frame defining the age correction factor.
#' @param RcT Numeric vector or data.frame defining the temperature correction
#'   factor.
#' @param RcW Numeric vector, data.frame, or NULL defining the wetness
#'   correction factor.  If NULL and not using simple_wetness, linear scaling is
#'   applied.
#' @param RcOpt Numeric scalar.  Optimal infection rate.
#' @param p Numeric vector or data.frame defining the probability of
#'   successful infection.
#' @param i Numeric vector or data.frame defining the infection efficiency.
#' @param RRS Numeric vector or data.frame defining the relative rate of
#'   senescence.
#' @param a Numeric vector or data.frame defining the aggregation parameter.
#' @param RRLEX Numeric vector or data.frame defining the relative rate of
#'   latent expansion.
#' @param RRG Numeric vector or data.frame defining the relative rate of growth.
#' @param rhlim Numeric scalar. Relative humidity threshold (%) for leaf
#'  wetness.
#' @param rainlim Numeric scalar.  Rainfall threshold (mm) for leaf wetness.
#' @param simple_wetness Logical. If TRUE, use binary wetness (wet/dry). If
#'   FALSE, calculate hourly leaf wetness duration.
#'
#' @return A named list containing interpolated daily values:
#'   * `Rc` - Combined correction factor (RcOpt * RcA * RcT * RcW)
#'   * `RRLEX` - Relative rate of latent expansion
#'   * `RRG` - Relative rate of growth
#'   * `RRS` - Relative rate of senescence
#'   * `p` - Probability of successful infection
#'   * `i` - Infection efficiency
#'   * `a` - Aggregation parameter
#'
#' @details
#' For simple wetness, a day is considered wet if either:
#' * Relative humidity >= rhlim, OR
#' * Rainfall >= rainlim
#'
#' For complex wetness, hourly leaf wetness duration is calculated and either
#' interpolated using RcW or linearly scaled (0-24 hours -> 0-1).
#'
#' @dev
#' @autoglobal
#'
.prepare_modifiers <- function(
  w,
  age_vec,
  duration,
  RcA,
  RcT,
  RcW,
  RcOpt,
  p,
  i,
  RRS,
  a,
  RRLEX,
  RRG,
  rhlim,
  rainlim,
  simple_wetness
) {
  # Interpolate age modifier
  RcA_daily <- .interp(RcA, age_vec)

  # Interpolate temperature modifier
  RcT_daily <- .interp(RcT, w$TEMP)

  # Calculate wetness modifier
  if (simple_wetness) {
    # Binary wetness
    rhum_wet <- !is.na(w$RHUM) & (w$RHUM >= rhlim)
    rain_wet <- w$RAIN >= rainlim
    RcW_daily <- as.numeric(rhum_wet | rain_wet)
  } else {
    # Complex wetness with hourly calculation
    wet_hours <- .calculate_leaf_wetness(
      wth = w,
      rhlim = rhlim,
      rainlim = rainlim,
      weighted_wetness = FALSE
    )

    if (!is.null(RcW)) {
      RcW_daily <- .interp(RcW, wet_hours)
    } else {
      # Linear:   0 hours = 0, 24 hours = 1
      RcW_daily <- wet_hours / 24.0
    }
  }

  # Combine into Rc
  Rc_daily <- RcOpt * RcA_daily * RcT_daily * RcW_daily

  # Interpolate other parameters
  p_daily <- .interp(p, seq_len(duration))
  i_daily <- .interp(i, seq_len(duration))
  RRS_daily <- .interp(RRS, age_vec)
  a_daily <- .interp(a, age_vec)
  RRLEX_daily <- .interp(RRLEX, seq_len(duration))
  RRG_daily <- .interp(RRG, seq_len(duration))

  return(list(
    Rc = Rc_daily,
    RRLEX = RRLEX_daily,
    RRG = RRG_daily,
    RRS = RRS_daily,
    p = p_daily,
    i = i_daily,
    a = a_daily
  ))
}

#' Build inoculum schedule
#'
#' @description
#' Creates a vector of daily inoculum values for the simulation period based
#' on the specified inoculum mode and parameters.
#'
#' @param I0 Either a numeric scalar (simple impulse mode) or a list containing:
#'   * `mode` - Character:  "impulse", "uniform", "triangle", or "series"
#'   * `I0` - Numeric: total inoculum amount (not used for "series" mode)
#'   * `start_offset` - Integer: day to begin inoculum (defaults to `onset`)
#'   * `duration` - Integer: number of days for distribution (for "uniform" and
#'     "triangle" modes)
#'   * `series` - Numeric vector: custom daily inoculum values (for
#'   "series" mode)
#' @param onset Integer. Default day for inoculum introduction if `start_offset`
#'   not specified in list mode.
#' @param duration Integer. Total number of simulation days.
#'
#' @return Numeric vector of length `duration` containing daily inoculum
#' amounts.
#'
#' @details
#' **Inoculum modes:**
#' * **impulse**: All inoculum applied on a single day
#' * **uniform**: Inoculum distributed evenly over specified duration
#' * **triangle**: Inoculum distributed with triangular weighting (peak at
#'                 midpoint)
#' * **series**: Custom daily values provided by user
#'
#' If `start_offset` or `onset` falls outside \[1, duration\], a warning is
#'  issued and no inoculum is applied (returns vector of zeros).
#'
#' @dev
#' @autoglobal
.build_inoculum <- function(I0, onset, duration) {
  # Simple mode: I0 is a scalar (impulse at onset)
  if (!is.list(I0)) {
    Iprim <- numeric(duration)

    if (onset >= 1 && onset <= duration) {
      Iprim[onset] <- I0
    } else {
      cli::cli_warn(
        c(
          call = rlang::caller_env(),
          "!" = "{.var onset} = {.val {onset}} outside simulation window",
          "i" = "No inoculum applied"
        )
      )
    }

    return(Iprim)
  }

  # Advanced mode: I0 is a list
  mode <- match.arg(I0$mode, c("impulse", "uniform", "triangle", "series"))

  # Use onset as default start_offset if not provided
  start_offset <- if (!is.null(I0$start_offset)) I0$start_offset else onset

  # Validate start_offset
  if (start_offset < 1 || start_offset > duration) {
    cli::cli_warn(
      c(
        call = rlang::caller_env(),
        "!" = "{.var start_offset} = {.val {start_offset}} outside simulation
          window",
        "i" = "No inoculum applied"
      ),
      .envir = parent.frame()
    )
    return(numeric(duration))
  }

  Iprim <- numeric(duration)

  if (mode == "impulse") {
    Iprim[start_offset] <- I0$I0
  } else if (mode == "uniform") {
    L <- max(1, I0$duration)
    idx <- start_offset:(start_offset + L - 1)
    idx <- idx[idx >= 1 & idx <= duration]

    if (length(idx) > 0) {
      Iprim[idx] <- I0$I0 / length(idx)
    }
  } else if (mode == "triangle") {
    L <- max(3, I0$duration)
    idx <- start_offset:(start_offset + L - 1)
    idx <- idx[idx >= 1 & idx <= duration]

    if (length(idx) > 0) {
      half <- ceiling(length(idx) / 2)
      wts <- c(seq_len(half), rev(seq_len(length(idx) - half)))
      Iprim[idx] <- I0$I0 * (wts / sum(wts))
    }
  } else if (mode == "series") {
    if (is.null(I0$series)) {
      cli::cli_abort(
        c(
          call = rlang::caller_env(),
          "{.var I0$series} required for series mode",
          "x" = "Got {.val NULL}"
        ),
        .envir = parent.frame()
      )
    }

    if (length(I0$series) != duration) {
      cli::cli_abort(
        c(
          call = rlang::caller_env(),
          "{.var I0$series} length must equal {.var duration}",
          "x" = "Got {.val {length(I0$series)}}, expected {.val {duration}}"
        ),
        .envir = parent.frame()
      )
    }

    Iprim <- I0$series
    total <- sum(Iprim)
  }

  return(Iprim)
}

#' Assemble output data.table
#'
#' @description
#' Constructs the final simulation output data.table with all daily state
#' variables, dates, and calculated summary statistics.
#'
#' @param core A list containing daily vectors of simulation state variables:
#'   * `sites` - Number of infection sites
#'   * `latent` - Latent (infected but not infectious) sites
#'   * `infectious` - Infectious sites
#'   * `removed` - Removed sites
#'   * `senesced` - Senesced sites
#'   * `rateinf` - Rate of infection
#'   * `rlex` - Rate of latent expansion
#'   * `rtransfer` - Rate of transfer (latent to infectious)
#'   * `rremoved` - Rate of removal
#'   * `rgrowth` - Rate of growth
#'   * `rsenesced` - Rate of senescence
#'   * `diseased` - Total diseased sites
#'   * `intensity` - Disease intensity
#' @param run_dates A Date vector of simulation dates.
#' @param duration Integer. Number of simulation days.
#' @param wth A data.table containing weather data with optional `LAT` and
#'   `LON` columns for location coordinates.
#'
#' @return A data.tableof class `c("epicrop.sim", "data.table",
#' "data.frame")`
#'   containing:
#'   * `simday` - Sequential day number (1 to duration)
#'   * `dates` - Calendar dates
#'   * All state variables from `core`
#'   * `lat` and `lon` columns if available in weather data
#'   * `AUDPC` attribute containing Area Under Disease Progress Curve
#'
#' @details
#' The AUDPC (Area Under Disease Progress Curve) is calculated from disease
#' intensity and stored as an attribute of the returned data.table.
#'
#' @dev
#' @autoglobal
#'
.assemble_output <- function(core, run_dates, duration, wth) {
  # Build output data.table
  out <- data.table::data.table(
    simday = seq_len(duration),
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

  # Add lat/lon if available
  if ("LAT" %in% names(wth)) {
    out[, lat := wth$LAT[1]]
  }
  if ("LON" %in% names(wth)) {
    out[, lon := wth$LON[1]]
  }

  # Calculate AUDPC and attach as attribute
  audpc_value <- .calculate_audpc(out$intensity)
  data.table::setattr(out, "AUDPC", audpc_value)

  # Set class
  data.table::setattr(
    out,
    "class",
    c("epicrop.sim", "data.table", "data.frame")
  )

  return(out[])
}
