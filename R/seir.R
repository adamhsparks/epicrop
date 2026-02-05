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
#' @param onset expected number of days until the onset of disease after
#'  emergence date (day, integer). Described in Table 1 of Savary _et al._
#'  2012.
#' @param duration simulation duration *i. e.*, growing season length (day,
#'  integer). Described in Table 1 of Savary _et al. _ 2012.
#' @param H0 initial number of plant's healthy sites (integer). Described in
#'  Table 1 of Savary _et al._ 2012.
#' @param I0 initial number of infective sites (scalar integer). Described in
#'  Table 1 of Savary _et al._ 2012. I0 is injected as new infections (become
#'  infectious after `p` days).
#' @param RcA modifier for _Rc_ (the basic infection rate corrected for
#'  removals) for crop age as a numeric two-column matrix with the first column
#'  being the crop age in days (0 to `duration-1`) or DVS (0 to 2) and the
#'  second being the modifier (bounded by 0 and 1). Described in Tables 1 and 2
#'  of Savary _et al._ 2012.
#' @param RcT modifier for _Rc_ (the basic infection rate corrected for
#'  removals) for temperature as a numeric two-column matrix with the first
#'  column being temperature in °C and the second the modifier (bounded by 0 and
#'  1). Described in Tables 1 and 2 of Savary _et al._ 2012.
#' @param RcOpt potential basic infection rate corrected for removals (numeric).
#'  Described in Table 1 of Savary _et al._ 2012. This value can be modified
#'  to reflect crop cultivar resistance ratings as well, see Kim _et al. _ 2015
#'  for more details.
#' @param p duration of latent period (day, integer or function of temperature).
#'  Described in Table 1 of Savary _et al._ 2012.
#' @param i duration of infectious period (day, integer or function of
#'  temperature). Described in Table 1 of Savary _et al._ 2012.
#' @param Sx maximum number of sites (integer). Described in Table 1 of Savary
#'  _et al._ 2012.
#' @param RRS relative rate of physiological senescence (numeric, vector, or
#'  function of DVS). Described in Table 1 of Savary _et al._ 2012.
#' @param RRG relative rate of growth (numeric, vector, or function of time).
#'  Described in Table 1 of Savary _et al._ 2012.
#' @param a aggregation coefficient, values are from 1 to >1 (numeric or
#'  function of DVS). Described in Table 1 of Savary _et al._ 2012 and Table 1
#'  of Savary _et al._ 2015. See further details in **_a_ - Aggregation**
#'  section. Defaults to 1.
#' @param rhlim relative humidity value threshold to decide whether leaves are
#'  wet or not (numeric). Described in Table 1 of Savary _et al._ 2012. Savary
#'  _et al._ 2012 used `90`. Defaults to 90.
#' @param rainlim rainfall amount (mm) threshold to decide whether leaves are
#'  wet or not (numeric). Described in Table 1 of Savary _et al._ 2012. Savary
#'  _et al._ 2012 used `5`. Defaults to 5.
#' @param RcW Optional modifier for _Rc_ to calculate leaf wetness effect
#'  from 1 if wet to 0 if dry.  This is a numeric two-column matrix with the
#'  first column being hours (0 to 24) and the second the modifier (bounded by 0
#'  and 1). Used when `simple_wetness = FALSE` (hourly/derived wetness). If
#'  `NULL` (default), a linear relationship is used where 0 hours wet = 0 and
#'  24 hours wet = 1. Ignored if `simple_wetness = TRUE`.
#' @param RRLEX Rate of lesion expansion (scalar, vector, or function). Allows
#'  healthy sites to be directly converted to infectious sites without passing
#'  through latent stage. Set to 0 (default) for standard SEIR behavior.
#' @param simple_wetness Logical.  If `TRUE` (default), uses binary wetness
#'  calculation (0 or 1) based on `rhlim` and `rainlim` thresholds. If `FALSE`,
#'  uses hourly wetness duration calculation (requires TMIN, TMAX, LAT, DOY in
#'  `wth`) and applies `RcW` curve.
#' @param weighted_wetness Logical. If `TRUE`, compute weighted hourly wetness
#'   (0 to 24); if `FALSE`, count hours where hourly RH >= rhlim (0 to 24).
#'   Only used when `simple_wetness = FALSE`. Defaults to FALSE.
#' @param age_driver Character. Use "day" (default, 0-indexed days from
#'  emergence) or "dvs" (development stage 0-2) for age-based modifiers in
#'  `RcA`. When "dvs", `thermal_time` parameter is required. Defaults to "day".
#' @param thermal_time List with `base_temp` (°C) and `gdd_to_maturity`
#'  (°C·days). Only required when `age_driver = "dvs"`. Determines how growing
#'  degree days are accumulated and converted to development stage (DVS).
#' @param steps_per_day Integer. Number of sub-daily time steps for numerical
#'  integration. Default is 1 (daily steps). Values of 2, 4, or 8 can improve
#'  accuracy for stiff systems at cost of computation time.  Should be a power
#'  of 2. Defaults to 1.
#' @param infection_window Optional list defining DVS-based infection window:
#'   * `dvs_start`: DVS when susceptibility begins
#'   * `dvs_end`: DVS when susceptibility ends
#'   * `base_temp`: Base temperature for GDD (optional, uses thermal_time if present)
#'   * `gdd_to_maturity`: GDD to maturity (optional, uses thermal_time if present)
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
seir <- function(
  wth,
  emergence,
  onset,
  duration,
  H0,
  I0,
  RcA,
  RcT,
  RcOpt,
  p,
  i,
  Sx,
  a = 1,
  rhlim = 90,
  rainlim = 5,
  RRS = NULL,
  RRG = NULL,
  RcW = NULL,
  RRLEX = 0,
  simple_wetness = TRUE,
  weighted_wetness = FALSE,
  age_driver = c("day", "dvs"),
  thermal_time = NULL,
  steps_per_day = 1L,
  infection_window = NULL
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

  age_driver <- rlang::arg_match(age_driver)

  if (!inherits(wth, "epicrop.wth")) {
    cli::cli_abort(
      c(
        "{.arg wth} must be an object of class {.class epicrop.wth}",
        "i" = "Use {.fun get_wth} or {.fun format_wth} to create one"
      )
    )
  }

  # Validate DVS requirements
  if (age_driver == "dvs" && is.null(thermal_time)) {
    cli::cli_abort(
      "{.arg age_driver} = {.val dvs} requires {.arg thermal_time}",
      class = "epicrop_missing_thermal_time"
    )
  }

  if (!is.null(infection_window)) {
    if (!rlang::is_list(infection_window)) {
      cli::cli_abort("{.arg infection_window} must be a list")
    }

    if (
      !rlang::has_name(infection_window, "dvs_start") ||
        !rlang::has_name(infection_window, "dvs_end")
    ) {
      cli::cli_abort(
        "{.arg infection_window} must contain {.field dvs_start} and
        {.field dvs_end}"
      )
    }

    thermal_time <- thermal_time %||%
      list(
        base_temp = infection_window$base_temp %||%
          cli::cli_abort("{.arg infection_window} needs {.field base_temp}"),
        gdd_to_maturity = infection_window$gdd_to_maturity %||%
          cli::cli_abort(
            "{.arg infection_window} needs {.field gdd_to_maturity}"
          )
      )
  }

  # Make working copy
  w <- data.table::copy(wth)

  # Create dates column if not present
  if (!rlang::has_name(w, "dates")) {
    w[, dates := YYYYMMDD]
  }

  # Parse emergence
  emergence_date <- .validate_emergence(emergence)
  emergence_day <- which(w$YYYYMMDD == emergence_date)

  # Check emergence is in weather data FIRST
  if (length(emergence_day) == 0) {
    cli::cli_abort(
      c(
        "Emergence date not found in weather data",
        "x" = "Emergence {.val {emergence_date}} not in weather data range
        {.val {min(w$YYYYMMDD)}} to {.val {max(w$YYYYMMDD)}}"
      )
    )
  }

  # NOW calculate available days (safe because emergence_day has length 1)
  available_days <- nrow(w) - emergence_day + 1

  if (available_days < duration) {
    cli::cli_abort(
      c(
        "Insufficient weather data for simulation duration",
        "x" = "Duration requires {duration} days but only {available_days}
        {cli::qty(available_days)}day{?s} available after emergence",
        "i" = "Emergence date: {.val {emergence_date}}",
        "i" = "Weather data ends: {.val {max(w$YYYYMMDD)}}"
      )
    )
  }

  if (duration < 50) {
    cli::cli_warn(
      c(
        "Short simulation duration may produce unrealistic results",
        "i" = "Duration is {duration} days (< 50 days recommended minimum)",
        "i" = "Consider extending the simulation period for more reliable
        epidemic development"
      )
    )
  }

  # Subset weather to simulation period
  w <- w[emergence_day:(emergence_day + duration - 1)]

  # Calculate age variable
  if (age_driver == "dvs") {
    age_values <- .calculate_dvs(w, thermal_time)
  } else {
    age_values <- seq(0, duration - 1, by = 1)
  }

  # Calculate modifiers
  Rc_age <- .interp(RcA, age_values)
  Rc_temp <- .interp(RcT, w$TEMP)

  # Wetness modifier
  if (simple_wetness) {
    Rc_wet <- as.numeric((w$RHUM >= rhlim) | (w$RAIN >= rainlim))
  } else {
    wetness_hours <- .calculate_leaf_wetness(
      wth = w,
      rhlim = rhlim,
      rainlim = rainlim,
      weighted_wetness = weighted_wetness
    )

    if (is.null(RcW)) {
      Rc_wet <- pmin(1, wetness_hours / 24)
    } else {
      Rc_wet <- .interp(RcW, wetness_hours)
    }
  }

  # Combined infection rate
  Rc <- RcOpt * Rc_age * Rc_temp * Rc_wet

  # Apply infection window
  if (!is.null(infection_window)) {
    window_dvs <- if (age_driver == "dvs") {
      age_values
    } else {
      .calculate_dvs(w, thermal_time)
    }
    susceptible <- (window_dvs >= infection_window$dvs_start) &
      (window_dvs <= infection_window$dvs_end)
    Rc <- Rc * as.numeric(susceptible)
  }

  # Other parameters
  p_days <- if (is.function(p)) p(w$TEMP) else .interp(p, age_values)
  i_days <- if (is.function(i)) i(w$TEMP) else .interp(i, age_values)
  RRG_vals <- if (!is.null(RRG)) {
    .interp(RRG, age_values)
  } else {
    rep(0, duration)
  }
  RRS_vals <- if (is.null(RRS)) {
    rep(0, duration)
  } else if (is.function(RRS)) {
    RRS(age_values)
  } else {
    .interp(RRS, age_values)
  }
  RRLEX_vals <- if (is.null(RRLEX)) {
    rep(0, duration)
  } else {
    .interp(RRLEX, age_values)
  }
  a_vals <- .interp(a, age_values)

  # Inoculum
  inoculum <- numeric(duration)
  inoculum[onset] <- I0

  # Initialize state vectors
  sites <- latent <- infectious <- removed <- senesced <- numeric(duration)
  rateinf <- rlex <- rtransfer <- rremoved <- rgrowth <- rsenesced <- numeric(
    duration
  )

  sites[1] <- H0
  dt <- 1.0 / steps_per_day

  # Main simulation loop

  for (day in 2:duration) {
    S <- sites[day - 1]
    E <- latent[day - 1]
    I <- infectious[day - 1]
    R <- removed[day - 1]
    Sen <- senesced[day - 1]

    daily_inf <- daily_lex <- daily_trans <- daily_rem <- daily_grw <-
      daily_sen <- 0

    for (step in 1:steps_per_day) {
      total <- S + E + I

      # Check for valid Rc value and positive totals
      if (!is.na(Rc[day]) && total > 0 && I > 0) {
        COFR <- 1 - (1 / (1 + a_vals[day] * I / total))
        inf_rate <- Rc[day] * COFR * (I / total) * S
      } else {
        inf_rate <- 0
      }

      inf_rate <- inf_rate + inoculum[day] * dt

      lex_rate <- RRLEX_vals[day] * S
      trans_rate <- E / max(p_days[day], 1)
      rem_rate <- I / max(i_days[day], 1)
      grw_rate <- RRG_vals[day] * S * (1 - total / Sx)
      sen_rate <- RRS_vals[day] * S

      S <- max(0, S + (grw_rate - sen_rate - inf_rate - lex_rate) * dt)
      E <- max(0, E + (inf_rate - trans_rate) * dt)
      I <- max(0, I + (trans_rate + lex_rate - rem_rate) * dt)
      R <- max(0, R + rem_rate * dt)
      Sen <- max(0, Sen + sen_rate * dt)

      daily_inf <- daily_inf + inf_rate * dt
      daily_lex <- daily_lex + lex_rate * dt
      daily_trans <- daily_trans + trans_rate * dt
      daily_rem <- daily_rem + rem_rate * dt
      daily_grw <- daily_grw + grw_rate * dt
      daily_sen <- daily_sen + sen_rate * dt
    }

    sites[day] <- S
    latent[day] <- E
    infectious[day] <- I
    removed[day] <- R
    senesced[day] <- Sen

    rateinf[day] <- daily_inf
    rlex[day] <- daily_lex
    rtransfer[day] <- daily_trans
    rremoved[day] <- daily_rem
    rgrowth[day] <- daily_grw
    rsenesced[day] <- daily_sen
  }

  # Calculate output metrics
  living <- sites + latent + infectious
  intensity <- ifelse(living > 0, (latent + infectious) / living, 0)

  # Build output data.table
  result <- data.table::data.table(
    simday = 1:duration,
    dates = w$dates,
    sites = sites,
    latent = latent,
    infectious = infectious,
    removed = removed,
    senesced = senesced,
    rateinf = rateinf,
    rlex = rlex,
    rtransfer = rtransfer,
    rremoved = rremoved,
    rgrowth = rgrowth,
    rsenesced = rsenesced,
    diseased = latent + infectious + removed,
    intensity = intensity
  )

  # Add spatial columns if available
  if (rlang::has_name(w, "LAT")) {
    result[, lat := w$LAT[1]]
  }
  if (rlang::has_name(w, "LON")) {
    result[, lon := w$LON[1]]
  }

  # Add AUDPC attribute
  data.table::setattr(result, "AUDPC", .calculate_audpc(intensity))
  data.table::setattr(result, "class", c("epicrop.sim", class(result)))

  result[]
}

#' Calculate development stage (DVS)
#'
#' @param w Weather data with TEMP column.
#' @param thermal_time List with base_temp and gdd_to_maturity.
#' @returns Numeric vector of DVS (0-2).
#' @autoglobal
#' @dev
.calculate_dvs <- function(w, thermal_time) {
  gdd <- cumsum(pmax(w$TEMP - thermal_time$base_temp, 0))
  pmin(2, 2 * gdd / thermal_time$gdd_to_maturity)
}
