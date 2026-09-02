#' Susceptible-Exposed-Infectious-Removed (SEIR) Model Framework with Lesion Expansion
#'
#' A dynamical systems model for plant disease epidemiology that tracks disease
#' progression through latent, infectious, and removed stages. Supports flexible
#' aging (day or development stage), lesion expansion, and hourly leaf wetness
#' calculations.
#'
#' This function originated from specific disease models in \sQuote{EPIRICE} and
#' \sQuote{EPIWHEAT} to model disease intensity of several rice diseases and two
#' wheat diseases respectively. Given proper values it can be used with other
#' pathosystems as well.
#'
#' @section Model Structure:
#'
#' The model tracks five site compartments over time:
#' * **Healthy sites (H)**: Susceptible to infection
#' * **Latent sites (E)**: Infected but not yet infectious (duration = `p` days)
#' * **Infectious sites (I)**: Can transmit infection (duration = `i` days)
#' * **Removed sites (R)**: Diseased but no longer infectious
#' * **Senesced sites (S)**: Physiologically dead
#'
#' Daily transitions are driven by:
#' * Infection rate: `Rc × I × (1 - D/Sx)^a` (corrected for crowding & aggregation)
#' * Latency progression: E→I after `p` days
#' * Recovery: I→R after `i` days
#' * Growth: H increases based on `RRG` (if room available)
#' * Senescence: H and R decrease based on `RRS`
#' * Lesion expansion (optional): H→I directly via `RRLEX`
#'
#' @param wth A `data.frame` of class `epicrop.wth` from [get_wth()] or
#'   [format_wth()] containing daily weather data with columns:
#'
#'   **Required Columns:**
#'   | Column | Description |
#'   |--------|-------------|
#'   | YYYYMMDD | Date (ISO 8601: YYYYMMDD) |
#'   | DOY | Day of year (1–366) |
#'   | TEMP | Mean daily temperature (°C) |
#'   | RHUM | Mean daily relative humidity (%) |
#'   | RAIN | Daily rainfall (mm) |
#'
#'   **Optional Columns (for advanced features):**
#'   | Column | Purpose |
#'   |--------|---------|
#'   | TMIN | Minimum daily temperature (°C); required if `simple_wetness = FALSE` |
#'   | TMAX | Maximum daily temperature (°C); required if `simple_wetness = FALSE` |
#'   | LAT | Latitude; required if `simple_wetness = FALSE` |
#'   | LON | Longitude; included in output if present |
#'
#' @param emergence Plant emergence (transplanting for rice) date in `YYYY-MM-DD`
#'   format (character).
#' @param onset Days until disease onset **after emergence date** (1-indexed
#'   integer). Value of 1 means disease begins on emergence day. When comparing
#'   with legacy SEIR(), use `legacy_onset = onset - 1`.
#' @param duration Growing season length / simulation duration (integer days).
#'   Table 1 in Savary *et al.* (2012) discusses typical ranges per crop.
#' @param H0 Initial healthy sites (integer). Starting condition for compartment
#'   H at emergence. Typically 1000 or proportional to plant canopy size.
#' @param I0 Initial infective sites (integer). Inoculum at `onset` day.
#'   Becomes infectious after `p` days.
#' @param RcA Age-based modifier for basic infection rate *Rc* (numeric matrix).
#'   Two-column format:
#'   * Column 1: crop age (days 0 to `duration-1` if `age_driver="day"`; or
#'     DVS 0–2 if `age_driver="dvs"`)
#'   * Column 2: modifier (0–1, bounded)
#'   Tables 1 and 2 in Savary *et al.* (2012).
#' @param RcT Temperature modifier for *Rc* (numeric matrix).
#'   Two-column format:
#'   * Column 1: temperature (°C)
#'   * Column 2: modifier (0–1, bounded)
#'   Tables 1 and 2 in Savary *et al.* (2012).
#' @param RcOpt Potential infection rate (numeric scalar, unbounded).
#'   *Rc* corrected for removals and optimal conditions. Can be modified to
#'   reflect cultivar resistance (see Kim *et al.* 2015 for details).
#' @param p Latent period duration (days). Can be:
#'   * Integer: constant duration across growing season
#'   * Numeric vector: interpolated against age values
#'   * Function: `f(TEMP)` to calculate latent days based on daily temperature
#'   Table 1 in Savary *et al.* (2012).
#' @param i Infectious period duration (days). Same flexibility as `p`.
#'   Table 1 in Savary *et al.* (2012).
#' @param Sx Maximum number of sites per plant/unit canopy (integer).
#'   Carrying capacity constraining growth. Table 1 in Savary *et al.* (2012).
#' @param RRS Relative rate of physiological senescence (numeric, vector, or
#'   function). Causes healthy sites to senesce independent of disease.
#'   Can be:
#'   * Scalar: constant rate (per day)
#'   * Vector or matrix: interpolated against age or DVS
#'   * Function: `f(DVS_or_days)` for dynamic senescence
#'   Table 1 in Savary *et al.* (2012).
#' @param RRG Relative rate of growth of healthy sites (numeric, vector, or
#'   function). Can be:
#'   * Scalar: constant growth rate
#'   * Vector or matrix: interpolated against age or DVS
#'   * Function: `f(time)` for flexible growth curves
#'   Table 1 in Savary *et al.* (2012).
#' @param a Aggregation coefficient (numeric, vector, or function). Controls
#'   spatial aggregation of infection:
#'   * `a = 1`: random infection (binomial); default
#'   * `a > 1`: aggregated infection (negative binomial)
#'   See **_a_ - Aggregation Details** for full explanation. Can be interpolated
#'   against age. Defaults to 1.
#' @param rhlim Relative humidity threshold (%) for leaf wetness (numeric).
#'   If `RHUM >= rhlim` or `RAIN >= rainlim`, leaves are wet. Savary *et al.*
#'   (2012) used 90. Defaults to 90.
#' @param rainlim Rainfall threshold (mm) for leaf wetness (numeric).
#'   If `RAIN >= rainlim`, leaves are wet regardless of RH. Savary *et al.*
#'   (2012) used 5. Defaults to 5.
#' @param RcW Optional modifier curve for leaf wetness effect (numeric matrix).
#'   Only used when `simple_wetness = FALSE` (hourly wetness calculation).
#'   Two-column format:
#'   * Column 1: hours of wetness (0–24)
#'   * Column 2: *Rc* modifier (0–1, bounded)
#'   If `NULL` (default), linear relationship: 0 hrs → modifier 0; 24 hrs → 1.
#'   Ignored if `simple_wetness = TRUE`.
#' @param RRLEX Rate of lesion expansion (numeric, vector, or function).
#'   Allows direct H→I conversion bypassing the latent period. Can be:
#'   * Scalar or vector: constant or age-dependent rate
#'   * Function: `f(TEMP)` or `f(DVS)` for temperature/age-dependent expansion
#'   Set to 0 (default) for standard SEIR (no lesion expansion).
#' @param simple_wetness Logical. Leaf wetness calculation method:
#'   * `TRUE` (default): Binary (0 or 1) based on `rhlim` and `rainlim` thresholds
#'   * `FALSE`: Hourly calculation (requires TMIN, TMAX, LAT, DOY in `wth`);
#'     applies `RcW` curve to 0–24 hour range
#' @param weighted_wetness Logical. Only used when `simple_wetness = FALSE`.
#'   * `FALSE` (default): Count hours where hourly RH ≥ `rhlim` (0–24)
#'   * `TRUE`: Compute weighted average RH over 24-hour day
#'   Defaults to FALSE.
#' @param age_driver Character. Age variable for modifiers:
#'   * `"day"` (default): 0-indexed days from emergence; `RcA` and `RRS` are
#'     functions of days
#'   * `"dvs"`: Development stage (0–2); `RcA` and `RRS` are functions of DVS;
#'     requires `thermal_time` parameter
#' @param thermal_time List with thermal accumulation parameters (required if
#'   `age_driver = "dvs"`). Must contain:
#'   * `base_temp`: Base temperature for GDD accumulation (°C)
#'   * `gdd_to_maturity`: Growing degree-days to reach maturity (°C·days)
#'   Internally computed as DVS = min(2, 2 × GDD / gdd_to_maturity).
#' @param infection_window Optional list restricting disease onset to a DVS
#'   window. Must contain:
#'   * `dvs_start`: DVS when susceptibility begins (0–2)
#'   * `dvs_end`: DVS when susceptibility ends (0–2)
#'   * `base_temp` (optional): Base temperature for GDD (overrides
#'     `thermal_time` if present)
#'   * `gdd_to_maturity` (optional): GDD to maturity (overrides `thermal_time`
#'     if present)
#'   When set, *Rc* is multiplied by 0 outside the window, preventing infection.
#'
#' @returns A [data.table::data.table()] object of class `epicrop.sim` with one
#'   row per day and columns:
#'
#'   **State Variables** (compartment sizes at end of day):
#'   * `simday`: Simulation day (1-indexed; 1 = emergence day)
#'   * `dates`: Calendar date (Date class)
#'   * `sites`: Healthy sites remaining
#'   * `latent`: Latent (E) sites
#'   * `infectious`: Infectious (I) sites
#'   * `removed`: Removed (R) sites
#'   * `senesced`: Senesced sites
#'
#'   **Flow Variables** (daily rate/change):
#'   * `rateinf`: New infections (H→E) on this day
#'   * `rlex`: Lesion expansion (H→I directly) on this day
#'   * `rtransfer`: Transfer from latent to infectious (E→I) on this day
#'   * `rremoved`: Transfer from infectious to removed (I→R) on this day
#'   * `rgrowth`: Growth of healthy sites on this day
#'   * `rsenesced`: Senescence of healthy sites on this day
#'
#'   **Derived Variables**:
#'   * `diseased`: Total diseased sites (latent + infectious + removed)
#'   * `intensity`: Proportion diseased = (latent + infectious) / (sites +
#'     latent + infectious); excludes removed and senesced
#'   * `lat`: Latitude (if provided by `wth`)
#'   * `lon`: Longitude (if provided by `wth`)
#'
#' **Area Under Disease Progress Curve (AUDPC):**
#'   Accessed via `attr(result, "AUDPC")`. Computed from `intensity` using
#'   the trapezoidal rule over all days.
#'
#' @section Mass Balance:
#'
#' At any day, the constraint is:
#' ```
#' sites[t] + latent[t] + infectious[t] + removed[t] + senesced[t]
#'   = H0 + cumulative_growth[t] - cumulative_senescence[t]
#' ```
#' Accumulated growth is `sum(rgrowth)` and accumulated senescence is
#' `sum(rsenesced)`. Small numerical deviations may occur due to floating-point
#' arithmetic.
#'
#' @section _a_ -- Aggregation Details:
#'
#' The infection rate is computed as:
#' ```
#' new_infections = I × Rc × (1 - D/Sx)^a
#' ```
#' where D = diseased sites, Sx = carrying capacity, and `a` is the aggregation
#' coefficient:
#' * **a = 1** (random, binomial): Assumes pathogen can access all healthy sites
#'   equally. Maximum infection rate per unit of disease.
#' * **a > 1** (aggregated, negative binomial): Pathogen cannot access all
#'   healthy sites; infections cluster spatially. Reduces effective infection
#'   rate. Refer to Savary *et al.* (2012) for biological interpretation and
#'   fitting guidance.
#'
#' @section TMIN/TMAX Details:
#'
#' When `simple_wetness = FALSE`, the function calculates hourly leaf wetness
#' for 24 hours using `TMIN`, `TMAX`, `LAT`, and `DOY` in the `wth` object.
#' The `rhlim` and `rainlim` thresholds determine wet hours. A leaf-wetness
#' modifier (`RcW`) converts wet hours to a 0–1 *Rc* scale.
#'
#' When `simple_wetness = TRUE`, this calculation is skipped; leaf wetness is
#' binary (0 or 1) based solely on daily `RHUM` and `RAIN`.
#'
#' @section LAT/LON Details:
#'
#' If the `wth` object contains `LAT` and `LON` columns, they are appended to
#' each row of the output for mapping or spatial analysis. Both must be present
#' to be included. By default, [get_wth()] and [format_wth()] provide these.
#'
#' @section Lesion Expansion Dynamics:
#'
#' When `RRLEX > 0`, the model adds a direct pathway H→I, bypassing latency:
#' ```
#' rlex[t] = RRLEX × I[t] × (1 - D/Sx)
#' ```
#' Lesion expansion is computed in a two-stage update to avoid circular
#' dependency: (1) compute pre-expansion infectious sites, (2) compute rlex,
#' (3) finalize infectious count. This ensures that *Rc*-driven infections and
#' lesion expansion are properly sequenced.
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
#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J. 2012.
#' Modeling and mapping potential epidemics of rice diseases globally.
#' *Crop Protection*, 34, 6–17.
#' \doi{10.1016/j.cropro.2011.11.009}.
#'
#' Kim, K.-S., A. H. Sparks, P. S. Neshev, S. Channing, P. S. Ojiambo, and
#' K. A. Garrett. 2015. A narrative review of the efficacy of management
#' strategies for bacterial spot of citrus. *Frontiers in Plant Science*, 6, 1043.
#'
#' @seealso
#' **Disease-specific helper functions:**
#' * [bacterial_blight()] – Rice bacterial blight
#' * [brown_spot()] – Rice brown spot
#' * [leaf_blast()] – Rice leaf blast
#' * [sheath_blight()] – Rice sheath blight
#' * [tungro()] – Rice tungro
#' * [modified_kim_leaf_blast()] – Cultivar-adjusted leaf blast
#' * [modified_kim_sheath_blight()] – Cultivar-adjusted sheath blight
#' * [leaf_rust()] – Wheat leaf rust
#' * [s_tritici_blotch()] – Wheat septoria tritici blotch
#'
#' **Utilities:**
#' * [get_wth()] – Fetch weather data
#' * [format_wth()] – Format weather data
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com} from original code by
#'   Robert J. Hijmans, Rene Pangga, and Jorrel Aunario.
#' @autoglobal
#' @export
#'
#' @examplesIf interactive()
#' # Create sample weather data
#' wth <- get_wth(
#'   lonlat = c(121.255669, 14.16742),
#'   dates = "2000-06-30",
#'   duration = 120L
#' )
#'
#' # Run SEIR model with standard parameters (rice leaf blast example)
#' res <- seir(
#'   wth = wth,
#'   emergence = "2000-06-30",
#'   onset = 20,      # disease begins on day 20
#'   duration = 120,  # 120-day growing season
#'   H0 = 1000,       # 1000 initial healthy sites
#'   I0 = 1,          # 1 initial infected site
#'   RcA = rbind(c(0, 0.1), c(120, 0.1)),     # age modifier (constant 0.1)
#'   RcT = rbind(c(25, 1), c(30, 1)),         # temp modifier (optimum at 25°C)
#'   RcOpt = 0.5,     # infection rate optimum
#'   p = 5,           # 5-day latent period
#'   i = 10,          # 10-day infectious period
#'   Sx = 10000,      # max 10,000 sites per plant
#'   a = 1,           # random infection (no aggregation)
#'   rhlim = 90,      # wetness threshold at 90% RH
#'   rainlim = 5      # or 5mm rain
#' )
#'
#' # Inspect results
#' head(res)
#' attr(res, "AUDPC")  # area under disease progress curve
#'
#' # Example with lesion expansion (H→I bypass)
#' res_lex <- seir(
#'   wth = wth,
#'   emergence = "2000-06-30",
#'   onset = 20,
#'   duration = 120,
#'   H0 = 1000,
#'   I0 = 1,
#'   RcA = rbind(c(0, 0.1), c(120, 0.1)),
#'   RcT = rbind(c(25, 1), c(30, 1)),
#'   RcOpt = 0.5,
#'   p = 5,
#'   i = 10,
#'   Sx = 10000,
#'   RRLEX = 0.01     # lesion expansion rate (1% per infectious site per day)
#' )
#'
#' # Compare disease intensity with and without lesion expansion
#' plot(res$simday, res$intensity, type = "l", main = "SEIR Model Comparison")
#' lines(res_lex$simday, res_lex$intensity, col = "red", lty = 2)
#' legend("topright", c("Standard SEIR", "With Lesion Expansion"),
#'        lty = c(1, 2), col = c("black", "red"))
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
  RRLEX = NULL,
  simple_wetness = TRUE,
  weighted_wetness = FALSE,
  age_driver = c("day", "dvs"),
  thermal_time = NULL,
  infection_window = NULL
) {
  age_driver <- rlang::arg_match(age_driver)
  if (!inherits(wth, "epicrop.wth")) {
    cli::cli_abort(c(
      "{.arg wth} must be of class {.class epicrop.wth}",
      i = "Use {.fun get_wth} or {.fun format_wth} to create one"
    ))
  }
  if (age_driver == "dvs" && is.null(thermal_time)) {
    cli::cli_abort(
      "{.arg age_driver} = {.val dvs} requires {.arg thermal_time}"
    )
  }
  if (!is.null(infection_window)) {
    if (
      !rlang::is_list(infection_window) ||
        !rlang::has_name(infection_window, "dvs_start") ||
        !rlang::has_name(infection_window, "dvs_end")
    ) {
      cli::cli_abort(
        "{.arg infection_window} must have {.field dvs_start} and
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

  # ---- Weather windowing
  w <- data.table::copy(wth)
  if (!rlang::has_name(w, "dates")) {
    w[, dates := YYYYMMDD]
  }

  emergence_date <- .validate_emergence(emergence)
  i0 <- which(w$YYYYMMDD == emergence_date)
  if (!length(i0)) {
    cli::cli_abort(c(
      "Emergence date not found in weather data",
      x = "Emergence {.val {emergence_date}} not in range
      {.val {min(w$YYYYMMDD)}}--{.val {max(w$YYYYMMDD)}}"
    ))
  }
  if ((nrow(w) - i0 + 1) < duration) {
    cli::cli_abort(
      "Insufficient weather data to run {duration} days after emergence"
    )
  }
  if (duration < 50) {
    cli::cli_warn(c(
      "Short simulation duration may produce unrealistic dynamics",
      i = "Duration is {duration} days (< 50 recommended)"
    ))
  }
  w <- w[i0:(i0 + duration - 1L)]

  # ---- Age driver & modifiers (vectorised)
  age_values <- if (age_driver == "dvs") {
    .calculate_dvs(w, thermal_time)
  } else {
    seq(0L, duration - 1L)
  }
  Rc_age <- .interp(RcA, age_values)
  Rc_temp <- .interp(RcT, w$TEMP)

  if (simple_wetness) {
    Rc_wet <- as.numeric((w$RHUM >= rhlim) | (w$RAIN >= rainlim))
  } else {
    wet_hrs <- .calculate_leaf_wetness(
      w,
      rhlim = rhlim,
      rainlim = rainlim,
      weighted_wetness = weighted_wetness
    )
    Rc_wet <- if (is.null(RcW)) pmin(1, wet_hrs / 24) else .interp(RcW, wet_hrs)
  }
  Rc <- pmax(0, RcOpt) * pmax(0, Rc_age) * pmax(0, Rc_temp) * pmax(0, Rc_wet)

  if (!is.null(infection_window)) {
    dvs <- if (age_driver == "dvs") {
      age_values
    } else {
      .calculate_dvs(w, thermal_time)
    }
    Rc <- Rc *
      as.numeric(
        dvs >= infection_window$dvs_start & dvs <= infection_window$dvs_end
      )
  }

  # ---- Daily drivers (vectorised)
  .as_daily <- function(x) {
    if (is.function(x)) {
      x(w$TEMP)
    } else if (is.matrix(x) || (length(x) > 1)) {
      .interp(x, age_values)
    } else {
      rep(x, duration)
    }
  }
  p_days <- pmax(0, .as_daily(p))
  i_days <- pmax(0, .as_daily(i))
  RRG_vals <- if (!is.null(RRG)) pmax(0, .as_daily(RRG)) else numeric(duration)
  RRS_vals <- if (is.null(RRS)) {
    numeric(duration)
  } else {
    pmax(0, if (is.function(RRS)) RRS(age_values) else .interp(RRS, age_values))
  }
  a_vals <- pmax(1, .interp(a, age_values))

  # ---- Fast path decision for lesion expansion
  lex_on <- FALSE
  RRLEX_vals <- NULL
  if (!is.null(RRLEX)) {
    RRLEX_vals <- pmax(0, .as_daily(RRLEX))
    lex_on <- any(RRLEX_vals > 0)
  }

  # ---- Primary inoculum timing (1-indexed onset)
  Iprim <- numeric(duration)
  onset_idx <- as.integer(onset)
  if (onset_idx < 1L || onset_idx > duration) {
    cli::cli_abort(
      "{.arg onset} out of range for duration={duration} (1-indexed)"
    )
  }
  Iprim[onset_idx] <- I0
  out <- if (lex_on) {
    .seir_core_with_lex(
      duration,
      onset_idx,
      H0,
      Iprim,
      Rc,
      p_days,
      i_days,
      RRG_vals,
      RRS_vals,
      RRLEX_vals,
      a_vals,
      Sx
    )
  } else {
    .seir_core_no_lex(
      duration,
      onset_idx,
      H0,
      Iprim,
      Rc,
      p_days,
      i_days,
      RRG_vals,
      RRS_vals,
      a_vals,
      Sx
    )
  }

  # ---- Attach calendar + spatial, attributes
  out[, `:=`(simday = seq_len(duration), dates = w$dates)]
  if (rlang::has_name(w, "LAT")) {
    out[, lat := w$LAT[1]]
  }
  if (rlang::has_name(w, "LON")) {
    out[, lon := w$LON[1]]
  }

  data.table::setcolorder(
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
      "rlex",
      "rtransfer",
      "rremoved",
      "rgrowth",
      "rsenesced",
      "diseased",
      "intensity",
      intersect(c("lat", "lon"), names(out))
    )
  )
  data.table::setattr(out, "AUDPC", .calculate_audpc(out$intensity))
  data.table::setattr(out, "class", c("epicrop.sim", class(out)))
  return(out[])
}

#' SEIR Core Simulation (No Lesion Expansion)
#'
#' Internal workhorse for SEIR model without lesion expansion. Implements the
#' standard Susceptible-Exposed-Infectious-Removed compartmental model using
#' efficient cumulative sum techniques to avoid O(n²) nested loops.
#'
#' @section Algorithm:
#'
#' Computes daily transitions using lagged cumulative sums to efficiently
#' extract windowed counts (infectious periods are handled via integer delays
#' into history arrays). Day 1 initializes pools; main loop (days 2+) updates
#' compartments based on infection rate, latency exit, and recovery.
#'
#' @param duration Simulation duration (integer days).
#' @param onset_idx 1-indexed day of primary inoculum injection (integer).
#' @param H0 Initial healthy sites (integer).
#' @param Iprim Vector of primary inoculum injection on each day (numeric).
#' @param Rc Daily infection rate (numeric vector, length = duration).
#' @param p_days Daily latent period (numeric vector, length = duration).
#' @param i_days Daily infectious period (numeric vector, length = duration).
#' @param RRG_vals Daily relative growth rate (numeric vector).
#' @param RRS_vals Daily relative senescence rate (numeric vector).
#' @param a_vals Daily aggregation coefficient (numeric vector, ≥ 1).
#' @param Sx Maximum site capacity (integer).
#'
#' @returns A [data.table::data.table()] with columns:
#'   * `sites`, `latent`, `infectious`, `removed`, `senesced`: state vars
#'   * `rateinf`, `rlex`, `rtransfer`, `rremoved`, `rgrowth`, `rsenesced`: flow rates
#'   * `diseased`, `intensity`: derived
#'
#' @details
#' **rlex** is always 0 in this variant (no lesion expansion).
#'
#' **Cumulative Sum Optimization:**
#' Instead of summing over a sliding window (latent/infectious durations),
#' this implementation maintains `cs_lat` and `cs_inf` (cumulative sums of
#' latency inflows and infectious inflows). The count of sites currently
#' infectious is: `cs_inf[t] - cs_inf[t - i_days[t] - 2]`, avoiding O(n)
#' inner loops.
#'
#' @autoglobal
#' @dev
.seir_core_no_lex <- function(
  duration,
  onset_idx,
  H0,
  Iprim,
  Rc,
  p_days,
  i_days,
  RRG_vals,
  RRS_vals,
  a_vals,
  Sx
) {
  sites <- latent <- infectious <- removed <- senesced <- numeric(duration)
  latency <- infectious_flow <- numeric(duration)
  rateinf <- rlex <- rtransfer <- rremoved <- rgrowth <- rsenesced <- numeric(
    duration
  )
  now_latent <- now_infectious <- diseased <- numeric(duration)
  cs_lat <- cs_inf <- numeric(duration)

  # Day 1
  sites[1L] <- H0
  rsenesced[1L] <- RRS_vals[1L] * sites[1L]
  senesced[1L] <- rsenesced[1L]
  rgrowth[1L] <- RRG_vals[1L] * sites[1L] * (1 - (sites[1L] / Sx))
  rateinf[1L] <- if (1L == onset_idx) Iprim[1L] else 0

  # Loop
  for (t in 2:duration) {
    lat_src <- t - floor(p_days[t]) + 2L
    inf_src <- t - floor(i_days[t])

    removed_today <- if (inf_src > 1L) infectious_flow[inf_src - 1L] else 0

    sites[t] <- max(
      0,
      sites[t - 1L] + rgrowth[t - 1L] - rateinf[t - 1L] - rsenesced[t - 1L]
    )
    rsenesced[t] <- removed_today + RRS_vals[t] * sites[t]
    senesced[t] <- senesced[t - 1L] + rsenesced[t]

    latency[t] <- rateinf[t - 1L]
    cs_lat[t] <- cs_lat[t - 1L] + latency[t]

    win_lat0 <- if (lat_src > 2L) cs_lat[lat_src - 2L] else 0
    now_latent[t] <- cs_lat[t] - win_lat0
    rtransfer[t] <- if (lat_src > 1L) latency[lat_src - 1L] else 0

    infectious_flow[t] <- rtransfer[t]
    cs_inf[t] <- cs_inf[t - 1L] + infectious_flow[t]
    win_inf0 <- if (inf_src > 2L) cs_inf[inf_src - 2L] else 0
    now_infectious[t] <- cs_inf[t] - win_inf0

    removed[t] <- cs_inf[t] - now_infectious[t]
    diseased[t] <- now_latent[t] + now_infectious[t] + removed[t]

    total_cur <- sites[t] + diseased[t]
    COFR <- if (total_cur > 0) 1 - (diseased[t] / total_cur) else 1
    COFR <- min(1, max(0, COFR))

    rateinf[t] <- if (t == onset_idx) {
      Iprim[t]
    } else {
      now_infectious[t] * Rc[t] * (COFR^a_vals[t])
    }

    total_sites <- sites[t] + diseased[t]
    rgrowth[t] <- RRG_vals[t] * sites[t] * (1 - (total_sites / Sx))
  }

  intensity <- ifelse(
    (sites + now_latent + now_infectious) > 0,
    (now_latent + now_infectious) / (sites + now_latent + now_infectious),
    0
  )

  data.table::data.table(
    sites = sites,
    latent = now_latent,
    infectious = now_infectious,
    removed = removed,
    senesced = senesced,
    rateinf = rateinf,
    rlex = numeric(duration),
    rtransfer = rtransfer,
    rremoved = c(0, diff(removed)),
    rgrowth = rgrowth,
    rsenesced = rsenesced,
    diseased = diseased,
    intensity = intensity
  )
}

#' SEIR Core Simulation (With Lesion Expansion)
#'
#' Internal workhorse for SEIR model with lesion expansion enabled. Extends
#' the standard SEIR model by adding a direct H→I pathway, allowing healthy
#' sites to become infectious without passing through the latent stage.
#'
#' @section Algorithm:
#'
#' Uses a two-stage update to avoid circular dependency:
#' 1. Compute windowed infectious sites before today's lesion expansion
#' 2. Compute rlex from pre-expansion infectious count and crowding factor
#' 3. Finalize infectious window including today's lesion expansion
#'
#' This ensures that *Rc*-driven infections and lesion expansion are
#' properly sequenced and both respond to the same crowding factor.
#'
#' @param duration Simulation duration (integer days).
#' @param onset_idx 1-indexed day of primary inoculum injection (integer).
#' @param H0 Initial healthy sites (integer).
#' @param Iprim Vector of primary inoculum injection on each day (numeric).
#' @param Rc Daily infection rate (numeric vector, length = duration).
#' @param p_days Daily latent period (numeric vector, length = duration).
#' @param i_days Daily infectious period (numeric vector, length = duration).
#' @param RRG_vals Daily relative growth rate (numeric vector).
#' @param RRS_vals Daily relative senescence rate (numeric vector).
#' @param RRLEX_vals Daily lesion expansion rate (numeric vector).
#' @param a_vals Daily aggregation coefficient (numeric vector, ≥ 1).
#' @param Sx Maximum site capacity (integer).
#'
#' @returns A [data.table::data.table()] with columns:
#'   * `sites`, `latent`, `infectious`, `removed`, `senesced`: state vars
#'   * `rateinf`, `rlex`, `rtransfer`, `rremoved`, `rgrowth`, `rsenesced`: flow rates
#'   * `diseased`, `intensity`: derived
#'
#' @details
#' **Lesion Expansion (rlex):**
#' On day t, computed as:
#' ```
#' rlex[t] = RRLEX_vals[t] × now_infectious[t-from-stage-A] × COFR
#' ```
#' where COFR = 1 - (diseased / total_living). This reduces direct lesion
#' expansion in crowded conditions, similar to *Rc*-driven infection.
#'
#' @autoglobal
#' @dev
.seir_core_with_lex <- function(
  duration,
  onset_idx,
  H0,
  Iprim,
  Rc,
  p_days,
  i_days,
  RRG_vals,
  RRS_vals,
  RRLEX_vals,
  a_vals,
  Sx
) {
  sites <- latent <- infectious <- removed <- senesced <- numeric(duration)
  latency <- infectious_flow <- numeric(duration)
  rateinf <- rlex <- rtransfer <- rremoved <- rgrowth <- rsenesced <- numeric(
    duration
  )
  now_latent <- now_infectious <- diseased <- numeric(duration)
  cs_lat <- cs_inf <- numeric(duration)

  # Day 1
  sites[1L] <- H0
  rsenesced[1L] <- RRS_vals[1L] * sites[1L]
  senesced[1L] <- rsenesced[1L]
  rgrowth[1L] <- RRG_vals[1L] * sites[1L] * (1 - (sites[1L] / Sx))
  rateinf[1L] <- if (1L == onset_idx) Iprim[1L] else 0

  for (t in 2:duration) {
    lat_src <- t - floor(p_days[t]) + 2L
    inf_src <- t - floor(i_days[t])

    removed_today <- if (inf_src > 1L) infectious_flow[inf_src - 1L] else 0

    sites[t] <- max(
      0,
      sites[t - 1L] +
        rgrowth[t - 1L] -
        rateinf[t - 1L] -
        rsenesced[t - 1L] -
        rlex[t - 1L]
    )
    rsenesced[t] <- removed_today + RRS_vals[t] * sites[t]
    senesced[t] <- senesced[t - 1L] + rsenesced[t]

    latency[t] <- rateinf[t - 1L]
    cs_lat[t] <- cs_lat[t - 1L] + latency[t]
    win_lat0 <- if (lat_src > 2L) cs_lat[lat_src - 2L] else 0
    now_latent[t] <- cs_lat[t] - win_lat0

    rtransfer[t] <- if (lat_src > 1L) latency[lat_src - 1L] else 0

    # Stage A: infectious window *before* today's lesion expansion
    inf_wo_lex <- rtransfer[t]
    cs_pre <- cs_inf[t - 1L] + inf_wo_lex
    win_inf0_pre <- if (inf_src > 2L) cs_inf[inf_src - 2L] else 0
    now_inf_pre <- cs_pre - win_inf0_pre

    # COFR from pre-lex states
    removed_pre <- cs_pre - now_inf_pre
    diseased_pre <- now_latent[t] + now_inf_pre + removed_pre
    total_cur <- sites[t] + diseased_pre
    COFR <- if (total_cur > 0) 1 - (diseased_pre / total_cur) else 1
    COFR <- min(1, max(0, COFR))

    # Today's lesion expansion
    rlex[t] <- RRLEX_vals[t] * now_inf_pre * max(0, COFR)

    # Finalize infectious window including lesion expansion
    infectious_flow[t] <- rtransfer[t] + rlex[t]
    cs_inf[t] <- cs_inf[t - 1L] + infectious_flow[t]
    win_inf0 <- if (inf_src > 2L) cs_inf[inf_src - 2L] else 0
    now_infectious[t] <- cs_inf[t] - win_inf0

    removed[t] <- cs_inf[t] - now_infectious[t]
    diseased[t] <- now_latent[t] + now_infectious[t] + removed[t]

    rateinf[t] <- if (t == onset_idx) {
      Iprim[t]
    } else {
      now_infectious[t] * Rc[t] * (COFR^a_vals[t])
    }

    total_sites <- sites[t] + diseased[t]
    rgrowth[t] <- RRG_vals[t] * sites[t] * (1 - (total_sites / Sx))
  }

  intensity <- ifelse(
    (sites + now_latent + now_infectious) > 0,
    (now_latent + now_infectious) / (sites + now_latent + now_infectious),
    0
  )

  data.table::data.table(
    sites = sites,
    latent = now_latent,
    infectious = now_infectious,
    removed = removed,
    senesced = senesced,
    rateinf = rateinf,
    rlex = rlex,
    rtransfer = rtransfer,
    rremoved = c(0, diff(removed)),
    rgrowth = rgrowth,
    rsenesced = rsenesced,
    diseased = diseased,
    intensity = intensity
  )
}

#' Calculate Development Stage (DVS) from Thermal Time
#'
#' Computes growing degree days (GDD) cumulative sum and converts to a
#' normalized development stage (DVS) on scale 0–2 (emergence to maturity).
#'
#' @section DVS Calculation:
#'
#' Development stage is computed as:
#' ```
#' GDD = cumsum(max(TEMP - base_temp, 0))
#' DVS = min(2, 2 × GDD / gdd_to_maturity)
#' ```
#'
#' At maturity (GDD = gdd_to_maturity), DVS reaches 2.
#'
#' @param w Weather data with a `TEMP` column (data.frame or data.table).
#' @param thermal_time List with thermal accumulation parameters:
#'   * `base_temp`: Base temperature for GDD accumulation (°C)
#'   * `gdd_to_maturity`: Cumulative GDD required to reach DVS = 2 (°C·days)
#'
#' @returns Numeric vector of DVS values (0–2), length = nrow(w).
#'
#' @details
#' **Validation:** Checks that `w` is a data.frame or data.table with a `TEMP`
#' column (no NAs), and `thermal_time` is a list with valid numeric fields.
#'
#' @examplesIf interactive()
#' # Create sample weather and thermal parameters
#' w <- data.frame(TEMP = c(15, 20, 25, 30, 25, 20, 15, 10))
#' thermal_time <- list(base_temp = 10, gdd_to_maturity = 300)
#' dvs <- .calculate_dvs(w, thermal_time)
#' head(dvs)
#' # [1] 0.0000000 0.01666667 0.05000000 0.10000000 0.15000000 0.19666667
#'
#' @autoglobal
#' @dev
.calculate_dvs <- function(w, thermal_time) {
  # Validate weather input
  if (is.null(w) || !(is.data.frame(w) || data.table::is.data.table(w))) {
    cli::cli_abort(
      "{.arg w} must be a data.frame-like object with a {.field TEMP} column"
    )
  }
  if (!rlang::has_name(w, "TEMP")) {
    cli::cli_abort("{.arg w} must contain a {.field TEMP} column")
  }
  if (anyNA(w$TEMP)) {
    cli::cli_abort("{.arg w}$TEMP contains NA values")
  }

  # Validate thermal_time
  if (missing(thermal_time) || !rlang::is_list(thermal_time)) {
    cli::cli_abort(
      "{.arg thermal_time} must be a list with {.field base_temp} and
      {.field gdd_to_maturity}"
    )
  }
  if (!rlang::has_name(thermal_time, "base_temp")) {
    cli::cli_abort("{.arg thermal_time} must contain {.field base_temp}")
  }
  if (!rlang::has_name(thermal_time, "gdd_to_maturity")) {
    cli::cli_abort("{.arg thermal_time} must contain {.field gdd_to_maturity}")
  }

  base_temp <- thermal_time$base_temp
  gdd_to_maturity <- thermal_time$gdd_to_maturity

  if (!is.numeric(base_temp) || length(base_temp) != 1L || is.na(base_temp)) {
    cli::cli_abort(
      "{.arg thermal_time$base_temp} must be a single numeric value"
    )
  }
  if (
    !is.numeric(gdd_to_maturity) ||
      length(gdd_to_maturity) != 1L ||
      is.na(gdd_to_maturity) ||
      gdd_to_maturity <= 0
  ) {
    cli::cli_abort(
      "{.arg thermal_time$gdd_to_maturity} must be a single positive numeric
      value"
    )
  }

  gdd <- cumsum(pmax(w$TEMP - base_temp, 0))
  return(pmin(2, 2 * gdd / gdd_to_maturity))
}
