#' Susceptible-Exposed-Infectious-Removed (SEIR) Model Framework with Lesion Expansion
#'
#' A dynamical systems model for plant disease epidemiology that tracks disease
#' progression through latent, infectious, and removed stages. Supports flexible
#' ageing (day or development stage), lesion expansion, and hourly leaf wetness
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
#' * Infection rate: \eqn{R_c \times I \times \left(1 - \frac{D}{S_x}\right)^a}{Rc * I * (1 - D/Sx)^a}
#'    (corrected for crowding & aggregation)
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
#'   | TEMP | Mean daily temperature (\ifelse{latex}{\out{$^\circ$}}{°}.RC) |
#'   | RHUM | Mean daily relative humidity (%) |
#'   | RAIN | Daily rainfall (mm) |
#'
#'   **Optional Columns (for advanced features):**
#'   | Column | Purpose |
#'   |--------|---------|
#'   | TMIN | Minimum daily temperature (\ifelse{latex}{\out{$^\circ$}}{°}.RC); required if `simple_wetness = FALSE` |
#'   | TMAX | Maximum daily temperature (\ifelse{latex}{\out{$^\circ$}}{°}.RC); required if `simple_wetness = FALSE` |
#'   | LAT | Latitude; required if `simple_wetness = FALSE` |
#'   | LON | Longitude; included in output if present |
#'
#' @param emergence Plant emergence (transplanting for rice) date in
#'   `YYYY-MM-DD` format (character).
#' @param onset Days until disease onset **after emergence date** (1-indexed
#'   integer). Value of 1 means disease begins on emergence day. When comparing
#'   with legacy SEIR(), use `legacy_onset = onset - 1`.
#' @param duration Growing season length / simulation duration (integer days).
#'   Table 1 in Savary *et al.* (2012) discusses typical ranges per rice crop.
#'   This value *must be* >2, but a warning is emitted for values <80 days.
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
#'   * Column 1: temperature (\ifelse{latex}{\out{$^\circ$}}{°}.RC)
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
#'   * `TRUE` (default): Binary (0 or 1) based on `rhlim` and `rainlim`
#'     thresholds
#'   * `FALSE`: Hourly calculation (requires TMIN, TMAX, LAT, DOY in `wth`);
#'     applies `RcW` curve to 0–24 hour range
#' @param weighted_wetness Logical. Only used when `simple_wetness = FALSE`.
#'   * `FALSE` (default): Count hours where hourly RH \ifelse{latex}{\out{$\geq$}}{>=} `rhlim` (0–24)
#'   * `TRUE`: Compute weighted average RH over 24-hour day
#'   Defaults to `FALSE`.
#' @param age_driver Character. Age variable for modifiers:
#'   * `"day"` (default): 0-indexed days from emergence; `RcA` and `RRS` are
#'     functions of days
#'   * `"dvs"`: Development stage (0–2); `RcA` and `RRS` are functions of DVS;
#'     requires `thermal_time` parameter
#' @param thermal_time List with thermal accumulation parameters (required if
#'   `age_driver = "dvs"`). Must contain:
#'   * `base_temp`: Base temperature for GDD accumulation (\ifelse{latex}{\out{$^\circ$}}{°}.RC)
#'   * `gdd_to_maturity`: Growing degree-days to reach maturity (\ifelse{latex}{\out{$^\circ$}}{°}.RC·days)
#'   Internally computed as:
#'    \deqn{DVS = \min\left(2,\ 2 \times \frac{\text{GDD}}{\text{gdd to maturity}}\right)}{DVS = min(2, 2 * GDD / gdd_to_maturity)}
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
#' \deqn{
#'   \text{sites}_t + \text{latent}_t + \text{infectious}_t + \text{removed}_t + \text{senesced}_t
#'   = H_0 + \text{growth}_t - \text{senescence}_t
#' }{sites[t] + latent[t] + infectious[t] + removed[t] + senesced[t] = H0 + cumulative_growth[t] - cumulative_senescence[t]}
#' Accumulated growth is `sum(rgrowth)` and accumulated senescence is
#' `sum(rsenesced)`. Small numerical deviations may occur due to floating-point
#' arithmetic.
#'
#' @section _a_ -- Aggregation Details:
#'
#' The infection rate is computed as:
#' \deqn{\text{new infections} = I \times R_c \times \left(1 - \frac{D}{S_x}\right)^a}{new_infections = I * Rc * (1 - D/Sx)^a}
#' where `D` = diseased sites, `Sx` = carrying capacity, and `a` is the
#' aggregation coefficient:
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
#' \deqn{\text{rlex}_t = RRLEX \times I_t \times \left(1 - \frac{D}{S_x}\right)}{rlex[t] = RRLEX * I[t] * (1 - D/Sx)}
#'
#' Lesion expansion is computed in a two-stage update to avoid circular
#' dependency: (1) compute pre-expansion infectious sites, (2) compute rlex,
#' (3) finalise infectious count. This ensures that *Rc*-driven infections and
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
#' *Crop Protection*, 34: 6–17.
#' \doi{10.1016/j.cropro.2011.11.009}
#'
#' Kim, K. H., Cho, J., Lee, Y. H., and Lee, W. S. 2015.
#' Predicting potential epidemics of rice leaf blast and sheath blight in South
#' Korea. *Agricultural and Forest Meteorology*, 203: 191–207.
#' \doi{10.1016/j.agrformet.2015.01.011}
#'
#' Savary, S., Stetkiewicz, S., Brun, F., and Willocquet, L. 2015.
#' Modelling and mapping potential epidemics of wheat diseases.
#' *European Journal of Plant Pathology*, 142: 771–790.
#' \doi{10.1007/s10658-015-0650-7}
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
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}, derived from original
#'   code by Robert J. Hijmans, Rene Pangga, Jorrel Aunario, and Kwang-Hyung
#'   Kim.
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
#'   RcT = rbind(c(25, 1), c(30, 1)),         # temp modifier (optimum at 25C)
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

  if (duration < 80L) {
    if (duration < 2L) {
      cli::cli_abort(
        x = "{.arg duration} is <2 days.",
        i = "For the model to work, duration must be >2 days,
        prefereably >100 days."
      )
    }
    cli::cli_alert_warning(
      "{.arg duration} is <80 days.",
      i = "Most broadacre crops mature in >= 100 days."
    )
  }

  thermal_time <- .resolve_thermal_time(
    age_driver = age_driver,
    thermal_time = thermal_time,
    infection_window = infection_window
  )

  w <- .window_weather(
    wth = wth,
    emergence = emergence,
    duration = duration
  )

  age_values <- .compute_age_values(
    age_driver = age_driver,
    duration = duration,
    w = w,
    thermal_time = thermal_time
  )

  Rc_age <- .interp(RcA, age_values)
  Rc_temp <- .interp(RcT, w$TEMP)
  Rc_wet <- .compute_wetness_modifier(
    w = w,
    simple_wetness = simple_wetness,
    rhlim = rhlim,
    rainlim = rainlim,
    weighted_wetness = weighted_wetness,
    RcW = RcW
  )

  Rc <- pmax(0, RcOpt) * pmax(0, Rc_age) * pmax(0, Rc_temp) * pmax(0, Rc_wet)
  Rc <- .apply_infection_window(
    Rc = Rc,
    infection_window = infection_window,
    age_driver = age_driver,
    age_values = age_values,
    w = w,
    thermal_time = thermal_time
  )

  p_days <- pmax(
    0,
    .expand_daily(
      x = p,
      w = w,
      age_values = age_values,
      duration = duration,
      fn_input = "temp"
    )
  )
  i_days <- pmax(
    0,
    .expand_daily(
      x = i,
      w = w,
      age_values = age_values,
      duration = duration,
      fn_input = "temp"
    )
  )

  RRG_vals <- if (is.null(RRG)) {
    numeric(duration)
  } else {
    pmax(
      0,
      .expand_daily(
        x = RRG,
        w = w,
        age_values = age_values,
        duration = duration,
        fn_input = "temp"
      )
    )
  }

  RRS_vals <- if (is.null(RRS)) {
    numeric(duration)
  } else {
    pmax(
      0,
      .expand_daily(
        x = RRS,
        w = w,
        age_values = age_values,
        duration = duration,
        fn_input = "age"
      )
    )
  }

  a_vals <- pmax(1, .interp(a, age_values))
  lex <- .prepare_lesion_expansion(
    RRLEX = RRLEX,
    w = w,
    age_values = age_values,
    duration = duration
  )
  inoc <- .build_primary_inoculum(
    onset = onset,
    duration = duration,
    I0 = I0
  )

  out <- .run_seir_core(
    lex_on = lex$lex_on,
    duration = duration,
    onset_idx = inoc$onset_idx,
    H0 = H0,
    Iprim = inoc$Iprim,
    Rc = Rc,
    p_days = p_days,
    i_days = i_days,
    RRG_vals = RRG_vals,
    RRS_vals = RRS_vals,
    a_vals = a_vals,
    Sx = Sx,
    RRLEX_vals = lex$RRLEX_vals
  )

  .finalise_output(out = out, w = w, duration = duration)
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
#' into history arrays). Day 1 initialises pools; main loop (days 2+) updates
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
#' @param a_vals Daily aggregation coefficient (numeric vector, \ifelse{latex}{\out{$\geq$}}{>=} 1).
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
#' **Cumulative Sum Optimisation:**
#' Instead of summing over a sliding window (latent/infectious durations),
#' this implementation maintains `cs_lat` and `cs_inf` (cumulative sums of
#' latency inflows and infectious inflows). The count of sites currently
#' infectious is: `cs_inf[t] - cs_inf[t - i_days[t] - 2]`, avoiding O(n)
#' inner loops.
#'
#' @keywords internal
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

  data.table(
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
#' 3. Finalise infectious window including today's lesion expansion
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
#' @param a_vals Daily aggregation coefficient (numeric vector, \ifelse{latex}{\out{$\geq$}}{>=} 1).
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
#' @keywords internal
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

    # Finalise infectious window including lesion expansion
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

  data.table(
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
#' normalised development stage (DVS) on scale 0–2 (emergence to maturity).
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
#'   * `base_temp`: Base temperature for GDD accumulation (\ifelse{latex}{\out{$^\circ$}}{°}.RC)
#'   * `gdd_to_maturity`: Cumulative GDD required to reach DVS = 2 (\ifelse{latex}{\out{$^\circ$}}{°}.RC·days)
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
#' @keywords internal
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

#' Resolve Thermal Time Configuration for SEIR
#'
#' Internal helper that validates and resolves thermal-time inputs required by
#' `seir()` when using development stage (`age_driver = "dvs"`) and/or an
#' infection-window constraint.
#'
#' @description
#' This function centralises logic for:
#' * enforcing that `thermal_time` is supplied when `age_driver = "dvs"`;
#' * validating `infection_window` structure when provided; and
#' * constructing a fallback `thermal_time` list from `infection_window`
#'   (`base_temp`, `gdd_to_maturity`) when possible.
#'
#' @param age_driver Character scalar. Age basis used by `seir()`; expected
#'   values are `"day"` or `"dvs"`.
#' @param thermal_time `NULL` or a list of thermal accumulation parameters.
#'   Expected fields are:
#'   * `base_temp` (numeric scalar)
#'   * `gdd_to_maturity` (numeric scalar > 0)
#' @param infection_window `NULL` or a list defining a DVS infection gate.
#'   Must include:
#'   * `dvs_start`
#'   * `dvs_end`
#'   Optionally may include:
#'   * `base_temp`
#'   * `gdd_to_maturity`
#'
#' @returns
#' A `thermal_time` list (possibly unchanged input, or derived from
#' `infection_window`) or `NULL` when not needed.
#'
#' @section Error Conditions:
#' Aborts when:
#' * `age_driver = "dvs"` and `thermal_time` is `NULL`;
#' * `infection_window` is malformed (missing `dvs_start`/`dvs_end`);
#' * `infection_window` is used to supply thermal-time values but is missing
#'   `base_temp` and/or `gdd_to_maturity`.
#'
#' @seealso [seir()], [.calculate_dvs()]
#' @keywords internal
#' @autoglobal
#' @dev
.resolve_thermal_time <- function(age_driver, thermal_time, infection_window) {
  if (age_driver == "dvs" && is.null(thermal_time)) {
    cli::cli_abort(
      "{.arg age_driver} = {.val dvs} requires {.arg thermal_time}"
    )
  }

  if (is.null(infection_window)) {
    return(thermal_time)
  }

  valid_window <- rlang::is_list(infection_window) &&
    rlang::has_name(infection_window, "dvs_start") &&
    rlang::has_name(infection_window, "dvs_end")

  if (!valid_window) {
    cli::cli_abort(
      "{.arg infection_window} must have {.field dvs_start} and {.field dvs_end}"
    )
  }

  thermal_time %||%
    list(
      base_temp = infection_window$base_temp %||%
        cli::cli_abort("{.arg infection_window} needs {.field base_temp}"),
      gdd_to_maturity = infection_window$gdd_to_maturity %||%
        cli::cli_abort("{.arg infection_window} needs {.field gdd_to_maturity}")
    )
}

#' Window and Align Weather Data to Emergence Date
#'
#' Internal helper that slices weather data to the simulation interval beginning
#' at emergence and spanning `duration` days.
#'
#' @description
#' Adds a `dates` column from `YYYYMMDD` when absent, locates the emergence date,
#' checks data sufficiency for the requested simulation duration, and returns the
#' cropped weather table used by `seir()`.
#'
#' @param wth A weather object (typically class `epicrop.wth`) containing
#'   `YYYYMMDD` and other required meteorological fields.
#' @param emergence Character date (`"YYYY-MM-DD"`) passed to
#'   [.validate_emergence()].
#' @param duration Integer simulation length in days.
#'
#' @returns
#' A `data.table` with exactly `duration` rows, starting on the emergence date.
#'
#' @section Warnings:
#' Emits a warning when `duration < 50` days because short runs may produce
#' unrealistic epidemic dynamics.
#'
#' @section Error Conditions:
#' Aborts when:
#' * emergence date is not present in weather records;
#' * there are not enough rows after emergence to cover `duration`.
#'
#' @seealso [seir()], [.validate_emergence()]
#' @keywords internal
#' @autoglobal
#' @dev
.window_weather <- function(wth, emergence, duration) {
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

  if ((nrow(w) - i0 + 1L) < duration) {
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

  w[i0:(i0 + duration - 1L)]
}

#' Compute Age Driver Vector for Daily Modifiers
#'
#' Internal helper that generates per-day age values used to evaluate
#' age-dependent modifiers in `seir()`.
#'
#' @param age_driver Character scalar: `"day"` or `"dvs"`.
#' @param duration Integer simulation length.
#' @param w Weather subset used for simulation; requires `TEMP` when
#'   `age_driver = "dvs"`.
#' @param thermal_time Thermal-time list used by [.calculate_dvs()] when
#'   `age_driver = "dvs"`.
#'
#' @returns
#' Numeric vector of length `duration`:
#' * day index `0:(duration-1)` when `age_driver = "day"`;
#' * DVS values on \[0, 2\] when `age_driver = "dvs"`.
#'
#' @seealso [seir()], [.calculate_dvs()]
#' @keywords internal
#' @autoglobal
#' @dev
.compute_age_values <- function(age_driver, duration, w, thermal_time) {
  if (age_driver == "dvs") {
    .calculate_dvs(w, thermal_time)
  } else {
    seq(0L, duration - 1L)
  }
}

#' Compute Leaf-Wetness Modifier for Infection Rate
#'
#' Internal helper that computes daily wetness effects applied to `Rc` in
#' `seir()`.
#'
#' @description
#' Supports two modes:
#' * **Simple wetness** (`simple_wetness = TRUE`): binary 0/1 using daily
#'   thresholds `RHUM >= rhlim` OR `RAIN >= rainlim`.
#' * **Hourly wetness** (`simple_wetness = FALSE`): computes wet hours via
#'   [.calculate_leaf_wetness()] and maps to \[0, 1\] either linearly (`wet/24`)
#'   or through interpolation over user-supplied `RcW`.
#'
#' @param w Weather data for the simulation window.
#' @param simple_wetness Logical; if `TRUE` use binary daily wetness.
#' @param rhlim Numeric RH threshold (%).
#' @param rainlim Numeric rainfall threshold (mm).
#' @param weighted_wetness Logical; passed to [.calculate_leaf_wetness()].
#' @param RcW `NULL` or 2-column numeric matrix giving wetness-response curve.
#'
#' @returns
#' Numeric vector of daily wetness multipliers (typically bounded \[0, 1\]).
#'
#' @seealso [seir()], [.calculate_leaf_wetness()], [.interp()]
#' @keywords internal
#' @autoglobal
#' @dev
.compute_wetness_modifier <- function(
  w,
  simple_wetness,
  rhlim,
  rainlim,
  weighted_wetness,
  RcW
) {
  if (simple_wetness) {
    return(as.numeric((w$RHUM >= rhlim) | (w$RAIN >= rainlim)))
  }

  wet_hrs <- .calculate_leaf_wetness(
    w,
    rhlim = rhlim,
    rainlim = rainlim,
    weighted_wetness = weighted_wetness
  )

  if (is.null(RcW)) pmin(1, wet_hrs / 24) else .interp(RcW, wet_hrs)
}

#' Expand Flexible Inputs to Daily Vectors
#'
#' Internal helper that normalises scalar, vector/matrix, or function inputs
#' into daily vectors for simulation.
#'
#' @description
#' Accepted input forms:
#' * **Function**: evaluated on either daily `TEMP` or `age_values`
#'   (selected by `fn_input`);
#' * **Matrix/vector length > 1**: interpolated against `age_values` via
#'   [.interp()];
#' * **Scalar**: replicated to length `duration`.
#'
#' @param x Input object to expand.
#' @param w Weather data; `w$TEMP` used when `fn_input = "temp"`.
#' @param age_values Numeric age vector used for interpolation / function input.
#' @param duration Integer simulation length.
#' @param fn_input Character scalar; either `"temp"` or `"age"` indicating what
#'   to pass to `x` when `x` is a function.
#'
#' @returns Numeric vector of length `duration`.
#'
#' @seealso [seir()], [.interp()]
#' @keywords internal
#' @autoglobal
#' @dev
.expand_daily <- function(
  x,
  w,
  age_values,
  duration,
  fn_input = c("temp", "age")
) {
  fn_input <- rlang::arg_match(fn_input)

  if (is.function(x)) {
    arg <- if (fn_input == "temp") w$TEMP else age_values
    return(x(arg))
  }

  if (is.matrix(x) || length(x) > 1L) {
    return(.interp(x, age_values))
  }

  rep(x, duration)
}

#' Apply Development-Stage Infection Window to Rc
#'
#' Internal helper that gates daily infection rates by a DVS susceptibility
#' window.
#'
#' @description
#' If `infection_window` is provided, multiplies `Rc` by 1 when
#' `dvs_start <= DVS <= dvs_end`, else by 0. DVS values are taken from
#' `age_values` when `age_driver = "dvs"`, otherwise computed from weather and
#' `thermal_time`.
#'
#' @param Rc Numeric vector of daily infection rates.
#' @param infection_window `NULL` or list with `dvs_start`, `dvs_end`.
#' @param age_driver Character scalar: `"day"` or `"dvs"`.
#' @param age_values Numeric age vector.
#' @param w Weather data used when DVS must be computed.
#' @param thermal_time Thermal-time list for DVS calculation.
#'
#' @returns Numeric vector of gated infection rates (`Rc`).
#'
#' @seealso [seir()], [.calculate_dvs()]
#' @keywords internal
#' @autoglobal
#' @dev
.apply_infection_window <- function(
  Rc,
  infection_window,
  age_driver,
  age_values,
  w,
  thermal_time
) {
  if (is.null(infection_window)) {
    return(Rc)
  }

  dvs <- if (age_driver == "dvs") {
    age_values
  } else {
    .calculate_dvs(w, thermal_time)
  }
  Rc *
    as.numeric(
      dvs >= infection_window$dvs_start & dvs <= infection_window$dvs_end
    )
}

#' Prepare Lesion-Expansion Inputs for Core SEIR Engine
#'
#' Internal helper that derives daily lesion-expansion rates and determines
#' whether lesion expansion is active.
#'
#' @param RRLEX Lesion expansion specification (`NULL`, scalar, vector/matrix,
#'   or function) as accepted by `seir()`.
#' @param w Weather data used when function inputs depend on temperature.
#' @param age_values Daily age vector for interpolation.
#' @param duration Integer simulation length.
#'
#' @returns
#' A named list with:
#' * `lex_on`: logical scalar, `TRUE` if any daily RRLEX > 0;
#' * `RRLEX_vals`: numeric vector of daily rates, or `NULL` when RRLEX is
#'    `NULL`.
#'
#' @seealso [seir()], [.expand_daily()]
#' @autoglobal
#' @keywords internal
#' @dev
.prepare_lesion_expansion <- function(RRLEX, w, age_values, duration) {
  if (is.null(RRLEX)) {
    return(list(lex_on = FALSE, RRLEX_vals = NULL))
  }

  vals <- pmax(
    0,
    .expand_daily(
      RRLEX,
      w,
      age_values,
      duration,
      fn_input = "temp"
    )
  )

  list(
    lex_on = any(vals > 0),
    RRLEX_vals = vals
  )
}

#' Build Primary Inoculum Schedule
#'
#' Internal helper that creates the daily primary inoculum vector and validates
#' onset indexing.
#'
#' @param onset 1-indexed day of disease onset (integer-like).
#' @param duration Integer simulation length in days.
#' @param I0 Initial inoculum amount injected at `onset`.
#'
#' @returns
#' A named list with:
#' * `Iprim`: numeric vector (length `duration`) with a single non-zero entry at
#'   `onset`;
#' * `onset_idx`: validated integer onset index.
#'
#' @section Error Conditions:
#' Aborts if `onset` is outside `[1, duration]`.
#'
#' @seealso [seir()]
#' @autoglobal
#' @keywords internal
#' @dev
.build_primary_inoculum <- function(onset, duration, I0) {
  onset_idx <- as.integer(onset)

  if (onset_idx < 1L || onset_idx > duration) {
    cli::cli_abort(
      "{.arg onset} out of range for duration={duration} (1-indexed)"
    )
  }

  Iprim <- numeric(duration)
  Iprim[onset_idx] <- I0

  list(Iprim = Iprim, onset_idx = onset_idx)
}

#' Dispatch to SEIR Core Engine Variant
#'
#' Internal helper that chooses between core simulators with or without lesion
#' expansion.
#'
#' @param lex_on Logical scalar; if `TRUE`, use `.seir_core_with_lex()`,
#'   otherwise `.seir_core_no_lex()`.
#' @param duration Integer simulation length.
#' @param onset_idx Integer day index for primary inoculum.
#' @param H0 Initial healthy sites.
#' @param Iprim Primary inoculum vector.
#' @param Rc Daily infection-rate vector.
#' @param p_days Daily latent-period vector.
#' @param i_days Daily infectious-period vector.
#' @param RRG_vals Daily growth-rate vector.
#' @param RRS_vals Daily senescence-rate vector.
#' @param a_vals Daily aggregation-coefficient vector.
#' @param Sx Carrying capacity (max sites).
#' @param RRLEX_vals Daily lesion-expansion vector; required only when
#'   `lex_on = TRUE`.
#'
#' @returns A `data.table` produced by the selected core simulator.
#'
#' @seealso [.seir_core_no_lex()], [.seir_core_with_lex()], [seir()]
#' @autoglobal
#' @keywords internal
#' @dev
.run_seir_core <- function(
  lex_on,
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
  Sx,
  RRLEX_vals = NULL
) {
  if (lex_on) {
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
}

#' Finalise SEIR Output Table and Attributes
#'
#' Internal helper that appends calendar/spatial columns, orders output fields,
#' and assigns standard simulation attributes.
#'
#' @param out Core simulation output `data.table`.
#' @param w Weather window used for simulation (contains `dates`, optionally
#'   `LAT`/`LON`).
#' @param duration Integer simulation length.
#'
#' @returns
#' A finalised `data.table` of class `epicrop.sim` with:
#' * ordered state/flow/derived columns;
#' * `simday` and `dates` columns;
#' * optional `lat`/`lon` columns;
#' * `AUDPC` attribute computed from `intensity`.
#'
#' @seealso [seir()], [.calculate_audpc()]
#' @autoglobal
#' @keywords internal
#' @dev
.finalise_output <- function(out, w, duration) {
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
  out[]
}
