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
#'  emergence date (day, integer). For this modern seir() variant onset is
#'  1-indexed (1 = emergence day). When comparing with legacy SEIR(),
#'  translate as legacy_onset = onset - 1.
#' @param duration simulation duration *i. e.*, growing season length (day,
#'  integer). Described in Table 1 of Savary _et al._ 2012.
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
#'  column being temperature in °C and the second the modifier (bounded by 0
#'  and 1). Described in Tables 1 and 2 of Savary _et al._ 2012.
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
#' ... (documentation omitted for brevity) ...
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com} from original by
#'   Robert J. Hijmans, Rene Pangga and Jorrel Aunario.
#'
#' @autoglobal
#' @export

seir <- function(
  wth,
  emergence,
  onset, # 1-indexed: 1..duration (1 = emergence day)
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
  RRLEX = NULL, # fast path: if NULL (or all zero) -> fully skipped
  simple_wetness = TRUE,
  weighted_wetness = FALSE,
  age_driver = c("day", "dvs"),
  thermal_time = NULL,
  steps_per_day = 1L, # sub-daily resolution; 1 means daily
  infection_window = NULL
) {
  age_driver <- rlang::arg_match(age_driver)
  if (!inherits(wth, "epicrop.wth")) {
    cli::cli_abort(c(
      "{.arg wth} must be of class {.class epicrop.wth}",
      "i" = "Use {.fun get_wth} or {.fun format_wth} to create one"
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

  # ---- Weather windowing -----------------------------------------------------
  w <- data.table::copy(wth)
  if (!rlang::has_name(w, "dates")) {
    w[, dates := YYYYMMDD]
  }

  emergence_date <- .validate_emergence(emergence)
  i0 <- which(w$YYYYMMDD == emergence_date)
  if (!length(i0)) {
    cli::cli_abort(c(
      "Emergence date not found in weather data",
      "x" = "Emergence {.val {emergence_date}} not in range
      {.val {min(w$YYYYMMDD)}}–{.val {max(w$YYYYMMDD)}}"
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
      "i" = "Duration is {duration} days (< 50 recommended)"
    ))
  }
  w <- w[i0:(i0 + duration - 1L)]

  # ---- Age driver & modifiers (vectorised) -----------------------------------
  age_values <- if (age_driver == "dvs") {
    .calculate_dvs(w, thermal_time)
  } else {
    seq(0, duration - 1L)
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

  # ---- Daily drivers (vectorised) --------------------------------------------
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
    if (is.function(RRS)) {
      pmax(0, RRS(age_values))
    } else {
      pmax(0, .interp(RRS, age_values))
    }
  }
  a_vals <- pmax(1, .interp(a, age_values))

  # ---- Fast path decision for lesion expansion -------------------------------
  lex_on <- FALSE
  RRLEX_vals <- NULL
  if (!is.null(RRLEX)) {
    RRLEX_vals <- pmax(0, .as_daily(RRLEX))
    lex_on <- any(RRLEX_vals > 0)
  }

  # ---- Primary inoculum timing (1-indexed onset) -----------------------------
  Iprim <- numeric(duration)
  onset_idx <- as.integer(onset) # onset is 1..duration
  if (onset_idx < 1L || onset_idx > duration) {
    cli::cli_abort(
      "{.arg onset} out of range for duration={duration} (1-indexed)"
    )
  }
  Iprim[onset_idx] <- I0

  # ---- Delegate to specialised core (supports steps_per_day) ---------------
  if (steps_per_day == 1L) {
    out <- if (!lex_on) {
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
    } else {
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
    }
  } else {
    out <- if (!lex_on) {
      .seir_core_no_lex_steps(
        duration,
        steps_per_day,
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
    } else {
      .seir_core_with_lex_steps(
        duration,
        steps_per_day,
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
    }
  }

  # ---- Attach calendar + spatial, attributes ---------------------------------
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

# --- Core without lesion expansion (fast path) --------------------------------
# Uses cumulative sums to avoid O(n) inner-window sums; Cropsim integer delays.
# Returns a data.table with required columns; rlex is identically zero.
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
  cs_lat <- cs_inf <- numeric(duration) # cumulative sums

  # Day 1
  sites[1L] <- H0
  rsenesced[1L] <- RRS_vals[1L] * sites[1L]
  senesced[1L] <- rsenesced[1L]
  rgrowth[1L] <- RRG_vals[1L] * sites[1L] * (1 - (sites[1L] / Sx))
  rateinf[1L] <- if (1L == onset_idx) Iprim[1L] else 0

  # Loop
  for (t in 2:duration) {
    lat_src <- t - floor(p_days[t])
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

    # sum(latency[max(1, lat_src):t]) via cumulative sums
    win_lat0 <- if (lat_src > 2L) cs_lat[lat_src - 2L] else 0
    now_latent[t] <- cs_lat[t] - win_lat0
    rtransfer[t] <- if (lat_src > 1L) latency[lat_src - 1L] else 0

    # Infectious (no lesion expansion)
    infectious_flow[t] <- rtransfer[t]
    cs_inf[t] <- cs_inf[t - 1L] + infectious_flow[t]
    win_inf0 <- if (inf_src > 2L) cs_inf[inf_src - 2L] else 0
    now_infectious[t] <- cs_inf[t] - win_inf0

    removed[t] <- cs_inf[t] - now_infectious[t]
    # compute diseased as latent + infectious + removed (canonical)
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

# --- Core with lesion expansion (RRLEX ON) ------------------------------------
# Computes lesion expansion using a two-stage update to avoid circularity:
#  1) compute infectious window ignoring today's rlex
#  2) compute rlex from pre-lex now_infectious & COFR
#  3) finalise infectious window including rlex
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
    lat_src <- t - floor(p_days[t])
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
    now_inf_pre <- if (inf_src > 2L) cs_pre - cs_inf[inf_src - 2L] else cs_pre

    # COFR from pre-lex states
    removed_pre <- cs_pre - now_inf_pre
    diseased_pre <- cs_pre + now_latent[t] + removed_pre
    total_cur <- sites[t] + diseased_pre
    COFR <- if (total_cur > 0) 1 - (diseased_pre / total_cur) else 1
    COFR <- min(1, max(0, COFR))

    # Today's lesion expansion from pre-lex infectious
    rlex[t] <- RRLEX_vals[t] * now_inf_pre * max(0, COFR)

    # Finalise infectious window including lesion expansion
    infectious_flow[t] <- rtransfer[t] + rlex[t]
    cs_inf[t] <- cs_inf[t - 1L] + infectious_flow[t]
    now_infectious[t] <- if (inf_src > 2L) {
      cs_inf[t] - cs_inf[inf_src - 2L]
    } else {
      cs_inf[t]
    }

    removed[t] <- cs_inf[t] - now_infectious[t]
    # compute diseased as latent + infectious + removed (canonical)
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

# --- Step-resolution cores (support steps_per_day > 1) -----------------------

.seir_core_no_lex_steps <- function(
  duration,
  steps_per_day,
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
  total_steps <- duration * steps_per_day
  rep_each <- function(x) rep(x, each = steps_per_day)

  Rc_steps <- rep_each(Rc)
  p_steps <- rep_each(p_days) * steps_per_day
  i_steps <- rep_each(i_days) * steps_per_day
  RRG_steps <- rep_each(RRG_vals) / steps_per_day
  RRS_steps <- rep_each(RRS_vals) / steps_per_day
  a_steps <- rep_each(a_vals)

  sites_s <- latent_s <- infectious_s <- removed_s <- senesced_s <- numeric(
    total_steps
  )
  latency_s <- infectious_flow_s <- numeric(total_steps)
  rateinf_s <- rlex_s <- rtransfer_s <- rremoved_s <- rgrowth_s <- rsenesced_s <- numeric(
    total_steps
  )
  now_latent_s <- now_infectious_s <- diseased_s <- numeric(total_steps)
  cs_lat_s <- cs_inf_s <- numeric(total_steps)

  sites_s[1L] <- H0
  rsenesced_s[1L] <- RRS_steps[1L] * sites_s[1L]
  senesced_s[1L] <- rsenesced_s[1L]
  rgrowth_s[1L] <- RRG_steps[1L] * sites_s[1L] * (1 - (sites_s[1L] / Sx))
  rateinf_s[1L] <- if (1L == onset_idx) Iprim[1L] else 0

  for (t in 2:total_steps) {
    lat_src <- t - floor(p_steps[t])
    inf_src <- t - floor(i_steps[t])

    removed_today <- if (inf_src > 1L) infectious_flow_s[inf_src - 1L] else 0

    sites_s[t] <- max(
      0,
      sites_s[t - 1L] +
        rgrowth_s[t - 1L] -
        rateinf_s[t - 1L] -
        rsenesced_s[t - 1L]
    )
    rsenesced_s[t] <- removed_today + RRS_steps[t] * sites_s[t]
    senesced_s[t] <- senesced_s[t - 1L] + rsenesced_s[t]

    latency_s[t] <- rateinf_s[t - 1L]
    cs_lat_s[t] <- cs_lat_s[t - 1L] + latency_s[t]

    win_lat0 <- if (lat_src > 2L) cs_lat_s[lat_src - 2L] else 0
    now_latent_s[t] <- cs_lat_s[t] - win_lat0
    rtransfer_s[t] <- if (lat_src > 1L) latency_s[lat_src - 1L] else 0

    infectious_flow_s[t] <- rtransfer_s[t]
    cs_inf_s[t] <- cs_inf_s[t - 1L] + infectious_flow_s[t]
    win_inf0 <- if (inf_src > 2L) cs_inf_s[inf_src - 2L] else 0
    now_infectious_s[t] <- cs_inf_s[t] - win_inf0

    removed_s[t] <- cs_inf_s[t] - now_infectious_s[t]
    diseased_s[t] <- now_latent_s[t] + now_infectious_s[t] + removed_s[t]

    total_cur <- sites_s[t] + diseased_s[t]
    COFR <- if (total_cur > 0) 1 - (diseased_s[t] / total_cur) else 1
    COFR <- min(1, max(0, COFR))

    current_day_idx <- ceiling(t / steps_per_day)
    if (current_day_idx == onset_idx && (t %% steps_per_day == 0)) {
      rateinf_s[t] <- Iprim[current_day_idx]
    } else {
      rateinf_s[t] <- now_infectious_s[t] * Rc_steps[t] * (COFR^a_steps[t])
    }

    total_sites <- sites_s[t] + diseased_s[t]
    rgrowth_s[t] <- RRG_steps[t] * sites_s[t] * (1 - (total_sites / Sx))
  }

  sel <- seq(steps_per_day, total_steps, by = steps_per_day)
  sites_d <- sites_s[sel]
  now_latent_d <- now_latent_s[sel]
  now_infectious_d <- now_infectious_s[sel]
  removed_d <- removed_s[sel]
  senesced_d <- senesced_s[sel]
  rateinf_d <- rateinf_s[sel]
  rlex_d <- rep(0, length(sel))
  rtransfer_d <- rtransfer_s[sel]
  rremoved_d <- c(0, diff(removed_d))
  rgrowth_d <- rgrowth_s[sel]
  rsenesced_d <- rsenesced_s[sel]
  diseased_d <- diseased_s[sel]
  intensity_d <- ifelse(
    (sites_d + now_latent_d + now_infectious_d) > 0,
    (now_latent_d + now_infectious_d) /
      (sites_d + now_latent_d + now_infectious_d),
    0
  )

  data.table::data.table(
    sites = sites_d,
    latent = now_latent_d,
    infectious = now_infectious_d,
    removed = removed_d,
    senesced = senesced_d,
    rateinf = rateinf_d,
    rlex = rlex_d,
    rtransfer = rtransfer_d,
    rremoved = rremoved_d,
    rgrowth = rgrowth_d,
    rsenesced = rsenesced_d,
    diseased = diseased_d,
    intensity = intensity_d
  )
}

.seir_core_with_lex_steps <- function(
  duration,
  steps_per_day,
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
  total_steps <- duration * steps_per_day
  rep_each <- function(x) rep(x, each = steps_per_day)

  Rc_steps <- rep_each(Rc)
  p_steps <- rep_each(p_days) * steps_per_day
  i_steps <- rep_each(i_days) * steps_per_day
  RRG_steps <- rep_each(RRG_vals) / steps_per_day
  RRS_steps <- rep_each(RRS_vals) / steps_per_day
  a_steps <- rep_each(a_vals)
  RRLEX_steps <- rep_each(RRLEX_vals)

  sites_s <- latent_s <- infectious_s <- removed_s <- senesced_s <- numeric(
    total_steps
  )
  latency_s <- infectious_flow_s <- numeric(total_steps)
  rateinf_s <- rlex_s <- rtransfer_s <- rremoved_s <- rgrowth_s <- rsenesced_s <- numeric(
    total_steps
  )
  now_latent_s <- now_infectious_s <- diseased_s <- numeric(total_steps)
  cs_lat_s <- cs_inf_s <- numeric(total_steps)

  sites_s[1L] <- H0
  rsenesced_s[1L] <- RRS_steps[1L] * sites_s[1L]
  senesced_s[1L] <- rsenesced_s[1L]
  rgrowth_s[1L] <- RRG_steps[1L] * sites_s[1L] * (1 - (sites_s[1L] / Sx))
  rateinf_s[1L] <- if (1L == onset_idx) Iprim[1L] else 0

  for (t in 2:total_steps) {
    lat_src <- t - floor(p_steps[t])
    inf_src <- t - floor(i_steps[t])

    removed_today <- if (inf_src > 1L) infectious_flow_s[inf_src - 1L] else 0

    sites_s[t] <- max(
      0,
      sites_s[t - 1L] +
        rgrowth_s[t - 1L] -
        rateinf_s[t - 1L] -
        rsenesced_s[t - 1L] -
        rlex_s[t - 1L]
    )
    rsenesced_s[t] <- removed_today + RRS_steps[t] * sites_s[t]
    senesced_s[t] <- senesced_s[t - 1L] + rsenesced_s[t]

    latency_s[t] <- rateinf_s[t - 1L]
    cs_lat_s[t] <- cs_lat_s[t - 1L] + latency_s[t]
    win_lat0 <- if (lat_src > 2L) cs_lat_s[lat_src - 2L] else 0
    now_latent_s[t] <- cs_lat_s[t] - win_lat0

    rtransfer_s[t] <- if (lat_src > 1L) latency_s[lat_src - 1L] else 0

    # Stage A: infectious window *before* today's lesion expansion
    inf_wo_lex <- rtransfer_s[t]
    cs_pre <- cs_inf_s[t - 1L] + inf_wo_lex
    now_inf_pre <- if (inf_src > 2L) cs_pre - cs_inf_s[inf_src - 2L] else cs_pre

    removed_pre <- cs_pre - now_inf_pre
    diseased_pre <- cs_pre + now_latent_s[t] + removed_pre
    total_cur <- sites_s[t] + diseased_pre
    COFR <- if (total_cur > 0) 1 - (diseased_pre / total_cur) else 1
    COFR <- min(1, max(0, COFR))

    rlex_s[t] <- RRLEX_steps[t] * now_inf_pre * max(0, COFR)

    infectious_flow_s[t] <- rtransfer_s[t] + rlex_s[t]
    cs_inf_s[t] <- cs_inf_s[t - 1L] + infectious_flow_s[t]
    now_infectious_s[t] <- if (inf_src > 2L) {
      cs_inf_s[t] - cs_inf_s[inf_src - 2L]
    } else {
      cs_inf_s[t]
    }

    removed_s[t] <- cs_inf_s[t] - now_infectious_s[t]
    diseased_s[t] <- now_latent_s[t] + now_infectious_s[t] + removed_s[t]

    current_day_idx <- ceiling(t / steps_per_day)
    if (current_day_idx == onset_idx && (t %% steps_per_day == 0)) {
      rateinf_s[t] <- Iprim[current_day_idx]
    } else {
      rateinf_s[t] <- now_infectious_s[t] * Rc_steps[t] * (COFR^a_steps[t])
    }

    total_sites <- sites_s[t] + diseased_s[t]
    rgrowth_s[t] <- RRG_steps[t] * sites_s[t] * (1 - (total_sites / Sx))
  }

  sel <- seq(steps_per_day, total_steps, by = steps_per_day)
  sites_d <- sites_s[sel]
  now_latent_d <- now_latent_s[sel]
  now_infectious_d <- now_infectious_s[sel]
  removed_d <- removed_s[sel]
  senesced_d <- senesced_s[sel]
  rateinf_d <- rateinf_s[sel]
  rlex_d <- rlex_s[sel]
  rtransfer_d <- rtransfer_s[sel]
  rremoved_d <- c(0, diff(removed_d))
  rgrowth_d <- rgrowth_s[sel]
  rsenesced_d <- rsenesced_s[sel]
  diseased_d <- diseased_s[sel]
  intensity_d <- ifelse(
    (sites_d + now_latent_d + now_infectious_d) > 0,
    (now_latent_d + now_infectious_d) /
      (sites_d + now_latent_d + now_infectious_d),
    0
  )

  data.table::data.table(
    sites = sites_d,
    latent = now_latent_d,
    infectious = now_infectious_d,
    removed = removed_d,
    senesced = senesced_d,
    rateinf = rateinf_d,
    rlex = rlex_d,
    rtransfer = rtransfer_d,
    rremoved = rremoved_d,
    rgrowth = rgrowth_d,
    rsenesced = rsenesced_d,
    diseased = diseased_d,
    intensity = intensity_d
  )
}

#' Calculate development stage (DVS)
#'
#' @param w Weather data with TEMP column.
#' @param thermal_time List with base_temp and gdd_to_maturity.
#' @returns Numeric vector of DVS (0-2).
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
      "{.arg thermal_time} must be a list with {.field base_temp} and {.field gdd_to_maturity}"
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
      "{.arg thermal_time$gdd_to_maturity} must be a single positive numeric value"
    )
  }

  gdd <- cumsum(pmax(w$TEMP - base_temp, 0))
  pmin(2, 2 * gdd / gdd_to_maturity)
}
# --- Core without lesion expansion (fast path) --------------------------------
# Uses cumulative sums to avoid O(n) inner-window sums; Cropsim integer delays.
# Returns a data.table with required columns; rlex is identically zero.
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
  cs_lat <- cs_inf <- numeric(duration) # cumulative sums

  # Day 1
  sites[1L] <- H0
  rsenesced[1L] <- RRS_vals[1L] * sites[1L]
  senesced[1L] <- rsenesced[1L]
  rgrowth[1L] <- RRG_vals[1L] * sites[1L] * (1 - (sites[1L] / Sx))
  rateinf[1L] <- if (1L == onset_idx) Iprim[1L] else 0

  # Loop
  for (t in 2:duration) {
    lat_src <- t - floor(p_days[t])
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

    # sum(latency[max(1, lat_src):t]) via cumulative sums
    win_lat0 <- if (lat_src > 2L) cs_lat[lat_src - 2L] else 0
    now_latent[t] <- cs_lat[t] - win_lat0
    rtransfer[t] <- if (lat_src > 1L) latency[lat_src - 1L] else 0

    # Infectious (no lesion expansion)
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

# --- Core with lesion expansion (RRLEX ON) ------------------------------------
# Computes lesion expansion using a two-stage update to avoid circularity:
#  1) compute infectious window ignoring today's rlex
#  2) compute rlex from pre-lex now_infectious & COFR
#  3) finalise infectious window including rlex
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
    lat_src <- t - floor(p_days[t])
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
    now_inf_pre <- if (inf_src > 2L) cs_pre - cs_inf[inf_src - 2L] else cs_pre

    # COFR from pre-lex states
    removed_pre <- cs_pre - now_inf_pre
    diseased_pre <- cs_pre + now_latent[t] + removed_pre
    total_cur <- sites[t] + diseased_pre
    COFR <- if (total_cur > 0) 1 - (diseased_pre / total_cur) else 1
    COFR <- min(1, max(0, COFR))

    # Today's lesion expansion from pre-lex infectious
    rlex[t] <- RRLEX_vals[t] * now_inf_pre * max(0, COFR)

    # Finalise infectious window including lesion expansion
    infectious_flow[t] <- rtransfer[t] + rlex[t]
    cs_inf[t] <- cs_inf[t - 1L] + infectious_flow[t]
    now_infectious[t] <- if (inf_src > 2L) {
      cs_inf[t] - cs_inf[inf_src - 2L]
    } else {
      cs_inf[t]
    }

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
