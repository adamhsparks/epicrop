#' Unified preparation & call into epicrop_core_cpp
#' Returns a data.table with states, flows, diagnostics, AUDPC and dates.
epicrop_sim <- function(
  wth,
  start = list(
    mode = c("emergence", "heat_sum")[1],
    emergence = NULL,
    base_temp = 0,
    heat_sum_threshold = 200
  ),
  run_days = 120L,
  inoculum = list(
    mode = c("impulse", "uniform", "triangle", "series")[1],
    I0 = 10,
    start_offset = 20L,
    duration = 1L,
    series = NULL
  ),
  RcOpt,
  RcA, # table [x,value], function(x)->scalar, or vector len run_days
  RcT, # idem, but x = TEMP
  RcW = list(
    preset = c("const1", "threshold", "consec_rain", "hourly")[1],
    fun = NULL,
    args = list(rhlim = 90, rainlim = 5, wet_hours = NULL, curve = NULL)
  ),
  RRLEX = 0, # scalar or vector (will be expanded)
  p,
  i, # scalar / vector / function(T)->scalar
  SMax,
  a = 1, # scalar / vector / function(dvs,crowding)->scalar
  RRG,
  RRS, # RRS scalar / vector / function(DVS)
  age_driver = c("day", "dvs")[2],
  steps_per_day = 1L,
  return_diagnostics = TRUE
) {
  if (!inherits(wth, "data.table")) wth <- data.table::as.data.table(wth)
  stopifnot(all(c("YYYYMMDD", "TEMP", "RAIN") %in% names(wth)))
  if (!"RHUM" %in% names(wth)) wth[, RHUM := NA_real_]
  data.table::setorderv(wth, "YYYYMMDD")

  # ---- choose run window (EPIWHEAT offers 'heat_sum' init) ----
  if (start$mode == "heat_sum") {
    y <- format(wth$YYYYMMDD[1], "%Y")
    wth[, gdd := cumsum(pmax(TEMP, start$base_temp) - start$base_temp)]
    srow <- which(wth$gdd >= start$heat_sum_threshold)[1]
    if (is.na(srow)) stop("heat_sum_threshold not reached in provided weather.")
    run_dates <- wth$YYYYMMDD[srow + seq_len(run_days) - 1L]
  } else {
    em <- as.Date(start$emergence)
    run_dates <- seq(em, by = "day", length.out = run_days)
  }
  w <- wth[YYYYMMDD %in% run_dates]
  if (nrow(w) != run_days)
    stop("Weather records do not fully cover the run window.")

  # ---- helpers ----
  .interp <- function(def, x) {
    if (is.function(def)) return(pmax(0, as.numeric(def(x))))
    if (is.numeric(def) && length(def) == length(x)) return(as.numeric(def))
    if (is.matrix(def) || is.data.frame(def)) {
      stats::approx(
        x = def[, 1],
        y = def[, 2],
        xout = x,
        method = "linear",
        yleft = 0,
        yright = 0
      )$y
    }
    stop("Unsupported definition (table/function/vector expected).")
  }

  # ---- DVS (0..2) from degree-days (base 0°C) ----
  gdd <- cumsum(pmax(w$TEMP, 0))
  dvs <- pmin(2, 2 * gdd / 1100) # scale 1100 °C·d -> DVS=2; expose if needed

  # ---- daily modifiers ----
  age_driver <- match.arg(age_driver)
  RcA_day <- if (age_driver == "dvs") .interp(RcA, dvs) else
    .interp(RcA, 0:(run_days - 1))
  RcT_day <- .interp(RcT, w$TEMP)

  # RcW presets / plugin
  consec_rain_fun <- function(d, R, H, env) {
    k <- max(1L, d - 3L)
    is_rain <- R > 0
    int
    streak = 0 # pseudo; replace below with R style
    streak <- 0L
    for (j in d:k) {
      if (is_rain[j]) streak <- streak + 1L else break
    }
    streak <- min(streak, 4L)
    c(`0` = 0.23, `1` = 0.36, `2` = 0.68, `3` = 0.93, `4` = 1.00)[as.character(
      streak
    )]
  }
  threshold_fun <- function(d, R, H, env) {
    if ((!is.na(H[d]) && H[d] >= env$rhlim) || R[d] >= env$rainlim) 1 else 0
  }
  const1_fun <- function(d, R, H, env) 1
  hourly_fun <- function(d, R, H, env) {
    wh <- env$wet_hours
    if (is.null(wh)) stop("hourly preset requires RcW$args$wet_hours")
    whd <- max(0, min(24, wh[d]))
    if (is.null(env$curve)) return(whd / 24)
    stats::approx(
      env$curve[, 1],
      env$curve[, 2],
      xout = whd,
      yleft = 0,
      yright = 1
    )$y
  }
  if (is.null(RcW$fun)) {
    RcW$fun <- switch(
      match.arg(RcW$preset, c("const1", "threshold", "consec_rain", "hourly")),
      const1 = const1_fun,
      threshold = threshold_fun,
      consec_rain = consec_rain_fun,
      hourly = hourly_fun
    )
  }
  RcW_day <- vapply(
    seq_len(run_days),
    function(d) RcW$fun(d, w$RAIN, w$RHUM, RcW$args),
    numeric(1)
  )

  # Combine into daily Rc
  Rc <- pmax(0, RcOpt * RcA_day * RcT_day * RcW_day)

  # ---- daily p/i (scalar / vector / function(T)) ----
  p_days <- .interp(p, w$TEMP)
  i_days <- .interp(i, w$TEMP)

  # ---- RRS (scalar / vector / function(DVS)) ----
  RRS_day <- .interp(RRS, dvs)

  # ---- a: aggregation exponent (>=1) ----
  a_day <- if (is.function(a)) pmax(1, a(dvs, (w$TEMP * 0))) else if (
    length(a) > 1
  )
    pmax(1, a) else rep_len(max(1, a), run_days)

  # ---- RRLEX & RRG vectors ----
  RRLEX_day <- if (length(RRLEX) == 1) rep(RRLEX, run_days) else
    as.numeric(RRLEX)
  RRG_day <- if (length(RRG) == 1) rep(RRG, run_days) else as.numeric(RRG)

  # ---- inoculum schedule ----
  Iprim <- numeric(run_days)
  so <- inoculum$start_offset
  if (inoculum$mode == "impulse") {
    if (so >= 1L && so <= run_days) Iprim[so] <- inoculum$I0
  } else if (inoculum$mode == "uniform") {
    L <- max(1L, inoculum$duration)
    idx <- so + seq_len(L) - 1L
    idx <- idx[idx >= 1 & idx <= run_days]
    if (length(idx)) Iprim[idx] <- inoculum$I0 / length(idx)
  } else if (inoculum$mode == "triangle") {
    L <- max(3L, inoculum$duration)
    idx <- so + seq_len(L) - 1L
    idx <- idx[idx >= 1 & idx <= run_days]
    if (length(idx)) {
      # triangular weights
      wts <- c(
        seq_len(ceiling(L / 2)),
        seq_len(floor(L / 2))[(floor(L / 2) - 1):1]
      )
      wts <- wts[seq_along(idx)]
      Iprim[idx] <- inoculum$I0 * (wts / sum(wts))
    }
  } else if (inoculum$mode == "series") {
    stopifnot(length(inoculum$series) == run_days)
    Iprim <- inoculum$series
  }

  # ---- call C++ core ----
  core <- epicrop_core_cpp(
    Rc = Rc,
    RRLEX = RRLEX_day,
    RRG = RRG_day,
    RRS = RRS_day,
    p_days = p_days,
    i_days = i_days,
    agg = a_day,
    Iprim = Iprim,
    H0 = 250.0, # default; expose if you want per-disease H0 here
    SMax = SMax,
    steps_per_day = as.integer(steps_per_day)
  )

  # ---- assemble output ----
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
  # diagnostics & attributes
  if (return_diagnostics) {
    out[, `:=`(
      Rc = Rc,
      RcA_today = RcA_day,
      RcT_today = RcT_day,
      RcW_today = RcW_day,
      COFR = core$cofr,
      DVS = dvs,
      p_today = p_days,
      i_today = i_days
    )]
  }
  # trapezoidal AUDPC over days
  audpc <- sum((out$intensity[-1] + out$intensity[-nrow(out)]) / 2)
  attr(out, "AUDPC_trapz") <- audpc
  out[]
}
