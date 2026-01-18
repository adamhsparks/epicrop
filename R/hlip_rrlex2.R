#' Healthy Latent Infectious Post-Infectious (HLIP) Model Framework With Lesion Expansion
#'
#' A model framework based on the medical idea of a Susceptible-Exposed-
#' Infectious-Removed (\acronym{SEIR}) model.  This framework is designed for
#' plant diseases. This function is originally used by specific disease models
#' in \sQuote{EPIWHEAT} to model disease intensity of two wheat diseases.  Given
#' proper values it can be used with other pathosystems as well.
#'
#' @section Differences between `hlip()` and `hlip_rrlex()`:
#' \describe{
#'   \item{`RRLEX`}{`hlip()`, does not include a parameter for the relative
#'   rate of lesion expansion whereas `hlip_rrlex()` does.}
#'   \item{`RcW`}{`hlip()` uses both relative humidity (RH) and rainfall to
#'   predict leaf wetness.  This differs from `hlip_rrlex()`, which uses only
#'   rainfall to predict leaf wetness.}
#' }
#'
#' @param wth a `data.frame` of weather on a daily time-step containing data
#' with the following field names.
#'
#'   **Field Name** | **Value**
#'   --------------:|:----------
#'   _YYYYMMDD_ | Date as Year Month Day (ISO8601).
#'   _DOY_ | Consecutive day of year, commonly called "Julian date".
#'   _TEMP_ | Mean daily temperature (°C).
#'   _RAIN_ | Mean daily rainfall (mm).
#'   _LAT_ | **Optional** latitude of weather observation. See LAT/LON Note.
#'   _LON_ | **Optional** longitude of weather observation. See LAT/LON Note.
#'
#' @param emergence expected date of plant emergence (or transplanting for rice)
#'  entered in `YYYY-MM-DD` format (character).  Described in Table 1 of Savary
#'  _et al._ 2012 and Table 1 of Savary _et al._ 2015.
#' @param onset expected number of days until the onset of disease after
#'  emergence date (day, integer).  Described in Table 1 of Savary _et al._
#'  2012 and Table 1 of Savary _et al._ 2015.
#' @param duration simulation duration _i.e._, growing season length (day,
#'  integer).  Described in Table 1 of Savary _et al._ 2012 and Table 1 of
#'  Savary _et al._ 2015.
#' @param rhlim relative humidity value threshold to decide whether leaves are
#'  wet or not (numeric).  Described in Table 1 of Savary _et al._ 2012. Savary
#'  _et al._ 2012 used 90%.
#' @param rainlim rainfall amount (mm) threshold to decide whether leaves are
#'  wet or not (numeric).  Described in Table 1 of Savary _et al._ 2012. Savary
#'  _et al._ 2012 used 5mm.
#' @param H0 initial number of plant's healthy sites (integer).  Described in
#'  Table 1 of Savary _et al._ 2012 and Table 1 of Savary _et al._ 2015.
#' @param I0 initial number of infective sites (integer).  Described in Table 1
#'  of Savary _et al._ 2012 and Table 1 of Savary _et al._ 2015.
#' @param RcA modifier for _Rc_ (the basic infection rate corrected for
#'  removals) for crop age (numeric vector).  Described in Table 1 of Savary
#'  _et al._ 2012 Table 1 of Savary _et al._ 2015.
#' @param RcT modifier for _Rc_ (the basic infection rate corrected for
#'  removals) for temperature (numeric vector).  Described in Table 1 of Savary
#'  _et al._ 2012 and Table 1 of Savary _et al._ 2015.
#' @param RcOpt potential basic infection rate corrected for removals (numeric).
#'  Derived from Table 1 of Savary _et al._ 2012 and Table 1 of Savary _et al._
#'  2015.
#' @param RRLEX relative rate of lesion expansion (numeric).  Described in Table
#'  1 of Savary _et al._ 2015.
#' @param i duration of infectious period (day, integer).  Described in Table 1
#'  of Savary _et al._ 2012 and Table 1 of Savary _et al._ 2015.
#' @param p duration of latent period (day, integer).  Described in  Table 1 of
#'  Savary _et al._ 2012 and Table 1 of Savary _et al._ 2015.
#' @param Sx maximum number of sites (integer).  Described in Table 1 of Savary
#'  _et al._ 2012 and Table 1 of Savary _et al._ 2015.
#' @param a aggregation coefficient, values are from 1 to >1 (numeric).
#'  Described in Table 1 of Savary _et al._ 2012 and Table 1 of Savary _et al._
#'  2015.  See further details in **_a_ - Aggregation** section.
#' @param RRS relative rate of physiological senescence (numeric).  Described in
#'  Table 1 of Savary _et al._ 2012 and Table 1 of Savary _et al._ 2015.
#' @param RRG relative rate of growth (numeric).  Described in Table 1 of Savary
#'  _et al._ 2012 and Table 1 of Savary _et al._ 2015.
#'
#' @references Savary, S., Stetkiewicz, S., Brun, R., and Willocquet, L.
#' Modelling and Mapping Potential Epidemics of Wheat Diseases—Examples on Leaf
#' Rust and Septoria Tritici Blotch Using EPIWHEAT. _European Journal of Plant
#' Pathology_ Volume 142, No. 4, 2015, Pages 771–90, DOI:
#' <https://doi.org/10.1007/s10658-015-0650-7>.
#'
#' @examplesIf interactive()
#' # get weather for Esperance, WA, AU for 2021 growing season
#' wth <- get_wth(
#'   lonlat = c(121.891945, -33.861111),
#'   dates = c("2021-01-01", "2021-12-31")
#' )
#'
#' # provide suitable values for leaf rust intensity
#' RcA <-
#'   cbind(c(0L, 20L, 40L, 60L, 80L, 100L, 120L),
#'         c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0))
#' RcT <-
#'   cbind(c(15L, 20L, 25L, 30L, 35L, 40L),
#'         c(0, 0.06, 1.0, 0.85, 0.16, 0))
#' emergence <- "2000-07-15"
#'
#' (hlip_rrlex(
#'   wth = wth,
#'   emergence = emergence,
#'   onset = 20,
#'   duration = 120,
#'   rhlim = 90,
#'   rainlim = 5,
#'   RcA = RcA,
#'   RcT = RcT,
#'   RcOpt = 1.17,
#'   RRLEX = 0.09,
#'   p =  11,
#'   i = 18,
#'   H0 = 250,
#'   I0 = 15,
#'   a = 1,
#'   Sx = 174000,
#'   RRS = 0.01,
#'   RRG = 0.1
#' ))
#'
#' @details # _a_ - Aggregation
#' When _a_ is set to `1` the assumption is that that there is no disease
#' aggregation with new infections occurring at random among the healthy sites.
#' When _a_ is greater than `1` there is aggregation in the disease occurrence,
#' the pathogen is unable to access the entire population of healthy sites,
#' which results in disease aggregation. Refer to Savary _et al._ (2012) for
#' greater detail.
#'
#' @note
#' If the `wth` object provides _LAT_ and _LON_ columns, these will be included
#' in the output for mapping purposes. Both values must be present. These
#' columns are provided by default when using [get_wth()].
#'
#' @seealso
#' `hlip_rrlex()` is called by the following specific disease modelling functions:
#' * [predict_leaf_rust()],
#' * [predict_septoria_tritici_blotch()]
#'
#' @author Adam H. Sparks
#'
#' @return A [data.table::data.table()] containing the following columns:
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
#'   \item{rlex}{Rate of lesion expansion}
#'   \item{diseased}{Number of diseased (latent + infectious + removed) sites on
#'    day "x"}
#'   \item{intensity}{Proportion of diseased (latent + infectious + removed)
#'    sites per total sites not including removed sites on day "x"}
#'   \item{lat}{Latitude value if provided by `wth` object}
#'   \item{lon}{Longitude value if provided by `wth` object}
#' }
#'

#' @export

#' Healthy–Latent–Infectious–Post-Infectious (HLIP) with Lesion Expansion
#' EPIWHEAT-aligned, robust implementation
#'
#' Key improvements vs. hlip_rrlex2():
#'  - Host age is DVS-based; RcA(DVS) lookup
#'  - RRPSN can increase with age (vector or function)
#'  - p and i can be fixed, vectors, or T-driven functions (fractional delays)
#'  - RcW is a pluggable function (with BR/SB presets)
#'  - Primary inoculum can be an impulse or a schedule
#'  - Dynamic aggregation 'a' supported
#'  - Numerical guardrails & diagnostics
hlip_rrlex3 <- function(
  wth, # data.table/data.frame with YYYYMMDD, TEMP, RAIN, optional RHUM/LAT/LON
  start = list(
    mode = c("emergence", "heat_sum")[1],
    emergence = NULL, # "YYYY-MM-DD" if mode=="emergence"
    base_temp = 0,
    heat_sum_threshold = 200 # °C·day from Jan 1 (paper’s winter-wheat init)
  ),
  run_days = 120L, # simulation length (paper: 120)
  inoculum = list(
    mode = c("impulse", "uniform", "triangle", "series")[1],
    I0 = 10, # total primary infections
    start_offset = 20L, # days after run start (paper: 20)
    duration = 1L, # length for uniform/triangle modes
    series = NULL # optional numeric vector length run_days
  ),
  RcOpt,
  RcA, # either matrix [DVS,value], or function(DVS)->scalar, or numeric length run_days
  RcT, # matrix [TEMP,value] or function(T)->scalar or numeric length run_days
  RcW = list(
    # pluggable wetness rule
    fun = NULL, # function(day, RAIN, RHUM, state_env)->scalar in [0,1]
    preset = c("const1", "consec_rain", "threshold")[1],
    args = list(rhlim = 90, rainlim = 5) # used by 'threshold' preset
  ),
  RRLEX, # lesion expansion rate
  p, # latency: scalar | vector | function(T)->scalar
  i, # infectious: scalar | vector | function(T)->scalar
  SMax, # carrying capacity
  a = 1, # aggregation: scalar | vector | function(DVS, crowding)->scalar
  RRG, # growth rate
  RRS, # phys senescence: scalar | vector | function(DVS)->scalar
  dt = 1.0, # sub-daily step (1.0 = paper)
  return_diagnostics = TRUE
) {
  # -- deps & coerce -----------------------------------------------------------
  if (!inherits(wth, "data.table")) wth <- data.table::as.data.table(wth)
  stopifnot(all(c("YYYYMMDD", "TEMP", "RAIN") %in% names(wth)))
  if (!"RHUM" %in% names(wth)) wth[, RHUM := NA_real_]
  setorderv(wth, "YYYYMMDD")

  # -- run window start (paper-aligned option) --------------------------------
  if (start$mode == "heat_sum") {
    jan1 <- as.Date(paste0(format(wth$YYYYMMDD[1], "%Y"), "-01-01"))
    # cum GDD from Jan 1, base 0°C
    wth[, gdd_jan1 := cumsum(pmax(TEMP, start$base_temp) - start$base_temp)]
    srow <- which(wth$gdd_jan1 >= start$heat_sum_threshold)[1]
    if (is.na(srow)) stop("heat_sum_threshold not reached in provided weather.")
    run_dates <- wth$YYYYMMDD[srow + seq_len(run_days) - 1L]
  } else {
    stopifnot(!is.null(start$emergence))
    em <- as.Date(start$emergence)
    run_dates <- seq(em, by = "day", length.out = run_days)
  }
  w <- wth[YYYYMMDD %in% run_dates]
  if (nrow(w) != run_days)
    stop("Weather records do not fully cover the run window.")

  # -- DVS (0..2) from degree-days (SUCROS-like, base 0°C) --------------------
  # Simple linear scaler: choose location-appropriate total GDD emergence->maturity
  # Expose as parameter if desired
  baseT <- 0
  gdd <- cumsum(pmax(w$TEMP, baseT) - baseT)
  # heuristic: 1100 °C·d to reach DVS=2; make configurable for cultivar/site
  dvs <- pmin(2, 2 * gdd / 1100)

  # -- Helper: generic interpolator over table or function --------------------
  .interp <- function(def, x, yname = "value") {
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
    } else stop("Unsupported modifier definition.")
  }

  # -- Daily modifiers ---------------------------------------------------------
  RcA_day <- .interp(RcA, dvs) # paper: RcA(DVS)
  RcT_day <- .interp(RcT, w$TEMP) # paper: RcT(T)
  # RcW preset functions
  consec_rain_fun <- function(d, R, H, env) {
    # # rainy streak in last 4 days including today (paper’s SB rule)
    k <- max(1L, d - 3L)
    is_rain <- R > 0
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
    if ((!is.na(H[d]) && H[d] >= env$rhlim) || (R[d] >= env$rainlim)) 1 else 0
  }
  const1_fun <- function(d, R, H, env) 1

  if (is.null(RcW$fun)) {
    RcW$fun <- switch(
      match.arg(RcW$preset, c("const1", "consec_rain", "threshold")),
      const1 = const1_fun,
      consec_rain = consec_rain_fun,
      threshold = threshold_fun
    )
  }
  RcW_day <- vapply(
    seq_len(run_days),
    function(d) RcW$fun(d, w$RAIN, w$RHUM, RcW$args),
    numeric(1)
  )
  Rc <- RcOpt * RcA_day * RcT_day * RcW_day # paper Eq. (4)

  # -- Flexible p(T) and i(T) (scalar/vector/function) ------------------------
  p_day <- .interp(p, w$TEMP)
  i_day <- .interp(i, w$TEMP)

  # -- Flexible aggregation a --------------------------------------------------
  crowding <- function(HS, LAT, INF, REM) (HS + LAT + INF + REM) / SMax
  .agg <- function(d, HS, LAT, INF, REM) {
    if (is.function(a)) return(max(1, a(dvs[d], crowding(HS, LAT, INF, REM))))
    if (length(a) > 1L) return(max(1, a[d]))
    return(max(1, a))
  }

  # -- Flexible RRPSN (scalar | vector | function of DVS) ---------------------
  RRS_day <- if (is.function(RRS)) pmax(0, RRS(dvs)) else if (length(RRS) > 1L)
    pmax(0, RRS) else rep_len(pmax(0, RRS), run_days)

  # -- Primary inoculum schedule ----------------------------------------------
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

  # -- Stocks, queues, and outputs --------------------------------------------
  steps <- as.integer(1 / dt)
  if (abs(steps * dt - 1) > 1e-9)
    stop("dt must divide 1.0 exactly (e.g., 1, 0.5, 0.25)")
  n <- run_days
  HS <- LAT <- INF <- REM <- SEN <- numeric(n)
  ri <- rlex <- rt <- rrem <- rg <- rsn <- diseased <- intensity <- numeric(n)
  cofr <- numeric(n)

  # maintain histories for variable (possibly fractional) delays
  RI_hist <- numeric(n)
  BecInf_hist <- numeric(n)

  # initial conditions (paper defaults: HSITES0=250; infections injected by Iprim)
  HS_prev <- 0 + 250
  LAT_prev <- INF_prev <- REM_prev <- SEN_prev <- 0

  # fractional back-reference helper
  .backref <- function(hist, d, delay) {
    if (delay <= 0) return(0)
    j1 <- d - floor(delay)
    j2 <- d - ceiling(delay)
    if (j2 < 1 && j1 < 1) return(0)
    if (j1 < 1) j1 <- 1
    if (j2 < 1) j2 <- 1
    w2 <- delay - floor(delay)
    w1 <- 1 - w2
    val <- 0
    if (j1 >= 1) val <- val + w1 * hist[j1]
    if (j2 >= 1) val <- val + w2 * hist[j2]
    val
  }

  for (d in seq_len(n)) {
    # optionally sub-step (Euler)
    for (s in seq_len(steps)) {
      # paper Eq. (1): RI = Rc * INF * COFR^a
      total_prev <- HS_prev + LAT_prev + INF_prev + REM_prev
      diseased_prev <- LAT_prev + INF_prev + REM_prev
      cofr_d <- if (total_prev <= 0) 0 else 1 - (diseased_prev / total_prev)
      agg_d <- .agg(d, HS_prev, LAT_prev, INF_prev, REM_prev)
      Rc_d <- Rc[d]

      # primary infections (at day boundary only)
      RI_primary <- if (s == 1) Iprim[d] else 0

      RI_from_INF <- if (d > 1 || s > 1)
        INF_prev * Rc_d * (cofr_d^agg_d) * dt else 0
      RI_today <- RI_primary + RI_from_INF

      # lesion expansion (HS -> INF)
      RLEX_today <- RRLEX * INF_prev * cofr_d * dt

      # transfer from latent to infectious after p[d] days (variable/fractional)
      RT_today <- .backref(RI_hist, d, p_day[d]) * dt

      BecInf_today <- RT_today + RLEX_today

      # removal from INF after i[d] days (variable/fractional)
      RREM_today <- .backref(BecInf_hist, d, i_day[d]) * dt

      # physiological senescence
      RRPSN_today <- RRS_day[d]
      RSN_today <- RREM_today + RRPSN_today * HS_prev * dt

      # growth (logistic on healthy sites with crowding by total sites)
      RG_today <- RRG * HS_prev * (1 - (total_prev / SMax)) * dt

      # ---- limits to avoid negatives ----------------------------------------
      # cap flows so HS cannot go negative
      cap_HS = max(0, HS_prev + RG_today) # available to lose
      loss_HS = RI_today + RLEX_today + RSN_today
      if (loss_HS > cap_HS) {
        # scale down proportionally
        scale <- ifelse(loss_HS > 0, cap_HS / loss_HS, 1)
        RI_today <- RI_today * scale
        RLEX_today <- RLEX_today * scale
        RSN_today <- RSN_today * scale
      }
      # cap removal by current infectious potential
      max_rem <- max(0, INF_prev + BecInf_today)
      if (RREM_today > max_rem) RREM_today <- max_rem

      # ---- update stocks -----------------------------------------------------
      HS_cur <- HS_prev + RG_today - RI_today - RLEX_today - RSN_today
      LAT_cur <- LAT_prev + RI_today - RT_today
      INF_cur <- INF_prev + BecInf_today - RREM_today
      REM_cur <- REM_prev + RREM_today
      SEN_cur <- SEN_prev + RSN_today

      # assign to prev for next substep
      HS_prev <- HS_cur
      LAT_prev <- LAT_cur
      INF_prev <- INF_cur
      REM_prev <- REM_cur
      SEN_prev <- SEN_cur
    } # substep

    # end-of-day logging
    HS[d] <- HS_prev
    LAT[d] <- LAT_prev
    INF[d] <- INF_prev
    REM[d] <- REM_prev
    SEN[d] <- SEN_prev
    total_cur <- HS[d] + LAT[d] + INF[d] + REM[d]
    diseased[d] <- LAT[d] + INF[d] + REM[d]
    denom <- HS[d] + LAT[d] + INF[d]
    intensity[d] <- if (denom > 0) (LAT[d] + INF[d]) / denom else 0
    cofr[d] <- if (total_cur <= 0) 0 else 1 - (diseased[d] / total_cur)

    # store daily aggregated flows
    ri[d] <- RI_today
    rlex[d] <- RLEX_today
    rt[d] <- RT_today
    rrem[d] <- RREM_today
    rg[d] <- RG_today
    rsn[d] <- RSN_today

    # update histories (queues)
    RI_hist[d] <- ri[d]
    BecInf_hist[d] <- (rt[d] + rlex[d])
  }

  out <- data.table::data.table(
    simday = seq_len(n),
    dates = run_dates,
    sites = HS,
    latent = LAT,
    infectious = INF,
    removed = REM,
    senesced = SEN,
    rateinf = ri,
    rtransfer = rt,
    rgrowth = rg,
    rsenesced = rsn,
    rlex = rlex,
    diseased = diseased,
    intensity = intensity
  )

  if ("LAT" %in% names(wth) && "LON" %in% names(wth)) {
    out[, `:=`(lat = rep_len(wth$LAT, .N), lon = rep_len(wth$LON, .N))]
  }

  if (return_diagnostics) {
    out[, `:=`(
      Rc = Rc,
      RcA_today = RcA_day,
      RcT_today = RcT_day,
      RcW_today = RcW_day,
      COFR = cofr,
      DVS = dvs,
      p_today = p_day,
      i_today = i_day
    )]
  }

  # AUDPC helper (paper uses 120 days)
  audpc <- sum((intensity[-1] + intensity[-length(intensity)]) / 2)
  attr(out, "AUDPC_trapz") <- audpc

  out[]
}
