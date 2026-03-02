#' Wrapper that dispatches to with-lex core and returns `data.table` object.
#' @param steps_per_day integer >= 1
#' @param audpc optional externally computed AUDPC (numeric scalar)
#' @param simday,dates,lat,lon optional metadata to attach
#' @dev
.seir_core_with_lex_r <- function(
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
  sites <- latency <- infectious_flow <- numeric(duration)
  cs_lat <- cs_inf <- numeric(duration)
  rsenesced <- senesced <- rgrowth <- rateinf <- numeric(duration)
  rlex <- rtransfer <- removed <- rremoved <- numeric(duration)
  now_latent <- now_infectious <- diseased <- numeric(duration)

  # Precompute floored window indices
  p_int <- as.integer(floor(p_days))
  i_int <- as.integer(floor(i_days))

  # Day 1
  sites[1L] <- H0
  rsenesced[1L] <- RRS_vals[1L] * H0
  senesced[1L] <- rsenesced[1L]
  rgrowth[1L] <- RRG_vals[1L] * H0 * (1 - H0 / Sx)
  rateinf[1L] <- if (1L == onset_idx) Iprim[1L] else 0

  for (t in 2:duration) {
    lat_src <- t - p_int[t] + 2L
    inf_src <- t - i_int[t]

    # Removed today
    removed_today <- if (inf_src > 1L) infectious_flow[inf_src - 1L] else 0

    # Sites (includes previous day's lesion expansion)
    s <- sites[t - 1L] +
      rgrowth[t - 1L] -
      rateinf[t - 1L] -
      rsenesced[t - 1L] -
      rlex[t - 1L]
    sites[t] <- if (s < 0) 0 else s

    rsenesced[t] <- removed_today + RRS_vals[t] * sites[t]
    senesced[t] <- senesced[t - 1L] + rsenesced[t]

    # Latency
    latency[t] <- rateinf[t - 1L]
    cs_lat[t] <- cs_lat[t - 1L] + latency[t]

    win_lat0 <- if (lat_src > 2L) cs_lat[lat_src - 2L] else 0
    now_latent[t] <- cs_lat[t] - win_lat0
    rtransfer[t] <- if (lat_src > 1L) latency[lat_src - 1L] else 0

    # Stage A: infectious window before today's lesion expansion
    cs_pre <- cs_inf[t - 1L] + rtransfer[t]
    base_pre <- if (inf_src > 2L) cs_inf[inf_src - 2L] else 0
    now_inf_pre <- cs_pre - base_pre
    removed_pre <- cs_pre - now_inf_pre

    # COFR from pre-lex state
    diseased_pre <- now_latent[t] + now_inf_pre + removed_pre
    total_pre <- sites[t] + diseased_pre
    COFR <- if (total_pre > 0) 1 - diseased_pre / total_pre else 1
    COFR <- min(1, max(0, COFR))

    # Lesion expansion
    rlex[t] <- RRLEX_vals[t] * now_inf_pre * max(0, COFR)

    # Finalise infectious including lesion expansion
    infectious_flow[t] <- rtransfer[t] + rlex[t]
    cs_inf[t] <- cs_inf[t - 1L] + infectious_flow[t]

    win_inf0 <- if (inf_src > 2L) cs_inf[inf_src - 2L] else 0
    now_infectious[t] <- cs_inf[t] - win_inf0

    removed[t] <- cs_inf[t] - now_infectious[t]
    rremoved[t] <- removed[t] - removed[t - 1L]
    diseased[t] <- now_latent[t] + now_infectious[t] + removed[t]

    total <- sites[t] + diseased[t]

    a <- a_vals[t]
    rateinf[t] <- if (t == onset_idx) {
      Iprim[t]
    } else {
      now_infectious[t] * Rc[t] * (if (a == 1) COFR else COFR^a)
    }

    rgrowth[t] <- RRG_vals[t] * sites[t] * (1 - total / Sx)
  }

  # Intensity (vectorised, single pass)
  denom <- sites + now_latent + now_infectious
  active <- now_latent + now_infectious
  intensity <- active / denom
  intensity[denom == 0] <- 0

  x <- data.table::data.table(
    sites = sites,
    latent = now_latent,
    infectious = now_infectious,
    removed = removed,
    senesced = senesced,
    rateinf = rateinf,
    rlex = rlex,
    rtransfer = rtransfer,
    rremoved = rremoved,
    rgrowth = rgrowth,
    rsenesced = rsenesced,
    diseased = diseased,
    intensity = intensity
  )

  data.table::setcolorder(x, intersect(cols_order, names(x)))

  if (!is.null(simday)) {
    x[, simday := simday]
  }
  if (!is.null(dates)) {
    x[, dates := dates]
  }
  if (!is.null(lat)) {
    x[, lat := lat]
  }
  if (!is.null(lon)) {
    x[, lon := lon]
  }

  front <- intersect(c("simday", "dates"), names(x))
  if (length(front)) {
    remaining <- setdiff(names(x), front)
    data.table::setcolorder(x, c(front, remaining))
  }

  if (!is.null(audpc)) {
    data.table::setattr(x, "AUDPC", audpc)
  } else {
    data.table::setattr(x, "AUDPC", .calculate_audpc(dt$intensity))
  }

  data.table::setattr(x, "class", unique(c("epicrop.sim", class(x))))
}

#' Wrapper that dispatches to no-lex core and returns data.table
#' @dev
#'
.seir_core_no_lex_r <- function(
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
  sites <- latency <- infectious_flow <- numeric(duration)
  cs_lat <- cs_inf <- numeric(duration)
  rsenesced <- senesced <- rgrowth <- rateinf <- numeric(duration)
  rtransfer <- removed <- rremoved <- numeric(duration)
  now_latent <- now_infectious <- diseased <- numeric(duration)

  # Precompute floored window indices
  p_int <- as.integer(floor(p_days))
  i_int <- as.integer(floor(i_days))

  # Day 1
  sites[1L] <- H0
  rsenesced[1L] <- RRS_vals[1L] * H0
  senesced[1L] <- rsenesced[1L]
  rgrowth[1L] <- RRG_vals[1L] * H0 * (1 - H0 / Sx)
  rateinf[1L] <- if (1L == onset_idx) Iprim[1L] else 0

  for (t in 2:duration) {
    lat_src <- t - p_int[t] + 2L
    inf_src <- t - i_int[t]

    # Removed today
    removed_today <- if (inf_src > 1L) infectious_flow[inf_src - 1L] else 0

    # Sites
    s <- sites[t - 1L] + rgrowth[t - 1L] - rateinf[t - 1L] - rsenesced[t - 1L]
    sites[t] <- if (s < 0) 0 else s

    rsenesced[t] <- removed_today + RRS_vals[t] * sites[t]
    senesced[t] <- senesced[t - 1L] + rsenesced[t]

    # Latency
    latency[t] <- rateinf[t - 1L]
    cs_lat[t] <- cs_lat[t - 1L] + latency[t]

    win_lat0 <- if (lat_src > 2L) cs_lat[lat_src - 2L] else 0
    now_latent[t] <- cs_lat[t] - win_lat0
    rtransfer[t] <- if (lat_src > 1L) latency[lat_src - 1L] else 0

    # Infectious
    infectious_flow[t] <- rtransfer[t]
    cs_inf[t] <- cs_inf[t - 1L] + infectious_flow[t]

    win_inf0 <- if (inf_src > 2L) cs_inf[inf_src - 2L] else 0
    now_infectious[t] <- cs_inf[t] - win_inf0

    removed[t] <- cs_inf[t] - now_infectious[t]
    rremoved[t] <- removed[t] - removed[t - 1L]
    diseased[t] <- now_latent[t] + now_infectious[t] + removed[t]

    total <- sites[t] + diseased[t]
    COFR <- if (total > 0) 1 - diseased[t] / total else 1
    COFR <- min(1, max(0, COFR))

    a <- a_vals[t]
    rateinf[t] <- if (t == onset_idx) {
      Iprim[t]
    } else {
      now_infectious[t] * Rc[t] * (if (a == 1) COFR else COFR^a)
    }

    rgrowth[t] <- RRG_vals[t] * sites[t] * (1 - total / Sx)
  }

  # Intensity (vectorised, single pass)
  denom <- sites + now_latent + now_infectious
  active <- now_latent + now_infectious
  intensity <- active / denom
  intensity[denom == 0] <- 0

  x <- data.table::data.table(
    sites = sites,
    latent = now_latent,
    infectious = now_infectious,
    removed = removed,
    senesced = senesced,
    rateinf = rateinf,
    rlex = numeric(duration),
    rtransfer = rtransfer,
    rremoved = rremoved,
    rgrowth = rgrowth,
    rsenesced = rsenesced,
    diseased = diseased,
    intensity = intensity
  )

  data.table::setcolorder(x, intersect(cols_order, names(x)))

  if (!is.null(simday)) {
    x[, simday := simday]
  }
  if (!is.null(dates)) {
    x[, dates := dates]
  }
  if (!is.null(lat)) {
    x[, lat := lat]
  }
  if (!is.null(lon)) {
    x[, lon := lon]
  }

  front <- intersect(c("simday", "dates"), names(x))
  if (length(front)) {
    remaining <- setdiff(names(x), front)
    data.table::setcolorder(x, c(front, remaining))
  }

  if (!is.null(audpc)) {
    data.table::setattr(x, "AUDPC", audpc)
  } else {
    data.table::setattr(x, "AUDPC", .calculate_audpc(dt$intensity))
  }

  data.table::setattr(x, "class", unique(c("epicrop.sim", class(x))))
}
