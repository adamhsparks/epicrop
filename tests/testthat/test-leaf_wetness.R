# `wth_formatted` is loaded in setup-testdata.R

test_that("setup: extended weather available", {
  skip_if(
    !exists("wth_formatted") ||
      !is.data.frame(wth_formatted) ||
      nrow(wth_formatted) < 185L,
    "Need extended weather data for leaf-wetness tests"
  )
})

# --- Inline helpers ----------------------------------------------------------

.lw_args <- list(rhlim = 90.0, rainlim = 5.0)

.call_lw <- function(
  wth = wth_formatted,
  weighted = FALSE,
  rhlim = .lw_args$rhlim,
  rainlim = .lw_args$rainlim
) {
  .calculate_leaf_wetness(
    wth = wth,
    rhlim = rhlim,
    rainlim = rainlim,
    weighted_wetness = weighted
  )
}

.expect_wetness_vector <- function(x, n) {
  expect_true(typeof(x) %in% c("integer", "double"))
  expect_length(x, n)
  expect_true(all(is.finite(x)))
  expect_true(all(x >= 0L & x <= 24L))
  invisible(x)
}

# Build a new weather table with exactly the requested columns; prevents hidden inheritance

.mk_wth_cols <- function(
  cols = c("YYYYMMDD", "DOY", "TEMP", "RHUM", "RAIN", "TMIN", "TMAX", "LAT")
) {
  cols <- intersect(cols, names(wth_formatted)) # ensure only real columns
  dt <- data.table::as.data.table(wth_formatted)[, ..cols] # <-- NOTE THE ..cols
  class(dt) <- c("epicrop.wth", "data.table", "data.frame")
  dt
}


modes <- list(
  unweighted = FALSE,
  weighted = TRUE
)

test_that(".calculate_leaf_wetness returns valid wet-hours (both modes)", {
  for (nm in names(modes)) {
    res <- .call_lw(wth = wth_formatted, weighted = modes[[nm]])
    .expect_wetness_vector(res, nrow(wth_formatted))
  }
})

test_that(".calculate_leaf_wetness handles unweighted & weighted modes (range check)", {
  for (nm in names(modes)) {
    res <- .call_lw(wth = wth_formatted, weighted = modes[[nm]])
    expect_true(all(res >= 0L & res <= 24L))
  }
})

test_that(".calculate_leaf_wetness falls back to binary without TMIN/TMAX (unweighted)", {
  # Construct a table that DEFINITELY lacks TMIN/TMAX
  wth_no_temp <- .mk_wth_cols(c(
    "YYYYMMDD",
    "DOY",
    "TEMP",
    "RHUM",
    "RAIN",
    "LAT"
  ))
  res <- .call_lw(wth = wth_no_temp, weighted = FALSE)

  # Binary mode: 0 or 24 only
  expect_true(all(res %in% c(0L, 24L)))
})


test_that(".calculate_leaf_wetness errors when RHUM or DOY are missing; handles missing RAIN", {
  cases <- list(
    list(
      name = "missing RHUM",
      wcols = c("YYYYMMDD", "DOY", "TEMP", "RAIN", "LAT"),
      expect = "error",
      patt = "RHUM"
    ),
    list(
      name = "missing DOY",
      wcols = c("YYYYMMDD", "TEMP", "RHUM", "RAIN", "LAT"),
      expect = "error",
      patt = "DOY"
    ),
    list(
      name = "missing RAIN",
      wcols = c("YYYYMMDD", "DOY", "TEMP", "RHUM", "LAT"),
      expect = "ok",
      patt = NULL
    )
  )

  for (cs in cases) {
    w <- .mk_wth_cols(cs$wcols)

    if (identical(cs$expect, "error")) {
      expect_error(
        .call_lw(wth = w, weighted = FALSE),
        regexp = cs$patt
      )
    } else {
      res <- .call_lw(wth = w, weighted = FALSE)
      .expect_wetness_vector(res, nrow(w))
    }
  }
})

test_that(".calculate_leaf_wetness handles NA rainfall", {
  w <- data.table::copy(wth_formatted)
  w$RAIN[1:5] <- NA_real_

  res <- .call_lw(wth = w, weighted = FALSE)
  expect_true(all(is.finite(res)))
})

test_that(".calculate_leaf_wetness requires LAT for hourly calculation", {
  w_no_lat <- .mk_wth_cols(setdiff(
    c("YYYYMMDD", "DOY", "TEMP", "RHUM", "RAIN", "TMIN", "TMAX", "LAT"),
    "LAT"
  ))

  expect_error(
    .call_lw(wth = w_no_lat, weighted = FALSE),
    regexp = "LAT"
  )
})

test_that("leaf wetness increases with rainfall (unweighted)", {
  w_no_rain <- data.table::copy(wth_formatted)
  w_no_rain$RAIN <- 0.0
  w_rain <- data.table::copy(wth_formatted)
  w_rain$RAIN <- 10.0

  res0 <- .call_lw(wth = w_no_rain, weighted = FALSE)
  res1 <- .call_lw(wth = w_rain, weighted = FALSE)

  expect_gte(mean(res1), mean(res0))
})

test_that("leaf wetness is bounded at 24 hours under extreme conditions", {
  w_ext <- data.table::copy(wth_formatted)
  w_ext$RHUM <- 100.0
  w_ext$RAIN <- 100.0

  res <- .call_lw(wth = w_ext, weighted = FALSE)
  expect_true(all(res <= 24L))
})

test_that(".diurnal_rh returns 24-hour RH values (single day)", {
  res <- .diurnal_rh(
    rh = wth_formatted$RHUM[1],
    tmin = wth_formatted$TMIN[1],
    tmax = wth_formatted$TMAX[1],
    tmp = wth_formatted$TEMP[1],
    lat = wth_formatted$LAT[1],
    doy = wth_formatted$DOY[1]
  )

  expect_type(res, "list")
  expect_length(res, 1)
  expect_length(res[[1]], 24)
  expect_true(all(res[[1]] >= 0.0 & res[[1]] <= 100.0))
})

test_that(".diurnal_temp returns 24-hour temperatures (single day) and supports multiple days", {
  # Single day
  res1 <- .diurnal_temp(
    lat = 14.6774,
    doy = 182L,
    tmin = 20.0,
    tmax = 30.0
  )
  expect_length(res1, 24L)
  expect_true(all(res1 >= 20.0 & res1 <= 30.0))
  expect_gte(min(res1), 19.9)
  expect_lte(max(res1), 30.1)

  # Multiple days (3 x 24 = 72)
  res3 <- .diurnal_temp(
    lat = rep(14.6774, 3),
    doy = c(182, 183, 184),
    tmin = c(20, 21, 19),
    tmax = c(30, 31, 29)
  )
  expect_length(res3, 72)
})

test_that(".day_length returns reasonable daylight hours and is vectorised", {
  r1 <- .day_length(lat = 14.6774, doy = 182)
  expect_length(r1, 1)
  expect_gt(r1, 0)
  expect_lt(r1, 24)

  rvec <- .day_length(lat = rep(14.6774, 10), doy = 1:10)
  expect_length(rvec, 10)
  expect_true(all(rvec > 0 & rvec < 24))
})

test_that(".day_length varies with latitude and season", {
  # Latitude effect (around June, doy ~ 182)
  eq <- .day_length(lat = 0, doy = 182)
  nh <- .day_length(lat = 45, doy = 182)
  sh <- .day_length(lat = -45, doy = 182)
  expect_gt(nh, eq)
  expect_lt(sh, eq)

  # Seasonal effect at mid-latitude
  lat <- 45
  summer <- .day_length(lat = lat, doy = 172) # June solstice
  winter <- .day_length(lat = lat, doy = 355) # December solstice
  expect_gt(summer, winter)
})

test_that(".saturated_vapour_pressure basic properties and vectorization", {
  es1 <- .saturated_vapour_pressure(25)
  expect_type(es1, "double")
  expect_length(es1, 1)
  expect_gt(es1, 0)

  es_low <- .saturated_vapour_pressure(10)
  es_high <- .saturated_vapour_pressure(30)
  expect_gt(es_high, es_low)

  temps <- c(10, 20, 30)
  esv <- .saturated_vapour_pressure(temps)
  expect_length(esv, 3)
  expect_true(all(esv > 0))

  es0 <- .saturated_vapour_pressure(0)
  expect_gt(es0, 0)
  expect_lt(es0, 1)
})


test_that(".calculate_weighted_hr_wet returns values in [0,1], handles edge cases, and vectorizes", {
  # Range checks and edges
  expect_gte(
    .calculate_weighted_hr_wet(rh = 85, rhlim = 90),
    0
  )
  expect_lte(
    .calculate_weighted_hr_wet(rh = 85, rhlim = 90),
    1
  )
  expect_identical(.calculate_weighted_hr_wet(rh = 95, rhlim = 90), 1)
  expect_identical(.calculate_weighted_hr_wet(rh = 75, rhlim = 90), 0)

  # Interpolation at midpoint (85 between 80 and 90)
  mid <- .calculate_weighted_hr_wet(rh = 85, rhlim = 90)
  expect_gt(mid, 0)
  expect_lt(mid, 1)
  expect_equal(mid, 0.5, tolerance = 0.01)

  # Vectorized
  rh_values <- c(75, 85, 95, 100)
  v <- .calculate_weighted_hr_wet(rh = rh_values, rhlim = 90)
  expect_length(v, 4)
  expect_identical(v[1], 0)
  expect_gt(v[2], 0)
  expect_identical(v[3], 1)
  expect_identical(v[4], 1)

  # Reject invalid thresholds
  expect_error(
    .calculate_weighted_hr_wet(rh = 85, rhlim = 75),
    regexp = "must be >= 80" # Match what the error actually says
  )
})

# ---- TEMP derivation fallback chain (TEMP -> T2M -> (TMIN+TMAX)/2) --------

test_that(".calculate_leaf_wetness derives TEMP from T2M when TEMP is absent", {
  w <- .mk_wth_cols(c("YYYYMMDD", "DOY", "RHUM", "RAIN", "TMIN", "TMAX", "LAT"))
  w[, T2M := (TMIN + TMAX) / 2]

  res <- .call_lw(wth = w, weighted = FALSE)
  .expect_wetness_vector(res, nrow(w))
})

test_that(".calculate_leaf_wetness derives TEMP from (TMIN+TMAX)/2 when TEMP and T2M are both absent", {
  w <- .mk_wth_cols(c("YYYYMMDD", "DOY", "RHUM", "RAIN", "TMIN", "TMAX", "LAT"))

  res <- .call_lw(wth = w, weighted = FALSE)
  .expect_wetness_vector(res, nrow(w))
})

# ---- missing-RAIN equivalence, both binary and hourly paths ----------------

test_that(".calculate_leaf_wetness with missing RAIN behaves like RAIN = 0 (binary path)", {
  w_missing <- .mk_wth_cols(c("YYYYMMDD", "DOY", "TEMP", "RHUM", "LAT"))
  w_explicit_zero <- .mk_wth_cols(c(
    "YYYYMMDD",
    "DOY",
    "TEMP",
    "RHUM",
    "RAIN",
    "LAT"
  ))
  w_explicit_zero$RAIN <- 0

  res_missing <- .call_lw(wth = w_missing, weighted = FALSE)
  res_zero <- .call_lw(wth = w_explicit_zero, weighted = FALSE)

  expect_identical(res_missing, res_zero)
})

test_that(".calculate_leaf_wetness with missing RAIN behaves like RAIN = 0 (hourly path)", {
  w_missing <- .mk_wth_cols(c(
    "YYYYMMDD",
    "DOY",
    "TEMP",
    "RHUM",
    "TMIN",
    "TMAX",
    "LAT"
  ))
  w_explicit_zero <- .mk_wth_cols(c(
    "YYYYMMDD",
    "DOY",
    "TEMP",
    "RHUM",
    "RAIN",
    "TMIN",
    "TMAX",
    "LAT"
  ))
  w_explicit_zero$RAIN <- 0

  res_missing <- .call_lw(wth = w_missing, weighted = FALSE)
  res_zero <- .call_lw(wth = w_explicit_zero, weighted = FALSE)

  expect_identical(res_missing, res_zero)
})

# ---- rainlim edge case: near-zero threshold --------------------------------

test_that(".calculate_leaf_wetness handles rainlim = 0 without producing NaN/Inf", {
  w <- data.table::copy(wth_formatted)
  w$RAIN[1] <- 10

  res <- .call_lw(wth = w, weighted = FALSE, rainlim = 0)

  expect_true(all(is.finite(res)))
  expect_true(all(res >= 0 & res <= 24))
})

# ---- deterministic combination formula (binary fallback path) -------------

test_that(".calculate_leaf_wetness combination logic is exact on a constructed example", {
  w <- data.table::data.table(
    YYYYMMDD = as.Date("2000-01-01"),
    DOY = 1L,
    RHUM = c(95, 50),
    RAIN = c(0, 10)
  )
  class(w) <- c("epicrop.wth", "data.table", "data.frame")

  res <- .call_lw(wth = w, weighted = FALSE, rhlim = 90, rainlim = 5)

  # Row 1: RH >= rhlim -> fully wet (24h) regardless of rain
  expect_identical(res[1], 24L)
  # Row 2: RH < rhlim but RAIN >= rainlim -> fully wet (24h) via binary rule
  expect_identical(res[2], 24L)
})

# ---- weighted and unweighted wetness behave as designed --------------------

test_that("weighted wetness remains valid but may exceed binary wetness", {
  unweighted <- .call_lw(
    wth = wth_formatted,
    weighted = FALSE
  )

  weighted <- .call_lw(
    wth = wth_formatted,
    weighted = TRUE
  )

  .expect_wetness_vector(unweighted, nrow(wth_formatted))
  .expect_wetness_vector(weighted, nrow(wth_formatted))

  # Weighting should have an observable effect for suitable weather data.
  expect_false(identical(weighted, unweighted))
})

test_that("weighted wetness gives partial credit below rhlim", {
  rh <- c(75, 85, 90, 95)

  weighted <- .calculate_weighted_hr_wet(
    rh = rh,
    rhlim = 90
  )

  unweighted <- as.double(rh >= 90)

  expect_identical(weighted[1], 0)
  expect_equal(weighted[2], 0.5, tolerance = 0.01)
  expect_identical(weighted[3], 1)
  expect_identical(weighted[4], 1)

  expect_gt(weighted[2], unweighted[2])
  expect_identical(weighted[c(1, 3, 4)], unweighted[c(1, 3, 4)])
})

test_that("weighted hourly wetness never exceeds full hourly wetness", {
  rh <- seq(0, 100, by = 0.1)

  weighted <- .calculate_weighted_hr_wet(
    rh = rh,
    rhlim = 90
  )

  expect_type(weighted, "double")
  expect_true(all(is.finite(weighted)))
  expect_true(all(weighted >= 0))
  expect_true(all(weighted <= 1))
})

# ---- .diurnal_rh at extreme daily RH ---------------------------------------

test_that(".diurnal_rh stays within [0, 100] at extreme daily RH", {
  res_low <- .diurnal_rh(
    rh = 0,
    tmin = 15,
    tmax = 30,
    tmp = 22,
    lat = 14.6774,
    doy = 182
  )
  res_high <- .diurnal_rh(
    rh = 100,
    tmin = 15,
    tmax = 30,
    tmp = 22,
    lat = 14.6774,
    doy = 182
  )

  expect_true(all(res_low[[1]] >= 0 & res_low[[1]] <= 100))
  expect_true(all(res_high[[1]] >= 0 & res_high[[1]] <= 100))
})

# ---- .diurnal_temp: zero-length input and recycling -----------------------

test_that(".diurnal_temp returns an empty vector for zero-length inputs", {
  res <- .diurnal_temp(
    lat = numeric(0),
    doy = numeric(0),
    tmin = numeric(0),
    tmax = numeric(0)
  )
  expect_length(res, 0)
})

test_that(".diurnal_temp recycles shorter vectors to the longest input", {
  res <- .diurnal_temp(
    lat = 14.6774, # scalar, recycled to length 3
    doy = c(182, 183, 184),
    tmin = c(20, 21, 19),
    tmax = c(30, 31, 29)
  )
  expect_length(res, 72)
})

test_that(".diurnal_temp stays within [tmin, tmax] bounds", {
  res <- .diurnal_temp(lat = 14.6774, doy = 182, tmin = 15, tmax = 30)

  expect_lte(max(res), 30 + 1e-6)
  expect_gte(min(res), 15 - 1e-6)
})

# ---- .day_length: DOY wraparound and polar extremes ------------------------

test_that(".day_length wraps day-of-year values outside [1, 365]", {
  expect_identical(
    .day_length(lat = 45, doy = 366),
    .day_length(lat = 45, doy = 1)
  )
  expect_identical(
    .day_length(lat = 45, doy = 0),
    .day_length(lat = 45, doy = 365)
  )
})

test_that(".day_length approaches polar day/night at high latitude", {
  summer <- .day_length(lat = 80, doy = 172) # June solstice
  winter <- .day_length(lat = 80, doy = 355) # December solstice

  expect_gt(summer, 20)
  expect_lt(winter, 4)
})

# ---- .saturated_vapour_pressure below 0C -----------------------------------

test_that(".saturated_vapour_pressure remains positive and increasing below 0C", {
  es_sub <- .saturated_vapour_pressure(-10)
  es_zero <- .saturated_vapour_pressure(0)

  expect_gt(es_sub, 0)
  expect_lt(es_sub, es_zero)
})

# ---- .calculate_weighted_hr_wet: rhlim = 80 boundary -----------------------

test_that(".calculate_weighted_hr_wet handles rhlim = 80 boundary without NaN", {
  res <- .calculate_weighted_hr_wet(rh = c(70, 80, 90), rhlim = 80)

  expect_true(all(is.finite(res)))
  expect_identical(res, c(0, 1, 1))
})
