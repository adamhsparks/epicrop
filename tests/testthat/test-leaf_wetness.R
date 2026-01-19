test_that("setup: extended weather available", {
  skip_if(
    !exists("wth_formatted") ||
      !is.data.frame(wth_formatted) ||
      nrow(wth_formatted) < 185,
    "Need extended weather data for leaf-wetness tests"
  )
})

# --- Inline helpers ----------------------------------------------------------

.lw_args <- list(rhlim = 90, rainlim = 5)

.call_lw <- function(
  wth = wth_formatted,
  weighted = FALSE,
  rhlim = .lw_args$rhlim,
  rainlim = .lw_args$rainlim
) {
  epicrop:::.calculate_leaf_wetness(
    wth = wth,
    rhlim = rhlim,
    rainlim = rainlim,
    weighted_wetness = weighted
  )
}

.expect_wetness_vector <- function(x, n) {
  expect_true(is.numeric(x)) # <-- more robust
  expect_length(x, n)
  expect_true(all(is.finite(x)))
  expect_true(all(x >= 0 & x <= 24))
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

# --- Modes to test -----------------------------------------------------------

modes <- list(
  unweighted = FALSE,
  weighted = TRUE
)

# =============================================================================
# .calculate_leaf_wetness — basic behavior across modes
# =============================================================================

test_that(".calculate_leaf_wetness returns valid wet-hours (both modes)", {
  for (nm in names(modes)) {
    res <- .call_lw(wth = wth_formatted, weighted = modes[[nm]])
    .expect_wetness_vector(res, nrow(wth_formatted))
  }
})

test_that(".calculate_leaf_wetness handles unweighted & weighted modes (range check)", {
  for (nm in names(modes)) {
    res <- .call_lw(wth = wth_formatted, weighted = modes[[nm]])
    expect_true(all(res >= 0 & res <= 24))
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
  expect_true(all(res %in% c(0, 24)))
})

# =============================================================================
# .calculate_leaf_wetness — missing columns / NA handling
# =============================================================================

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

# =============================================================================
# Rainfall effects & bounding
# =============================================================================

test_that("leaf wetness increases with rainfall (unweighted)", {
  w_no_rain <- data.table::copy(wth_formatted)
  w_no_rain$RAIN <- 0
  w_rain <- data.table::copy(wth_formatted)
  w_rain$RAIN <- 10

  res0 <- .call_lw(wth = w_no_rain, weighted = FALSE)
  res1 <- .call_lw(wth = w_rain, weighted = FALSE)

  expect_true(mean(res1) >= mean(res0))
})

test_that("leaf wetness is bounded at 24 hours under extreme conditions", {
  w_ext <- data.table::copy(wth_formatted)
  w_ext$RHUM <- 100
  w_ext$RAIN <- 100

  res <- .call_lw(wth = w_ext, weighted = FALSE)
  expect_true(all(res <= 24))
})

# =============================================================================
# .diurnal_rh and .diurnal_temp
# =============================================================================

test_that(".diurnal_rh returns 24-hour RH values (single day)", {
  res <- epicrop:::.diurnal_rh(
    rh = wth_formatted$RHUM[1],
    tmin = wth_formatted$TMIN[1],
    tmax = wth_formatted$TMAX[1],
    tmp = wth_formatted$TEMP[1],
    lat = wth_formatted$LAT[1],
    doy = wth_formatted$DOY[1],
    yyyymmdd = wth_formatted$YYYYMMDD[1]
  )

  expect_type(res, "list")
  expect_length(res, 1)
  expect_length(res[[1]], 24)
  expect_true(all(res[[1]] >= 0 & res[[1]] <= 100))
})

test_that(".diurnal_temp returns 24-hour temperatures (single day) and supports multiple days", {
  # Single day
  res1 <- epicrop:::.diurnal_temp(
    lat = 14.6774,
    doy = 182,
    tmin = 20,
    tmax = 30
  )
  expect_length(res1, 24)
  expect_true(all(res1 >= 20 & res1 <= 30))
  expect_true(min(res1) >= 19.9)
  expect_true(max(res1) <= 30.1)

  # Multiple days (3 x 24 = 72)
  res3 <- epicrop:::.diurnal_temp(
    lat = rep(14.6774, 3),
    doy = c(182, 183, 184),
    tmin = c(20, 21, 19),
    tmax = c(30, 31, 29)
  )
  expect_length(res3, 72)
})

# =============================================================================
# .day_length
# =============================================================================

test_that(".day_length returns reasonable daylight hours and is vectorized", {
  r1 <- epicrop:::.day_length(lat = 14.6774, doy = 182)
  expect_length(r1, 1)
  expect_gt(r1, 0)
  expect_lt(r1, 24)

  rvec <- epicrop:::.day_length(lat = rep(14.6774, 10), doy = 1:10)
  expect_length(rvec, 10)
  expect_true(all(rvec > 0 & rvec < 24))
})

test_that(".day_length varies with latitude and season", {
  # Latitude effect (around June, doy ~ 182)
  eq <- epicrop:::.day_length(lat = 0, doy = 182)
  nh <- epicrop:::.day_length(lat = 45, doy = 182)
  sh <- epicrop:::.day_length(lat = -45, doy = 182)
  expect_gt(nh, eq)
  expect_lt(sh, eq)

  # Seasonal effect at mid-latitude
  lat <- 45
  summer <- epicrop:::.day_length(lat = lat, doy = 172) # June solstice
  winter <- epicrop:::.day_length(lat = lat, doy = 355) # December solstice
  expect_gt(summer, winter)
})

# =============================================================================
# .saturated_vapour_pressure
# =============================================================================

test_that(".saturated_vapour_pressure basic properties and vectorization", {
  es1 <- epicrop:::.saturated_vapour_pressure(25)
  expect_type(es1, "double")
  expect_length(es1, 1)
  expect_gt(es1, 0)

  es_low <- epicrop:::.saturated_vapour_pressure(10)
  es_high <- epicrop:::.saturated_vapour_pressure(30)
  expect_gt(es_high, es_low)

  temps <- c(10, 20, 30)
  esv <- epicrop:::.saturated_vapour_pressure(temps)
  expect_length(esv, 3)
  expect_true(all(esv > 0))

  es0 <- epicrop:::.saturated_vapour_pressure(0)
  expect_gt(es0, 0)
  expect_lt(es0, 1)
})

# =============================================================================
# .check_threshold and .calculate_weighted_hr_wet
# =============================================================================

test_that(".check_threshold counts wet hours correctly across scenarios", {
  scenarios <- list(
    all_above = list(rh = rep(95, 24), expect = 24L),
    half_above = list(rh = c(rep(95, 12), rep(80, 12)), expect = 12L),
    none_above = list(rh = rep(70, 24), expect = 0L)
  )
  for (nm in names(scenarios)) {
    sc <- scenarios[[nm]]
    got <- epicrop:::.check_threshold(rh_hr = sc$rh, rhlim = 90)
    expect_equal(got, sc$expect)
  }
})

test_that(".calculate_weighted_hr_wet returns values in [0,1], handles edge cases, and vectorizes", {
  # Range checks and edges
  expect_gte(
    epicrop:::.calculate_weighted_hr_wet(rh = 85, rhlim = 90),
    0
  )
  expect_lte(
    epicrop:::.calculate_weighted_hr_wet(rh = 85, rhlim = 90),
    1
  )
  expect_equal(
    epicrop:::.calculate_weighted_hr_wet(rh = 95, rhlim = 90),
    1
  )
  expect_equal(
    epicrop:::.calculate_weighted_hr_wet(rh = 75, rhlim = 90),
    0
  )

  # Interpolation at midpoint (85 between 80 and 90)
  mid <- epicrop:::.calculate_weighted_hr_wet(rh = 85, rhlim = 90)
  expect_gt(mid, 0)
  expect_lt(mid, 1)
  expect_equal(mid, 0.5, tolerance = 0.01)

  # Vectorized
  rh_values <- c(75, 85, 95, 100)
  v <- epicrop:::.calculate_weighted_hr_wet(rh = rh_values, rhlim = 90)
  expect_length(v, 4)
  expect_equal(v[1], 0)
  expect_gt(v[2], 0)
  expect_equal(v[3], 1)
  expect_equal(v[4], 1)

  # Reject invalid thresholds
  expect_error(
    epicrop:::.calculate_weighted_hr_wet(rh = 85, rhlim = 75),
    regexp = "below 80"
  )
})
