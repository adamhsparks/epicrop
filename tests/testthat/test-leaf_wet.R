no_weight_default <- .calculate_leaf_wetness(
  weighted_wetness = FALSE,
  wth = wth_formatted,
  rhlim = 90.0,
  rainlim = 5.0
)

no_weight_low_limit <- .calculate_leaf_wetness(
  weighted_wetness = FALSE,
  wth = wth_formatted,
  rhlim = 80.0,
  rainlim = 1.0
)

no_weight_high_limit <- .calculate_leaf_wetness(
  weighted_wetness = FALSE,
  wth = wth_formatted,
  rhlim = 95.0,
  rainlim = 10.0
)

weighted_default <- .calculate_leaf_wetness(
  weighted_wetness = TRUE,
  wth = wth_formatted,
  rhlim = 90.0,
  rainlim = 5.0
)

test_that("a vector of daily summed hourly values is returned w/ no weights", {
  expect_length(no_weight_default, 185L)
  expect_equal(no_weight_default[[1L]], 13.101, tolerance = 0.1)
})

test_that("Values increase as limits decrease w/ no weights", {
  expect_length(no_weight_low_limit, 185L)
  expect_equal(no_weight_low_limit[[1L]], 20.7250, tolerance = 0.1)
  expect_gt(no_weight_low_limit[[1L]], no_weight_default[[1L]])
})


test_that("Values decrease as limits increase w/ no weights", {
  expect_length(no_weight_high_limit, 185L)
  expect_equal(no_weight_high_limit[[1L]], 9.688125, tolerance = 0.1)
  expect_gt(no_weight_default[[1L]], no_weight_high_limit[[1L]])
})

test_that("weighted_wetness returns plausible values", {
  expect_length(weighted_default, 185L)
})

test_that("weighted_wetness stops if `rhlim < 80`", {
  expect_snapshot(
    .calculate_leaf_wetness(
      weighted_wetness = TRUE,
      wth = wth_formatted,
      rhlim = 70.0,
      rainlim = 5.0
    ),
    error = TRUE
  )
})

test_that(".calculate_leaf_wetness() handles missing RHUM", {
  wth_bad <- data.table::copy(wth_formatted)
  wth_bad[, RHUM := NULL]

  expect_error(
    .calculate_leaf_wetness(
      wth = wth_bad,
      rhlim = 90.0,
      rainlim = 5.0,
      weighted_wetness = FALSE
    ),
    regexp = "RHUM.*missing"
  )
})

test_that(".calculate_leaf_wetness() handles missing DOY", {
  wth_bad <- data.table::copy(wth_formatted)
  wth_bad[, DOY := NULL]

  expect_error(
    .calculate_leaf_wetness(
      wth = wth_bad,
      rhlim = 90.0,
      rainlim = 5.0,
      weighted_wetness = FALSE
    ),
    regexp = "DOY.*missing"
  )
})

test_that(".calculate_leaf_wetness() falls back to simple when TMIN/TMAX missing", {
  wth_simple <- data.table::copy(wth_formatted)
  wth_simple[, c("TMIN", "TMAX") := NULL]

  result <- .calculate_leaf_wetness(
    wth = wth_simple,
    rhlim = 90.0,
    rainlim = 5.0,
    weighted_wetness = FALSE
  )

  # Should only return 0 or 24 (binary wetness)
  expect_true(all(result %in% c(0, 24)))
})

test_that(".calculate_leaf_wetness() handles NA rainfall gracefully", {
  wth_na_rain <- data.table::copy(wth_formatted)
  wth_na_rain[1:5, RAIN := NA]

  result <- .calculate_leaf_wetness(
    wth = wth_na_rain,
    rhlim = 90.0,
    rainlim = 5.0,
    weighted_wetness = FALSE
  )

  expect_length(result, nrow(wth_na_rain))
  expect_false(anyNA(result))
})

test_that(".calculate_leaf_wetness() enforces 0-24 hour bounds", {
  # Create extreme conditions that might try to exceed bounds
  wth_extreme <- data.table::copy(wth_formatted)
  wth_extreme[, RAIN := 500.0] # Extreme rainfall
  wth_extreme[, RHUM := 100.0] # Max RH

  result <- .calculate_leaf_wetness(
    wth = wth_extreme,
    rhlim = 90.0,
    rainlim = 5.0,
    weighted_wetness = FALSE
  )

  expect_true(all(result >= 0))
  expect_true(all(result <= 24))
})

test_that(".calculate_leaf_wetness() handles single-row weather data", {
  wth_single <- wth_formatted[1L, ]

  result <- .calculate_leaf_wetness(
    wth = wth_single,
    rhlim = 90.0,
    rainlim = 5.0,
    weighted_wetness = FALSE
  )

  expect_length(result, 1L)
  expect_true(result >= 0 && result <= 24)
})

test_that("weighted wetness produces fractional values between 0 and 24", {
  expect_true(all(weighted_default >= 0))
  expect_true(all(weighted_default <= 24))

  # Should have some fractional values (not all integers)
  expect_false(all(weighted_default %% 1 == 0))
})

test_that("rain contribution scales correctly with rainlim", {
  wth_rain <- data.table::copy(wth_formatted)
  wth_rain[, RHUM := 70.0] # Below threshold
  wth_rain[, RAIN := c(5.0, 10.0, 15.0, rep(0, .N - 3))]

  result_low <- .calculate_leaf_wetness(
    wth = wth_rain,
    rhlim = 90.0,
    rainlim = 10.0,
    weighted_wetness = FALSE
  )

  result_high <- .calculate_leaf_wetness(
    wth = wth_rain,
    rhlim = 90.0,
    rainlim = 5.0,
    weighted_wetness = FALSE
  )

  # Lower rainlim should give more wet hours
  expect_gt(sum(result_high[1:3]), sum(result_low[1:3]))
})

## Leaf Wet Helper Functions ----

test_that(".diurnal_temp() produces 24 hourly values", {
  temps <- .diurnal_temp(
    lat = 14.6774,
    doy = 200L,
    tmin = 22.0,
    tmax = 32.0
  )

  expect_length(temps, 24L)
  expect_gte(min(temps), 22.0)
  expect_lte(max(temps), 32.0)
})

test_that(".diurnal_rh() produces list of 24-hour vectors", {
  rh_hr <- .diurnal_rh(
    rh = wth_formatted$RHUM[1:3],
    tmin = wth_formatted$TMIN[1:3],
    tmax = wth_formatted$TMAX[1:3],
    tmp = wth_formatted$TEMP[1:3],
    lat = rep(14.6774, 3),
    doy = wth_formatted$DOY[1:3],
    yyyymmdd = wth_formatted$YYYYMMDD[1:3]
  )

  expect_length(rh_hr, 3L)
  expect_length(rh_hr[[1L]], 24L)
  expect_true(all(sapply(rh_hr, function(x) all(x >= 0 & x <= 100))))
})

test_that(".day_length() returns reasonable values", {
  # Tropical location
  dl_tropical <- .day_length(lat = 0, doy = 180L)
  expect_true(dl_tropical >= 11 && dl_tropical <= 13)

  # High latitude summer
  dl_arctic <- .day_length(lat = 70, doy = 180L)
  expect_gt(dl_arctic, 18)
})

test_that(".saturated_vapour_pressure() matches expected values", {
  # At 0°C
  svp_0 <- .saturated_vapour_pressure(0)
  expect_equal(svp_0, 0.61094, tolerance = 0.001)

  # At 25°C (should be ~3. 17 kPa)
  svp_25 <- .saturated_vapour_pressure(25)
  expect_equal(svp_25, 3.17, tolerance = 0.1)
})

test_that(".check_threshold() counts hours correctly", {
  rh_all_high <- rep(95, 24)
  rh_half_high <- c(rep(95, 12), rep(85, 12))

  expect_identical(.check_threshold(rh_all_high, rhlim = 90), 24L)
  expect_identical(.check_threshold(rh_half_high, rhlim = 90), 12L)
})
