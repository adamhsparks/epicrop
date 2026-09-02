test_that("seir runs with minimal valid inputs", {
  wth <- wth_formatted

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200
  )

  expect_s3_class(result, "epicrop.sim")
  expect_s3_class(result, "data.table")
  expect_identical(nrow(result), 120L)
  expect_true(all(result$sites >= 0))
  expect_true(all(result$latent >= 0))
  expect_true(all(result$infectious >= 0))
})

test_that("seir output has correct structure", {
  wth <- wth_formatted

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200
  )

  expected_cols <- c(
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
    "intensity"
  )

  expect_true(all(expected_cols %in% names(result)))
  expect_false(is.null(attr(result, "AUDPC")))
  expect_type(attr(result, "AUDPC"), "double")
})

test_that("seir handles functions for p and i", {
  wth <- wth_formatted

  p_func <- function(temp) pmax(1, 10 - 0.2 * temp)
  i_func <- function(temp) pmax(1, 20 - 0.3 * temp)

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = p_func,
    i = i_func,
    Sx = 3200
  )

  expect_s3_class(result, "epicrop.sim")
  expect_equal(nrow(result), 120)
})

test_that("seir handles RRS and RRG parameters", {
  wth <- wth_formatted

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200,
    RRS = 0.01,
    RRG = 0.1
  )

  expect_true(any(result$rsenesced > 0))
  expect_true(any(result$rgrowth > 0))
})

test_that("seir handles RRLEX parameter", {
  wth <- wth_formatted

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200,
    RRLEX = 0.01
  )

  expect_true(any(result$rlex > 0))
})

test_that("seir handles aggregation parameter a", {
  wth <- wth_formatted

  # Scalar a
  result1 <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200,
    a = 1
  )

  # Vector a
  result2 <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200,
    a = cbind(c(0, 60, 120), c(1, 2, 1))
  )

  expect_s3_class(result1, "epicrop.sim")
  expect_s3_class(result2, "epicrop.sim")
  expect_false(identical(result1$intensity, result2$intensity))
})

test_that("seir handles simple_wetness = FALSE", {
  wth <- wth_formatted

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200,
    simple_wetness = FALSE
  )

  expect_s3_class(result, "epicrop.sim")
  expect_equal(nrow(result), 120)
})

test_that("seir handles RcW parameter", {
  wth <- wth_formatted

  RcW <- cbind(c(0, 6, 12, 18, 24), c(0, 0.25, 0.5, 0.75, 1))

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200,
    simple_wetness = FALSE,
    RcW = RcW
  )

  expect_s3_class(result, "epicrop.sim")
})

test_that("seir handles weighted_wetness parameter", {
  wth <- wth_formatted

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200,
    simple_wetness = FALSE,
    weighted_wetness = TRUE
  )

  expect_s3_class(result, "epicrop.sim")
})

test_that("seir handles age_driver = 'dvs'", {
  wth <- wth_formatted

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 0.5, 1, 1.5, 2), c(0.1, 0.5, 1, 0.8, 0.4)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200,
    age_driver = "dvs",
    thermal_time = list(base_temp = 10, gdd_to_maturity = 1500)
  )

  expect_s3_class(result, "epicrop.sim")
})

test_that("seir requires thermal_time when age_driver = 'dvs'", {
  wth <- wth_formatted

  expect_error(
    seir(
      wth = wth,
      emergence = "2000-07-01",
      onset = 20,
      duration = 120,
      H0 = 600,
      I0 = 1,
      RcA = cbind(c(0, 120), c(0.35, 0.35)),
      RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
      RcOpt = 0.61,
      p = 6,
      i = 19,
      Sx = 3200,
      age_driver = "dvs"
    ),
    '`age_driver` = "dvs" requires `thermal_time`'
  )
})

test_that("seir handles infection_window parameter", {
  wth <- wth_formatted

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200,
    age_driver = "dvs",
    thermal_time = list(base_temp = 10, gdd_to_maturity = 1500),
    infection_window = list(
      dvs_start = 0.3,
      dvs_end = 1.5
    )
  )

  expect_s3_class(result, "epicrop.sim")
})

test_that("seir validates infection_window structure", {
  wth <- wth_formatted

  expect_error(
    seir(
      wth = wth,
      emergence = "2000-07-01",
      onset = 20,
      duration = 120,
      H0 = 600,
      I0 = 1,
      RcA = cbind(c(0, 120), c(0.35, 0.35)),
      RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
      RcOpt = 0.61,
      p = 6,
      i = 19,
      Sx = 3200,
      infection_window = "invalid"
    ),
    "infection_window` must have dvs_start and dvs_end"
  )

  expect_error(
    seir(
      wth = wth,
      emergence = "2000-07-01",
      onset = 20,
      duration = 120,
      H0 = 600,
      I0 = 1,
      RcA = cbind(c(0, 120), c(0.35, 0.35)),
      RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
      RcOpt = 0.61,
      p = 6,
      i = 19,
      Sx = 3200,
      infection_window = list(dvs_start = 0.5)
    ),
    "dvs_end"
  )
})

test_that("seir includes lat/lon when present in weather", {
  wth <- wth_formatted

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200
  )

  if ("LAT" %in% names(wth)) {
    expect_true("lat" %in% names(result))
  }
  if ("LON" %in% names(wth)) {
    expect_true("lon" %in% names(result))
  }
})

test_that("seir errors on insufficient weather data", {
  wth <- wth_formatted[1:50, ]

  expect_error(
    seir(
      wth = wth,
      emergence = "2000-07-01",
      onset = 20,
      duration = 120,
      H0 = 600,
      I0 = 1,
      RcA = cbind(c(0, 120), c(0.35, 0.35)),
      RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
      RcOpt = 0.61,
      p = 6,
      i = 19,
      Sx = 3200
    ),
    "Insufficient weather data"
  )
})

test_that("seir warns on short duration", {
  wth <- wth_formatted

  expect_warning(
    seir(
      wth = wth,
      emergence = "2000-07-01",
      onset = 5,
      duration = 30,
      H0 = 600,
      I0 = 1,
      RcA = cbind(c(0, 30), c(0.35, 0.35)),
      RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
      RcOpt = 0.61,
      p = 6,
      i = 19,
      Sx = 3200
    ),
    "Short simulation duration"
  )
})

test_that("seir errors on emergence not in weather range", {
  wth <- wth_formatted

  expect_error(
    seir(
      wth = wth,
      emergence = "1999-01-01",
      onset = 20,
      duration = 120,
      H0 = 600,
      I0 = 1,
      RcA = cbind(c(0, 120), c(0.35, 0.35)),
      RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
      RcOpt = 0.61,
      p = 6,
      i = 19,
      Sx = 3200
    ),
    "Emergence date not found in weather data"
  )
})

test_that("seir validates wth is epicrop.wth class", {
  wth <- data.frame(
    YYYYMMDD = seq.Date(as.Date("2000-01-01"), by = 1, length.out = 365),
    TEMP = 25,
    RHUM = 80,
    RAIN = 5
  )

  expect_error(
    seir(
      wth = wth,
      emergence = "2000-07-01",
      onset = 20,
      duration = 120,
      H0 = 600,
      I0 = 1,
      RcA = cbind(c(0, 120), c(0.35, 0.35)),
      RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
      RcOpt = 0.61,
      p = 6,
      i = 19,
      Sx = 3200
    ),
    "epicrop.wth"
  )
})

test_that("seir maintains mass balance", {
  wth <- wth_formatted

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200,
    RRG = 0.1,
    RRS = 0.01
  )

  # Check mass balance (allowing for numerical precision)
  for (i in seq_len(nrow(result))) {
    total <- result$sites[i] +
      result$latent[i] +
      result$infectious[i] +
      result$removed[i] +
      result$senesced[i]
    expect_true(is.finite(total))
    expect_true(total >= 0)
  }
})

test_that("seir calculates diseased correctly", {
  wth <- wth_formatted

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200
  )

  expected_diseased <- result$latent + result$infectious + result$removed
  expect_equal(result$diseased, expected_diseased)
})

test_that("seir calculates intensity correctly", {
  wth <- wth_formatted

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200
  )

  living <- result$sites + result$latent + result$infectious
  expected_intensity <- ifelse(
    living > 0,
    (result$latent + result$infectious) / living,
    0
  )

  expect_equal(result$intensity, expected_intensity)
})

test_that(".calculate_dvs works correctly", {
  wth <- data.table::data.table(
    TEMP = c(15, 20, 25, 20, 15)
  )
  thermal_time <- list(base_temp = 10, gdd_to_maturity = 100)

  dvs <- .calculate_dvs(wth, thermal_time)

  expect_length(dvs, 5)
  expect_true(all(dvs >= 0))
  expect_true(all(dvs <= 2))
  expect_true(all(diff(dvs) >= 0)) # Should be monotonically increasing
})

# ---- .calculate_dvs input validation ---------------------------------------

test_that(".calculate_dvs errors on invalid weather/thermal_time inputs", {
  good_wth <- data.table::data.table(TEMP = c(15, 20, 25))
  tt <- list(base_temp = 10, gdd_to_maturity = 100)

  expect_error(.calculate_dvs(NULL, tt), "TEMP")

  expect_error(
    .calculate_dvs(data.table::data.table(x = 1:3), tt),
    "TEMP"
  )

  bad_temp <- data.table::data.table(TEMP = c(15, NA, 25))
  expect_error(.calculate_dvs(bad_temp, tt), "NA")

  expect_error(
    .calculate_dvs(
      good_wth,
      thermal_time = list(gdd_to_maturity = 100)
    ),
    "base_temp"
  )

  expect_error(
    .calculate_dvs(good_wth, thermal_time = list(base_temp = 10)),
    "gdd_to_maturity"
  )

  expect_error(
    .calculate_dvs(
      good_wth,
      thermal_time = list(base_temp = 10, gdd_to_maturity = -5)
    ),
    "gdd_to_maturity"
  )

  expect_error(
    .calculate_dvs(good_wth, thermal_time = "not_a_list"),
    "thermal_time"
  )
})

# ---- onset validation and boundaries ---------------------------------------

test_that("seir errors when onset is out of range", {
  wth <- wth_formatted
  args <- list(
    wth = wth,
    emergence = "2000-07-01",
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200
  )

  expect_error(rlang::exec(seir, !!!args, onset = 0), "onset")
  expect_error(rlang::exec(seir, !!!args, onset = 121), "onset")
})

test_that("seir accepts onset at the first and last day of the simulation", {
  wth <- wth_formatted
  args <- list(
    wth = wth,
    emergence = "2000-07-01",
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200
  )

  first <- rlang::exec(seir, !!!args, onset = 1)
  expect_equal(first$rateinf[1], 1)

  last <- rlang::exec(seir, !!!args, onset = 120)
  expect_equal(last$rateinf[120], 1)
})

# ---- duration warning boundary ---------------------------------------------

test_that("seir duration warning boundary: 49 warns, 50 does not", {
  wth <- wth_formatted
  make_args <- function(duration) {
    list(
      wth = wth,
      emergence = "2000-07-01",
      onset = 5,
      duration = duration,
      H0 = 600,
      I0 = 1,
      RcA = cbind(c(0, duration), c(0.35, 0.35)),
      RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
      RcOpt = 0.61,
      p = 6,
      i = 19,
      Sx = 3200
    )
  }

  expect_warning(
    rlang::exec(seir, !!!make_args(49)),
    "Short simulation duration"
  )

  expect_no_warning(rlang::exec(seir, !!!make_args(50)))
})

# ---- RRLEX: fast path, table, and function forms ---------------------------

test_that("seir RRLEX = 0 is identical to RRLEX = NULL (fast path)", {
  wth <- wth_formatted
  args <- list(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200
  )

  result_null <- rlang::exec(seir, !!!args, RRLEX = NULL)
  result_zero <- rlang::exec(seir, !!!args, RRLEX = 0)

  expect_identical(result_null, result_zero)
  expect_true(all(result_zero$rlex == 0))
})

test_that("seir accepts RRLEX as an age-dependent table", {
  wth <- wth_formatted

  RRLEX_table <- cbind(c(0, 60, 120), c(0, 0.05, 0.1))

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200,
    RRLEX = RRLEX_table
  )

  expect_s3_class(result, "epicrop.sim")
  expect_true(any(result$rlex > 0))
})

test_that("seir accepts RRLEX as a function of temperature", {
  wth <- wth_formatted

  RRLEX_fn <- function(temp) pmax(0, (temp - 20) * 0.01)

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200,
    RRLEX = RRLEX_fn
  )

  expect_s3_class(result, "epicrop.sim")
})

# ---- aggregation parameter a: clamp and function form ----------------------

test_that("seir clamps aggregation parameter a to a minimum of 1", {
  wth <- wth_formatted
  args <- list(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200
  )

  result_low <- rlang::exec(seir, !!!args, a = 0.5)
  result_one <- rlang::exec(seir, !!!args, a = 1)

  expect_identical(result_low, result_one)
})

test_that("seir accepts a as a function of age/DVS", {
  wth <- wth_formatted

  a_fn <- function(age) 1 + (age / max(age))

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200,
    a = a_fn
  )

  expect_s3_class(result, "epicrop.sim")
})

# ---- RcW / weighted_wetness ignored under simple_wetness = TRUE -----------

test_that("seir ignores RcW when simple_wetness = TRUE (default)", {
  wth <- wth_formatted
  args <- list(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200
  )

  RcW <- cbind(c(0, 6, 12, 18, 24), c(0, 0.25, 0.5, 0.75, 1))

  result_no_RcW <- rlang::exec(seir, !!!args, simple_wetness = TRUE)
  result_with_RcW <- rlang::exec(
    seir,
    !!!args,
    simple_wetness = TRUE,
    RcW = RcW
  )

  expect_identical(result_no_RcW, result_with_RcW)
})

test_that("seir ignores weighted_wetness when simple_wetness = TRUE", {
  wth <- wth_formatted
  args <- list(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200,
    simple_wetness = TRUE
  )

  result_unweighted <- rlang::exec(seir, !!!args, weighted_wetness = FALSE)
  result_weighted <- rlang::exec(seir, !!!args, weighted_wetness = TRUE)

  expect_identical(result_unweighted, result_weighted)
})

# ---- p and i as vectors -----------------------------------------------------

test_that("seir accepts p and i as vectors matching duration", {
  wth <- wth_formatted
  duration <- 120

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = duration,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = rep(6, duration),
    i = rep(19, duration),
    Sx = 3200
  )

  expect_s3_class(result, "epicrop.sim")
  expect_equal(nrow(result), duration)
})

test_that("seir errors when p/i vector length does not match duration", {
  wth <- wth_formatted

  expect_error(
    seir(
      wth = wth,
      emergence = "2000-07-01",
      onset = 20,
      duration = 120,
      H0 = 600,
      I0 = 1,
      RcA = cbind(c(0, 120), c(0.35, 0.35)),
      RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
      RcOpt = 0.61,
      p = rep(6, 50),
      i = 19,
      Sx = 3200
    ),
    "length mismatch"
  )
})

# ---- default growth/senescence behaviour -----------------------------------

test_that("seir produces zero growth when RRG is NULL (default)", {
  wth <- wth_formatted

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200
  )

  expect_true(all(result$rgrowth == 0))
})

test_that("seir senescence reflects disease-removal flow even when RRS is NULL", {
  wth <- wth_formatted

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200
  )

  # RRS = NULL disables *physiological* senescence, but sites that finish
  # their infectious period (p + i days after infection) still flow into
  # `rsenesced` via the core loop, so this is not identically zero once
  # the infectious period has elapsed within the simulation window.
  expect_true(any(result$rsenesced > 0))
})

# ---- missing required arguments --------------------------------------------

test_that("seir errors when required arguments RcA/RcT/RcOpt are missing", {
  wth <- wth_formatted
  base <- list(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    p = 6,
    i = 19,
    Sx = 3200
  )

  expect_error(
    rlang::exec(
      seir,
      !!!base,
      RcT = cbind(c(15, 35), c(0, 1)),
      RcOpt = 0.61
    )
  )
  expect_error(
    rlang::exec(
      seir,
      !!!base,
      RcA = cbind(c(0, 120), c(0.35, 0.35)),
      RcOpt = 0.61
    )
  )
  expect_error(
    rlang::exec(
      seir,
      !!!base,
      RcA = cbind(c(0, 120), c(0.35, 0.35)),
      RcT = cbind(c(15, 35), c(0, 1))
    )
  )
})

test_that("seir errors for an invalid age_driver value", {
  wth <- wth_formatted

  expect_error(
    seir(
      wth = wth,
      emergence = "2000-07-01",
      onset = 20,
      duration = 120,
      H0 = 600,
      I0 = 1,
      RcA = cbind(c(0, 120), c(0.35, 0.35)),
      RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
      RcOpt = 0.61,
      p = 6,
      i = 19,
      Sx = 3200,
      age_driver = "week"
    )
  )
})

# ---- dates column fallback --------------------------------------------------

test_that("seir falls back to YYYYMMDD when wth lacks a dates column", {
  wth_no_dates <- data.table::copy(wth_formatted)
  if ("dates" %in% names(wth_no_dates)) {
    wth_no_dates[, dates := NULL]
  }

  result <- seir(
    wth = wth_no_dates,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200
  )

  expect_s3_class(result, "epicrop.sim")
  expect_false(anyNA(result$dates))
})

# ---- infection_window: thermal_time fallback and actual restriction -------

test_that("seir infection_window derives thermal_time from itself when age_driver = 'day'", {
  wth <- wth_formatted

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200,
    age_driver = "day",
    infection_window = list(
      dvs_start = 0.3,
      dvs_end = 1.5,
      base_temp = 10,
      gdd_to_maturity = 1500
    )
  )

  expect_s3_class(result, "epicrop.sim")
})

test_that("seir errors when infection_window lacks base_temp/gdd_to_maturity and no thermal_time is given", {
  wth <- wth_formatted

  expect_error(
    seir(
      wth = wth,
      emergence = "2000-07-01",
      onset = 20,
      duration = 120,
      H0 = 600,
      I0 = 1,
      RcA = cbind(c(0, 120), c(0.35, 0.35)),
      RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
      RcOpt = 0.61,
      p = 6,
      i = 19,
      Sx = 3200,
      age_driver = "day",
      infection_window = list(dvs_start = 0.3, dvs_end = 1.5)
    ),
    "base_temp"
  )
})

test_that("seir infection_window restricts infection to the specified DVS range", {
  wth <- wth_formatted

  base_args <- list(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 2), c(1, 1)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200,
    age_driver = "dvs",
    thermal_time = list(base_temp = 10, gdd_to_maturity = 1500)
  )

  wide <- rlang::exec(
    seir,
    !!!base_args,
    infection_window = list(dvs_start = 0, dvs_end = 2)
  )
  narrow <- rlang::exec(
    seir,
    !!!base_args,
    infection_window = list(dvs_start = 0.05, dvs_end = 0.1)
  )

  expect_lte(attr(narrow, "AUDPC"), attr(wide, "AUDPC"))
})

# ---- exact rate-equation conservation ---------------------------------------

test_that("seir sites obey the exact rate equation (no lesion expansion)", {
  wth <- wth_formatted

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200,
    RRG = 0.1,
    RRS = 0.01
  )

  for (t in 2:nrow(result)) {
    expected_sites <- max(
      0,
      result$sites[t - 1] +
        result$rgrowth[t - 1] -
        result$rateinf[t - 1] -
        result$rsenesced[t - 1]
    )
    expect_equal(result$sites[t], expected_sites, tolerance = 1e-8)
  }
})

test_that("seir sites obey the exact rate equation (with lesion expansion)", {
  wth <- wth_formatted

  result <- seir(
    wth = wth,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 600,
    I0 = 1,
    RcA = cbind(c(0, 120), c(0.35, 0.35)),
    RcT = cbind(c(15, 20, 25, 30, 35), c(0, 0.06, 1.0, 0.85, 0.16)),
    RcOpt = 0.61,
    p = 6,
    i = 19,
    Sx = 3200,
    RRG = 0.1,
    RRS = 0.01,
    RRLEX = 0.02
  )

  for (t in 2:nrow(result)) {
    expected_sites <- max(
      0,
      result$sites[t - 1] +
        result$rgrowth[t - 1] -
        result$rateinf[t - 1] -
        result$rsenesced[t - 1] -
        result$rlex[t - 1]
    )
    expect_equal(result$sites[t], expected_sites, tolerance = 1e-8)
  }
})
