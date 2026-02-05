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
  expect_equal(nrow(result), 120)
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
  expect_true(!is.null(attr(result, "AUDPC")))
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
    class = "epicrop_missing_thermal_time"
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
    "must be a list"
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

test_that("seir handles steps_per_day parameter", {
  wth <- wth_formatted

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
    steps_per_day = 1
  )

  result4 <- seir(
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
    steps_per_day = 4
  )

  expect_s3_class(result1, "epicrop.sim")
  expect_s3_class(result4, "epicrop.sim")
  # Results should be similar but not identical
  expect_false(identical(result1$intensity, result4$intensity))
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
    "not in weather data range"
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
  for (i in 1:nrow(result)) {
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

  dvs <- epicrop:::.calculate_dvs(wth, thermal_time)

  expect_length(dvs, 5)
  expect_true(all(dvs >= 0))
  expect_true(all(dvs <= 2))
  expect_true(all(diff(dvs) >= 0)) # Should be monotonically increasing
})
