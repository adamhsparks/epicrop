test_that("epicrop_sim runs successfully with valid inputs", {
  RcA <- cbind(
    c(0L, 30L, 60L, 90L, 120L),
    c(1.0, 0.9, 0.7, 0.5, 0.4)
  )

  RcT <- cbind(
    c(16L, 22L, 28L, 34L, 40L),
    c(0.0, 0.5, 1.0, 0.5, 0.0)
  )

  result <- epicrop_sim(
    wth = wth_formatted,
    emergence = "2000-07-01",
    onset = 20,
    duration = 120,
    H0 = 100,
    I0 = 1,
    RcA = RcA,
    RcT = RcT,
    RcOpt = 0.87,
    p = 5,
    i = 30,
    Sx = 3200,
    RRS = 0.01,
    RRG = 0.1
  )

  expect_s3_class(result, "epicrop.sim")
  expect_identical(nrow(result), 120L)
})

test_that("validation rejects negative/invalid parameters", {
  RcA <- cbind(
    c(0L, 30L, 60L, 90L, 120L),
    c(1.0, 0.9, 0.7, 0.5, 0.4)
  )

  RcT <- cbind(
    c(16L, 22L, 28L, 34L, 40L),
    c(0.0, 0.5, 1.0, 0.5, 0.0)
  )

  # Must use epicrop_sim directly to test parameter validation
  expect_error(
    epicrop_sim(
      wth = wth_formatted,
      emergence = "2000-07-01",
      onset = 20,
      duration = 120,
      H0 = -10, # Invalid
      I0 = 1,
      RcA = RcA,
      RcT = RcT,
      RcOpt = 0.87,
      p = 5,
      i = 30,
      Sx = 3200,
      RRS = 0.01,
      RRG = 0.1,
      a = 4
    ),
    regexp = "H0"
  )

  expect_error(
    epicrop_sim(
      wth = wth_formatted,
      emergence = "2000-07-01",
      onset = 20,
      duration = 120,
      H0 = 100,
      I0 = 1,
      RcA = RcA,
      RcT = RcT,
      RcOpt = 0.87,
      p = 5,
      i = 30,
      Sx = 3200,
      RRS = 0.01,
      RRG = 0.1,
      a = 0.5 # Invalid - must be >= 1
    ),
    regexp = "a.*1"
  )
})

test_that("validation warns about edge cases", {
  RcA <- cbind(
    c(0L, 10L, 20L, 30L, 40L),
    c(1.0, 0.9, 0.7, 0.5, 0.4)
  )

  RcT <- cbind(
    c(16L, 22L, 28L, 34L, 40L),
    c(0.0, 0.5, 1.0, 0.5, 0.0)
  )

  wth_short <- wth_formatted[1:45, ]

  # Must use epicrop_sim directly to test duration warnings
  expect_warning(
    epicrop_sim(
      wth = wth_short,
      emergence = "2000-07-01",
      onset = 20,
      duration = 45, # Short duration
      H0 = 100,
      I0 = 1,
      RcA = RcA,
      RcT = RcT,
      RcOpt = 0.87,
      p = 5,
      i = 30,
      Sx = 3200,
      RRS = 0.01,
      RRG = 0.1,
      a = 4
    ),
    regexp = "duration.*50"
  )
})

test_that("validation rejects simple_wetness=FALSE without TMIN/TMAX", {
  wth_no_temp <- data.frame(
    YYYYMMDD = seq(as.Date("2000-07-01"), by = "day", length.out = 120),
    DOY = 182:301,
    TEMP = rnorm(120, 25, 3),
    RHUM = runif(120, 70, 95),
    RAIN = rpois(120, 5)
  )

  data.table::setDT(wth_no_temp)
  class(wth_no_temp) <- c("epicrop.wth", "data.table", "data.frame")

  RcA <- cbind(
    c(0L, 30L, 60L, 90L, 120L),
    c(1.0, 0.9, 0.7, 0.5, 0.4)
  )

  RcT <- cbind(
    c(16L, 22L, 28L, 34L, 40L),
    c(0.0, 0.5, 1.0, 0.5, 0.0)
  )

  # Must use epicrop_sim directly to test simple_wetness = FALSE
  expect_error(
    epicrop_sim(
      wth = wth_no_temp,
      emergence = "2000-07-01",
      onset = 20,
      duration = 120,
      H0 = 100,
      I0 = 1,
      RcA = RcA,
      RcT = RcT,
      RcOpt = 0.87,
      p = 5,
      i = 30,
      Sx = 3200,
      RRS = 0.01,
      RRG = 0.1,
      a = 4,
      simple_wetness = FALSE # Requires TMIN/TMAX
    ),
    regexp = "TMIN.*TMAX"
  )
})

test_that("disease model wrappers work correctly", {
  # Test that disease model functions run without errors
  result <- bacterial_blight(wth_formatted, emergence = "2000-07-01")

  expect_s3_class(result, "epicrop.sim")
  expect_identical(nrow(result), 120L)
  expect_gt(attr(result, "AUDPC"), 0)
})
