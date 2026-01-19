test_that("validation rejects invalid weather object", {
  wth_bad <- data.frame(
    YYYYMMDD = seq(as.Date("2000-07-01"), by = "day", length.out = 120),
    TEMP = rnorm(120, 25, 3),
    RAIN = rpois(120, 5)
  )

  expect_error(
    bacterial_blight(wth_bad, emergence = "2000-07-01"),
    regexp = "epicrop.wth"
  )
})

test_that("validation rejects invalid date formats", {
  expect_error(
    bacterial_blight(wth_formatted, emergence = "not-a-date"),
    regexp = "valid date format"
  )
})

test_that("validation rejects negative/invalid parameters via epicrop_sim", {
  # Must use epicrop_sim directly to test all parameter validation
  RcA <- cbind(
    c(0L, 30L, 60L, 90L, 120L),
    c(1.0, 0.9, 0.7, 0.5, 0.4)
  )

  RcT <- cbind(
    c(16L, 19L, 22L, 25L, 28L, 31L, 34L, 37L, 40L),
    c(0.0, 0.29, 0.44, 0.90, 0.90, 1.0, 0.88, 0.01, 0.0)
  )

  # Test H0
  expect_error(
    epicrop_sim(
      wth = wth_formatted,
      emergence = "2000-07-01",
      onset = 20,
      duration = 120,
      H0 = -10,
      I0 = 1,
      RcA = RcA,
      RcT = RcT,
      RcOpt = 0.87,
      p = 5,
      i = 30,
      Sx = 3200,
      RRS = 0.01,
      RRG = 0.1
    ),
    regexp = "H0"
  )

  # Test RcOpt
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
      RcOpt = -0.5,
      p = 5,
      i = 30,
      Sx = 3200,
      RRS = 0.01,
      RRG = 0.1
    ),
    regexp = "RcOpt"
  )

  # Test Sx
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
      Sx = -100,
      RRS = 0.01,
      RRG = 0.1
    ),
    regexp = "Sx"
  )

  # Test a
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
      a = 0.5,
      RRS = 0.01,
      RRG = 0.1
    ),
    regexp = "a.*1"
  )

  # Test onset
  expect_error(
    epicrop_sim(
      wth = wth_formatted,
      emergence = "2000-07-01",
      onset = 0,
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
    ),
    regexp = "onset"
  )

  # Test duration
  expect_error(
    epicrop_sim(
      wth = wth_formatted,
      emergence = "2000-07-01",
      onset = 20,
      duration = 0,
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
    ),
    regexp = "duration"
  )

  # Test rhlim
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
      rhlim = 150
    ),
    regexp = "rhlim"
  )
})

test_that("validation warns about edge cases", {
  # Test short duration warning
  RcA_short <- cbind(
    c(0L, 10L, 20L, 30L, 40L),
    c(1.0, 1.0, 0.9, 0.62, 0.43)
  )

  RcT <- cbind(
    c(16L, 19L, 22L, 25L, 28L, 31L, 34L, 37L, 40L),
    c(0.0, 0.29, 0.44, 0.90, 0.90, 1.0, 0.88, 0.01, 0.0)
  )

  wth_short <- wth_formatted[1:40, ]

  # Capture all warnings
  warnings_caught <- NULL
  result <- withCallingHandlers(
    epicrop_sim(
      wth = wth_short,
      emergence = "2000-07-01",
      onset = 20,
      duration = 40,
      H0 = 100,
      I0 = 1,
      RcA = RcA_short,
      RcT = RcT,
      RcOpt = 0.87,
      p = 5,
      i = 30,
      Sx = 3200,
      RRS = 0.01,
      RRG = 0.1,
      steps_per_day = 1L
    ),
    warning = function(w) {
      warnings_caught <<- c(warnings_caught, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # Check that duration warning was issued
  expect_true(any(grepl("duration.*50", warnings_caught)))

  # Test non-power-of-2 steps_per_day warning
  RcA_full <- cbind(
    c(0L, 30L, 60L, 90L, 120L),
    c(1.0, 0.9, 0.7, 0.5, 0.4)
  )

  warnings_caught2 <- NULL
  result2 <- withCallingHandlers(
    epicrop_sim(
      wth = wth_formatted,
      emergence = "2000-07-01",
      onset = 20,
      duration = 120,
      H0 = 100,
      I0 = 1,
      RcA = RcA_full,
      RcT = RcT,
      RcOpt = 0.87,
      p = 5,
      i = 30,
      Sx = 3200,
      RRS = 0.01,
      RRG = 0.1,
      steps_per_day = 3L
    ),
    warning = function(w) {
      warnings_caught2 <<- c(warnings_caught2, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )

  # Check that steps_per_day warning was issued
  expect_true(any(grepl("power of 2", warnings_caught2)))
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
    c(16L, 19L, 22L, 25L, 28L, 31L, 34L, 37L, 40L),
    c(0.0, 0.29, 0.44, 0.90, 0.90, 1.0, 0.88, 0.01, 0.0)
  )

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
      simple_wetness = FALSE
    ),
    regexp = "TMIN.*TMAX"
  )
})

test_that(".check_date handles various date formats", {
  expect_s3_class(.check_date("2000-07-01"), "POSIXct")
  expect_s3_class(.check_date("01-07-2000"), "POSIXct")
  expect_s3_class(.check_date("July 1, 2000"), "POSIXct")
})

test_that(".is_boolean correctly identifies booleans", {
  expect_true(.is_boolean(TRUE))
  expect_true(.is_boolean(FALSE))
  expect_false(.is_boolean(1))
  expect_false(.is_boolean(c(TRUE, FALSE)))
})
