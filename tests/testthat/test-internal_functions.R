RcA <- cbind(
  c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L, 100L, 110L, 120L),
  c(
    1.0,
    1.0,
    1.0,
    0.9,
    0.62,
    0.43,
    0.42,
    0.42,
    0.41,
    0.41,
    0.41,
    0.41,
    0.41
  )
)
RcT <- cbind(
  c(16L, 19L, 22L, 25L, 28L, 31L, 34L, 37L, 40L),
  c(0.0, 0.29, 0.44, 0.90, 0.90, 1.0, 0.88, 0.01, 0.0)
)

test_that("non numeric values are not accepted", {
  var_args <- list(
    "wth" = wth_formatted,
    "emergence" = "1999-07-01",
    "onset" = "twenty",
    "duration" = 120,
    "rhlim" = 90,
    "rainlim" = 5,
    "RcA" = RcA,
    "RcT" = RcT,
    "I0" = 1,
    "Rcopt" = 0.61,
    "p" = 6,
    "i" = 19,
    "H0" = 600,
    "a" = 1,
    "Sx" = 100000,
    "RRS" = 0.01,
    "RRG" = 0.1,
    "RcW" = NULL,
    "simple_wetness" = TRUE
  )

  expect_snapshot(
    .check_inputs(var_args),
    error = TRUE
  )
  rm(var_args)
})

test_that("Formatting fails if weather are not daily values", {
  var_args <- list(
    "wth" = wth_formatted,
    "emergence" = "1999-07-01",
    "onset" = 20,
    "duration" = 120,
    "rhlim" = 90,
    "rainlim" = 5,
    "RcA" = RcA,
    "RcT" = RcT,
    "I0" = 1,
    "Rcopt" = 0.61,
    "p" = 6,
    "i" = 19,
    "H0" = 600,
    "a" = 1,
    "Sx" = 100000,
    "RRS" = 0.01,
    "RRG" = 0.1,
    "RcW" = NULL,
    "simple_wetness" = "no"
  )

  expect_snapshot(
    .check_inputs(var_args),
    error = TRUE
  )
  rm(var_args)
})

test_that("Failure if `wth` is not of class `epicrop.wth`", {
  var_args <- list(
    "wth" = wth,
    "emergence" = "1999-07-01",
    "onset" = 20,
    "duration" = 120,
    "rhlim" = 90,
    "rainlim" = 5,
    "RcA" = RcA,
    "RcT" = RcT,
    "I0" = 1,
    "Rcopt" = 0.61,
    "p" = 6,
    "i" = 19,
    "H0" = 600,
    "a" = 1,
    "Sx" = 100000,
    "RRS" = 0.01,
    "RRG" = 0.1,
    "RcW" = NULL,
    "simple_wetness" = TRUE
  )

  expect_snapshot(
    .check_inputs(var_args),
    error = TRUE
  )
  rm(var_args)
})


test_that("Failure if `duration` is <50", {
  var_args <- list(
    "wth" = wth_formatted,
    "emergence" = "1999-07-01",
    "onset" = 20,
    "duration" = 45,
    "rhlim" = 90,
    "rainlim" = 5,
    "RcA" = RcA,
    "RcT" = RcT,
    "I0" = 1,
    "Rcopt" = 0.61,
    "p" = 6,
    "i" = 19,
    "H0" = 600,
    "a" = 1,
    "Sx" = 100000,
    "RRS" = 0.01,
    "RRG" = 0.1,
    "RcW" = NULL,
    "simple_wetness" = TRUE
  )

  expect_snapshot(
    .check_inputs(var_args),
    error = TRUE
  )
  rm(var_args)
})


test_that("Failure if `rhlim` is >100", {
  var_args <- list(
    "wth" = wth_formatted,
    "emergence" = "1999-07-01",
    "onset" = 20,
    "duration" = 120,
    "rhlim" = 190,
    "rainlim" = 5,
    "RcA" = RcA,
    "RcT" = RcT,
    "I0" = 1,
    "Rcopt" = 0.61,
    "p" = 6,
    "i" = 19,
    "H0" = 600,
    "a" = 1,
    "Sx" = 100000,
    "RRS" = 0.01,
    "RRG" = 0.1,
    "RcW" = NULL,
    "simple_wetness" = TRUE
  )

  expect_snapshot(
    .check_inputs(var_args),
    error = TRUE
  )
  rm(var_args)
})

test_that("Warning if `duration` is > max RcA days", {
  var_args <- list(
    "wth" = wth_formatted,
    "emergence" = "1999-07-01",
    "onset" = 20,
    "duration" = 140,
    "rhlim" = 90,
    "rainlim" = 5,
    "RcA" = RcA,
    "RcT" = RcT,
    "I0" = 1,
    "Rcopt" = 0.61,
    "p" = 6,
    "i" = 19,
    "H0" = 600,
    "a" = 1,
    "Sx" = 100000,
    "RRS" = 0.01,
    "RRG" = 0.1,
    "RcW" = NULL,
    "simple_wetness" = TRUE
  )

  expect_snapshot(
    .check_inputs(var_args)
  )
  rm(var_args)
})


test_that("Warning if `duration` is < max RcA days", {
  var_args <- list(
    "wth" = wth_formatted,
    "emergence" = "1999-07-01",
    "onset" = 20,
    "duration" = 100,
    "rhlim" = 90,
    "rainlim" = 5,
    "RcA" = RcA,
    "RcT" = RcT,
    "I0" = 1,
    "Rcopt" = 0.61,
    "p" = 6,
    "i" = 19,
    "H0" = 600,
    "a" = 1,
    "Sx" = 100000,
    "RRS" = 0.01,
    "RRG" = 0.1,
    "RcW" = NULL,
    "simple_wetness" = TRUE
  )

  expect_snapshot(
    .check_inputs(var_args),
    error = TRUE
  )
  rm(var_args)
})


test_that("Failure if TMIN/TMAX not supplied with `simple_wetness = FALSE`", {
  wth2 <- wth_formatted[, -c("TMIN", "TMAX")]
  var_args <- list(
    "wth" = wth2,
    "emergence" = "1999-07-01",
    "onset" = 20,
    "duration" = 120,
    "rhlim" = 90,
    "rainlim" = 5,
    "RcA" = RcA,
    "RcT" = RcT,
    "I0" = 1,
    "Rcopt" = 0.61,
    "p" = 6,
    "i" = 19,
    "H0" = 600,
    "a" = 1,
    "Sx" = 100000,
    "RRS" = 0.01,
    "RRG" = 0.1,
    "RcW" = NULL,
    "simple_wetness" = FALSE
  )

  expect_snapshot(
    .check_inputs(var_args),
    error = TRUE
  )
  rm(wth2, var_args)
})

# .check_date()

test_that("Failure if object should be date but cannot be formatted as such", {
  expect_snapshot(.check_date(x = "string"), error = TRUE)
})

# .check_establishment

test_that("Vectors of establishment dates only use the first", {
  expect_snapshot(
    .check_emergence_date(x = c("2000-06-01", "2000-06-02"))
  )
})
