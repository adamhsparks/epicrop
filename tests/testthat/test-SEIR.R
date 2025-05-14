withr::local_options(list(scipen = 999L))

RcA <-
  cbind(0L:6L * 20L, c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0))
RcT <-
  cbind(15L + (0L:5L) * 5L, c(0.0, 0.06, 1.0, 0.85, 0.16, 0.0))

test_that("seir() stops if dates do not align", {
  # wth data starts in 2000
  expect_error(
    seir(
      wth = wth_formatted,
      emergence = "1999-07-01",
      onset = 20.0,
      duration = 120.0,
      rhlim = 90.0,
      rainlim = 5.0,
      RcA = RcA,
      RcT = RcT,
      I0 = 1.0,
      RcOpt = 0.61,
      p = 6.0,
      i = 19.0,
      H0 = 600.0,
      a = 1.0,
      Sx = 100000.0,
      RRS = 0.01,
      RRG = 0.1,
      RcW = NULL,
      simple_wetness = TRUE
    ),
    regexp = "Incomplete weather data or dates do not align"
  )
})

test_that("seir() stops if `a` < 1", {
  expect_error(
    seir(
      wth = wth_formatted,
      emergence = "2000-07-01",
      onset = 20.0,
      duration = 120.0,
      rhlim = 90.0,
      rainlim = 5.0,
      RcA = RcA,
      RcT = RcT,
      I0 = 1.0,
      RcOpt = 0.61,
      p = 6.0,
      i = 19.0,
      H0 = 600.0,
      a = 0.9,
      Sx = 100000.0,
      RRS = 0.01,
      RRG = 0.1,
      RcW = NULL,
      simple_wetness = TRUE
    )
  )
})

test_that("seir() stops if `H0` < 0", {
  expect_error(
    seir(
      wth = wth_formatted,
      emergence = "2000-07-01",
      onset = 20.0,
      duration = 120.0,
      rhlim = 90.0,
      rainlim = 5.0,
      RcA = RcA,
      RcT = RcT,
      H0 = -1.0,
      I0 = 1.0,
      RcOpt = 0.61,
      p = 6.0,
      i = 19.0,
      a = 1.0,
      Sx = 100000.0,
      RRS = 0.01,
      RRG = 0.1,
      RcW = NULL,
      simple_wetness = TRUE
    )
  )
})

test_that("seir() stops if `I0` < 0", {
  expect_error(
    seir(
      wth = wth_formatted,
      emergence = "2000-07-01",
      onset = 20.0,
      duration = 120.0,
      rhlim = 90.0,
      rainlim = 5.0,
      RcA = RcA,
      RcT = RcT,
      H0 = 600.0,
      I0 = -1.0,
      RcOpt = 0.61,
      p = 6.0,
      i = 19.0,
      a = 1.0,
      Sx = 100000.0,
      RRS = 0.01,
      RRG = 0.1,
      RcW = NULL,
      simple_wetness = TRUE
    )
  )
})

test_that("seir() converts wth to a data.table object if it is not already", {
  expect_error(
    seir(
      wth = wth_formatted,
      emergence = "2000-07-01",
      onset = 20.0,
      duration = 120.0,
      rhlim = 90.0,
      rainlim = 5.0,
      RcA = RcA,
      RcT = RcT,
      H0 = 600.0,
      I0 = -1.0,
      RcOpt = 0.61,
      p = 6.0,
      i = 19.0,
      a = 1.0,
      Sx = 100000.0,
      RRS = 0.01,
      RRG = 0.1,
      RcW = NULL,
      simple_wetness = TRUE
    )
  )
})

test_that("`seir()` fails when `RcW` is not a matrix for complex leaf wetness calculation", {
  expect_snapshot(
    seir(
      wth = wth_formatted,
      emergence = "2000-07-01",
      onset = 20.0,
      duration = 120.0,
      rhlim = 90.0,
      rainlim = 5.0,
      RcA = RcA,
      RcT = RcT,
      I0 = 1.0,
      RcOpt = 0.61,
      p = 6.0,
      i = 19.0,
      H0 = 600.0,
      a = 1.0,
      Sx = 100000.0,
      RRS = 0.01,
      RRG = 0.1,
      RcW = NULL,
      simple_wetness = FALSE
    ),
    error = TRUE
  )
})


test_that("`seir()` switches to complex leaf wetness calculation", {
  seir_out <- seir(
    wth = wth_formatted,
    emergence = "2000-07-01",
    onset = 20.0,
    duration = 120.0,
    rhlim = 90.0,
    rainlim = 5.0,
    RcA = RcA,
    RcT = RcT,
    I0 = 1.0,
    RcOpt = 0.61,
    p = 6.0,
    i = 19.0,
    H0 = 600.0,
    a = 1.0,
    Sx = 100000.0,
    RRS = 0.01,
    RRG = 0.1,
    RcW = cbind(
      c(4L, 6L, 8L, 10L, 12L, 14L, 16L, 18L, 20L, 22L, 24L),
      c(0.0, 0.24, 0.41, 0.68, 0.94, 0.97, 1.0, 1.0, 1.0, 1.0, 1.0)
    ),
    simple_wetness = FALSE
  )
  expect_equal(seir_out[[120L, "intensity"]], 0.3364381, tolerance = 0.1)
})

withr::deferred_clear()
