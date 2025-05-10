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
      onset = 20,
      duration = 120,
      rhlim = 90,
      rainlim = 5,
      RcA = RcA,
      RcT = RcT,
      I0 = 1,
      RcOpt = 0.61,
      p = 6,
      i = 19,
      H0 = 600,
      a = 1,
      Sx = 100000,
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
      onset = 20,
      duration = 120,
      rhlim = 90,
      rainlim = 5,
      RcA = RcA,
      RcT = RcT,
      I0 = 1,
      RcOpt = 0.61,
      p = 6,
      i = 19,
      H0 = 600,
      a = 0.9,
      Sx = 100000,
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
      onset = 20,
      duration = 120,
      rhlim = 90,
      rainlim = 5,
      RcA = RcA,
      RcT = RcT,
      H0 = -1,
      I0 = 1,
      RcOpt = 0.61,
      p = 6,
      i = 19,
      a = 1,
      Sx = 100000,
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
      onset = 20,
      duration = 120,
      rhlim = 90,
      rainlim = 5,
      RcA = RcA,
      RcT = RcT,
      H0 = 600,
      I0 = -1,
      RcOpt = 0.61,
      p = 6,
      i = 19,
      a = 1,
      Sx = 100000,
      RRS = 0.01,
      RRG = 0.1,
      RcW = NULL,
      simple_wetness = TRUE
    )
  )
})

wth <-
  test_that("seir() converts wth to a data.table object if it is not already", {
    expect_error(
      seir(
        wth = wth_formatted,
        emergence = "2000-07-01",
        onset = 20,
        duration = 120,
        rhlim = 90,
        rainlim = 5,
        RcA = RcA,
        RcT = RcT,
        H0 = 600,
        I0 = -1,
        RcOpt = 0.61,
        p = 6,
        i = 19,
        a = 1,
        Sx = 100000,
        RRS = 0.01,
        RRG = 0.1,
        RcW = NULL,
        simple_wetness = TRUE
      )
    )
  })
withr::deferred_clear()
