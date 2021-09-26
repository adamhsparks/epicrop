
test_that("SEIR() stops if dates do not align", {
  wth <- epicrop:::wth

  expect_error(predict_bacterial_blight(wth, emergence = "1999-07-01"),
               regexp = "Incomplete weather data or dates do not align")
})

RcA <-
  cbind(0:6 * 20, c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0))
RcT <-
  cbind(15 + (0:5) * 5, c(0, 0.06, 1.0, 0.85, 0.16, 0))

test_that("SEIR() stops if `a` < 1", {
  wth <- epicrop:::wth
  expect_error(
    SEIR(
      wth = wth,
      emergence = "1999-07-01",
      onset = 20,
      duration = 120,
      rhlim = 90,
      rainlim = 5,
      RcA = RcA,
      RcT = RcT,
      I0 = 1,
      RcOpt = 0.61,
      p =  6,
      i = 19,
      H0 = 600,
      a = 0.9,
      Sx = 100000,
      RRS = 0.01,
      RRG = 0.1
    )
  )
})

test_that("SEIR() stops if `H0` < 0", {
  expect_error(
    SEIR(
      wth = data.frame(wth),
      emergence = "1999-07-01",
      onset = 20,
      duration = 120,
      rhlim = 90,
      rainlim = 5,
      RcA = RcA,
      RcT = RcT,
      H0 = -1,
      I0 = 1,
      RcOpt = 0.61,
      p =  6,
      i = 19,
      a = 1,
      Sx = 100000,
      RRS = 0.01,
      RRG = 0.1
    )
  )
})

test_that("SEIR() stops if `I0` < 0", {
  wth <- epicrop:::wth
  expect_error(
    SEIR(
      wth = wth,
      emergence = "1999-07-01",
      onset = 20,
      duration = 120,
      rhlim = 90,
      rainlim = 5,
      RcA = RcA,
      RcT = RcT,
      H0 = 600,
      I0 = -1,
      RcOpt = 0.61,
      p =  6,
      i = 19,
      a = 1,
      Sx = 100000,
      RRS = 0.01,
      RRG = 0.1
    )
  )
})

wth <-
test_that("SEIR() converts wth to a data.table object if it is not already", {
  wth <- epicrop:::wth
  expect_error(
    SEIR(
      wth = wth,
      emergence = "1999-07-01",
      onset = 20,
      duration = 120,
      rhlim = 90,
      rainlim = 5,
      RcA = RcA,
      RcT = RcT,
      H0 = 600,
      I0 = -1,
      RcOpt = 0.61,
      p =  6,
      i = 19,
      a = 1,
      Sx = 100000,
      RRS = 0.01,
      RRG = 0.1
    )
  )
})
