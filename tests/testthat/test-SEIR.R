
test_that("SEIR() stops if dates do not align", {
  wth <- epicrop:::wth

  expect_error(predict_bacterial_blight(wth, emergence = "1999-07-01"),
               regexp = "Incomplete weather data or dates do not align")
})

test_that("SEIR() stops if `a` < 1", {
  wth <- epicrop:::wth
  expect_error(
    SEIR(
      wth = wth,
      emergence = emergence,
      onset = 20,
      duration = 120,
      RcA = RcA,
      RcT = RcT,
      RcOpt = 0.61,
      p =  6,
      i = 19,
      H0 = 600,
      a = 0.5,
      Sx = 100000,
      RRS = 0.01,
      RRG = 0.1
    )
  )
})
