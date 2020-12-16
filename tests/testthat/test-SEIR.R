
test_that("SEIR() stops if dates do not align", {
  wth <- epicrop:::wth
  expect_error(predict_bacterial_blight(wth, emergence = "1999-07-01"),
               regexp = "Incomplete weather data or dates do not align")
})
