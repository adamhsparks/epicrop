
context("SEIR date checks")
test_that("SEIR stops if emergence is before weather data start", {
   wth <- get_wth(
     lonlat = c(121.25562, 14.6774),
     dates = c("2000-06-15", "2000-12-31")
   )
   expect_error(predict_bacterial_blight(wth, emergence = "2000-05-01"))
})

test_that("SEIR stops if weather data does not span `duration`", {
  wth <- get_wth(
    lonlat = c(121.25562, 14.6774),
    dates = c("2000-06-15", "2000-06-15")
  )
  expect_error(predict_bacterial_blight(wth, emergence = "2000-06-15"))
})
