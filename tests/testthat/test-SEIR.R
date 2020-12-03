

context("SEIR date checks")
test_that("SEIR stops if emergence is before weather data start", {
  wth <- get_wth(
    lonlat = c(121.25562, 14.6774),
    dates = c("2000-06-15", "2000-12-31")
  )
  expect_error(predict_bacterial_blight(
    wth,
    emergence = "2000-05-01",
    duration = 120,
    onset = 10
  ))
})

test_that("SEIR stops if weather data does not span `duration`", {
  wth <- get_wth(
    lonlat = c(121.25562, 14.6774),
    dates = c("2000-06-15", "2000-06-15")
  )
  expect_error(predict_bacterial_blight(
    wth,
    emergence = "2000-06-15",
    duration = 120,
    onset = 10
  ))
})

test_that("SEIR handles seasons that span new years",
          {
            wth <- get_wth(
              lonlat = c(121.25562, 14.6774),
              dates = c("2000-11-15", "2001-03-15")
            )
            x <- predict_bacterial_blight(wth,
                                          emergence = "2000-11-16")
            expect_is(x, c("data.table", "data.frame"))
          })
