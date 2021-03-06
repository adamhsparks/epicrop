
# weather handling and checking ------------------------------------------------
test_that("Weather data is as expected after fetching from POWER API", {
  wth <- get_wth(lonlat = c(151.81, -27.48),
                 dates = c("2015-01-15", "2015-05-15"))
  expect_named(wth,
               c(
                 "YYYYMMDD",
                 "DOY",
                 "TEMP",
                 "RHUM",
                 "RAIN",
                 "LAT",
                 "LON"
               ))
  expect_is(wth, c("data.table", "data.frame"))
})

test_that("Supplying the season overrides an end-date value", {
  wth_season <- get_wth(lonlat = c(151.81, -27.48),
                        dates = c("2015-01-15", "2015-05-15"),
                        duration = 90)
  expect_equal(nrow(wth_season), 91)
})

test_that("Any NA values in the POWER data will emit a message", {
  expect_message(.check_na(
    .wth = data.frame(
      "YYYYMMDD" = 2015 - 01 - 15,
      "DOY" = 15,
      "TEMP" = 26.05,
      "RHUM" = 56.6,
      "RAIN" = NA,
      "LAT" = -27.48,
      "LON" = 151.8
    )
  ))
})
