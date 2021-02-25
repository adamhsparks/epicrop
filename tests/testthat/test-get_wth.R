
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
