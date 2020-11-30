
# weather handling and checking ------------------------------------------------
context("Weather data is properly retrieved")
test_that("Weather data is as expected after fetching from POWER API", {
  wth <- get_wth(lonlat = c(151.81, -27.48),
                 dates = c("2015-01-15", "2015-05-15"))
  expect_named(wth,
               c(
                 "YYYYMMDD",
                 "DOY",
                 "TM",
                 "TN",
                 "TX",
                 "TDEW",
                 "RH",
                 "RAIN",
                 "LON",
                 "LAT"
               ))
  expect_is(wth, c("data.table", "data.frame"))
})
