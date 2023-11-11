
# weather handling and checking ------------------------------------------------
test_that("Weather data is as expected after fetching from POWER API", {
  skip_on_cran()
  vcr::use_cassette("get_wth", {
    wth <- get_wth(
      lonlat = c(151.81, -27.48),
      dates = c("2015-01-15", "2015-01-16")
    )
    expect_named(wth,
                 c("YYYYMMDD",
                   "DOY",
                   "TEMP",
                   "RHUM",
                   "RAIN",
                   "LAT",
                   "LON"))
    expect_s3_class(wth, c("data.table", "data.frame"))
  })
})

test_that("Supplying the season overrides an end-date value", {
  skip_on_cran()
  vcr::use_cassette("get_wth_w_season", {
    wth_season <- get_wth(
      lonlat = c(151.81, -27.48),
      dates = c("2015-01-15", "2015-05-15"),
      duration = 2
    )
    expect_equal(nrow(wth_season), 3)
  })
})

test_that("Any NA values in the POWER data will emit a message", {
  skip_on_cran()
  expect_message(.check_na(
    .wth = data.frame(
      "YYYYMMDD" = as.Date("2015-01-15"),
      "DOY" = 15,
      "TEMP" = 26.05,
      "RHUM" = 56.6,
      "RAIN" = NA,
      "LAT" = -27.48,
      "LON" = 151.8
    )
  ))
})
