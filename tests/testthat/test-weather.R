test_that("get_wth works with basic inputs", {
  skip_on_cran()
  skip_if_offline()

  vcr::use_cassette("get_wth", {
    wth <- get_wth(
      lonlat = c(151.81, -27.48),
      dates = c("2015-01-15", "2015-01-16")
    )

    expect_named(
      wth,
      c("YYYYMMDD", "DOY", "TEMP", "TMIN", "TMAX", "RHUM", "RAIN", "LAT", "LON")
    )
    expect_s3_class(wth, c("epicrop.wth", "data.table", "data.frame"))
  })
})

test_that("get_wth works with duration parameter", {
  skip_on_cran()
  skip_if_offline()

  vcr::use_cassette("get_wth_w_season", {
    wth_season <- get_wth(
      lonlat = c(151.81, -27.48),
      dates = c("2015-01-15", "2015-05-15"),
      duration = 2
    )
    expect_identical(nrow(wth_season), 3L)
  })
})

test_that("format_wth works with all required columns", {
  wth_8 <- format_wth(
    wth,
    date = "YYYYMMDD",
    temp = "T2M",
    rhum = "RH2M",
    rain = "PRECTOTCORR",
    tmin = "T2M_MIN",
    tmax = "T2M_MAX",
    lat = "LAT",
    lon = "LON"
  )

  expect_s3_class(wth_8, c("epicrop.wth", "data.table", "data.frame"))
  expect_named(
    wth_8,
    c("YYYYMMDD", "DOY", "TEMP", "TMIN", "TMAX", "RHUM", "RAIN", "LAT", "LON")
  )
})

test_that("format_wth works with only required columns", {
  wth_4 <- format_wth(
    wth,
    date = "YYYYMMDD",
    temp = "T2M",
    rhum = "RH2M",
    rain = "PRECTOTCORR"
  )

  expect_s3_class(wth_4, c("epicrop.wth", "data.table", "data.frame"))
  expect_named(wth_4, c("YYYYMMDD", "DOY", "TEMP", "RHUM", "RAIN"))
})

test_that("format_wth errors with missing required columns", {
  expect_error(
    format_wth(
      wth = wth,
      date = "NONEXISTENT",
      temp = "T2M",
      rhum = "RH2M",
      rain = "PRECTOTCORR"
    ),
    regexp = "NONEXISTENT"
  )
})

test_that("format_wth validates daily time-step", {
  # Gaps in data
  wth_gaps <- wth[seq(1, min(30, .N), by = 2)]
  expect_error(
    format_wth(
      wth = wth_gaps,
      date = "YYYYMMDD",
      temp = "T2M",
      rhum = "RH2M",
      rain = "PRECTOTCORR"
    ),
    "gap|daily|consecutive"
  )

  # Duplicate dates
  wth_test <- wth[1:20]
  wth_dup <- data.table::rbindlist(list(wth_test, wth_test[5:10]))
  expect_error(
    format_wth(
      wth = wth_dup,
      date = "YYYYMMDD",
      temp = "T2M",
      rhum = "RH2M",
      rain = "PRECTOTCORR"
    ),
    "duplicate"
  )

  # Weekly data (7-day gaps)
  wth_weekly <- wth[seq(1, min(56, .N), by = 7)]
  expect_error(
    format_wth(
      wth = wth_weekly,
      date = "YYYYMMDD",
      temp = "T2M",
      rhum = "RH2M",
      rain = "PRECTOTCORR"
    ),
    "gap|daily"
  )
})

test_that("format_wth accepts consecutive daily data", {
  wth_daily <- wth[1:30]

  result <- format_wth(
    wth = wth_daily,
    date = "YYYYMMDD",
    temp = "T2M",
    rhum = "RH2M",
    rain = "PRECTOTCORR"
  )

  expect_s3_class(result, "epicrop.wth")
  expect_equal(nrow(result), 30)
  expect_true(all(diff(result$YYYYMMDD) == 1))
})
