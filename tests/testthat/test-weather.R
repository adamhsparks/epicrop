# Test weather data functions (get_wth + format_wth)

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
      duration = 2.0
    )
    expect_identical(nrow(wth_season), 4L)
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

test_that("format_wth fails with non-daily data", {
  wth_monthly <- wth |>
    dplyr::group_by(YEAR, MM) |>
    dplyr::summarise_all(mean)

  expect_error(
    format_wth(
      wth = wth_monthly,
      date = "YYYYMMDD",
      temp = "T2M",
      rhum = "RH2M",
      rain = "PRECTOTCORR"
    )
  )
})

test_that("NA values in POWER data emit a message", {
  skip_if_offline()

  expect_message(
    .check_na(
      .wth = data.frame(
        "YYYYMMDD" = as.Date("2015-01-15"),
        "DOY" = 15.0,
        "TEMP" = 26.05,
        "TMIN" = 25.1,
        "TMAX" = 27.0,
        "RHUM" = 56.6,
        "RAIN" = NA,
        "LAT" = -27.48,
        "LON" = 151.8
      )
    )
  )
})
