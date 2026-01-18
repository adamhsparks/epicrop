# weather formatting
test_that("Weather data is formatted as expected", {
  expect_no_condition(
    format_wth(
      wth = wth,
      date = "YYYYMMDD",
      temp = "T2M",
      rhum = "RH2M",
      rain = "PRECTOTCORR",
      tmin = "T2M_MIN",
      tmax = "T2M_MAX",
      lat = "LAT",
      lon = "LON"
    )
  )
})

test_that("Formatting fails if weather are not daily values", {
  wth_monthly <-
    wth |>
    dplyr::group_by(YEAR, MM) |>
    dplyr::summarise_all(mean)

  expect_error(
    format_wth(
      wth = wth_monthly,
      date = "YYYYMMDD",
      temp = "T2M",
      rhum = "RH2M",
      rain = "PRECTOTCORR",
      tmin = "T2M_MIN",
      tmax = "T2M_MAX",
      lat = "LAT",
      lon = "LON"
    )
  )
})

test_that("Formatting weather fails if cols are not provided", {
  expect_error(
    format_wth(
      wth = 1:20,
      date = "YYYYMMDD",
      temp = "T2M",
      rhum = "RH2M",
      rain = "PRECTOTCORR",
      tmin = "T2M_MIN",
      tmax = "T2M_MAX",
      lat = "LAT",
      lon = "LON"
    )
  )
})

test_that("format_wth() handles data.table input", {
  wth_dt <- data.table::as.data.table(wth)

  out <- format_wth(
    wth = wth_dt,
    date = "YYYYMMDD",
    temp = "T2M",
    rhum = "RH2M",
    rain = "PRECTOTCORR"
  )

  expect_s3_class(out, c("epicrop.wth", "data.table", "data.frame"))
})

test_that("format_wth() errors when column names don't exist", {
  # Single invalid column
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

  # Multiple invalid columns
  expect_error(
    format_wth(
      wth = wth,
      date = "BADDATE",
      temp = "BADTEMP",
      rhum = "RH2M",
      rain = "PRECTOTCORR"
    ),
    regexp = "BADDATE.*BADTEMP|not in.*wth"
  )
})

test_that("format_wth() handles partial arguments correctly", {
  # Only required args
  out_4 <- format_wth(
    wth = wth,
    date = "YYYYMMDD",
    temp = "T2M",
    rhum = "RH2M",
    rain = "PRECTOTCORR"
  )

  expect_false("TMIN" %in% names(out_4))
  expect_false("LAT" %in% names(out_4))

  # With TMIN/TMAX but no LAT/LON
  out_6 <- format_wth(
    wth = wth,
    date = "YYYYMMDD",
    temp = "T2M",
    rhum = "RH2M",
    rain = "PRECTOTCORR",
    tmin = "T2M_MIN",
    tmax = "T2M_MAX"
  )

  expect_true("TMIN" %in% names(out_6))
  expect_true("TMAX" %in% names(out_6))
  expect_false("LAT" %in% names(out_6))

  # With LAT/LON but no TMIN/TMAX
  out_6b <- format_wth(
    wth = wth,
    date = "YYYYMMDD",
    temp = "T2M",
    rhum = "RH2M",
    rain = "PRECTOTCORR",
    lat = "LAT",
    lon = "LON"
  )

  expect_false("TMIN" %in% names(out_6b))
  expect_true("LAT" %in% names(out_6b))
  expect_true("LON" %in% names(out_6b))

  # All 8 fields
  out_8 <- format_wth(
    wth = wth,
    date = "YYYYMMDD",
    temp = "T2M",
    rhum = "RH2M",
    rain = "PRECTOTCORR",
    tmin = "T2M_MIN",
    tmax = "T2M_MAX",
    lat = "LAT",
    lon = "LON"
  )

  expect_true(all(c("TMIN", "TMAX", "LAT", "LON") %in% names(out_8)))
})

test_that("format_wth() preserves data integrity", {
  out <- format_wth(
    wth = wth,
    date = "YYYYMMDD",
    temp = "T2M",
    rhum = "RH2M",
    rain = "PRECTOTCORR",
    tmin = "T2M_MIN",
    tmax = "T2M_MAX",
    lat = "LAT",
    lon = "LON"
  )

  expect_identical(out$TEMP, wth$T2M)
  expect_identical(out$RHUM, wth$RH2M)
  expect_identical(out$RAIN, wth$PRECTOTCORR)
})
