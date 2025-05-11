# weather formatting
test_that("Weather data is formated as expected", {
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
