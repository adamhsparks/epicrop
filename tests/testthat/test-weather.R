# ---- get_wth(): POWER (vcr cassettes) ---------------------------------------

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

  # duration is inclusive and takes precedence over the end date in `dates`
  vcr::use_cassette("get_wth_w_season", {
    wth_season <- get_wth(
      lonlat = c(151.81, -27.48),
      dates = c("2015-01-15", "2015-05-15"),
      duration = 2
    )
    expect_identical(nrow(wth_season), 2L)
  })
})

# ---- get_wth(): ERA5 (vcr cassette) ------------------------------------------

test_that("get_wth works with source = 'era5'", {
  skip_on_cran()
  skip_if_offline()

  # NOTE: delete tests/testthat/fixtures/get_wth_era5.json before running so
  # the cassette is re-recorded with the current request (models=era5)
  vcr::use_cassette("get_wth_era5", {
    wth_era5 <- get_wth(
      lonlat = c(151.81, -27.48),
      dates = c("2015-01-15", "2015-01-16"),
      source = "era5"
    )

    expect_named(
      wth_era5,
      c("YYYYMMDD", "DOY", "TEMP", "TMIN", "TMAX", "RHUM", "RAIN", "LAT", "LON")
    )
    expect_s3_class(wth_era5, c("epicrop.wth", "data.table", "data.frame"))
    expect_identical(nrow(wth_era5), 2L)
    expect_false(anyNA(wth_era5$TEMP))
    expect_false(anyNA(wth_era5$RHUM))
    expect_false(anyNA(wth_era5$RAIN))
    expect_true(all(wth_era5$RHUM > 0 & wth_era5$RHUM <= 100))
    expect_true(all(wth_era5$RAIN >= 0))
  })
})

# ---- get_wth(): dispatch and dates -------------------------------------------

test_that("get_wth dispatches on `source` and validates it", {
  testthat::local_mocked_bindings(
    .get_era5 = function(lon, lat, start, end) list(src = "era5"),
    .get_power = function(lon, lat, start, end) list(src = "power")
  )

  expect_identical(
    get_wth(c(121.25, 14.19), "2000-06-30", 3L, source = "era5")$src,
    "era5"
  )
  expect_identical(get_wth(c(121.25, 14.19), "2000-06-30", 3L)$src, "power")
  expect_error(get_wth(c(121.25, 14.19), "2000-06-30", 3L, source = "chirps"))
})

test_that("duration is inclusive (duration = 120 spans 120 days)", {
  testthat::local_mocked_bindings(
    .get_era5 = function(lon, lat, start, end) as.integer(end - start) + 1L
  )

  expect_identical(
    get_wth(c(121.25, 14.19), "2000-06-30", 120L, source = "era5"),
    120L
  )
})

test_that(".resolve_end_date() is inclusive and `duration` takes precedence", {
  start <- as.Date("2000-06-30")

  expect_identical(
    .resolve_end_date("2000-06-30", start, 3L),
    as.Date("2000-07-02")
  )
  expect_identical(
    .resolve_end_date(c("2000-06-30", "2000-12-31"), start, NULL),
    as.Date("2000-12-31")
  )
  expect_identical(
    .resolve_end_date(c("2000-06-30", "2000-12-31"), start, 3L),
    as.Date("2000-07-02")
  )
})

test_that(".resolve_end_date() validates its inputs", {
  start <- as.Date("2000-06-30")

  expect_error(.resolve_end_date("2000-06-30", start, NULL), "duration")
  expect_error(.resolve_end_date("2000-06-30", start, 0L), "positive")
  expect_error(.resolve_end_date("2000-06-30", start, 2.5), "positive integer")
})

# ---- .get_era5(): offline tests using JSON fixtures --------------------------

era5_start <- as.Date("2000-06-30")
era5_end <- as.Date("2000-07-02")

test_that(".get_era5() returns an epicrop.wth object matching the POWER structure", {
  httr2::local_mocked_responses(\(req) era5_response("era5-ok.json"))

  wth_e <- .get_era5(121.25, 14.19, era5_start, era5_end)

  expect_s3_class(wth_e, c("epicrop.wth", "data.table", "data.frame"))
  expect_named(
    wth_e,
    c("YYYYMMDD", "DOY", "TEMP", "TMIN", "TMAX", "RHUM", "RAIN", "LAT", "LON")
  )
  expect_identical(data.table::key(wth_e), "YYYYMMDD")
  expect_s3_class(wth_e$YYYYMMDD, "IDate")
  expect_identical(nrow(wth_e), 3L)
})

test_that(".get_era5() returns double value columns (whole numbers in JSON)", {
  httr2::local_mocked_responses(\(req) era5_response("era5-ok.json"))

  wth_e <- .get_era5(121.25, 14.19, era5_start, era5_end)

  expect_true(all(vapply(
    as.list(wth_e)[c("TEMP", "TMIN", "TMAX", "RHUM", "RAIN")],
    is.double,
    logical(1L)
  )))
})

test_that(".get_era5() aggregates hourly values to daily correctly", {
  httr2::local_mocked_responses(\(req) era5_response("era5-ok.json"))

  wth_e <- .get_era5(121.25, 14.19, era5_start, era5_end)

  expect_equal(wth_e$TEMP[1:2], c(21.5, 20))
  expect_equal(wth_e$TMIN[1], 10)
  expect_equal(wth_e$TMAX[1], 33)
  expect_equal(wth_e$RHUM[1:2], c(80, 60))
  expect_equal(wth_e$RAIN[1:2], c(10, 0))
  expect_identical(wth_e$DOY, c(182L, 183L, 184L)) # 2000 is a leap year
})

test_that(".get_era5() gives NA (not NaN, Inf or 0) for all-missing days", {
  httr2::local_mocked_responses(\(req) era5_response("era5-ok.json"))

  expect_no_warning(wth_e <- .get_era5(121.25, 14.19, era5_start, era5_end))

  for (col in c("TEMP", "TMIN", "TMAX", "RHUM", "RAIN")) {
    expect_true(is.na(wth_e[[col]][3]), info = col)
    expect_false(is.nan(wth_e[[col]][3]), info = col)
  }
  # missing rain must not be reported as a dry day
  expect_false(identical(wth_e$RAIN[3], 0))
})

test_that(".get_era5() uses the grid cell coordinates from the response", {
  httr2::local_mocked_responses(\(req) era5_response("era5-ok.json"))

  wth_e <- .get_era5(121.255669, 14.167425, era5_start, era5_end)

  expect_equal(unique(wth_e$LAT), 14.1875)
  expect_equal(unique(wth_e$LON), 121.25)
})

test_that(".get_era5() sends the expected request", {
  httr2::local_mocked_responses(function(req) {
    expect_match(req$url, "archive-api.open-meteo.com/v1/archive", fixed = TRUE)
    expect_match(req$url, "start_date=2000-06-30", fixed = TRUE)
    expect_match(req$url, "end_date=2000-07-02", fixed = TRUE)
    expect_match(req$url, "timezone=auto", fixed = TRUE)
    expect_match(req$url, "models=era5$") # plain ERA5, not era5_land
    expect_match(
      req$url,
      "hourly=temperature_2m%2Crelative_humidity_2m%2Cprecipitation",
      fixed = TRUE
    )
    era5_response("era5-ok.json")
  })

  .get_era5(121.25, 14.19, era5_start, era5_end)
})

# ---- .get_era5(): error handling ---------------------------------------------

test_that(".get_era5() surfaces the API's reason on HTTP errors", {
  httr2::local_mocked_responses(\(req) era5_response("era5-error.json", 400L))

  expect_error(
    .get_era5(121.25, 95, era5_start, era5_end),
    "Latitude must be in range"
  )
})

test_that(".get_era5() errors when no hourly data is returned", {
  httr2::local_mocked_responses(\(req) era5_response("era5-no-hourly.json"))

  expect_error(.get_era5(121.25, 14.19, era5_start, era5_end), "no hourly data")
})

test_that(".get_era5() errors when a whole variable is all-null", {
  # all-null JSON arrays are the case yyjsonr returns as a list of NULLs

  httr2::local_mocked_responses(\(req) era5_response("era5-no-temp.json"))
  expect_error(
    .get_era5(121.25, 14.19, era5_start, era5_end),
    "no ERA5 data"
  )

  httr2::local_mocked_responses(\(req) era5_response("era5-no-rh.json"))
  expect_error(
    .get_era5(121.25, 14.19, era5_start, era5_end),
    "no ERA5 relative humidity"
  )

  httr2::local_mocked_responses(\(req) era5_response("era5-no-precip.json"))
  expect_error(
    .get_era5(121.25, 14.19, era5_start, era5_end),
    "no ERA5 precipitation"
  )
})

# ---- ERA5 vs POWER structure -------------------------------------------------

test_that("ERA5 and POWER outputs are structurally interchangeable", {
  httr2::local_mocked_responses(\(req) era5_response("era5-ok.json"))
  era5 <- .get_era5(121.25, 14.19, era5_start, era5_end)

  # small POWER-shaped table run through the shared post-processor
  power <- data.table::data.table(
    YEAR = 2000L,
    MM = 6L,
    DD = 30L,
    LON = 121.25,
    LAT = 14.19,
    DOY = 182L,
    YYYYMMDD = "20000630",
    T2M = 25,
    T2M_MIN = 22,
    T2M_MAX = 29,
    RH2M = 80,
    PRECTOTCORR = 1
  )
  power <- .postprocess_wth(power)

  expect_identical(names(era5), names(power))
  expect_identical(class(era5), class(power))
  expect_identical(data.table::key(era5), data.table::key(power))
  expect_identical(
    vapply(era5, typeof, character(1L)),
    vapply(power, typeof, character(1L))
  )
})

# ---- format_wth() ------------------------------------------------------------

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
  expect_identical(nrow(result), 30L)
  expect_true(all(diff(result$YYYYMMDD) == 1))
})
