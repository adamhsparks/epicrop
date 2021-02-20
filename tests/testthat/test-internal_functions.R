
# saturated vapour pressure ----------------------------------------------------
test_that("es is properly calculated", {
  expect_equal(.saturated_vapour_pressure(0), 0.6109, tolerance = 0.0001)
})

# diurnal rh to hourly rh ------------------------------------------------------
# Create a wth object for use in multiple tests

wth <- epicrop:::wth
wth <- subset(wth, YYYYMMDD == "2000-06-30")

test_that("hourly rh is properly calculated", {
  expect_equal(
    .diurnal_rh(
      rh = wth[, RHUM],
      tmin = wth[, TMIN],
      tmax = wth[, TMAX],
      tmp = wth[, TEMP],
      doy = wth[, DOY],
      lat = wth[, LAT]
    ),
    c(
      100.00,
      100.00,
      100.00,
      100.00,
      100.00,
      100.00,
      93.52,
      87.45,
      82.36,
      78.30,
      75.26,
      73.25,
      72.24,
      72.24,
      73.25,
      75.26,
      78.30,
      82.36,
      86.97,
      90.77,
      93.85,
      96.34,
      98.32,
      99.90
    ),
    tolerance = 0.01
  )
})

# leaf wetness calculations ----------------------------------------------------

test_that(".leaf_wet simple returns a simple integer value", {
  expect_equal(.leaf_wet(wth = wth, simple = TRUE), 12)
})

test_that(".leaf_wet simple returns a simple integer value", {
  expect_equal(.leaf_wet(wth = wth, simple = FALSE), 12.82, tolerance = 0.01)
})

test_that(".daylength stops if lon/lat out of bounds", {
  expect_error(.daylength(lat = 100, doy = 1))
})

test_that(".daylength stops if lon/lat out of bounds", {
  expect_error(.daylength(lat = -90.1, doy = 1))
})
