
# saturated vapour pressure ----------------------------------------------------
test_that("es is properly calculated", {
  expect_equal(.saturated_vapour_pressure(0), 0.6109, tolerance = 0.0001)
})

# diurnal rh to hourly rh ------------------------------------------------------
# Create a wth object for use in multiple tests
wth <- get_wth(lonlat = c(121.25562, 14.6774),
               dates = "2000-06-30")

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
    74.20,
    73.51,
    72.98,
    72.57,
    72.25,
    74.67,
    80.07,
    85.57,
    90.88,
    95.68,
    99.66,
    100.00,
    100.00,
    100.00,
    100.00,
    99.66,
    95.68,
    90.88,
    86.03,
    82.46,
    79.79,
    77.78,
    76.26,
    75.09
  ),
  tolerance = 0.01)
})

# leaf wetness calculations ----------------------------------------------------

test_that(".leaf_wet simple returns a simple integer value", {
  expect_equal(.leaf_wet(wth = wth, simple = TRUE), 10)
})

test_that(".leaf_wet simple returns a simple integer value", {
  expect_equal(.leaf_wet(wth = wth, simple = FALSE), 10.39, tolerance = 0.01)
})
