test_that(".calculate_dvs() calculates correctly", {
  # Simple test case: constant temperature
  wth <- data.table::data.table(
    TEMP = rep(20, 100)
  )

  thermal_time <- list(
    base_temp = 10,
    gdd_to_maturity = 1000
  )

  dvs <- .calculate_dvs(wth, thermal_time)

  # Each day adds 10 GDD (20 - 10)
  # After 50 days: 500 GDD = DVS 1.0
  # After 100 days: 1000 GDD = DVS 2.0

  expect_equal(dvs[1], 0.02) # Day 1: 10 / 1000 * 2 = 0.02
  expect_equal(dvs[50], 1.0) # Day 50: 500 / 1000 * 2 = 1.0
  expect_equal(dvs[100], 2.0) # Day 100: 1000 / 1000 * 2 = 2.0

  expect_true(all(dvs >= 0))
  expect_true(all(dvs <= 2))
  expect_true(all(diff(dvs) >= 0)) # Monotonically increasing
})

test_that(".calculate_dvs() handles below base temp", {
  wth <- data.table::data.table(
    TEMP = c(5, 8, 15, 20, 25)
  )

  thermal_time <- list(
    base_temp = 10,
    gdd_to_maturity = 100
  )

  dvs <- .calculate_dvs(wth, thermal_time)

  # Day 1-2: no GDD (temp < base)
  expect_equal(dvs[1], 0)
  expect_equal(dvs[2], 0)

  # Day 3: 5 GDD
  expect_equal(dvs[3], 2 * 5 / 100)

  # Day 4: 5 + 10 = 15 GDD
  expect_equal(dvs[4], 2 * 15 / 100)

  # Day 5: 15 + 15 = 30 GDD
  expect_equal(dvs[5], 2 * 30 / 100)
})

test_that(".calculate_dvs() caps at DVS = 2", {
  wth <- data.table::data.table(
    TEMP = rep(30, 200)
  )

  thermal_time <- list(
    base_temp = 10,
    gdd_to_maturity = 100
  )

  dvs <- .calculate_dvs(wth, thermal_time)

  # Should reach DVS = 2 and stay there
  expect_true(all(dvs <= 2))
  expect_equal(dvs[100], 2)
  expect_equal(dvs[200], 2)
})
