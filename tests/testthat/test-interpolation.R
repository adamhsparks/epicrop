# Test interpolation function

test_that(".interp handles scalar input", {
  age_vec <- 0:100
  result <- epicrop:::.interp(0.8, age_vec)

  expect_length(result, 101)
  expect_true(all(result == 0.8))
})

test_that(".interp handles vector input with matching length", {
  age_vec <- 0:10
  param_vec <- seq(1.0, 0.0, length.out = 11)

  result <- epicrop:::.interp(param_vec, age_vec)

  expect_identical(result, param_vec)
})

test_that(".interp rejects vector with mismatched length", {
  age_vec <- 0:100
  param_vec <- c(1.0, 0.8, 0.6)

  expect_error(
    epicrop:::.interp(param_vec, age_vec),
    regexp = "Vector length mismatch"
  )
})

test_that(".interp handles matrix lookup table", {
  param <- cbind(
    c(0, 50, 100),
    c(1.0, 0.5, 0.0)
  )
  age_vec <- c(0, 25, 50, 75, 100)

  result <- epicrop:::.interp(param, age_vec)

  expect_length(result, 5)
  expect_equal(result[1], 1.0)
  expect_equal(result[2], 0.75, tolerance = 1e-10)
  expect_equal(result[3], 0.5)
  expect_equal(result[4], 0.25, tolerance = 1e-10)
  expect_equal(result[5], 0.0)
})

test_that(".interp handles data.frame lookup table", {
  param <- data.frame(
    age = c(0, 50, 100),
    value = c(1.0, 0.5, 0.0)
  )
  age_vec <- c(0, 50, 100)

  result <- epicrop:::.interp(param, age_vec)

  expect_equal(result[1], 1.0)
  expect_equal(result[2], 0.5)
  expect_equal(result[3], 0.0)
})

test_that(".interp extrapolates at boundaries", {
  param <- cbind(
    c(20, 80),
    c(0.5, 1.0)
  )
  age_vec <- c(0, 10, 20, 50, 80, 90, 100)

  result <- epicrop:::.interp(param, age_vec)

  # Before first breakpoint - use first value
  expect_equal(result[1], 0.5)
  expect_equal(result[2], 0.5)

  # After last breakpoint - use last value
  expect_equal(result[6], 1.0)
  expect_equal(result[7], 1.0)
})

test_that(".interp handles function input", {
  param_func <- function(x) {
    1.0 - x / 100
  }
  age_vec <- c(0, 25, 50, 75, 100)

  result <- epicrop:::.interp(param_func, age_vec)

  expect_equal(result[1], 1.00)
  expect_equal(result[2], 0.75)
  expect_equal(result[3], 0.50)
  expect_equal(result[4], 0.25)
  expect_equal(result[5], 0.00)
})

test_that(".interp sets negative function results to zero", {
  param_func <- function(x) {
    0.5 - x / 50 # Goes negative after x = 25
  }
  age_vec <- c(0, 25, 50, 75, 100)

  result <- epicrop:::.interp(param_func, age_vec)

  expect_equal(result[1], 0.5)
  expect_equal(result[2], 0.0)
  expect_true(all(result >= 0))
})

test_that(".interp rejects matrix with wrong number of columns", {
  param <- cbind(c(0, 50, 100), c(1.0, 0.5, 0.0), c(0.1, 0.2, 0.3))
  age_vec <- 0:100

  expect_error(
    epicrop:::.interp(param, age_vec),
    regexp = "exactly 2 columns"
  )
})

test_that(".interp rejects non-numeric matrix columns", {
  param <- cbind(
    c("0", "50", "100"),
    c(1.0, 0.5, 0.0)
  )
  age_vec <- 0:100

  expect_error(
    epicrop:::.interp(param, age_vec),
    regexp = "must be numeric"
  )
})

test_that(".interp rejects unsupported types", {
  age_vec <- 0:100

  expect_error(
    epicrop:::.interp(list(a = 1, b = 2), age_vec),
    regexp = "Unsupported parameter type"
  )
})

test_that(".interp handles function that fails", {
  bad_func <- function(x) {
    stop("Intentional error")
  }
  age_vec <- 0:10

  expect_error(
    epicrop:::.interp(bad_func, age_vec),
    regexp = "Function evaluation failed"
  )
})

test_that(".interp interpolates linearly", {
  param <- cbind(
    c(0, 100),
    c(0.0, 1.0)
  )
  age_vec <- seq(0, 100, by = 10)

  result <- epicrop:::.interp(param, age_vec)
  expected <- seq(0.0, 1.0, by = 0.1)

  expect_equal(result, expected, tolerance = 1e-10)
})

test_that(".interp works with real disease model parameters", {
  # Bacterial blight RcA
  RcA <- cbind(
    c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120),
    c(1, 1, 1, 0.9, 0.62, 0.43, 0.42, 0.42, 0.41, 0.41, 0.41, 0.41, 0.41)
  )
  age_vec <- 0:120

  result <- epicrop:::.interp(RcA, age_vec)

  expect_length(result, 121)
  expect_equal(result[1], 1.0)
  expect_equal(result[121], 0.41)
  expect_true(all(result >= 0.4))
  expect_true(all(result <= 1.0))
})

test_that(".interp works with temperature response", {
  # Temperature response curve
  RcT <- cbind(
    c(15, 20, 25, 30, 35),
    c(0.0, 0.5, 1.0, 0.5, 0.0)
  )
  temps <- c(15, 17.5, 20, 22.5, 25, 27.5, 30, 32.5, 35)

  result <- epicrop:::.interp(RcT, temps)

  expect_length(result, 9)
  expect_equal(result[1], 0.0)
  expect_equal(result[5], 1.0) # Peak at 25°C
  expect_equal(result[9], 0.0)
})

test_that(".interp handles single breakpoint", {
  # stats::approx requires at least 2 points, so single breakpoint should error
  # OR the function should handle this case specially

  param <- cbind(c(0), c(0.8))
  age_vec <- 0:100

  # Check what actually happens - likely an error
  expect_error(
    epicrop:::.interp(param, age_vec),
    regexp = "at least two"
  )
})

test_that(".interp handles non-uniform breakpoints", {
  param <- cbind(
    c(0, 5, 10, 100),
    c(0.0, 1.0, 0.5, 0.5)
  )
  age_vec <- c(0, 2.5, 5, 7.5, 10, 50, 100, 120)

  result <- epicrop:::.interp(param, age_vec)

  expect_equal(result[1], 0.0)
  expect_equal(result[2], 0.5, tolerance = 1e-10)
  expect_equal(result[3], 1.0)
  expect_equal(result[4], 0.75, tolerance = 1e-10)
  expect_equal(result[5], 0.5)
  expect_equal(result[6], 0.5)
  expect_equal(result[7], 0.5)
  expect_equal(result[8], 0.5) # Extrapolated
})

test_that(".interp returns numeric vector", {
  param <- cbind(c(0, 100), c(1.0, 0.5))
  age_vec <- 0:10

  result <- epicrop:::.interp(param, age_vec)

  expect_type(result, "double")
  expect_true(is.vector(result))
  expect_false(is.matrix(result))
})

test_that(".interp handles empty age vector", {
  param <- cbind(c(0, 100), c(1.0, 0.5))
  age_vec <- numeric(0)

  result <- epicrop:::.interp(param, age_vec)

  expect_length(result, 0)
})

test_that(".interp handles complex function", {
  # Gaussian-like response
  param_func <- function(x) {
    exp(-((x - 50)^2) / 500)
  }
  age_vec <- c(0, 25, 50, 75, 100)

  result <- epicrop:::.interp(param_func, age_vec)

  expect_length(result, 5)
  expect_true(all(result >= 0))
  expect_true(all(result <= 1))
  expect_gt(result[3], result[1]) # Peak at center
  expect_gt(result[3], result[5])
})
