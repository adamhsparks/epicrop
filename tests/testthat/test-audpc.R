test_that("setup: weather OK", {
  skip_if(
    !exists("wth_formatted") ||
      !is.data.frame(wth_formatted) ||
      nrow(wth_formatted) < 185,
    "Need extended weather data"
  )
})

# Extract AUDPC for convenience
.get_audpc <- function(result) attr(result, "AUDPC")

# Manual trapezoidal AUDPC calculator
.trapz <- function(intensity) {
  n <- length(intensity)
  if (n < 2) {
    return(0L)
  }
  sum((intensity[-n] + intensity[-1]) / 2)
}

# Generic expectation for AUDPC (double or integer)
.expect_valid_audpc <- function(a) {
  expect_true(is.numeric(a))
  expect_length(a, 1)
  expect_true(is.finite(a))
  expect_gte(a, 0)
}

# Disease models for parametric tests
rice_models <- list(
  bacterial_blight,
  brown_spot,
  leaf_blast,
  sheath_blight,
  tungro
)

.emergence <- "2000-07-01"

test_that("AUDPC is calculated correctly for normal simulation", {
  result <- bacterial_blight(wth_formatted, emergence = .emergence)
  audpc <- .get_audpc(result)

  .expect_valid_audpc(audpc)
  expect_gt(audpc, 0)
})

test_that("AUDPC uses trapezoidal rule", {
  result <- bacterial_blight(wth_formatted, emergence = .emergence)
  manual <- .trapz(result$intensity)
  audpc <- .get_audpc(result)

  expect_equal(audpc, manual, tolerance = 1e-10)
})

test_that("AUDPC returns 0 for simulations with < 2 days", {
  w1 <- wth_formatted[1:2, ]

  RcA <- cbind(c(0L, 1L), c(1, 1))
  RcT <- cbind(c(15L, 25L, 35L), c(0.5, 1, 0.5))

  # Duration 1 → AUDPC = 0
  result <- suppressWarnings(
    seir(
      wth = w1,
      emergence = .emergence,
      onset = 1,
      duration = 1,
      H0 = 100,
      I0 = 1,
      RcA = RcA,
      RcT = RcT,
      RcOpt = 0.9,
      p = 5,
      i = 20,
      Sx = 5000,
      RRS = 0.01,
      RRG = 0.1
    )
  )

  audpc <- .get_audpc(result)
  expect_equal(audpc, 0)
  expect_true(is.numeric(audpc))
})

test_that("AUDPC is zero when no disease occurs", {
  result <- seir(
    wth = wth_formatted,
    emergence = .emergence,
    onset = 20,
    duration = 100,
    H0 = 100,
    I0 = 0, # no inoculum
    RcA = cbind(c(0L, 50L, 100L), c(0, 0, 0)),
    RcT = cbind(c(15L, 25L, 35L), c(0, 0, 0)),
    RcOpt = 0,
    p = 5,
    i = 20,
    Sx = 5000,
    RRS = 0.01,
    RRG = 0.1
  )

  expect_equal(.get_audpc(result), 0, tolerance = 1e-6)
})

test_that("AUDPC increases with disease intensity", {
  base <- bacterial_blight(wth_formatted, emergence = .emergence)

  low <- base
  low$intensity <- base$intensity * 0.5
  manual_low <- .trapz(low$intensity)

  high <- .get_audpc(base)

  expect_gt(high, manual_low)
})

test_that("AUDPC scales with simulation duration", {
  short <- seir(
    wth = wth_formatted,
    emergence = .emergence,
    onset = 20,
    duration = 60,
    H0 = 100,
    I0 = 1,
    RcA = cbind(c(0L, 60L), c(1, 1)),
    RcT = cbind(c(15L, 25L, 35L), c(0.5, 1, 0.5)),
    RcOpt = 0.9,
    p = 5,
    i = 20,
    Sx = 5000,
    RRS = 0.01,
    RRG = 0.1
  )

  long <- seir(
    wth = wth_formatted,
    emergence = .emergence,
    onset = 20,
    duration = 120,
    H0 = 100,
    I0 = 1,
    RcA = cbind(c(0L, 120L), c(1, 1)),
    RcT = cbind(c(15L, 25L, 35L), c(0.5, 1, 0.5)),
    RcOpt = 0.9,
    p = 5,
    i = 20,
    Sx = 5000,
    RRS = 0.01,
    RRG = 0.1
  )

  expect_gt(.get_audpc(long), .get_audpc(short))
})

test_that("AUDPC is present in all disease models and > 0", {
  for (fn in rice_models) {
    result <- fn(wth_formatted, .emergence)
    audpc <- .get_audpc(result)

    expect_false(is.null(audpc))
    expect_true(is.numeric(audpc))
    expect_gt(audpc, 0)
  }
})

test_that("AUDPC is reproducible", {
  r1 <- bacterial_blight(wth_formatted, .emergence)
  r2 <- bacterial_blight(wth_formatted, .emergence)

  expect_equal(.get_audpc(r1), .get_audpc(r2))
})

test_that("AUDPC is bounded by maximum possible value", {
  result <- bacterial_blight(wth_formatted, .emergence)
  audpc <- .get_audpc(result)
  duration <- nrow(result)

  # Maximum if intensity = 1 every day → (1+1)/2 * (duration-1)
  max_possible <- duration - 1

  expect_lte(audpc, max_possible)
})

test_that("AUDPC handles intensity values correctly", {
  result <- bacterial_blight(wth_formatted, .emergence)

  # Intensity field sanity
  expect_true(all(result$intensity >= 0))
  expect_true(all(result$intensity <= 1))
  expect_false(anyNA(result$intensity))

  # AUDPC field sanity
  audpc <- .get_audpc(result)
  .expect_valid_audpc(audpc)
})
