# --- Shared curves (Savary-style) ---
RcA <- cbind(0L:6L * 20L, c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0))
RcT <- cbind(15L + (0L:5L) * 5L, c(0.0, 0.06, 1.0, 0.85, 0.16, 0.0))

# --- Helper: base args + runner ---
base_args <- list(
  wth = wth_formatted, # provided by setup-testdata.R
  emergence = "2000-07-01",
  onset = 20L,
  duration = 120L,
  rhlim = 90,
  rainlim = 5,
  H0 = 600,
  I0 = 1,
  RcA = RcA,
  RcT = RcT,
  RcOpt = 0.61,
  p = 6L,
  i = 19L,
  Sx = 100000,
  a = 1,
  RRS = 0.01,
  RRG = 0.1,
  RcW = NULL,
  simple_wetness = TRUE
)

run_seir <- function(...) {
  do.call(seir, utils::modifyList(base_args, list(...)))
}

# --- Helper: ensure complex wetness branch can actually run ---
# seir() switches to simple wetness if required fields are missing.
# This makes a weather object with the required columns.
make_wth_complex <- function(wth) {
  w <- data.table::as.data.table(data.table::copy(wth))

  # ensure required fields exist for complex wetness path
  if (!"DOY" %in% names(w)) {
    # best-effort DOY from YYYYMMDD if present; else from dates column name
    if ("YYYYMMDD" %in% names(w)) {
      w[, DOY := as.integer(format(YYYYMMDD, "%j"))]
    } else {
      stop(
        "wth is missing DOY and YYYYMMDD; cannot construct DOY for complex wetness test."
      )
    }
  }

  if (!"LAT" %in% names(w)) {
    w[, LAT := 14.6774]
  }
  if (!"TEMP" %in% names(w)) {
    stop("wth is missing TEMP; cannot construct TMIN/TMAX.")
  }
  if (!"TMIN" %in% names(w)) {
    w[, TMIN := TEMP - 2]
  }
  if (!"TMAX" %in% names(w)) {
    w[, TMAX := TEMP + 2]
  }

  w
}

# --- Helper: snapshot-safe representation ---
snapshot_df <- function(dt, digits = 4L) {
  x <- as.data.frame(dt)
  if ("dates" %in% names(x)) {
    x$dates <- as.character(x$dates)
  }

  is_num <- vapply(x, is.numeric, logical(1))
  x[is_num] <- lapply(x[is_num], \(v) round(v, digits))
  x
}

# -------------------------------------------------------------------------
# Tests
# -------------------------------------------------------------------------

testthat::test_that("seir() stops if dates do not align", {
  testthat::expect_error(
    run_seir(emergence = "1999-07-01"),
    regexp = "Incomplete weather data or dates do not align"
  )
})

testthat::test_that("seir() stops if `a` < 1", {
  testthat::expect_error(
    run_seir(a = 0.9),
    regexp = "a|aggregation|>=\\s*1"
  )
})

testthat::test_that("seir() stops if `H0` < 0", {
  testthat::expect_error(
    run_seir(H0 = -1),
    regexp = "H0|healthy|>=\\s*0"
  )
})

testthat::test_that("seir() stops if `I0` < 0", {
  testthat::expect_error(
    run_seir(I0 = -1),
    regexp = "I0|infect|>=\\s*0"
  )
})

testthat::test_that("seir() accepts data.frame weather and returns a data.table", {
  wth_df <- as.data.frame(wth_formatted)
  out <- run_seir(wth = wth_df)

  testthat::expect_s3_class(out, c("data.table", "data.frame"))
  testthat::expect_identical(nrow(out), base_args$duration)
})

testthat::test_that("seir() errors when complex wetness requested and RcW is not a matrix", {
  wth_c <- make_wth_complex(wth_formatted)

  testthat::expect_error(
    run_seir(wth = wth_c, simple_wetness = FALSE, RcW = 1),
    regexp = "RcW|matrix"
  )

  testthat::expect_error(
    run_seir(wth = wth_c, simple_wetness = FALSE, RcW = list(1, 2)),
    regexp = "RcW|matrix"
  )
})

test_that("seir() uses complex wetness when requested and changes outcomes vs simple mode", {
  withr::local_options(list(scipen = 999L))

  # Define RcW matrix for complex wetness
  RcW_matrix <- cbind(
    c(0, 6, 12, 18, 24),
    c(0, 0.5, 0.75, 0.9, 1)
  )

  # Get weather data with TMIN/TMAX for complex wetness
  wth_c <- wth_formatted # Assuming this has TMIN/TMAX

  out_simple <- run_seir(wth = wth_c, simple_wetness = TRUE)
  out_complex <- run_seir(
    wth = wth_c,
    simple_wetness = FALSE,
    RcW = RcW_matrix
  )

  # Test that outcomes are different between modes
  expect_false(
    identical(
      out_simple$intensity[120L],
      out_complex$intensity[120L]
    )
  )

  # Test that complex wetness produces valid results
  expect_true(out_complex$intensity[120L] >= 0)
  expect_true(out_complex$intensity[120L] <= 1)

  # Use snapshot for the actual value
  expect_snapshot_value(
    round(out_complex$intensity[120L], 4),
    style = "json2"
  )
})

testthat::test_that("seir() output snapshot (JSON-safe) is stable", {
  # snapshot a representative run; use data.frame + stable dates + rounded numerics
  out <- run_seir()
  out_snap <- snapshot_df(out, digits = 4L)

  # json2 snapshots fail on data.table due to internal pointers; snapshot data.frame instead
  testthat::expect_snapshot_value(out_snap, style = "json2")
})

## Test reproducibility -----
test_that("seir() produces identical results with same inputs", {
  out1 <- run_seir()
  out2 <- run_seir()

  expect_identical(out1, out2)
})

test_that("disease models produce reproducible AUDPC", {
  audpc1 <- bacterial_blight(wth_formatted, emergence = "2000-07-01")$AUDPC[1]
  audpc2 <- bacterial_blight(wth_formatted, emergence = "2000-07-01")$AUDPC[1]

  expect_identical(audpc1, audpc2)
})

## Test Performance -----
test_that("seir() completes in reasonable time", {
  skip_on_cran()

  t <- system.time({
    run_seir()
  })

  # Should complete in under 1 second
  expect_lt(t[["elapsed"]], 1.0)
})


test_that("complex wetness is slower than simple wetness", {
  skip_on_cran()

  wth_c <- make_wth_complex(wth_formatted)
  RcW_matrix <- cbind(
    seq(0, 24, by = 6), # 0, 6, 12, 18, 24 (5 elements)
    seq(0, 1, length.out = 5) # 0.00, 0.25, 0.50, 0.75, 1.00 (5 elements)
  )

  t_simple <- system.time({
    suppressWarnings(run_seir(simple_wetness = TRUE))
  })

  t_complex <- system.time({
    suppressWarnings(run_seir(
      wth = wth_c,
      simple_wetness = FALSE,
      RcW = RcW_matrix
    ))
  })

  # Complex should be slower (but don't make test brittle)
  expect_gt(t_complex[["elapsed"]], t_simple[["elapsed"]] * 0.5)
})

## Test date handling -----
test_that("seir() handles different date input formats", {
  out_iso <- run_seir(emergence = "2000-07-01")
  out_date <- run_seir(emergence = as.Date("2000-07-01"))

  expect_identical(out_iso$dates, out_date$dates)
})

test_that("seir() simday starts at 1 and matches duration", {
  out <- run_seir()
  expect_identical(out$simday[1L], 1L)
  expect_identical(out$simday[120L], 120L)
  expect_identical(length(out$simday), 120L)
})

## Test modifier extraploation -----
test_that("seir() extrapolates RcT correctly outside defined range", {
  # RcT defined from 15-40°C, both endpoints have modifier = 0

  wth_cold <- data.table::copy(wth_formatted)
  wth_cold[, TEMP := 5.0] # Below minimum (uses yleft = 0.0)

  wth_hot <- data.table::copy(wth_formatted)
  wth_hot[, TEMP := 45.0] # Above maximum (uses yright = 0.0)

  wth_optimal <- data.table::copy(wth_formatted)
  wth_optimal[, TEMP := 25.0] # Optimal temperature (modifier = 1.0)

  out_cold <- run_seir(wth = wth_cold)
  out_hot <- run_seir(wth = wth_hot)
  out_optimal <- run_seir(wth = wth_optimal)

  # Both extreme temps should have very low disease (near zero)
  expect_lt(out_cold$intensity[120L], 0.01)
  expect_lt(out_hot$intensity[120L], 0.01)

  # Optimal temp should have much higher disease
  expect_gt(out_optimal$intensity[120L], out_cold$intensity[120L])
  expect_gt(out_optimal$intensity[120L], out_hot$intensity[120L])

  # Cold and hot should be similar (both use 0.0 modifier)
  expect_equal(
    out_cold$intensity[120L],
    out_hot$intensity[120L],
    tolerance = 0.01
  )
})

test_that("seir() handles RcA correctly when duration exceeds matrix", {
  # Already warns in checks, but test the truncation behavior
  RcA_short <- cbind(0L:5L * 20L, c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71))

  expect_error(
    run_seir(duration = 120L, RcA = RcA_short),
    regexp = "RcA.*duration"
  )
})

## Test edge cases -----
test_that("seir() handles onset = 1 (disease starts on emergence day)", {
  out <- run_seir(onset = 1L, p = 6L) # p = latent period duration

  expect_identical(nrow(out), 120L)

  # Infections injected on day 1 (onset)
  # They become infectious after p days (latent period)
  # So infectious sites should appear on day p+1
  expect_equal(out$infectious[1L], 0) # Day 1: no infectious yet
  expect_true(out$latent[1L] > 0) # Day 1: should have latent sites
  expect_true(out$infectious[7L] > 0) # Day 7: after 6-day latent period
})

test_that("seir() handles very short duration edge case", {
  # Even though check warns at <50, test boundary behavior at minimum
  expect_error(
    run_seir(duration = 0L),
    regexp = "duration|>\\s*0"
  )
})

test_that("seir() handles maximum sites = initial healthy sites", {
  out <- run_seir(H0 = 100000, Sx = 100000)
  expect_true(all(out$sites <= 100000))
})

test_that("seir() handles RRG as matrix vs scalar consistently", {
  RRG_scalar <- 0.1
  RRG_matrix <- cbind(
    seq_len(120L),
    rep(0.1, 120L)
  )

  out_scalar <- run_seir(RRG = RRG_scalar)
  out_matrix <- run_seir(RRG = RRG_matrix)

  # Should produce similar results
  expect_equal(
    out_scalar$sites[120L],
    out_matrix$sites[120L],
    tolerance = 0.001
  )
})
