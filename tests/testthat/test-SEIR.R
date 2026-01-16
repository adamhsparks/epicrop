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
