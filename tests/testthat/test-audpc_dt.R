# Helper that loads the epicrop.sim object for all tests.
# Adjust filename if needed.
epicrop_sim <- testthat::test_path("testdata", "epicrop_sim.rda")
load(epicrop_sim)  # loads object 'epicrop.sim'
epicrop_sim <- bb_sim # rename for clarity in tests

test_that("audpc_dt() errors with non-epicrop.sim input", {
  expect_error(
    audpc_dt(data.frame()),
    regexp = "epicrop\\.sim",
    ignore.case = TRUE
  )
  dt <- data.table::data.table(
    lat = 0,
    lon = 0,
    AUDPC = 1,
    location = "A",
    emergence = "2020-01-01"
  )
  # Even if columns exist, class must be epicrop.sim
  expect_error(audpc_dt(dt), regexp = "epicrop\\.sim", ignore.case = TRUE)
})

test_that("audpc_dt() returns a data.table with expected five columns", {

  out <- audpc_dt(epicrop_sim)

  # Class and structure
  expect_s3_class(out, "data.table")
  expect_s3_class(out, "data.frame")
  expect_equal(
    sort(names(out)),
    sort(c("lat", "lon", "AUDPC", "location", "emergence"))
  )
  expect_equal(ncol(out), 5L)

  # Column types
  expect_true(is.numeric(out$lat))
  expect_true(is.numeric(out$lon))
  expect_true(is.numeric(out$AUDPC))
  expect_true(is.character(out$location) || is.factor(out$location))
  expect_true(is.character(out$emergence) || is.factor(out$emergence))
})

test_that("audpc_dt() sets key to location,emergence and ordering reflects the key", {
  skip_if_not(
    exists("epicrop.sim", inherits = FALSE),
    "no epicrop.sim test data"
  )

  out <- audpc_dt(epicrop_sim)

  # data.table key
  expect_identical(data.table::key(out), c("location", "emergence"))

  # Check that order is non-decreasing by (location, emergence)
  # Convert to character for robust comparison if factors are used
  ord <- out[order(as.character(location), as.character(emergence))]
  expect_equal(out, ord, ignore_attr = TRUE)
})

test_that("audpc_dt() removes duplicate rows on the five selected columns", {

  # Create a version with deliberate duplicated rows on the five columns
  dt <- data.table::copy(epicrop˙sim)
  # Keep only required cols to control duplication
  req_cols <- c("lat", "lon", "AUDPC", "location", "emergence")
  # If epicrop.sim has additional columns, we can still duplicate rows;
  # audpc_dt() will subset to the five columns, then unique().
  if (nrow(dt) >= 1L) {
    dup_block <- dt[1:min(3L, nrow(dt))] # few rows to duplicate
    dt <- data.table::rbindlist(
      list(dt, dup_block),
      use.names = TRUE,
      fill = TRUE
    )
  }

  out <- audpc_dt(dt)
  # Unique on the five columns should match unique(dt[, .SD, .SDcols = req_cols])
  expected <- unique(dt[, .SD, .SDcols = req_cols])
  data.table::setkey(expected, location, emergence)

  # Same set and order after keying
  expect_identical(out, expected)
})

test_that("audpc_dt() does not key or mutate the input by reference", {
  skip_if_not(
    exists("epicrop.sim", inherits = FALSE),
    "no epicrop.sim test data"
  )

  dt <- data.table::copy(epicrop_sim)
  # Remove any keys from the copy to know baseline
  if (!is.null(data.table::key(dt))) data.table::setkey(dt, NULL)
  in_key_before <- data.table::key(dt)

  invisible(audpc_dt(dt))

  # Ensure original 'dt' key unchanged (function subsets internally then keys the subset)
  expect_identical(data.table::key(dt), in_key_before)
})

test_that("audpc_dt() tolerates factors/characters for grouping fields", {

  dt_char <- data.table::copy(epicrop_sim)
  # Force to character
  dt_char[, location := as.character(location)]
  dt_char[, emergence := as.character(emergence)]
  out_char <- audpc_dt(dt_char)
  expect_s3_class(out_char, "data.table")

  dt_fac <- data.table::copy(epicrop_sim)
  dt_fac[, location := as.factor(location)]
  dt_fac[, emergence := as.factor(emergence)]
  out_fac <- audpc_dt(dt_fac)
  expect_s3_class(out_fac, "data.table")

  # Same rows regardless of type (after converting for comparison)
  out_char_cmp <- out_char[, .(
    lat,
    lon,
    AUDPC,
    location = as.character(location),
    emergence = as.character(emergence)
  )]
  out_fac_cmp <- out_fac[, .(
    lat,
    lon,
    AUDPC,
    location = as.character(location),
    emergence = as.character(emergence)
  )]
  data.table::setkey(out_char_cmp, location, emergence, lat, lon, AUDPC)
  data.table::setkey(out_fac_cmp, location, emergence, lat, lon, AUDPC)
  expect_identical(out_char_cmp, out_fac_cmp)
})

test_that("audpc_dt() fails informatively if required columns are missing", {
  skip_if_not(
    exists("epicrop.sim", inherits = FALSE),
    "no epicrop.sim test data"
  )

  # Start from a valid epicrop.sim object but drop a required column
  dt_bad <- data.table::copy(epicrop_sim)
  # Ensure it still has class epicrop.sim so we reach the .SDcols selection
  class(dt_bad) <- unique(c(
    "epicrop.sim",
    setdiff(class(dt_bad), "epicrop.sim")
  ))

  # Drop one of the required columns
  if ("AUDPC" %in% names(dt_bad)) dt_bad[, AUDPC := NULL]

  # data.table will error if .SDcols contains a missing name
  expect_error(
    audpc_dt(dt_bad),
    regexp = "column.*AUDPC|undefined columns|cannot select|object.*not found",
    ignore.case = TRUE
  )
})
