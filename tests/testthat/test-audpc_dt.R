# Helper that loads the epicrop.sim object for all tests.
# Adjust filename if needed.
epicrop_sim <- testthat::test_path("testdata", "epicrop_sim.rda")
load(epicrop_sim) # loads object 'epicrop.sim'
epicrop_sim <- bb_sim # rename for clarity in tests

test_that("audpc_dt() errors with non-epicrop.sim input", {
  expect_error(
    audpc_dt(data.frame()),
    regexp = "epicrop\\.sim",
    ignore.case = TRUE
  )
  x <- data.table::data.table(
    lat = 0,
    lon = 0,
    AUDPC = 1,
    location = "A",
    emergence = "2020-01-01"
  )
  # Even if columns exist, class must be epicrop.sim
  expect_error(audpc_dt(x), regexp = "epicrop\\.sim", ignore.case = TRUE)
})

test_that("audpc_dt() returns a data.table with expected four columns", {
  out <- audpc_dt(epicrop_sim)

  # Class and structure
  expect_s3_class(out, "data.table")
  expect_s3_class(out, "data.frame")
  expect_equal(
    sort(names(out)),
    sort(c("lat", "lon", "AUDPC", "emergence"))
  )
  expect_equal(ncol(out), 4L)

  # Column types
  expect_true(is.numeric(out$lat))
  expect_true(is.numeric(out$lon))
  expect_true(is.numeric(out$AUDPC))
  expect_true(is.character(out$emergence) || is.factor(out$emergence))
})

test_that("audpc_dt() sets key to emergence and ordering reflects the key", {
  out <- audpc_dt(epicrop_sim)

  # data.table key
  expect_identical(data.table::key(out), "emergence")
})

test_that("audpc_dt() removes duplicate rows on the five selected columns", {
  # Create a version with deliberate duplicated rows on the five columns
  x <- data.table::copy(epicrop_sim)

  # If epicrop.sim has additional columns, we can still duplicate rows;
  # audpc_dt() will subset to the five columns, then unique().
  if (nrow(x) >= 1L) {
    dup_block <- x[seq_len(min(3L, nrow(x)))] # few rows to duplicate
    x <- data.table::rbindlist(
      list(x, dup_block),
      use.names = TRUE,
      fill = TRUE
    )
  }

  class(x) <- unique(c(
    "epicrop.sim",
    setdiff(class(x), "epicrop.sim")
  ))

  # these next two lines come from epicrop_sim.R to calculate AUDPC and add it
  # as an attribute to the epicrop.sim object
  audpc_value <- .calculate_audpc(x$intensity)
  data.table::setattr(x, "AUDPC", audpc_value)

  out <- audpc_dt(x)
  # Unique on the five columns should match unique(x[, .SD, .SDcols = req_cols])
  expected <- unique(x[, .SD, .SDcols = req_cols])
  data.table::setkey(expected, location, emergence)

  # Same set and order after keying
  expect_identical(out, expected)
})

test_that("audpc_dt() does not key or mutate the input by reference", {
  skip_if_not(
    exists("epicrop.sim", inherits = FALSE),
    "no epicrop.sim test data"
  )

  x <- data.table::copy(epicrop_sim)
  # Remove any keys from the copy to know baseline
  if (!is.null(data.table::key(x))) {
    data.table::setkey(x, NULL)
  }
  in_key_before <- data.table::key(x)

  invisible(audpc_dt(x))

  # Ensure original 'dt' key unchanged (function subsets internally then keys the subset)
  expect_identical(data.table::key(x), in_key_before)
})

test_that("audpc_dt() tolerates factors/characters for grouping fields", {
  dt_char <- data.table::copy(epicrop_sim)
  # Force to character
  dt_char[, emergence := as.character(emergence)]
  out_char <- audpc_dt(x_char)
  expect_s3_class(out_char, "data.table")

  dt_fac <- data.table::copy(epicrop_sim)
  dt_fac[, location := as.factor(location)]
  dt_fac[, emergence := as.factor(emergence)]
  out_fac <- audpc_dt(x_fac)
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
  # Start from a valid epicrop.sim object but drop a required column
  dt_bad <- data.table::copy(epicrop_sim)
  # Ensure it still has class epicrop.sim so we reach the .SDcols selection
  class(x_bad) <- unique(c(
    "epicrop.sim",
    setdiff(class(x_bad), "epicrop.sim")
  ))

  # Drop one of the required columns
  if ("AUDPC" %in% names(x_bad)) {
    dt_bad[, AUDPC := NULL]
  }

  # data.table will error if .SDcols contains a missing name
  expect_error(
    audpc_dt(x_bad),
    regexp = "column.*AUDPC|undefined columns|cannot select|object.*not found",
    ignore.case = TRUE
  )
})
