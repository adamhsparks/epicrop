# Wrap all tests in a single cassette
vcr::use_cassette("audpc_dt_tests", {
  # Setup: Create test data
  setup_test_sim <- function() {
    wth <- get_wth(
      lonlat = c(121.25562, 14.6774),
      dates = c("2000-07-01", "2000-10-31")
    )

    sim <- brown_spot(
      wth = wth,
      emergence = "2000-07-01"
    )

    sim
  }

  test_that("audpc_dt validates input class", {
    expect_error(
      audpc_dt(data.frame()),
      class = "rlang_error"
    )

    expect_error(
      audpc_dt(data.frame()),
      "epicrop.sim"
    )

    fake_dt <- data.table::data.table(
      lat = 14.6,
      lon = 121.3,
      AUDPC = 10,
      emergence = "2000-07-01"
    )

    expect_error(
      audpc_dt(fake_dt),
      "epicrop.sim"
    )
  })

  test_that("audpc_dt returns data.table with correct structure", {
    sim <- setup_test_sim()
    result <- audpc_dt(sim)

    expect_s3_class(result, "data.table")
    expect_s3_class(result, "data.frame")
    expect_true("AUDPC" %in% names(result))
    expect_true("emergence" %in% names(result))
    expect_type(result$AUDPC, "double")
    expect_type(result$emergence, "character")
    expect_identical(nrow(result), 1L)
  })

  test_that("audpc_dt includes spatial columns when available", {
    sim <- setup_test_sim()
    result <- audpc_dt(sim)

    expect_true("lat" %in% names(result))
    expect_true("lon" %in% names(result))
    expect_type(result$lat, "double")
    expect_type(result$lon, "double")
  })

  test_that("audpc_dt handles missing spatial columns", {
    sim <- setup_test_sim()
    sim[, `:=`(lat = NULL, lon = NULL)]

    result <- audpc_dt(sim)

    expect_s3_class(result, "data.table")
    expect_true("AUDPC" %in% names(result))
    expect_true("emergence" %in% names(result))
    expect_false("lat" %in% names(result))
    expect_false("lon" %in% names(result))
  })

  test_that("audpc_dt sets key to emergence", {
    sim <- setup_test_sim()
    result <- audpc_dt(sim)

    expect_identical(data.table::key(result), "emergence")
  })

  test_that("audpc_dt has correct column order", {
    sim <- setup_test_sim()
    result <- audpc_dt(sim)

    actual_cols <- names(result)

    expect_true("AUDPC" %in% actual_cols)
    expect_true("emergence" %in% actual_cols)

    audpc_idx <- which(actual_cols == "AUDPC")
    emerg_idx <- which(actual_cols == "emergence")
    expect_lt(audpc_idx, emerg_idx)

    if ("lat" %in% actual_cols) {
      lat_idx <- which(actual_cols == "lat")
      expect_lt(lat_idx, audpc_idx)
    }

    if ("lon" %in% actual_cols) {
      lon_idx <- which(actual_cols == "lon")
      expect_lt(lon_idx, audpc_idx)
    }
  })

  test_that("audpc_dt extracts AUDPC from attribute", {
    sim <- setup_test_sim()

    audpc_attr <- attr(sim, "AUDPC")
    expect_type(audpc_attr, "double")
    expect_gt(audpc_attr, 0)

    result <- audpc_dt(sim)
    expect_identical(result$AUDPC, audpc_attr)
  })

  test_that("audpc_dt does not modify input by reference", {
    sim <- setup_test_sim()

    original_key <- data.table::key(sim)
    original_names <- names(sim)
    original_nrow <- nrow(sim)

    result <- audpc_dt(sim)

    expect_identical(data.table::key(sim), original_key)
    expect_named(sim, original_names)
    expect_identical(nrow(sim), original_nrow)
  })

  test_that("audpc_dt works with multiple disease models", {
    wth <- get_wth(
      lonlat = c(121.25562, 14.6774),
      dates = c("2000-07-01", "2000-10-31")
    )

    bb_sim <- bacterial_blight(wth, emergence = "2000-07-01")
    lb_sim <- leaf_blast(wth, emergence = "2000-07-01")
    sb_sim <- sheath_blight(wth, emergence = "2000-07-01")

    bb_result <- audpc_dt(bb_sim)
    lb_result <- audpc_dt(lb_sim)
    sb_result <- audpc_dt(sb_sim)

    expect_s3_class(bb_result, "data.table")
    expect_s3_class(lb_result, "data.table")
    expect_s3_class(sb_result, "data.table")

    expect_gt(bb_result$AUDPC, 0)
    expect_gt(lb_result$AUDPC, 0)
    expect_gt(sb_result$AUDPC, 0)

    expect_false(isTRUE(all.equal(bb_result$AUDPC, lb_result$AUDPC)))
  })

  test_that("audpc_dt errors informatively when AUDPC missing", {
    sim <- setup_test_sim()

    attr(sim, "AUDPC") <- NULL
    if ("AUDPC" %in% names(sim)) {
      sim[, AUDPC := NULL]
    }

    expect_error(
      audpc_dt(sim),
      "AUDPC"
    )
  })

  test_that("audpc_dt errors informatively when dates missing", {
    sim <- setup_test_sim()

    sim[, dates := NULL]
    if ("emergence" %in% names(sim)) {
      sim[, emergence := NULL]
    }

    expect_error(
      audpc_dt(sim),
      "dates|emergence"
    )
  })
}) # End vcr::use_cassette
