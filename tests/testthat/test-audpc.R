test_that(".calculate_audpc handles edge cases", {
  # Single day (no area)
  expect_equal(.calculate_audpc(c(0)), 0)

  # Two days
  result <- .calculate_audpc(c(0, 0.1))
  expect_true(result >= 0)
  expect_true(is.finite(result))

  # All zeros
  expect_equal(.calculate_audpc(rep(0, 10)), 0)

  # Constant intensity
  result <- .calculate_audpc(rep(0.5, 10))
  expect_true(result > 0)
  expect_true(is.finite(result))

  # Increasing intensity
  result <- .calculate_audpc(seq(0, 1, length.out = 10))
  expect_true(result > 0)
  expect_true(is.finite(result))
})

test_that(".calculate_audpc uses trapezoidal rule correctly", {
  # Simple trapezoid: area = (b1 + b2) * h / 2
  # For intensity c(0, 1), with 1 day between: (0 + 1) * 1 / 2 = 0.5
  result <- .calculate_audpc(c(0, 1))
  expect_equal(result, 0.5)

  # Rectangle: c(1, 1) over 1 day = 1
  result <- .calculate_audpc(c(1, 1))
  expect_equal(result, 1)

  # Multiple days: c(0, 0.5, 1)
  # Day 1: (0 + 0.5)/2 = 0.25
  # Day 2: (0.5 + 1)/2 = 0.75
  # Total: 1.0
  result <- .calculate_audpc(c(0, 0.5, 1))
  expect_equal(result, 1.0)
})

test_that("AUDPC calculation handles minimal simulations", {
  vcr::use_cassette("audpc_short_sim_new", {
    wth <- get_wth(
      lonlat = c(121.25562, 14.6774),
      dates = c("2000-07-01", "2000-08-25") # ~55 days
    )

    # Use minimum viable duration (50+ days)
    sim <- seir(
      wth = wth,
      emergence = "2000-07-01",
      onset = 5,
      duration = 50,
      H0 = 600,
      I0 = 1,
      RcA = cbind(c(0, 50), c(1, 1)),
      RcT = cbind(c(15, 35), c(0.5, 1)),
      RcOpt = 0.61,
      p = 6,
      i = 19,
      Sx = 100000,
      a = 1
    )

    expect_s3_class(sim, "epicrop.sim")

    audpc_val <- attr(sim, "AUDPC")
    expect_type(audpc_val, "double")
    expect_true(is.finite(audpc_val))
    expect_gt(audpc_val, 0)
  })
})


test_that("get_audpc extracts AUDPC attribute from epicrop.sim", {
  vcr::use_cassette("audpc_get_audpc_new", {
    wth <- get_wth(
      lonlat = c(121.25562, 14.6774),
      dates = c("2000-07-01", "2000-11-30") # 5 months
    )

    sim <- brown_spot(wth, emergence = "2000-07-01")

    audpc <- get_audpc(sim)

    expect_type(audpc, "double")
    expect_length(audpc, 1)
    expect_true(audpc > 0)
    expect_true(is.finite(audpc))

    # Should match attribute
    expect_equal(audpc, attr(sim, "AUDPC"))
  })
})

test_that("get_audpc errors on non-epicrop.sim objects", {
  # Just check for error with message
  expect_error(
    get_audpc(data.frame()),
    "epicrop.sim"
  )

  expect_error(
    get_audpc(list()),
    "epicrop.sim"
  )
})

test_that("get_audpc errors when AUDPC attribute missing", {
  vcr::use_cassette("audpc_missing_attr", {
    wth <- get_wth(
      lonlat = c(121.25562, 14.6774),
      dates = c("2000-07-01", "2000-11-30")
    )

    sim <- brown_spot(wth, emergence = "2000-07-01")

    # Remove AUDPC attribute
    attr(sim, "AUDPC") <- NULL

    expect_error(
      get_audpc(sim),
      "AUDPC attribute not found"
    )
  })
})
test_that("AUDPC is calculated for all disease models", {
  vcr::use_cassette("audpc_all_models_new", {
    wth <- get_wth(
      lonlat = c(121.25562, 14.6774),
      dates = c("2000-07-01", "2000-11-30") # 5 months
    )

    # Rice diseases
    bb <- bacterial_blight(wth, emergence = "2000-07-01")
    bs <- brown_spot(wth, emergence = "2000-07-01")
    lb <- leaf_blast(wth, emergence = "2000-07-01")
    sb <- sheath_blight(wth, emergence = "2000-07-01")
    tg <- tungro(wth, emergence = "2000-07-01")

    # All should have AUDPC attribute
    expect_false(is.null(attr(bb, "AUDPC")))
    expect_false(is.null(attr(bs, "AUDPC")))
    expect_false(is.null(attr(lb, "AUDPC")))
    expect_false(is.null(attr(sb, "AUDPC")))
    expect_false(is.null(attr(tg, "AUDPC")))

    # All should be positive and finite
    expect_true(attr(bb, "AUDPC") > 0 && is.finite(attr(bb, "AUDPC")))
    expect_true(attr(bs, "AUDPC") > 0 && is.finite(attr(bs, "AUDPC")))
    expect_true(attr(lb, "AUDPC") > 0 && is.finite(attr(lb, "AUDPC")))
    expect_true(attr(sb, "AUDPC") > 0 && is.finite(attr(sb, "AUDPC")))
    expect_true(attr(tg, "AUDPC") > 0 && is.finite(attr(tg, "AUDPC")))

    # Different diseases should have different AUDPC values
    audpc_values <- c(
      attr(bb, "AUDPC"),
      attr(bs, "AUDPC"),
      attr(lb, "AUDPC"),
      attr(sb, "AUDPC"),
      attr(tg, "AUDPC")
    )
    expect_gt(length(unique(audpc_values)), 1)
  })
})

test_that("AUDPC varies with simulation duration", {
  vcr::use_cassette("audpc_duration_new", {
    wth <- get_wth(
      lonlat = c(121.25562, 14.6774),
      dates = c("2000-07-01", "2000-12-31") # ~6 months
    )

    # Short simulation
    sim_short <- seir(
      wth = wth,
      emergence = "2000-07-01",
      onset = 20,
      duration = 60,
      H0 = 600,
      I0 = 1,
      RcA = cbind(c(0, 120), c(1, 1)),
      RcT = cbind(c(15, 35), c(0.5, 1)),
      RcOpt = 0.61,
      p = 6,
      i = 19,
      Sx = 100000,
      a = 1
    )
    audpc_short <- attr(sim_short, "AUDPC")

    # Long simulation
    sim_long <- seir(
      wth = wth,
      emergence = "2000-07-01",
      onset = 20,
      duration = 120,
      H0 = 600,
      I0 = 1,
      RcA = cbind(c(0, 120), c(1, 1)),
      RcT = cbind(c(15, 35), c(0.5, 1)),
      RcOpt = 0.61,
      p = 6,
      i = 19,
      Sx = 100000,
      a = 1
    )
    audpc_long <- attr(sim_long, "AUDPC")

    # Both should be valid
    expect_true(is.finite(audpc_short))
    expect_true(is.finite(audpc_long))
    expect_gte(audpc_short, 0)
    expect_gte(audpc_long, 0)

    # Longer simulation generally has higher AUDPC
    expect_gte(audpc_long, audpc_short)
  })
})

test_that("AUDPC handles low disease conditions", {
  vcr::use_cassette("audpc_low_disease_new", {
    wth <- get_wth(
      lonlat = c(121.25562, 14.6774),
      dates = c("2000-07-01", "2000-11-30")
    )

    # Create unfavorable conditions with very low RcOpt
    sim <- seir(
      wth = wth,
      emergence = "2000-07-01",
      onset = 20,
      duration = 120,
      H0 = 600,
      I0 = 1,
      RcA = cbind(c(0, 120), c(1, 1)),
      RcT = cbind(c(0, 50), c(1, 1)),
      RcOpt = 0.001, # Very low infection rate
      p = 6,
      i = 19,
      Sx = 100000,
      a = 1
    )

    audpc <- attr(sim, "AUDPC")

    # Should be very low
    expect_true(is.finite(audpc))
    expect_gte(audpc, 0)
    expect_lte(audpc, 1) # Very little disease
  })
})

test_that("AUDPC attribute persists in original object", {
  vcr::use_cassette("audpc_persist_new", {
    wth <- get_wth(
      lonlat = c(121.25562, 14.6774),
      dates = c("2000-07-01", "2000-11-30")
    )

    sim <- brown_spot(wth, emergence = "2000-07-01")
    original_audpc <- attr(sim, "AUDPC")

    # Subset (creates new object)
    sim_subset <- sim[1:50]

    # Original should still have it
    expect_identical(attr(sim, "AUDPC"), original_audpc)
    expect_true(is.finite(original_audpc))
  })
})

test_that("AUDPC is calculated from intensity column", {
  vcr::use_cassette("audpc_from_intensity_new", {
    wth <- get_wth(
      lonlat = c(121.25562, 14.6774),
      dates = c("2000-07-01", "2000-11-30")
    )

    sim <- brown_spot(wth, emergence = "2000-07-01")

    # Manual calculation
    manual_audpc <- .calculate_audpc(sim$intensity)
    attr_audpc <- attr(sim, "AUDPC")

    expect_identical(manual_audpc, attr_audpc)
  })
})

test_that("AUDPC varies with onset timing", {
  vcr::use_cassette("audpc_onset_timing_new", {
    wth <- get_wth(
      lonlat = c(121.25562, 14.6774),
      dates = c("2000-07-01", "2000-11-30")
    )

    # Early onset
    sim_early <- seir(
      wth = wth,
      emergence = "2000-07-01",
      onset = 5, # Early
      duration = 120,
      H0 = 600,
      I0 = 10,
      RcA = cbind(c(0, 120), c(1, 1)),
      RcT = cbind(c(15, 35), c(0.5, 1)),
      RcOpt = 0.61,
      p = 6,
      i = 19,
      Sx = 100000,
      a = 1
    )

    # Late onset
    sim_late <- seir(
      wth = wth,
      emergence = "2000-07-01",
      onset = 50, # Late
      duration = 120,
      H0 = 600,
      I0 = 10,
      RcA = cbind(c(0, 120), c(1, 1)),
      RcT = cbind(c(15, 35), c(0.5, 1)),
      RcOpt = 0.61,
      p = 6,
      i = 19,
      Sx = 100000,
      a = 1
    )

    audpc_early <- attr(sim_early, "AUDPC")
    audpc_late <- attr(sim_late, "AUDPC")

    expect_true(is.finite(audpc_early))
    expect_true(is.finite(audpc_late))
    expect_gte(audpc_early, 0)
    expect_gte(audpc_late, 0)

    # Early onset generally produces higher AUDPC
    expect_gte(audpc_early, audpc_late)
  })
})
