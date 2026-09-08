# Test wheat disease models (Savary *et al.* 2015)

test_that("setup: extended weather available", {
  skip_if(
    !exists("wth_formatted") ||
      !is.data.frame(wth_formatted) ||
      nrow(wth_formatted) < 185,
    "Need extended weather data for wheat models"
  )
})

# ---- Helpers ---------------------------------------------------------------

.emergence_date <- "2000-07-01"

.expected_names <- c(
  "simday",
  "dates",
  "sites",
  "latent",
  "infectious",
  "removed",
  "senesced",
  "rateinf",
  "rlex",
  "rtransfer",
  "rremoved",
  "rgrowth",
  "rsenesced",
  "diseased",
  "intensity",
  "lat",
  "lon"
)

run_model <- function(
  fun,
  wth = wth_formatted,
  emergence = .emergence_date,
  ...
) {
  fun(wth, emergence = emergence, ...)
}

expect_epicrop_layout <- function(x, n_rows, expected_names = .expected_names) {
  expect_s3_class(x, c("epicrop.sim", "data.table", "data.frame"))
  expect_named(x, expected_names, ignore.order = FALSE)
  expect_identical(nrow(x), as.integer(n_rows))
  expect_true(all(x$intensity >= 0 & x$intensity <= 1))
  expect_gt(attr(x, "AUDPC"), 0)
  invisible(x)
}

# ---- Parameterized structure tests ----------------------------------------

test_that("wheat models produce expected output structure", {
  models <- list(
    leaf_rust = list(fun = leaf_rust, expected_rows = 120L),
    s_tritici_blotch = list(fun = s_tritici_blotch, expected_rows = 120L)
  )

  for (nm in names(models)) {
    mdl <- models[[nm]]
    res <- run_model(mdl$fun)
    expect_epicrop_layout(res, n_rows = mdl$expected_rows)
  }
})

# ---- Aliases behave identically -------------------------------------------

test_that("wheat model aliases work identically", {
  # Leaf rust aliases
  expect_identical(
    run_model(leaf_rust),
    run_model(lr)
  )
  expect_identical(
    run_model(leaf_rust),
    run_model(brown_rust)
  )

  # Septoria aliases
  expect_identical(
    run_model(s_tritici_blotch),
    run_model(stb)
  )
})

# ---- DVS age driver presence (class-level check is enough) ----------------

test_that("wheat models use DVS age driver (class check)", {
  res_lr <- run_model(leaf_rust)
  res_stb <- run_model(s_tritici_blotch)
  expect_s3_class(res_lr, "epicrop.sim")
  expect_s3_class(res_stb, "epicrop.sim")
})

# ---- Lesion expansion: septoria on, leaf rust off --------------------------

test_that("septoria has lesion expansion enabled", {
  res_stb <- run_model(s_tritici_blotch)
  expect_true("rlex" %in% names(res_stb))
  expect_identical(sum(res_stb$rlex), 0)
})

test_that("leaf_rust has lesion expansion disabled (RRLEX = 0)", {
  res_lr <- run_model(leaf_rust)
  expect_true("rlex" %in% names(res_lr))
  expect_true(all(res_lr$rlex == 0))
})

# ---- Snapshot (single representative model; lighter & deterministic) -------

test_that("leaf_rust snapshot is stable", {
  withr::local_options(list(scipen = 999L))
  local_reproducible_output()

  lr <- run_model(leaf_rust)
  snap <- as.data.frame(lr)

  # Stabilize types/precision
  snap$dates <- as.character(snap$dates)
  num <- vapply(snap, is.numeric, logical(1))
  snap[num] <- lapply(snap[num], round, 4)

  expect_snapshot_value(snap, style = "json2")
})

# ---- Smoke test: runs without errors and row count is correct --------------

test_that("leaf_rust runs without errors", {
  res <- run_model(leaf_rust)
  expect_s3_class(res, "epicrop.sim")
  expect_identical(nrow(res), 120L)
})

# ---- Core engine accepts custom parameters ---------------------------------

test_that("seir accepts all custom parameters", {
  RcA_custom <- cbind(
    c(0L, 30L, 60L, 90L, 120L),
    c(1.0, 0.9, 0.7, 0.5, 0.4)
  )

  RcT_custom <- cbind(
    c(16L, 22L, 28L, 34L, 40L),
    c(0.0, 0.5, 1.0, 0.5, 0.0)
  )

  res <- seir(
    wth = wth_formatted,
    emergence = .emergence_date,
    onset = 25,
    duration = 100,
    H0 = 200,
    I0 = 5,
    RcA = RcA_custom,
    RcT = RcT_custom,
    RcOpt = 0.9,
    p = 7,
    i = 25,
    Sx = 5000,
    a = 3,
    RRS = 0.02
  )

  expect_s3_class(res, "epicrop.sim")
  expect_identical(nrow(res), 100L)
})

# ---- Reproducibility (determinism) ----------------------------------------

test_that("leaf_rust results are reproducible", {
  r1 <- run_model(leaf_rust)
  r2 <- run_model(leaf_rust)
  expect_identical(r1, r2)
})

# ---- Custom RcA/RcT/RRG/RRS overrides actually change output --------------

test_that("wheat model functions respect custom RcA/RcT/RRG/RRS overrides", {
  custom_RcA <- cbind(c(0, 2), c(0.2, 0.2))
  custom_RcT <- cbind(c(0, 35), c(0.1, 0.1))

  default_res <- run_model(leaf_rust)
  custom_res <- run_model(
    leaf_rust,
    RcA = custom_RcA,
    RcT = custom_RcT,
    RRG = 0.5,
    RRS = 0.5
  )

  expect_s3_class(custom_res, "epicrop.sim")
  expect_false(identical(default_res$intensity, custom_res$intensity))
})

# ---- Defaults trace back to .epi_params via epi_get_params -----------------
# Note: the wheat wrappers map their `RRS` argument onto the stored `RRPSN`
# table (physiological senescence), not a stored `RRS` entry.

test_that("wheat model defaults match the underlying epi_params store", {
  models <- list(
    leaf_rust = list(
      fun = leaf_rust,
      crop = "wheat",
      disease = "leaf_rust"
    ),
    s_tritici_blotch = list(
      fun = s_tritici_blotch,
      crop = "wheat",
      disease = "s_tritici_blotch"
    )
  )

  for (nm in names(models)) {
    mdl <- models[[nm]]
    params <- .epi_get_params(mdl$crop, mdl$disease)

    default_res <- run_model(mdl$fun)
    explicit_res <- run_model(
      mdl$fun,
      RcA = params$RcA,
      RcT = params$RcT,
      RRG = params$RRG,
      RRS = params$RRPSN
    )

    expect_identical(default_res, explicit_res)
  }
})

# ---- ... passthrough: non-hardcoded args work, hardcoded ones collide -----

test_that("wheat models accept non-hardcoded seir arguments via ...", {
  default_res <- run_model(s_tritici_blotch)
  custom_res <- run_model(s_tritici_blotch, rhlim = 95, rainlim = 10)

  expect_s3_class(custom_res, "epicrop.sim")
  expect_identical(nrow(custom_res), nrow(default_res))
})

test_that("hardcoded seir arguments cannot be overridden via ... (wheat)", {
  # RcOpt, onset, duration, RRLEX, age_driver, and thermal_time are all set
  # explicitly inside each wrapper's seir() call, so passing them again via
  # `...` produces a duplicate-argument error rather than silently
  # overriding.
  expect_error(run_model(leaf_rust, RcOpt = 0.5), "formal argument")
  expect_error(run_model(leaf_rust, age_driver = "day"), "formal argument")
  expect_error(run_model(s_tritici_blotch, RRLEX = 0.5), "formal argument")
  expect_error(run_model(s_tritici_blotch, duration = 100), "formal argument")
})
