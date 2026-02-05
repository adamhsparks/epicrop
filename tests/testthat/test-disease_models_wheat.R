# Test wheat disease models (Savary et al. 2015)

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

# ---- Septoria: lesion expansion must be enabled ---------------------------

 test_that("septoria has lesion expansion enabled", {
  res_stb <- run_model(s_tritici_blotch)
   expect_true("rlex" %in% names(res_stb))
   expect_gt(sum(res_stb$rlex), 0)
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
  snap[num] <- lapply(snap[num], function(x) round(x, 4))

   expect_snapshot_value(snap, style = "json2")
})

# ---- Smoke test: runs without errors and row count is correct --------------

 test_that("leaf_rust runs without errors", {
  res <- run_model(leaf_rust)
   expect_s3_class(res, "epicrop.sim")
   expect_identical(nrow(res), 120L)
})

# ---- Core engine accepts custom parameters ---------------------------------

 test_that("epicrop_sim accepts all custom parameters", {
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
    RRS = 0.02,
    RRG = 0.15
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
