# ---- Coverage of every crop/disease pair -----------------------------------

test_that("epi_get_params retrieves parameters for every crop/disease pair", {
  crops <- names(.epi_params)
  expect_gt(length(crops), 0)

  for (crop in crops) {
    diseases <- names(.epi_params[[crop]])
    expect_gt(length(diseases), 0)

    for (disease in diseases) {
      p <- epi_get_params(crop, disease)
      expect_type(p, "list")
      expect_true(all(c("RcA", "RcT", "RRG") %in% names(p)))
      # Rice diseases use RRS; wheat diseases use RRPSN
      expect_true("RRS" %in% names(p) || "RRPSN" %in% names(p))
    }
  }
})

test_that("epi_get_params RcA/RcT tables are 2-column numeric matrices", {
  for (crop in names(.epi_params)) {
    for (disease in names(.epi_params[[crop]])) {
      p <- epi_get_params(crop, disease)
      for (nm in c("RcA", "RcT")) {
        tbl <- p[[nm]]
        expect_true(is.matrix(tbl) || is.data.frame(tbl))
        expect_identical(ncol(tbl), 2L)
        expect_true(is.numeric(tbl[, 1]))
        expect_true(is.numeric(tbl[, 2]))
      }
    }
  }
})

test_that("epi_get_params RRG/RRS/RRPSN entries are scalar or 2-col tables", {
  for (crop in names(.epi_params)) {
    for (disease in names(.epi_params[[crop]])) {
      p <- epi_get_params(crop, disease)
      rate_names <- intersect(c("RRG", "RRS", "RRPSN"), names(p))
      for (nm in rate_names) {
        val <- p[[nm]]
        is_scalar <- is.numeric(val) && length(val) == 1L
        is_table <- (is.matrix(val) || is.data.frame(val)) && ncol(val) == 2L
        expect_true(
          is_scalar || is_table,
          info = sprintf(
            "%s/%s/%s is neither scalar nor 2-col table",
            crop,
            disease,
            nm
          )
        )
      }
    }
  }
})

# ---- Lookup behaviour --------------------------------------------------

test_that("epi_get_params is case-insensitive for crop and disease", {
  p_lower <- epi_get_params("rice", "leaf_blast")
  p_upper <- epi_get_params("RICE", "LEAF_BLAST")
  p_mixed <- epi_get_params("Rice", "Leaf_Blast")

  expect_identical(p_lower, p_upper)
  expect_identical(p_lower, p_mixed)
})

test_that("epi_get_params returns the exact objects stored in .epi_params", {
  expect_identical(
    epi_get_params("rice", "tungro"),
    .epi_params$rice$tungro
  )
  expect_identical(
    epi_get_params("wheat", "s_tritici_blotch"),
    .epi_params$wheat$s_tritici_blotch
  )
  expect_identical(
    epi_get_params("rice_modified_kim", "sheath_blight"),
    .epi_params$rice_modified_kim$sheath_blight
  )
})

# ---- Error handling ----------------------------------------------------

test_that("epi_get_params errors informatively for unknown crop", {
  expect_error(epi_get_params("barley", "leaf_blast"), "Unknown crop")
  expect_error(epi_get_params("barley", "leaf_blast"), "barley")
})

test_that("epi_get_params errors informatively for unknown disease", {
  expect_error(epi_get_params("rice", "stem_rust"), "Unknown disease")
  expect_error(epi_get_params("rice", "stem_rust"), "stem_rust")
})

test_that("epi_get_params error messages list the available options", {
  err <- tryCatch(
    epi_get_params("wheat", "not_a_disease"),
    error = function(e) e
  )
  expect_match(conditionMessage(err), "leaf_rust")
  expect_match(conditionMessage(err), "s_tritici_blotch")

  err2 <- tryCatch(
    epi_get_params("not_a_crop", "x"),
    error = function(e) e
  )
  expect_match(conditionMessage(err2), "rice")
  expect_match(conditionMessage(err2), "wheat")
})

# ---- .epi_params is internal, not part of the public API ------------------

test_that(".epi_params is not exported from the package namespace", {
  expect_false(".epi_params" %in% getNamespaceExports("epicrop"))
})

# ---- epi_eval_param ---------------------------------------------------

test_that("epi_eval_param evaluates a scalar parameter by recycling", {
  expect_identical(epi_eval_param(0.1, at = c(1, 2, 3)), rep(0.1, 3))
  expect_identical(epi_eval_param(5L, at = numeric(0)), numeric(0))
})

test_that("epi_eval_param matches stats::approx(rule = 2) for table parameters", {
  tbl <- cbind(x = c(0, 10, 20), y = c(0, 1, 0.5))
  at <- c(-5, 0, 5, 10, 15, 20, 25)

  expected <- stats::approx(tbl[, "x"], tbl[, "y"], xout = at, rule = 2)$y

  expect_equal(epi_eval_param(tbl, at = at), expected)
})

test_that("epi_eval_param works on real stored parameter tables", {
  p <- epi_get_params("wheat", "leaf_rust")

  # RcT: optimum near 15C for leaf rust (Savary et al. 2015, Table 2)
  vals <- epi_eval_param(p$RcT, at = c(0, 15, 40))
  expect_length(vals, 3)
  expect_true(all(vals >= 0))
  expect_equal(vals[2], max(p$RcT[, 2]))
})

test_that("epi_eval_param errors for invalid parameter types", {
  expect_error(epi_eval_param("not_valid", at = 1), "numeric scalar")
  expect_error(epi_eval_param(list(1, 2), at = 1), "numeric scalar")
})
