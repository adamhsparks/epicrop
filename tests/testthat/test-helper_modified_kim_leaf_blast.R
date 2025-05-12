withr::local_options(list(scipen = 999L))

test_that("Values are as expected", {
  # wth_formatted is loaded in setup-testdata.R
  lb <- modified_kim_leaf_blast(wth_formatted, emergence = "2000-07-01")

  expect_named(
    lb,
    c(
      "simday",
      "dates",
      "sites",
      "latent",
      "infectious",
      "removed",
      "senesced",
      "rateinf",
      "rtransfer",
      "rgrowth",
      "rsenesced",
      "diseased",
      "intensity",
      "AUDPC",
      "lat",
      "lon"
    )
  )

  expect_s3_class(lb, c("data.table", "data.frame"))
  expect_identical(nrow(lb), 77L)

  expect_identical(lb[[1L, "sites"]], 600.0)
  expect_equal(lb[[77L, "sites"]], 76696.3, tolerance = 0.1)

  expect_equal(lb[[1L, "latent"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[77L, "latent"]], 2.2, tolerance = 0.1)

  expect_equal(lb[[1L, "infectious"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[77L, "infectious"]], 20.3, tolerance = 0.1)

  expect_equal(lb[[1L, "removed"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[77L, "removed"]], 32.2, tolerance = 0.1)

  expect_equal(lb[[1L, "senesced"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[77L, "senesced"]], 11111.5, tolerance = 0.1)

  expect_equal(lb[[1L, "rateinf"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[77L, "rateinf"]], 0.0, tolerance = 0.1)

  expect_equal(lb[[1L, "rtransfer"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[77L, "rtransfer"]], 0.54, tolerance = 0.1)

  expect_equal(lb[[1L, "rgrowth"]], 71.52, tolerance = 0.1)
  expect_equal(lb[[77L, "rgrowth"]], 1016.1, tolerance = 0.1)

  expect_equal(lb[[1L, "rsenesced"]], 3.0, tolerance = 0.1)
  expect_equal(lb[[77L, "rsenesced"]], 383.5, tolerance = 0.1)

  expect_equal(lb[[1L, "diseased"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[77L, "diseased"]], 57.00316, tolerance = 0.1)

  expect_equal(lb[[1L, "intensity"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[77L, "intensity"]], 0.0, tolerance = 0.1)

  expect_equal(lb[[1L, "lat"]], 14.6774, tolerance = 0.01)
  expect_equal(lb[[1L, "lon"]], 121.2556, tolerance = 0.01)
})

withr::deferred_clear()
