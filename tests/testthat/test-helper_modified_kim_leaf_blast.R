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

  expect_is(lb, c("data.table", "data.frame"))
  expect_identical(nrow(lb), 100L)

  expect_identical(lb[[1L, "sites"]], 600.0)
  expect_equal(lb[[100L, "sites"]], 85301.36, tolerance = 0.1)

  expect_equal(lb[[1L, "latent"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[100L, "latent"]], 0.0, tolerance = 0.1)

  expect_equal(lb[[1L, "infectious"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[100L, "infectious"]], 3.183338, tolerance = 0.1)

  expect_equal(lb[[1L, "removed"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[100L, "removed"]], 56.80631, tolerance = 0.1)

  expect_equal(lb[[1L, "senesced"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[100L, "senesced"]], 19241.75, tolerance = 0.1)

  expect_equal(lb[[1L, "rateinf"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[100L, "rateinf"]], 0.0, tolerance = 0.1)

  expect_equal(lb[[1L, "rtransfer"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[100L, "rtransfer"]], 0.0, tolerance = 0.1)

  expect_equal(lb[[1L, "rgrowth"]], 71.52, tolerance = 0.1)
  expect_equal(lb[[100L, "rgrowth"]], 346.2452, tolerance = 0.1)

  expect_equal(lb[[1L, "rsenesced"]], 3.0, tolerance = 0.1)
  expect_equal(lb[[100L, "rsenesced"]], 426.5068, tolerance = 0.1)

  expect_equal(lb[[1L, "diseased"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[100L, "diseased"]], 57.00316, tolerance = 0.1)

  expect_equal(lb[[1L, "intensity"]], 0.0, tolerance = 0.1)
  expect_equal(lb[[100L, "intensity"]], 0.0, tolerance = 0.1)

  expect_equal(lb[[1L, "lat"]], 14.6774, tolerance = 0.01)
  expect_equal(lb[[1L, "lon"]], 121.2556, tolerance = 0.01)
})

withr::deferred_clear()
