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
  expect_equal(nrow(lb), 120)

  expect_equal(lb[[1, "sites"]], 600)
  expect_equal(lb[[120, "sites"]], 85301.36, tolerance = 0.1)

  expect_equal(lb[[1, "latent"]], 0, tolerance = 0.1)
  expect_equal(lb[[120, "latent"]], 0, tolerance = 0.1)

  expect_equal(lb[[1, "infectious"]], 0, tolerance = 0.1)
  expect_equal(lb[[120, "infectious"]], 0.1924758, tolerance = 0.001)

  expect_equal(lb[[1, "removed"]], 0, tolerance = 0.1)
  expect_equal(lb[[120, "removed"]], 56.80631, tolerance = 0.1)

  expect_equal(lb[[1, "senesced"]], 0, tolerance = 0.1)
  expect_equal(lb[[120, "senesced"]], 27742.37, tolerance = 0.1)

  expect_equal(lb[[1, "rateinf"]], 0, tolerance = 0.1)
  expect_equal(lb[[120, "rateinf"]], 0, tolerance = 0.1)

  expect_equal(lb[[1, "rtransfer"]], 0, tolerance = 0.1)
  expect_equal(lb[[120, "rtransfer"]], 0, tolerance = 0.1)

  expect_equal(lb[[1, "rgrowth"]], 58.8, tolerance = 0.1)
  expect_equal(lb[[120, "rgrowth"]], 439.9315, tolerance = 0.1)

  expect_equal(lb[[1, "rsenesced"]], 3.0, tolerance = 0.1)
  expect_equal(lb[[120, "rsenesced"]], 426.5068, tolerance = 0.1)

  expect_equal(lb[[1, "diseased"]], 0, tolerance = 0.1)
  expect_equal(lb[[120, "diseased"]], 57.00316, tolerance = 0.1)

  expect_equal(lb[[1, "intensity"]], 0, tolerance = 0.1)
  expect_equal(lb[[120, "intensity"]], 0, tolerance = 0.1)

  expect_equal(lb[[1, "lat"]], 14.6774, tolerance = 0.01)
  expect_equal(lb[[1, "lon"]], 121.2556, tolerance = 0.01)
})
