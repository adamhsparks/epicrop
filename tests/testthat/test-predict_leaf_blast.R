
test_that("Values are as expected", {
  wth <- epicrop:::wth
  lb <- predict_leaf_blast(wth, emergence = "2000-07-01")

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
      "lat",
      "lon"
    )
  )

  expect_is(lb, c("data.table", "data.frame"))
  expect_equal(nrow(lb), 120)

  expect_equal(lb[[1, "sites"]], 600)
  expect_equal(lb[[120, "sites"]], 26932.6, tolerance = 0.1)

  expect_equal(lb[[1, "latent"]], 0, tolerance = 0.1)
  expect_equal(lb[[120, "latent"]], 0, tolerance = 0.1)

  expect_equal(lb[[1, "infectious"]], 0, tolerance = 0.1)
  expect_equal(lb[[120, "infectious"]], 0.275, tolerance = 0.001)

  expect_equal(lb[[1, "removed"]], 0, tolerance = 0.1)
  expect_equal(lb[[120, "removed"]], 38.4, tolerance = 0.1)

  expect_equal(lb[[1, "senesced"]], 0, tolerance = 0.1)
  expect_equal(lb[[120, "senesced"]], 20606.0, tolerance = 0.1)

  expect_equal(lb[[1, "rateinf"]], 0, tolerance = 0.1)
  expect_equal(lb[[120, "rateinf"]], 0, tolerance = 0.1)

  expect_equal(lb[[1, "rtransfer"]], 0, tolerance = 0.1)
  expect_equal(lb[[120, "rtransfer"]], 0, tolerance = 0.1)

  expect_equal(lb[[1, "rgrowth"]], 58.8, tolerance = 0.1)
  expect_equal(lb[[120, "rgrowth"]], 271.4, tolerance = 0.1)

  expect_equal(lb[[1, "rsenesced"]], 6.0, tolerance = 0.1)
  expect_equal(lb[[120, "rsenesced"]], 269.3, tolerance = 0.1)

  expect_equal(lb[[1, "diseased"]], 0, tolerance = 0.1)
  expect_equal(lb[[120, "diseased"]], 38.7, tolerance = 0.1)

  expect_equal(lb[[1, "intensity"]], 0, tolerance = 0.1)
  expect_equal(lb[[120, "intensity"]], 0, tolerance = 0.1)

  expect_equal(lb[[1, "lat"]], 14.67741, tolerance = 0.00001)
  expect_equal(lb[[1, "lon"]], 121.2556, tolerance = 0.0001)
})

