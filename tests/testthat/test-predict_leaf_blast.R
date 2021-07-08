
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
  expect_equal(nrow(lb), 121)

  # check total sites
  expect_equal(lb[[1, 3]], 600) # 600 default to start sites
  expect_equal(lb[[121, 3]], 26937, tolerance = 0.1)

  # check latent sites
  expect_equal(lb[[1, 4]], 0) # 600 default to start sites
  expect_equal(lb[[121, 4]], 0.00371, tolerance = 0.1)

  # check infectious sites
  expect_equal(lb[[1, 5]], 0)
  expect_equal(lb[[121, 5]], 0.1201, tolerance = 0.1)

  # check removed sites
  expect_equal(lb[[1, 6]], 0)
  expect_equal(lb[[121, 6]], 46.1, tolerance = 0.1)

  # check senesced sites
  expect_equal(lb[[1, 7]], 0)
  expect_equal(lb[[121, 7]], 20605, tolerance = 0.1)

  # check rateinf
  expect_equal(lb[[1, 8]], 0)
  expect_equal(lb[[121, 8]], 0.00111, tolerance = 0.001)

  # check rtransfer
  expect_equal(lb[[1, 9]], 0)
  expect_equal(lb[[121, 9]], 0, tolerance = 0.001)

  # check rgrowth
  expect_equal(lb[[1, 10]], 58.80, tolerance = 0.1)
  expect_equal(lb[[121, 10]], 271.4, tolerance = 0.1)

  # check rsenesced
  expect_equal(lb[[1, 11]], 6)
  expect_equal(lb[[121, 11]], 269.4, tolerance = 0.01)

  # check diseased
  expect_equal(lb[[1, 12]], 0)
  expect_equal(lb[[121, 12]], 46.26, tolerance = 0.01)

  # check intensity values
  expect_equal(lb[[1, 13]], 0)
  expect_equal(lb[[121, 13]], 0.0006381, tolerance = 0.0000001)

  # check lat/lon values
  expect_equal(lb[[1, 14]], 14.68, tolerance = 0.01)
  expect_equal(lb[[1, 15]], 121.3, tolerance = 0.1)
})
