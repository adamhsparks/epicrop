
context("predict_leaf_blast()")
test_that("Values are as expected", {
   wth <- get_wth(
     lonlat = c(121.25562, 14.6774),
     dates = c("2000-05-15", "2000-12-31")
   )
   lb <- predict_leaf_blast(wth, emergence = "2000-05-18")

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
      "severity"
    )
  )
  expect_is(lb, c("data.table", "data.frame"))
  expect_equal(nrow(lb), 121)

  # check total sites
  expect_equal(lb[[1, 3]], 600) # 600 default to start sites
  expect_equal(lb[[121, 3]], 26931.2, tolerance = 0.01)

  # check latent sites
  expect_equal(lb[[1, 4]], 0) # 600 default to start sites
  expect_equal(lb[[121, 4]], 0.004055, tolerance = 0.0000001)

  # check infectious sites
  expect_equal(lb[[1, 5]], 0)
  expect_equal(lb[[121, 5]], 0, tolerance = 0.1)

  # check removed sites
  expect_equal(lb[[1, 6]], 0)
  expect_equal(lb[[121, 6]], 45.90, tolerance = 0.1)

  # check senesced sites
  expect_equal(lb[[1, 7]], 0)
  expect_equal(lb[[121, 7]], 20608.17, tolerance = 0.01)

  # check rateinf
  expect_equal(lb[[1, 8]], 0)
  expect_equal(lb[[121, 8]], 0.0006259, tolerance = 0.0000001)

  # check rtransfer
  expect_equal(lb[[1, 9]], 0)
  expect_equal(lb[[121, 9]], 0.0008146, tolerance = 0.0000001)

  # check rgrowth
  expect_equal(lb[[1, 10]], 58.80, tolerance = 0.01)
  expect_equal(lb[[121, 10]], 271.36, tolerance = 0.01)

  # check rsenesced
  expect_equal(lb[[1, 11]], 6)
  expect_equal(lb[[121, 11]], 269.325, tolerance = 0.01)

  # check diseased
  expect_equal(lb[[1, 12]], 0)
  expect_equal(lb[[121, 12]], 45.99, tolerance = 0.01)

  # check severity values
  expect_equal(lb[[1, 13]], 0)
  expect_equal(lb[[121, 13]], 0.0003365, tolerance = 0.0000001)
})
