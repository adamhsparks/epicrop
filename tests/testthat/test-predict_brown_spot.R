
context("predict_brown_spot()")
test_that("Values are as expected", {
   wth <- get_wth(
     lonlat = c(121.25562, 14.6774),
     dates = c("2000-05-15", "2000-12-31")
   )
   bs <- predict_brown_spot(wth, emergence = "2000-05-18")

  expect_named(
    bs,
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
      "severity",
      "lat",
      "lon"
    )
  )
  expect_is(bs, c("data.table", "data.frame"))
  expect_equal(nrow(bs), 121)

  # check total sites
  expect_equal(bs[[1, 3]], 600) # 600 default to start sites
  expect_equal(bs[[121, 3]], 85803.1, tolerance = 0.01)

  # check latent sites
  expect_equal(bs[[1, 4]], 0) # 600 default to start sites
  expect_equal(bs[[121, 4]], 2500, tolerance = 0.1)

  # check infectious sites
  expect_equal(bs[[1, 5]], 0)
  expect_equal(bs[[121, 5]], 1217.6, tolerance = 0.01)

  # check removed sites
  expect_equal(bs[[1, 6]], 0)
  expect_equal(bs[[121, 6]], 143, tolerance = 0.01)

  # check senesced sites
  expect_equal(bs[[1, 7]], 0)
  expect_equal(bs[[121, 7]], 56012.04, tolerance = 0.01)

  # check rateinf
  expect_equal(bs[[1, 8]], 0)
  expect_equal(bs[[121, 8]], 652, tolerance = 0.01)

  # check rtransfer
  expect_equal(bs[[1, 9]], 0)
  expect_equal(bs[[121, 9]], 296.3, tolerance = 0.01)

  # check rgrowth
  expect_equal(bs[[1, 10]], 59.64, tolerance = 0.01)
  expect_equal(bs[[121, 10]], 886.86, tolerance = 0.01)

  # check rsenesced
  expect_equal(bs[[1, 11]], 6)
  expect_equal(bs[[121, 11]], 881.112, tolerance = 0.0001)

  # check diseased
  expect_equal(bs[[1, 12]], 0)
  expect_equal(bs[[121, 12]], 3861, tolerance = 0.01)

  # check severity values
  expect_equal(bs[[1, 13]], 0)
  expect_equal(bs[[121, 13]], 4.153, tolerance = 0.0001)

  # check lat/lon values
  expect_equal(bs[[1, 14]], 14.68, tolerance = 0.01)
  expect_equal(bs[[1, 15]], 121.3, tolerance = 0.1)
})
