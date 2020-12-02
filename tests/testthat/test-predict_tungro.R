
context("predict_tungro()")
test_that("Values are as expected", {
   wth <- get_wth(
     lonlat = c(121.25562, 14.6774),
     dates = c("2000-05-15", "2000-12-31")
   )
   t <- predict_tungro(wth, emergence = "2000-05-18")

  expect_named(
    t,
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
  expect_is(t, c("data.table", "data.frame"))
  expect_equal(nrow(t), 121)

  # check total sites
  expect_equal(t[[1, 3]], 100) # 600 default to start sites
  expect_equal(t[[121, 3]], 77.41, tolerance = 0.01)

  # check latent sites
  expect_equal(t[[1, 4]], 0) # 600 default to start sites
  expect_equal(t[[121, 4]], 0, tolerance = 0.01)

  # check infectious sites
  expect_equal(t[[1, 5]], 0)
  expect_equal(t[[121, 5]], 12.59, tolerance = 0.01)

  # check removed sites
  expect_equal(t[[1, 6]], 0)
  expect_equal(t[[121, 6]], 0, tolerance = 0.01)

  # check senesced sites
  expect_equal(t[[1, 7]], 0)
  expect_equal(t[[121, 7]], 100.947, tolerance = 0.01)

  # check rateinf
  expect_equal(t[[1, 8]], 0)
  expect_equal(t[[121, 8]], 0, tolerance = 0.01)

  # check rtransfer
  expect_equal(t[[1, 9]], 0)
  expect_equal(t[[121, 9]], 0, tolerance = 0.01)

  # check rgrowth
  expect_equal(t[[1, 10]], 0, tolerance = 0.01)
  expect_equal(t[[121, 10]], 0.7741, tolerance = 0.0001)

  # check rsenesced
  expect_equal(t[[1, 11]], 1)
  expect_equal(t[[121, 11]], 0.7741, tolerance = 0.0001)

  # check diseased
  expect_equal(t[[1, 12]], 0)
  expect_equal(t[[121, 12]], 12.59, tolerance = 0.001)

  # check severity values
  expect_equal(t[[1, 13]], 0)
  expect_equal(t[[121, 13]], 13.99, tolerance = 0.001)
})
