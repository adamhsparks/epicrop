
test_that("Values are as expected", {
  wth <- epicrop:::wth
  t <- predict_tungro(wth, emergence = "2000-07-01")

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
      "severity",
      "intensity",
      "lat",
      "lon"
    )
  )
  expect_is(t, c("data.table", "data.frame"))
  expect_equal(nrow(t), 121)

  # check total sites
  expect_equal(t[[1, 3]], 100) # 600 default to start sites
  expect_equal(t[[121, 3]], 81.28, tolerance = 0.001)

  # check latent sites
  expect_equal(t[[1, 4]], 0) # 600 default to start sites
  expect_equal(t[[121, 4]], 0, tolerance = 0.01)

  # check infectious sites
  expect_equal(t[[1, 5]], 0)
  expect_equal(t[[121, 5]], 8.71, tolerance = 0.001)

  # check removed sites
  expect_equal(t[[1, 6]], 0)
  expect_equal(t[[121, 6]], 0, tolerance = 0.01)

  # check senesced sites
  expect_equal(t[[1, 7]], 0)
  expect_equal(t[[121, 7]], 103.94, tolerance = 0.001)

  # check rateinf
  expect_equal(t[[1, 8]], 0)
  expect_equal(t[[121, 8]], 0, tolerance = 0.01)

  # check rtransfer
  expect_equal(t[[1, 9]], 0)
  expect_equal(t[[121, 9]], 0, tolerance = 0.01)

  # check rgrowth
  expect_equal(t[[1, 10]], 0, tolerance = 0.01)
  expect_equal(t[[121, 10]], 0.812, tolerance = 0.001)

  # check rsenesced
  expect_equal(t[[1, 11]], 1)
  expect_equal(t[[121, 11]], 0.812, tolerance = 0.001)

  # check diseased
  expect_equal(t[[1, 12]], 0)
  expect_equal(t[[121, 12]], 8.71, tolerance = 0.001)

  # check severity values
  expect_equal(t[[1, 13]], 0)
  expect_equal(t[[121, 13]], 9.678, tolerance = 0.0001)

  # check intensity values
  expect_equal(t[[1, 14]], 0)
  expect_equal(t[[121, 14]], 0.9678, tolerance = 0.0001)

  # check lat/lon values
  expect_equal(t[[1, 15]], 14.68, tolerance = 0.01)
  expect_equal(t[[1, 16]], 121.3, tolerance = 0.1)
})
