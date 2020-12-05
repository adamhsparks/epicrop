
test_that("Values are as expected", {
   wth <- get_wth(
     lonlat = c(121.25562, 14.6774),
     dates = c("2000-05-15", "2000-12-31")
   )
   sb <- predict_sheath_blight(wth, emergence = "2000-05-18")

  expect_named(
    sb,
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
  expect_is(sb, c("data.table", "data.frame"))
  expect_equal(nrow(sb), 121)

  # check total sites
  expect_equal(sb[[1, 3]], 25.00)
  expect_equal(sb[[121, 3]], 171.20, tolerance = 0.01)

  # check latent sites
  expect_equal(sb[[1, 4]], 0)
  expect_equal(sb[[121, 4]], 12.66, tolerance = 0.001)

  # check infectious sites
  expect_equal(sb[[1, 5]], 0)
  expect_equal(sb[[121, 5]], 596.1, tolerance = 0.01)

  # check removed sites
  expect_equal(sb[[1, 6]], 0)
  expect_equal(sb[[121, 6]], 0, tolerance = 0.01)

  # check senesced sites
  expect_equal(sb[[1, 7]], 0)
  expect_equal(sb[[121, 7]], 280.5521, tolerance = 0.0001)

  # check rateinf
  expect_equal(sb[[1, 8]], 0)
  expect_equal(sb[[121, 8]], 3.724, tolerance = 0.01)

  # check rtransfer
  expect_equal(sb[[1, 9]], 0)
  expect_equal(sb[[121, 9]], 4.479, tolerance = 0.001)

  # check rgrowth
  expect_equal(sb[[1, 10]], 4.8438, tolerance = 0.01)
  expect_equal(sb[[121, 10]], 0.8560, tolerance = 0.0001)

  # check rsenesced
  expect_equal(sb[[1, 11]], 0.1250)
  expect_equal(sb[[121, 11]], 0.8560, tolerance = 0.0001)

  # check diseased
  expect_equal(sb[[1, 12]], 0)
  expect_equal(sb[[121, 12]], 608.8, tolerance = 0.1)

  # check severity values
  expect_equal(sb[[1, 13]], 0)
  expect_equal(sb[[121, 13]], 78.05, tolerance = 0.01)

  # check lat/lon values
  expect_equal(sb[[1, 14]], 14.68, tolerance = 0.01)
  expect_equal(sb[[1, 15]], 121.3, tolerance = 0.1)
})
