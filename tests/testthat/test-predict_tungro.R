
test_that("Values are as expected", {
  wth <- epicrop:::wth
  tungro <- predict_tungro(wth, emergence = "2000-07-01")

  expect_named(
    tungro,
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

  expect_is(tungro, c("data.table", "data.frame"))
  expect_equal(nrow(tungro), 121)

  expect_equal(tungro[[1, "sites"]], 100)
  expect_equal(tungro[[121, "sites"]], 80.7, tolerance = 0.1)

  expect_equal(tungro[[1, "latent"]], 0, tolerance = 0.1)
  expect_equal(tungro[[121, "latent"]], 0, tolerance = 0.1)

  expect_equal(tungro[[1, "infectious"]], 0, tolerance = 0.1)
  expect_equal(tungro[[121, "infectious"]], 9.2, tolerance = 0.1)

  expect_equal(tungro[[1, "removed"]], 0, tolerance = 0.1)
  expect_equal(tungro[[121, "removed"]], 0, tolerance = 0.1)

  expect_equal(tungro[[1, "senesced"]], 0, tolerance = 0.1)
  expect_equal(tungro[[121, "senesced"]], 103.6, tolerance = 0.1)

  expect_equal(tungro[[1, "rateinf"]], 0, tolerance = 0.1)
  expect_equal(tungro[[121, "rateinf"]], 0, tolerance = 0.1)

  expect_equal(tungro[[1, "rtransfer"]], 0, tolerance = 0.1)
  expect_equal(tungro[[121, "rtransfer"]], 0, tolerance = 0.1)

  expect_equal(tungro[[1, "rgrowth"]], 0, tolerance = 0.1)
  expect_equal(tungro[[121, "rgrowth"]], 0.8, tolerance = 0.1)

  expect_equal(tungro[[1, "rsenesced"]], 1)
  expect_equal(tungro[[121, "rsenesced"]], 0.8, tolerance = 0.1)

  expect_equal(tungro[[1, "diseased"]], 0)
  expect_equal(tungro[[121, "diseased"]], 9.2, tolerance = 0.1)

  expect_equal(tungro[[1, "intensity"]], 0)
  expect_equal(tungro[[121, "intensity"]], 0.1, tolerance = 0.1)

  expect_equal(tungro[[1, "lat"]], 14.67741, tolerance = 0.00001)
  expect_equal(tungro[[1, "lon"]], 121.2556, tolerance = 0.0001)
})
