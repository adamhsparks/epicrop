
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
  expect_equal(nrow(tungro), 120)

  expect_equal(tungro[[1, "sites"]], 100)
  expect_equal(tungro[[120, "sites"]], 80.7, tolerance = 0.1)

  expect_equal(tungro[[1, "latent"]], 0, tolerance = 0.1)
  expect_equal(tungro[[120, "latent"]], 0, tolerance = 0.1)

  expect_equal(tungro[[1, "infectious"]], 0, tolerance = 0.1)
  expect_equal(tungro[[120, "infectious"]], 9.2, tolerance = 0.1)

  expect_equal(tungro[[1, "removed"]], 0, tolerance = 0.1)
  expect_equal(tungro[[120, "removed"]], 0, tolerance = 0.1)

  expect_equal(tungro[[1, "senesced"]], 0, tolerance = 0.1)
  expect_equal(tungro[[120, "senesced"]], 103.6, tolerance = 0.1)

  expect_equal(tungro[[1, "rateinf"]], 0, tolerance = 0.1)
  expect_equal(tungro[[120, "rateinf"]], 0, tolerance = 0.1)

  expect_equal(tungro[[1, "rtransfer"]], 0, tolerance = 0.1)
  expect_equal(tungro[[120, "rtransfer"]], 0, tolerance = 0.1)

  expect_equal(tungro[[1, "rgrowth"]], 0, tolerance = 0.1)
  expect_equal(tungro[[120, "rgrowth"]], 0.8, tolerance = 0.1)

  expect_equal(tungro[[1, "rsenesced"]], 1)
  expect_equal(tungro[[120, "rsenesced"]], 0.8, tolerance = 0.1)

  expect_equal(tungro[[1, "diseased"]], 0)
  expect_equal(tungro[[120, "diseased"]], 9.2, tolerance = 0.1)

  expect_equal(tungro[[1, "intensity"]], 0)
  expect_equal(tungro[[120, "intensity"]], 0.1, tolerance = 0.1)

  expect_equal(tungro[[1, "lat"]], 14.67741, tolerance = 0.00001)
  expect_equal(tungro[[1, "lon"]], 121.2556, tolerance = 0.0001)
})
