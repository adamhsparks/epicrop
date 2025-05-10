withr::local_options(list(scipen = 999L))

test_that("Values are as expected", {
  # wth_formatted is loaded in setup-testdata.R
  tungro <- tungro(wth_formatted, emergence = "2000-07-01")

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
      "AUDPC",
      "lat",
      "lon"
    )
  )

  expect_is(tungro, c("data.table", "data.frame"))
  expect_identical(nrow(tungro), 120L)

  expect_equal(tungro[[1L, "sites"]], 100.0, tolerance = 0.1)
  expect_equal(tungro[[120L, "sites"]], 80.7, tolerance = 0.1)

  expect_equal(tungro[[1L, "latent"]], 0.0, tolerance = 0.1)
  expect_equal(tungro[[120L, "latent"]], 0.0, tolerance = 0.1)

  expect_equal(tungro[[1L, "infectious"]], 0.0, tolerance = 0.1)
  expect_equal(tungro[[120L, "infectious"]], 9.2, tolerance = 0.1)

  expect_equal(tungro[[1L, "removed"]], 0.0, tolerance = 0.1)
  expect_equal(tungro[[120L, "removed"]], 0.0, tolerance = 0.1)

  expect_equal(tungro[[1L, "senesced"]], 0.0, tolerance = 0.1)
  expect_equal(tungro[[120L, "senesced"]], 103.6, tolerance = 0.1)

  expect_equal(tungro[[1L, "rateinf"]], 0.0, tolerance = 0.1)
  expect_equal(tungro[[120L, "rateinf"]], 0.0, tolerance = 0.1)

  expect_equal(tungro[[1L, "rtransfer"]], 0.0, tolerance = 0.1)
  expect_equal(tungro[[120L, "rtransfer"]], 0.0, tolerance = 0.1)

  expect_equal(tungro[[1L, "rgrowth"]], 0.0, tolerance = 0.1)
  expect_equal(tungro[[120L, "rgrowth"]], 0.8, tolerance = 0.1)

  expect_equal(tungro[[1L, "rsenesced"]], 1.0, tolerance = 0.1)
  expect_equal(tungro[[120L, "rsenesced"]], 0.8, tolerance = 0.1)

  expect_equal(tungro[[1L, "diseased"]], 0.0, tolerance = 0.1)
  expect_equal(tungro[[120L, "diseased"]], 9.2, tolerance = 0.1)

  expect_equal(tungro[[1L, "intensity"]], 0.0, tolerance = 0.1)
  expect_equal(tungro[[120L, "intensity"]], 0.1, tolerance = 0.1)

  expect_equal(tungro[[1L, "lat"]], 14.6774, tolerance = 0.01)
  expect_equal(tungro[[1L, "lon"]], 121.2556, tolerance = 0.01)
})

withr::deferred_clear()
