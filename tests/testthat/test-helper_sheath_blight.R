withr::local_options(list(scipen = 999L))

test_that("Values are as expected", {
  # wth_formatted is loaded in setup-testdata.R
  sb <- sheath_blight(wth_formatted, emergence = "2000-07-01")

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
      "intensity",
      "AUDPC",
      "lat",
      "lon"
    )
  )

  expect_is(sb, c("data.table", "data.frame"))
  expect_identical(nrow(sb), 120L)

  expect_equal(sb[[1L, "sites"]], 25.0, tolerance = 0.1)
  expect_equal(sb[[120L, "sites"]], 165.1, tolerance = 0.1)

  expect_equal(sb[[1L, "latent"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[120L, "latent"]], 14.3, tolerance = 0.1)

  expect_equal(sb[[1L, "infectious"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[120L, "infectious"]], 603.1, tolerance = 0.1)

  expect_equal(sb[[1L, "removed"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[120L, "removed"]], 0.0, tolerance = 0.1)

  expect_equal(sb[[1L, "senesced"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[120L, "senesced"]], 281.8, tolerance = 0.1)

  expect_equal(sb[[1L, "rateinf"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[120L, "rateinf"]], 4.14, tolerance = 0.01)

  expect_equal(sb[[1L, "rtransfer"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[120L, "rtransfer"]], 0.0, tolerance = 0.1)

  expect_equal(sb[[1L, "rgrowth"]], 4.8, tolerance = 0.1)
  expect_equal(sb[[120L, "rgrowth"]], 0.89, tolerance = 0.1)

  expect_equal(sb[[1L, "rsenesced"]], 0.1, tolerance = 0.1)
  expect_equal(sb[[120L, "rsenesced"]], 0.89, tolerance = 0.1)

  expect_equal(sb[[1L, "diseased"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[120L, "diseased"]], 614.8, tolerance = 0.1)

  expect_equal(sb[[1L, "intensity"]], 0.0, tolerance = 0.1)
  expect_equal(sb[[120L, "intensity"]], 0.77, tolerance = 0.1)

  expect_equal(sb[[1L, "lat"]], 14.6774, tolerance = 0.01)
  expect_equal(sb[[1L, "lon"]], 121.2556, tolerance = 0.01)
})

withr::deferred_clear()
