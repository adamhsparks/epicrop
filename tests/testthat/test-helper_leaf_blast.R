test_that("Values are as expected", {
  withr::local_options(list(scipen = 999L))
  # wth_formatted is loaded in setup-testdata.R
  bs <- leaf_blast(wth_formatted, emergence = "2000-07-01")

  expected_names <- c(
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
  expect_named(bs, expected_names, ignore.order = FALSE)
  expect_s3_class(bs, c("data.table", "data.frame"))
  expect_identical(nrow(bs), 120L)

  bs_snap <- as.data.frame(bs)
  bs_snap$dates <- as.character(bs_snap$dates)

  num <- vapply(bs_snap, is.numeric, logical(1))
  bs_snap[num] <- lapply(bs_snap[num], \(x) round(x, 4))

  expect_snapshot_value(bs_snap, style = "json2")
})

test_that("lb() alias works identically", {
  full <- leaf_blast(wth_formatted, emergence = "2000-07-01")
  alias <- lb(wth_formatted, emergence = "2000-07-01")

  expect_identical(full, alias)
})
