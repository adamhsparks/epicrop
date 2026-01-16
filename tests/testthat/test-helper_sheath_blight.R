test_that("Values are as expected", {
  withr::local_options(list(scipen = 999L))
  # wth_formatted is loaded in setup-testdata.R
  sb <- sheath_blight(wth_formatted, emergence = "2000-07-01")

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
  expect_named(sb, expected_names, ignore.order = FALSE)
  expect_s3_class(sb, c("data.table", "data.frame"))
  expect_identical(nrow(sb), 120L)

  sb_snap <- as.data.frame(sb)
  sb_snap$dates <- as.character(sb_snap$dates)

  num <- vapply(sb_snap, is.numeric, logical(1))
  sb_snap[num] <- lapply(sb_snap[num], \(x) round(x, 4))

  expect_snapshot_value(sb_snap, style = "json2")
})
