test_that("Values are as expected", {
  withr::local_options(list(scipen = 999L))
  # wth_formatted is loaded in setup-testdata.R
  mk_sb <- modified_kim_sheath_blight(wth_formatted, emergence = "2000-07-01")

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
  expect_named(mk_sb, expected_names, ignore.order = FALSE)
  expect_s3_class(mk_sb, c("data.table", "data.frame"))
  expect_identical(nrow(mk_sb), 88L)

  mk_sb_snap <- as.data.frame(mk_sb)

  mk_sb_snap$dates <- as.character(mk_sb_snap$dates)

  num <- vapply(mk_sb_snap, is.numeric, logical(1))
  mk_sb_snap[num] <- lapply(mk_sb_snap[num], \(x) round(x, 4))

  expect_snapshot_value(mk_sb_snap, style = "json2")
})
