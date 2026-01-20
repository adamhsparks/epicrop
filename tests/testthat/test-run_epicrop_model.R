test_that("sequential vs future are identical with seed", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")

  set.seed(1)
  yrs <- 2001:2003
  em <- build_epicrop_emergence(yrs, c("-06-01", "-06-14", "-06-30"))
  wth <- fetch_epicrop_weather_list(
    yrs,
    c(121.255669, 14.16742),
    "-06-01",
    200L
  )

  seq_res <- run_epicrop_model(
    bacterial_blight,
    em,
    wth,
    window_days = 120L,
    mode = "none",
    output = "audpc",
    dt_threads = 1L,
    seed = 42
  )
  par_res <- run_epicrop_model(
    bacterial_blight,
    em,
    wth,
    window_days = 120L,
    mode = "future",
    cores = 2,
    chunk = "auto",
    output = "audpc",
    dt_threads = 1L,
    seed = 42
  )

  expect_equal(
    seq_res[order(emergence)],
    par_res[order(emergence)],
    tolerance = 0
  )
})
