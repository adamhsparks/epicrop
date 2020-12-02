
# SEIR modelling ---------------------------------------------------------------
context("SEIR functions properly")
test_that("SEIR returns a data.table with the proper columns and values", {
   # get weather for IRRI Zeigler Experiment Station in dry season 2000
   wth <- get_wth(
     lonlat = c(121.25562, 14.6774),
     dates = c("2000-06-30", "2000-12-31")
   )

   # provide suitable values for brown spot severity
   RcA <-
     cbind(0:6 * 20, c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0))
   RcT <-
     cbind(15 + (0:5) * 5, c(0, 0.06, 1.0, 0.85, 0.16, 0))
   RcW <- cbind(0:8 * 3,
                c(0, 0.12, 0.20, 0.38, 0.46, 0.60, 0.73, 0.87, 1.0))
   emergence <- "2000-01-15"

   x <- SEIR(
     wth = wth,
     emergence = emergence,
     RcA = RcA,
     RcT = RcT,
     RcW = RcW,
     RcOpt = 0.61,
     l = 6,
     i = 19,
     H0 = 600,
     a = 1,
     Sx = 100000,
     RRS = 0.01,
     RRG = 0.1
   )

  expect_named(x,
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
                 "severity"
               ))
  expect_is(x, c("data.table", "data.frame"))
  expect_equal(nrow(x), 121)

  # check severity values
  expect_equal(x[[1, 13]], 0)
  expect_equal(x[[121, 13]], 6.003, tolerance = 0.0001)

  # check total sites
  expect_equal(x[[1, 3]], 600) # 600 default to start sites
  expect_equal(x[[121, 3]], 84026.5, tolerance = 0.01)

  # check latent sites
  expect_equal(x[[1, 4]], 0) # 600 default to start sites
  expect_equal(x[[121, 4]], 3235.8, tolerance = 0.01)

  # check infectious sites
  expect_equal(x[[1, 5]], 0)
  expect_equal(x[[121, 5]], 2131, tolerance = 0.01)

  # check removed sites
  expect_equal(x[[1, 6]], 0)
  expect_equal(x[[121, 6]], 225.6, tolerance = 0.01)

  # check senesced sites
  expect_equal(x[[1, 7]], 0)
  expect_equal(x[[121, 7]], 55957.42, tolerance = 0.01)

  # check rateinf
  expect_equal(x[[1, 8]], 0)
  expect_equal(x[[121, 8]], 1209, tolerance = 0.01)

  # check rtransfer
  expect_equal(x[[1, 9]], 0)
  expect_equal(x[[119, 9]], 356.4, tolerance = 0.01)
  expect_equal(x[[121, 9]], 0, tolerance = 0.01)

  # check rgrowth
  expect_equal(x[[1, 10]], 59.64, tolerance = 0.01)
  expect_equal(x[[121, 10]], 872.32, tolerance = 0.01)

  # check rsenesced
  expect_equal(x[[1, 11]], 6)
  expect_equal(x[[121, 11]], 840.265, tolerance = 0.0001)

  # check diseased
  expect_equal(x[[1, 12]], 0)
  expect_equal(x[[121, 12]], 5592, tolerance = 0.0001)
})
