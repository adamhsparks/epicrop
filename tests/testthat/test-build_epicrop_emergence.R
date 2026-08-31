# ---- Basic correctness -----------------------------------------------------

test_that("build_epicrop_emergence combines years and month_day correctly", {
  em <- build_epicrop_emergence(2001:2002, "-06-01")

  expect_length(em, 2)
  expect_identical(
    as.character(em),
    c("2001-06-01", "2002-06-01")
  )
})

test_that("build_epicrop_emergence produces one date per year x month_day combination", {
  years <- 2001:2002
  month_day <- c("-06-01", "-06-14", "-06-30")

  em <- build_epicrop_emergence(years, month_day)

  expect_length(em, length(years) * length(month_day))
  expect_setequal(
    as.character(em),
    c(
      "2001-06-01", "2001-06-14", "2001-06-30",
      "2002-06-01", "2002-06-14", "2002-06-30"
    )
  )
})

test_that("build_epicrop_emergence returns dates sorted ascending regardless of input order", {
  em <- build_epicrop_emergence(
    years = c(2002, 2001),
    month_day = c("-06-30", "-06-01")
  )

  expect_identical(em, sort(em))
  expect_identical(as.character(em[1]), "2001-06-01")
  expect_identical(as.character(em[length(em)]), "2002-06-30")
})

test_that("build_epicrop_emergence handles a single year and single month_day", {
  em <- build_epicrop_emergence(2001, "-06-01")

  expect_length(em, 1)
  expect_identical(as.character(em), "2001-06-01")
})

# ---- Class structure --------------------------------------------------

test_that("build_epicrop_emergence tags output with class epicrop.emerge", {
  em <- build_epicrop_emergence(2001, "-06-01")

  expect_true(inherits(em, "epicrop.emerge"))
})

test_that("build_epicrop_emergence preserves IDate/Date classes alongside epicrop.emerge", {
  em <- build_epicrop_emergence(2001, "-06-01")

  expect_true(inherits(em, "IDate"))
  expect_true(inherits(em, "Date"))
  expect_identical(class(em), c("epicrop.emerge", "IDate", "Date"))
})

# ---- month_day normalization ("MM-DD" vs "-MM-DD") -------------------------

test_that("build_epicrop_emergence accepts month_day without a leading dash", {
  em_no_dash <- build_epicrop_emergence(2001, "06-01")
  em_with_dash <- build_epicrop_emergence(2001, "-06-01")

  expect_identical(as.character(em_no_dash), as.character(em_with_dash))
})

test_that("build_epicrop_emergence accepts a mix of dashed and non-dashed month_day", {
  em <- build_epicrop_emergence(2001, c("06-01", "-06-14"))

  expect_setequal(as.character(em), c("2001-06-01", "2001-06-14"))
})

# ---- Invalid month_day format: regex rejection -----------------------------

test_that("build_epicrop_emergence errors on an out-of-range month", {
  expect_error(
    build_epicrop_emergence(2001, "-13-01"),
    "Invalid.*month_day"
  )
  expect_error(
    build_epicrop_emergence(2001, "-00-01"),
    "Invalid.*month_day"
  )
})

test_that("build_epicrop_emergence errors on an out-of-range day", {
  expect_error(
    build_epicrop_emergence(2001, "-01-00"),
    "Invalid.*month_day"
  )
  expect_error(
    build_epicrop_emergence(2001, "-01-32"),
    "Invalid.*month_day"
  )
})

test_that("build_epicrop_emergence errors on non-zero-padded month/day", {
  expect_error(
    build_epicrop_emergence(2001, "-6-01"),
    "Invalid.*month_day"
  )
  expect_error(
    build_epicrop_emergence(2001, "-06-1"),
    "Invalid.*month_day"
  )
})

test_that("build_epicrop_emergence errors on malformed strings (extra characters, wrong separators)", {
  expect_error(build_epicrop_emergence(2001, "-06/01"), "Invalid.*month_day")
  expect_error(build_epicrop_emergence(2001, "-06-01 "), "Invalid.*month_day")
  expect_error(build_epicrop_emergence(2001, "june-01"), "Invalid.*month_day")
  expect_error(build_epicrop_emergence(2001, ""), "Invalid.*month_day")
})

test_that("build_epicrop_emergence error message lists only the offending month_day value(s)", {
  err <- tryCatch(
    build_epicrop_emergence(2001, c("-06-01", "-13-01", "-99-99")),
    error = function(e) e
  )
  msg <- conditionMessage(err)

  expect_match(msg, "-13-01", fixed = TRUE)
  expect_match(msg, "-99-99", fixed = TRUE)
  expect_false(grepl("-06-01", msg, fixed = TRUE))
})

test_that("build_epicrop_emergence treats NA month_day as invalid and reports it", {
  expect_error(
    build_epicrop_emergence(2001, NA_character_),
    "Invalid.*month_day"
  )
})

# ---- Regex-valid but calendar-invalid dates --------------------------------

test_that("build_epicrop_emergence errors when a regex-valid month_day is not a real calendar date", {
  # "-02-30" passes the MM-DD regex (day 30 is in range) but February never
  # has 30 days, so date construction itself fails downstream.
  expect_error(
    build_epicrop_emergence(2001, "-02-30"),
    "Failed to construct emergence dates"
  )
})

test_that("build_epicrop_emergence respects leap-year validity for Feb 29", {
  # 2000 is a leap year: Feb 29 is valid.
  expect_no_error(build_epicrop_emergence(2000, "-02-29"))
  em <- build_epicrop_emergence(2000, "-02-29")
  expect_identical(as.character(em), "2000-02-29")

  # 2001 is not a leap year: Feb 29 does not exist.
  expect_error(
    build_epicrop_emergence(2001, "-02-29"),
    "Failed to construct emergence dates"
  )
})

test_that("build_epicrop_emergence errors when NA years produce unparseable dates", {
  expect_error(
    build_epicrop_emergence(c(2001, NA_integer_), "-06-01"),
    "Failed to construct emergence dates"
  )
})

# ---- CJ cross-join / duplicate handling (documents current behavior) -------

test_that("build_epicrop_emergence does not deduplicate repeated month_day inputs", {
  # Both entries normalize to the same "-06-01" string; CJ() with its default
  # unique = FALSE does not collapse them, so the cross join (and therefore
  # the output) contains the resulting duplicate date twice.
  em <- build_epicrop_emergence(2001, c("06-01", "-06-01"))

  expect_length(em, 2)
  expect_identical(as.character(em), c("2001-06-01", "2001-06-01"))
})

test_that("build_epicrop_emergence does not deduplicate repeated years", {
  em <- build_epicrop_emergence(c(2001, 2001), "-06-01")

  expect_length(em, 2)
  expect_identical(as.character(em), c("2001-06-01", "2001-06-01"))
})

# ---- Empty inputs -----------------------------------------------------

test_that("build_epicrop_emergence returns a zero-length result for empty years", {
  em <- build_epicrop_emergence(integer(0), "-06-01")

  expect_length(em, 0)
  expect_type(em, "epicrop.emerge")
})

test_that("build_epicrop_emergence returns a zero-length result for empty month_day", {
  em <- build_epicrop_emergence(2001, character(0))

  expect_length(em, 0)
  expect_type(em, "epicrop.emerge")
})

# ---- Downstream usability ---------------------------------------------

test_that("build_epicrop_emergence output supports standard Date operations", {
  em <- build_epicrop_emergence(2001:2002, c("-06-01", "-06-14"))

  expect_true(all(data.table::year(em) %in% 2001:2002))
  expect_true(is.numeric(as.numeric(em)))
  expect_identical(
    as.character(min(em)),
    as.character(em[1])
  )
})
