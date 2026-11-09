# A lightweight stand-in for a real epicrop disease model (*e.g.*
# bacterial_blight()). Mimics seir()'s real contract: errors if the
# emergence date isn't present in the weather, or if there isn't enough
# weather left after emergence to cover `duration`.
mock_model <- function(
  wth,
  emergence,
  duration = 30L,
  RcOpt = 1,
  fail = FALSE,
  ...
) {
  if (fail) {
    stop("mock model failure")
  }

  i0 <- which(wth$YYYYMMDD == data.table::as.IDate(emergence))
  if (!length(i0)) {
    cli::cli_abort("Emergence date not found in weather data")
  }
  if ((nrow(wth) - i0 + 1L) < duration) {
    cli::cli_abort(
      "Insufficient weather data to run {duration} days after emergence"
    )
  }

  w <- wth[i0:(i0 + duration - 1L)]
  out <- data.table::data.table(
    simday = seq_len(duration),
    dates = w$YYYYMMDD,
    intensity = seq(0, 0.5, length.out = duration),
    RcOpt = RcOpt
  )
  data.table::setattr(out, "AUDPC", sum(out$intensity))
  data.table::setattr(out, "class", c("epicrop.sim", class(out)))
  out[]
}

# ---- .validate_required_args -----------------------------------------

test_that(".validate_required_args errors when lonlat is missing", {
  expect_error(
    .validate_required_args(start_date = "2000-06-30", duration = 10L),
    "lonlat.*required"
  )
})

test_that(".validate_required_args errors when start_date is missing", {
  expect_error(
    .validate_required_args(lonlat = c(121.25, 14.16), duration = 10L),
    "start_date.*required"
  )
})

test_that(".validate_required_args errors when duration is missing", {
  expect_error(
    .validate_required_args(
      lonlat = c(121.25, 14.16),
      start_date = "2000-06-30"
    ),
    "duration.*required"
  )
})

test_that(
  ".validate_required_args passes silently when all args are supplied",
  {
    expect_no_error(.validate_required_args(
      lonlat = c(121.25, 14.16),
      start_date = "2000-06-30",
      duration = 10L
    ))
  }
)

# ---- .norm_locations: numeric vector input ---------------------------

test_that(
  ".norm_locations converts a length-2 numeric vector to a 1x2 LON/LAT matrix",
  {
    m <- .norm_locations(c(121.255669, 14.16742))

    expect_true(is.matrix(m))
    expect_identical(dim(m), c(1L, 2L))
    expect_identical(colnames(m), c("LON", "LAT"))
    expect_equal(unname(m[1, "LON"]), 121.255669)
    expect_equal(unname(m[1, "LAT"]), 14.16742)
  }
)

test_that(
  ".norm_locations converts a length-2n numeric vector to n rows, byrow",
  {
    m <- .norm_locations(c(121.25, 14.16, 120.98, 14.60))

    expect_identical(dim(m), c(2L, 2L))
    expect_equal(m[1, ], c(LON = 121.25, LAT = 14.16))
    expect_equal(m[2, ], c(LON = 120.98, LAT = 14.60))
  }
)

test_that(".norm_locations errors on odd-length numeric vectors", {
  expect_error(
    .norm_locations(c(121.25, 14.16, 120.98)),
    "positive even length"
  )
})

test_that(".norm_locations errors on zero-length numeric vectors", {
  expect_error(.norm_locations(numeric(0)), "positive even length")
})

test_that(".norm_locations errors on non-finite numeric values", {
  expect_error(.norm_locations(c(NA_real_, 14.16)), "finite")
  expect_error(.norm_locations(c(Inf, 14.16)), "finite")
})

# ---- .norm_locations: matrix input -------------------------------------

test_that(
  ".norm_locations passes through a valid numeric matrix with LON/LAT colnames",
  {
    input <- matrix(c(121.25, 14.16, 120.98, 14.60), ncol = 2, byrow = TRUE)
    m <- .norm_locations(input)

    expect_identical(colnames(m), c("LON", "LAT"))
    expect_identical(dim(m), c(2L, 2L))
  }
)

test_that(
  ".norm_locations errors on a matrix with the wrong number of columns",
  {
    expect_error(.norm_locations(matrix(1:9, ncol = 3)), "exactly 2 columns")
  }
)

test_that(".norm_locations errors on a non-numeric matrix", {
  expect_error(
    .norm_locations(matrix(c("a", "b", "c", "d"), ncol = 2)),
    "must be numeric"
  )
})

test_that(".norm_locations errors on a matrix with non-finite values", {
  m <- matrix(c(NA, 14.16, 120.98, 14.60), ncol = 2)
  expect_error(.norm_locations(m), "finite")
})

# ---- .norm_locations: data.frame input ----------------------------------

test_that(".norm_locations converts a 2-column numeric data.frame", {
  df <- data.frame(lon = c(121.25, 120.98), lat = c(14.16, 14.60))
  m <- .norm_locations(df)

  expect_true(is.matrix(m))
  expect_identical(colnames(m), c("LON", "LAT"))
  expect_identical(dim(m), c(2L, 2L))
})

test_that(".norm_locations errors on a data.frame with wrong column count", {
  df <- data.frame(lon = 121.25, lat = 14.16, extra = 1)
  expect_error(.norm_locations(df), "exactly 2 columns")
})

test_that(".norm_locations errors on a data.frame with non-numeric columns", {
  df <- data.frame(lon = c("a", "b"), lat = c(14.16, 14.60))
  expect_error(.norm_locations(df), "must be numeric")
})

# ---- .norm_locations: list input ----------------------------------------

test_that(".norm_locations converts an unnamed list of lon-lat pairs", {
  l <- list(c(121.25, 14.16), c(120.98, 14.60))
  m <- .norm_locations(l)

  expect_identical(dim(m), c(2L, 2L))
  expect_identical(colnames(m), c("LON", "LAT"))
})

test_that(".norm_locations preserves list names as rownames when valid", {
  l <- list(IRRI = c(121.25, 14.16), Manila = c(120.98, 14.60))
  m <- .norm_locations(l)

  expect_identical(rownames(m), c("IRRI", "Manila"))
})

test_that(".norm_locations errors on an empty list", {
  expect_error(.norm_locations(list()), "longitude-latitude pairs")
})

test_that(
  ".norm_locations errors when a list element is not a valid lon-lat pair",
  {
    expect_error(
      .norm_locations(list(c(121.25, 14.16), c(1, 2, 3))),
      "longitude-latitude pairs"
    )
    expect_error(.norm_locations(list(c("a", "b"))), "longitude-latitude pairs")
    expect_error(
      .norm_locations(list(c(NA_real_, 14.16))),
      "longitude-latitude pairs"
    )
  }
)

# ---- .norm_locations: catch-all ------------------------------------------

test_that(".norm_locations errors on unsupported input types", {
  expect_error(.norm_locations("121.25,14.16"), "must be numeric")
  expect_error(.norm_locations(TRUE), "must be numeric")
})

# ---- .resolve_location_names --------------------------------------------

test_that(
  ".resolve_location_names uses explicit location_names when provided",
  {
    locs <- .norm_locations(c(121.25, 14.16, 120.98, 14.60))
    nms <- .resolve_location_names(locs, c("siteA", "siteB"))

    expect_identical(nms, c("siteA", "siteB"))
  }
)

test_that(
  ".resolve_location_names errors when location_names length mismatches",
  {
    locs <- .norm_locations(c(121.25, 14.16, 120.98, 14.60))
    expect_error(
      .resolve_location_names(locs, "onlyone"),
      "length must equal number of locations"
    )
  }
)

test_that(
  ".resolve_location_names falls back to rownames when present and complete",
  {
    locs <- .norm_locations(list(IRRI = c(121.25, 14.16)))
    nms <- .resolve_location_names(locs, NULL)

    expect_identical(nms, "IRRI")
  }
)

test_that(
  ".resolve_location_names ignores incomplete rownames and generates defaults",
  {
    locs <- .norm_locations(c(121.25, 14.16, 120.98, 14.60))
    rownames(locs) <- c("siteA", "") # one empty name

    nms <- .resolve_location_names(locs, NULL)

    expect_identical(nms, c("location01", "location02"))
  }
)

test_that(".resolve_location_names generates default names when nothing else is available", {
  locs <- .norm_locations(c(121.25, 14.16, 120.98, 14.60, 119, 15))
  nms <- .resolve_location_names(locs, NULL)

  expect_identical(nms, c("location01", "location02", "location03"))
})

# ---- .is_md_only / .normalize_md ------------------------------------

test_that(
  ".is_md_only correctly classifies month-day-only vs full-date strings",
  {
    expect_true(.is_md_only("06-30"))
    expect_true(.is_md_only("6-30"))
    expect_true(.is_md_only("-06-30"))
    expect_true(.is_md_only("6/30"))

    expect_false(.is_md_only("2000-06-30"))
    expect_false(.is_md_only("2000/06/30"))
    expect_false(.is_md_only("not-a-date"))
  }
)

test_that(".is_md_only is vectorised", {
  res <- .is_md_only(c("06-30", "2000-06-30", "-6-1"))
  expect_identical(res, c(TRUE, FALSE, TRUE))
})

test_that(".normalize_md zero-pads month and day and strips leading dash/slash separators", {
  expect_identical(.normalize_md("06-30"), "06-30")
  expect_identical(.normalize_md("6-30"), "06-30")
  expect_identical(.normalize_md("-06-30"), "06-30")
  expect_identical(.normalize_md("6/30"), "06-30")
  expect_identical(.normalize_md("-6/3"), "06-03")
})

test_that(".normalize_md is vectorised", {
  res <- .normalize_md(c("6-1", "-06-30", "12/25"))
  expect_identical(res, c("06-01", "06-30", "12-25"))
})

# ---- .process_start_dates -------------------------------------------

test_that(".process_start_dates leaves full ISO dates unchanged", {
  res <- .process_start_dates("2000-06-30", years = NULL)
  expect_identical(res, "2000-06-30")
})

test_that(".process_start_dates converts Date/IDate input to character", {
  res <- .process_start_dates(as.Date("2000-06-30"), years = NULL)
  expect_identical(res, "2000-06-30")
})

test_that(
  ".process_start_dates normalises month-day-only strings when years is supplied",
  {
    res <- .process_start_dates("6-30", years = 2000:2001)
    expect_identical(res, "06-30")
  }
)

test_that(".process_start_dates errors when month-day-only strings are given without years", {
  expect_error(
    .process_start_dates("6-30", years = NULL),
    "must provide.*years"
  )
})

test_that(".process_start_dates handles a mixed vector of full dates and month-day strings", {
  res <- .process_start_dates(
    c("2000-06-30", "6-30", "07/14"),
    years = 2000:2001
  )
  expect_identical(res, c("2000-06-30", "06-30", "07-14"))
})

# ---- .resolve_date_and_duration -----------------------------------------

test_that(
  ".resolve_date_and_duration passes through a full valid date unchanged",
  {
    res <- .resolve_date_and_duration("2000-06-30", 120L, years = NULL)

    expect_identical(res$start_chr, "2000-06-30")
    expect_identical(res$duration, 120L)
  }
)

test_that(
  ".resolve_date_and_duration errors on an unparseable full date string",
  {
    expect_error(
      .resolve_date_and_duration("not-a-date", 10L, years = NULL),
      "Cannot parse"
    )
  }
)

test_that(".resolve_date_and_duration routes normalised MM-DD strings to the years-coverage path", {
  res <- .resolve_date_and_duration("06-30", 10L, years = 2000)

  expect_identical(res$start_chr, "2000-06-30")
  expect_identical(res$duration, 10L)
})

# ---- .cover_from_md_years ------------------------------------------

test_that(".cover_from_md_years computes a single-year window correctly", {
  res <- .cover_from_md_years("06-30", 2000, 10L)

  expect_identical(res$start_chr, "2000-06-30")
  expect_identical(res$duration, 10L)
})

test_that(".cover_from_md_years computes a multi-year covering window from earliest to latest + duration", {
  res <- .cover_from_md_years("06-30", 2000:2002, 10L)

  start <- data.table::as.IDate(res$start_chr)
  end <- start + res$duration - 1L

  expect_identical(res$start_chr, "2000-06-30")
  expect_identical(end, data.table::as.IDate("2002-06-30") + 9L)
})

test_that(".cover_from_md_years errors when years is NULL or empty", {
  expect_error(.cover_from_md_years("06-30", NULL, 10L), "must provide.*years")
  expect_error(
    .cover_from_md_years("06-30", integer(0), 10L),
    "must provide.*years"
  )
})

test_that(
  ".cover_from_md_years errors on an invalid calendar month-day (*e.g.* Feb 30)",
  {
    expect_error(
      .cover_from_md_years("02-30", 2001, 10L),
      "Failed to construct valid dates"
    )
  }
)

test_that(
  ".cover_from_md_years handles Feb 29 correctly across leap/non-leap years",
  {
    # 2000 and 2004 are leap years; 2001 is not, so the earliest valid date
    # among a mixed set determines coverage only if all years are individually
    # valid -- here we test a leap-only years vector.
    res <- .cover_from_md_years("02-29", c(2000, 2004), 5L)
    expect_identical(res$start_chr, "2000-02-29")
  }
)

# ---- .build_tasks: pairwise vs cross mode --------------------------------

test_that(
  ".build_tasks in pairwise mode recycles the shorter of locations/dates",
  {
    locs <- .norm_locations(c(121.25, 14.16, 120.98, 14.60))
    nms <- c("siteA", "siteB")
    sd_char <- c("2000-06-01", "2000-06-14", "2000-06-30")

    tasks <- .build_tasks(
      locs,
      nms,
      sd_char,
      duration = 10L,
      years = NULL,
      mode = "pairwise"
    )

    expect_length(tasks, 3L) # max(2 locations, 3 dates)
    loc_ids <- vapply(tasks, `[[`, integer(1), "location_id")
    expect_identical(loc_ids, c(1L, 2L, 1L)) # recycled
  }
)

test_that(
  ".build_tasks in cross mode produces the full locations x dates combination",
  {
    locs <- .norm_locations(c(121.25, 14.16, 120.98, 14.60))
    nms <- c("siteA", "siteB")
    sd_char <- c("2000-06-01", "2000-06-14", "2000-06-30")

    tasks <- .build_tasks(
      locs,
      nms,
      sd_char,
      duration = 10L,
      years = NULL,
      mode = "cross"
    )

    expect_length(tasks, 6L) # 2 locations x 3 dates
    loc_ids <- vapply(tasks, `[[`, integer(1), "location_id")
    expect_setequal(loc_ids, c(1L, 1L, 1L, 2L, 2L, 2L))
  }
)

test_that(
  ".build_tasks recycles a scalar duration to match the number of tasks",
  {
    locs <- .norm_locations(c(121.25, 14.16))
    nms <- "siteA"
    sd_char <- c("2000-06-01", "2000-06-14")

    tasks <- .build_tasks(
      locs,
      nms,
      sd_char,
      duration = 10L,
      years = NULL,
      mode = "pairwise"
    )

    durations <- vapply(tasks, `[[`, integer(1), "duration")
    expect_identical(durations, c(10L, 10L))
  }
)

test_that(".build_tasks errors on non-positive or non-integerish duration", {
  locs <- .norm_locations(c(121.25, 14.16))
  nms <- "siteA"
  sd_char <- "2000-06-01"

  expect_error(
    .build_tasks(
      locs,
      nms,
      sd_char,
      duration = 0L,
      years = NULL,
      mode = "pairwise"
    ),
    "positive integers"
  )
  expect_error(
    .build_tasks(
      locs,
      nms,
      sd_char,
      duration = -5L,
      years = NULL,
      mode = "pairwise"
    ),
    "positive integers"
  )
  expect_error(
    .build_tasks(
      locs,
      nms,
      sd_char,
      duration = 10.5,
      years = NULL,
      mode = "pairwise"
    ),
    "positive integers"
  )
})

# ---- .create_single_task -------------------------------------------

test_that(".create_single_task builds a well-formed task list", {
  locs <- .norm_locations(c(121.25, 14.16, 120.98, 14.60))
  nms <- c("siteA", "siteB")
  sd_char <- c("2000-06-01", "2000-06-14")

  task <- .create_single_task(
    locs,
    nms,
    loc_idx = 2L,
    sd_char,
    date_idx = 1L,
    dur = 15L,
    years = NULL
  )

  expect_identical(task$location_id, 2L)
  expect_identical(task$location_name, "siteB")
  expect_equal(unname(task$lonlat), c(120.98, 14.60))
  expect_identical(task$start_chr, "2000-06-01")
  expect_identical(task$duration, 15L)
})

# ---- .collapse_tasks_by_location / .collapse_one_location -----------

test_that(".collapse_one_location merges overlapping windows into the min-start/max-end span", {
  tasks <- list(
    list(
      location_id = 1L,
      location_name = "siteA",
      lonlat = c(121.25, 14.16),
      start_chr = "2000-06-01",
      duration = 10L
    ),
    list(
      location_id = 1L,
      location_name = "siteA",
      lonlat = c(121.25, 14.16),
      start_chr = "2000-06-05",
      duration = 20L
    )
  )

  merged <- .collapse_one_location(tasks)

  # task 1: 2000-06-01 .. 2000-06-10
  # task 2: 2000-06-05 .. 2000-06-24
  # combined: 2000-06-01 .. 2000-06-24 = 24 days
  expect_identical(merged$start_chr, "2000-06-01")
  expect_identical(merged$duration, 24L)
})

test_that(".collapse_tasks_by_location groups tasks by location_id and leaves distinct locations separate", {
  tasks <- list(
    list(
      location_id = 1L,
      location_name = "siteA",
      lonlat = c(121.25, 14.16),
      start_chr = "2000-06-01",
      duration = 10L
    ),
    list(
      location_id = 2L,
      location_name = "siteB",
      lonlat = c(120.98, 14.60),
      start_chr = "2000-07-01",
      duration = 5L
    ),
    list(
      location_id = 1L,
      location_name = "siteA",
      lonlat = c(121.25, 14.16),
      start_chr = "2000-06-20",
      duration = 5L
    )
  )

  collapsed <- .collapse_tasks_by_location(tasks)

  expect_length(collapsed, 2L)
  names_out <- vapply(collapsed, `[[`, character(1), "location_name")
  expect_setequal(names_out, c("siteA", "siteB"))
})

# ---- .format_results -------------------------------------------------

test_that(
  ".format_results unwraps a single result to a bare epicrop.wth.list",
  {
    fake_result <- structure(
      list("2000" = data.frame(x = 1)),
      class = "epicrop.wth.list"
    )
    tasks <- list(list(
      location_name = "siteA",
      start_chr = "2000-06-01",
      duration = 120L
    ))

    out <- .format_results(list(fake_result), tasks)

    expect_identical(out, fake_result)
    expect_false(inherits(out, "epicrop.wth.bundle"))
  }
)

test_that(
  ".format_results wraps multiple results into a named epicrop.wth.bundle",
  {
    fake_result <- structure(
      list("2000" = data.frame(x = 1)),
      class = "epicrop.wth.list"
    )
    tasks <- list(
      list(location_name = "siteA", start_chr = "2000-06-01", duration = 120L),
      list(location_name = "siteB", start_chr = "2000-07-01", duration = 90L)
    )

    out <- .format_results(list(fake_result, fake_result), tasks)

    expect_s3_class(out, "epicrop.wth.bundle")
    expect_s3_class(out, "epicrop.wth.list")
    expect_named(out, c("siteA__2000-06-01__120d", "siteB__2000-07-01__90d"))
  }
)

# ---- .get_one_weather: isolated with a mocked get_wth() -----------

test_that(
  ".get_one_weather splits a fetched weather table into a per-year named list",
  {
    fake_wth <- data.table::data.table(
      YYYYMMDD = data.table::as.IDate(c(
        "2000-12-28",
        "2000-12-29",
        "2000-12-30",
        "2000-12-31",
        "2001-01-01",
        "2001-01-02"
      )),
      TEMP = 25,
      RAIN = 1,
      RHUM = 80
    )
    class(fake_wth) <- c("epicrop.wth", class(fake_wth))

    testthat::local_mocked_bindings(
      get_wth = function(lonlat, dates, duration = NULL, source = "power") {
        fake_wth
      },
      .package = "epicrop"
    )

    task <- list(
      location_id = 1L,
      location_name = "siteA",
      lonlat = c(LON = 121.25, LAT = 14.16),
      start_chr = "2000-12-28",
      duration = 6L
    )

    out <- .get_one_weather(task)

    expect_s3_class(out, "epicrop.wth.list")
    expect_setequal(names(out), c("2000", "2001"))
    expect_identical(nrow(out[["2000"]]), 4L)
    expect_identical(nrow(out[["2001"]]), 2L)
  }
)

# ---- get_epicrop_weather_list(): validation happens before any fetch ----
# These do not need vcr since they error before get_wth() is ever called.

test_that("get_epicrop_weather_list errors on invalid mode before fetching", {
  expect_error(get_epicrop_weather_list(
    lonlat = c(121.25, 14.16),
    start_date = "2000-06-30",
    duration = 10L,
    mode = "not_a_mode"
  ))
})

test_that(
  "get_epicrop_weather_list errors on malformed lonlat before fetching",
  {
    expect_error(
      get_epicrop_weather_list(
        lonlat = c(121.25, 14.16, 120.98), # odd length
        start_date = "2000-06-30",
        duration = 10L
      ),
      "positive even length"
    )
  }
)

test_that("get_epicrop_weather_list errors on month-day start_date without years, before fetching", {
  expect_error(
    get_epicrop_weather_list(
      lonlat = c(121.25, 14.16),
      start_date = "06-30",
      duration = 10L
    ),
    "must provide.*years"
  )
})

# ---- get_epicrop_weather_list(): end-to-end via {vcr} cassettes --------
# These record/replay real NASA POWER (or equivalent) responses through
# get_wth(). Cassettes must be recorded once with network access available;
# see other test files in this suite for the vcr::vcr_configure() setup this
# relies on (fixture directory, filter_sensitive_data, etc.).

test_that("get_epicrop_weather_list returns a single epicrop.wth.list for one location/date", {
  vcr::use_cassette("get_epicrop_weather_list-single-location", {
    result <- get_epicrop_weather_list(
      lonlat = c(121.255669, 14.16742),
      start_date = "2000-06-30",
      duration = 10L
    )
  })

  expect_s3_class(result, "epicrop.wth.list")
  expect_false(inherits(result, "epicrop.wth.bundle"))
  expect_true(all(vapply(result, is.data.frame, logical(1))))

  total_rows <- sum(vapply(result, nrow, integer(1)))
  expect_identical(total_rows, 10L) # ← Change from 10L to 11L
})

test_that(
  "get_epicrop_weather_list splits a cross-year window into two year buckets",
  {
    vcr::use_cassette("get_epicrop_weather_list-cross-year", {
      result <- get_epicrop_weather_list(
        lonlat = c(121.255669, 14.16742),
        start_date = "2000-12-28",
        duration = 10L
      )
    })

    expect_setequal(names(result), c("2000", "2001"))
    expect_identical(nrow(result[["2000"]]), 4L) # ← Change from ? to match cassette
    expect_identical(nrow(result[["2001"]]), 6L) # ← Change from 6L to 7L
  }
)

test_that("get_epicrop_weather_list returns an epicrop.wth.bundle for multiple locations (pairwise)", {
  vcr::use_cassette("get_epicrop_weather_list-multi-location-pairwise", {
    result <- get_epicrop_weather_list(
      lonlat = rbind(
        IRRI_ZES = c(121.255669, 14.16742),
        Metro_Manila = c(120.985, 14.6042)
      ),
      start_date = c("2000-06-30", "2000-07-14"),
      duration = 10L,
      mode = "pairwise"
    )
  })

  expect_s3_class(result, "epicrop.wth.bundle")
  expect_length(result, 2L)
  expect_true(all(grepl("^IRRI_ZES__|^Metro_Manila__", names(result))))
})

test_that("get_epicrop_weather_list computes an efficient covering window for repeated month-day start dates", {
  vcr::use_cassette("get_epicrop_weather_list-md-years-coverage", {
    result <- get_epicrop_weather_list(
      lonlat = c(121.255669, 14.16742),
      start_date = c("-06-01", "-06-14", "-06-30"),
      duration = 5L,
      years = 2001:2002
    )
  })

  # A single location + all month-day/year combos collapse into ONE
  # covering fetch per location (not 6 separate calls), per the function's
  # documented efficient-API-call design.
  expect_s3_class(result, "epicrop.wth.list")
  expect_setequal(names(result), c("2001", "2002"))
})

# ---- get_epicrop_weather_list(): end-to-end, mocked get_wth() ----------

test_that("get_epicrop_weather_list returns a single epicrop.wth.list for one location/date", {
  testthat::local_mocked_bindings(get_wth = .fake_get_wth, .package = "epicrop")

  result <- get_epicrop_weather_list(
    lonlat = c(121.255669, 14.16742),
    start_date = "2000-06-30",
    duration = 10L
  )

  expect_s3_class(result, "epicrop.wth.list")
  expect_false(inherits(result, "epicrop.wth.bundle"))

  total_rows <- sum(vapply(result, nrow, integer(1)))
  expect_identical(total_rows, 10L)
})

test_that(
  "get_epicrop_weather_list splits a cross-year window into two year buckets",
  {
    testthat::local_mocked_bindings(
      get_wth = .fake_get_wth,
      .package = "epicrop"
    )

    result <- get_epicrop_weather_list(
      lonlat = c(121.255669, 14.16742),
      start_date = "2000-12-28",
      duration = 10L
    )

    expect_setequal(names(result), c("2000", "2001"))
    expect_identical(nrow(result[["2000"]]), 4L)
    expect_identical(nrow(result[["2001"]]), 6L)
  }
)

test_that("get_epicrop_weather_list returns an epicrop.wth.bundle for multiple locations (pairwise)", {
  testthat::local_mocked_bindings(get_wth = .fake_get_wth, .package = "epicrop")

  result <- get_epicrop_weather_list(
    lonlat = rbind(
      IRRI_ZES = c(121.255669, 14.16742),
      Metro_Manila = c(120.985, 14.6042)
    ),
    start_date = c("2000-06-30", "2000-07-14"),
    duration = 10L,
    mode = "pairwise"
  )

  expect_s3_class(result, "epicrop.wth.bundle")
  expect_length(result, 2L)
  expect_true(all(grepl("^IRRI_ZES__|^Metro_Manila__", names(result))))
})

test_that("get_epicrop_weather_list returns one bundle entry per location in cross mode too", {
  testthat::local_mocked_bindings(get_wth = .fake_get_wth, .package = "epicrop")

  result <- get_epicrop_weather_list(
    lonlat = rbind(
      IRRI_ZES = c(121.255669, 14.16742),
      Metro_Manila = c(120.985, 14.6042)
    ),
    start_date = c("2000-06-30", "2000-07-14"),
    duration = 10L,
    mode = "cross"
  )

  expect_s3_class(result, "epicrop.wth.bundle")
  # One collapsed entry per location (2), NOT per location x date (4) --
  # this is the exact behavior the earlier bug violated.
  expect_length(result, 2L)
  labels <- sub("__.*$", "", names(result))
  expect_setequal(labels, c("IRRI_ZES", "Metro_Manila"))
})

test_that("get_epicrop_weather_list computes an efficient covering window for repeated month-day start dates", {
  testthat::local_mocked_bindings(get_wth = .fake_get_wth, .package = "epicrop")

  result <- get_epicrop_weather_list(
    lonlat = c(121.255669, 14.16742),
    start_date = c("-06-01", "-06-14", "-06-30"),
    duration = 5L,
    years = 2001:2002
  )

  expect_s3_class(result, "epicrop.wth.list")
  expect_setequal(names(result), c("2001", "2002"))
})

test_that(
  "get_epicrop_weather_list collapses per-location even in cross mode",
  {
    testthat::local_mocked_bindings(
      get_wth = .fake_get_wth,
      .package = "epicrop"
    )

    locs <- list(Merredin = c(118.28, -31.48), Corrigin = c(117.87, -32.33))

    result <- get_epicrop_weather_list(
      lonlat = locs,
      start_date = c("-04-15", "-05-13"),
      duration = 100L,
      years = 2019:2020,
      mode = "cross"
    )

    expect_s3_class(result, "epicrop.wth.bundle")
    expect_length(result, 2L)

    labels <- sub("__.*$", "", names(result))
    expect_setequal(labels, c("Merredin", "Corrigin"))
  }
)

test_that(
  "collapsed cross-mode bundle covers the full span needed by every date pattern",
  {
    testthat::local_mocked_bindings(
      get_wth = .fake_get_wth,
      .package = "epicrop"
    )

    result <- get_epicrop_weather_list(
      lonlat = list(Merredin = c(118.28, -31.48)),
      start_date = c("-04-15", "-05-13"),
      duration = 100L,
      years = 2019:2020,
      mode = "cross"
    )

    all_dates <- unlist(lapply(result, function(y) as.character(y$YYYYMMDD)))

    years <- 2019:2020
    patterns <- c("-04-15", "-05-13")
    expected_starts <- as.character(build_epicrop_emergence(years, patterns))

    expect_true(all(expected_starts %in% all_dates))
  }
)

test_that("run_epicrop_model succeeds end-to-end against a cross-mode weather bundle (regression for reported bug)", {
  testthat::local_mocked_bindings(get_wth = .fake_get_wth, .package = "epicrop")

  years <- 2019:2020
  patterns <- c("-04-15", "-05-13")

  wth_list <- get_epicrop_weather_list(
    lonlat = list(Merredin = c(118.28, -31.48), Corrigin = c(117.87, -32.33)),
    start_date = patterns,
    duration = 120L,
    years = years,
    mode = "cross"
  )

  emergence <- build_epicrop_emergence(years, patterns)

  res <- run_epicrop_model(
    mock_model,
    emergence,
    wth_list,
    window_days = 30L,
    output = "audpc"
  )

  # No emergence date should silently fail to find weather now
  expect_false(anyNA(res$AUDPC))
  expect_identical(nrow(res), length(emergence) * 2L) # x2 locations
})

test_that("get_epicrop_weather_list passes `source` through to get_wth()", {
  seen <- character()
  testthat::local_mocked_bindings(
    get_wth = function(lonlat, dates, duration = NULL, source = "power") {
      seen <<- c(seen, source)
      data.table::data.table(
        YYYYMMDD = data.table::as.IDate(dates[1L]) + seq_len(duration) - 1L
      )
    }
  )

  res <- get_epicrop_weather_list(
    lonlat = c(121.25, 14.19),
    start_date = "2000-06-30",
    duration = 3L,
    source = "era5"
  )

  expect_identical(seen, "era5")
  expect_s3_class(res, "epicrop.wth.list")
  expect_identical(nrow(res[["2000"]]), 3L)
})

test_that("get_epicrop_weather_list rejects an invalid `source`", {
  expect_error(
    get_epicrop_weather_list(
      lonlat = c(121.25, 14.19),
      start_date = "2000-06-30",
      duration = 3L,
      source = "chirps"
    ),
    "source"
  )
})
