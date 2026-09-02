#' Fetch weather once (single or multiple locations) and split by calendar year
#'
#' Retrieve one or more continuous blocks of weather for one or more locations
#' from given start dates and durations, then split each result into a named
#' list of per‑year tables. Each (location, start, duration) result is an
#' `epicrop.wth.list`. The top-level return is an `epicrop.wth.bundle` when
#' multiple requests are processed, otherwise a single `epicrop.wth.list`.
#'
#' ## Flexible inputs
#' * `lonlat`: a single numeric vector, `c(lon, lat)`; or multiple locations as
#'   a numeric vector of length `2n`, or a 2‑column matrix/data.frame, or a list
#'   of `list(c(lon, lat))`.
#' * `start_date`: a single `"YYYY-MM-DD"` (or `Date`/`IDate`), or a vector of
#'   the same; or a month–day string like `"06-30"` or `"6-30"` (leading `"-"`
#'   optional) provided together with `years`.
#' * `duration`: single integer or a vector (recycled); interpreted as inclusive
#'   days to fetch (`end = start + duration - 1`).
#' * `years`: only used when `start_date` lacks a year. We compute a **cover
#'   window** per distinct month–day: `start = min(paste(years, md))`,
#'   `end = max(paste(years, md)) + (duration - 1)`.
#'
#' @param lonlat Numeric length‑2 vector `c(lon, lat)`, OR multiple locations
#'   as: numeric vector of length `2n`, a 2‑column matrix/data.frame, or a list
#'   of numeric length‑2 vectors.
#' @param start_date A starting date (character `"YYYY-MM-DD"`, [Date], or
#'   [data.table::IDate]) **or** a month–day string like `"06-30"` (leading
#'   `"-"` optional) used together with `years`.
#' @param duration Integer(s). Number of days to retrieve **from** `start_date`
#'   (inclusive). Single value or vector (recycled).
#' @param years Optional integer vector of years. **Required** if `start_date`
#'   contains any month–day values without a year.
#' @param mode Character scalar: `"pairwise"` (default) pairs locations and
#'   dates by position (recycling length-1), or `"cross"` forms all combinations
#'   (locations × dates).
#' @param location_names Optional character names for locations (length = number
#'   of unique locations).
#'
#' @return
#' * If only one (location, start, duration) is requested: an
#'  `epicrop.wth.list`.
#' * Otherwise: an `epicrop.wth.bundle` (a named list of `epicrop.wth.list`).
#'
#' @examplesIf interactive()
#' # Single location, month-day start date (any of "6/30", "-06-30", "06-30")
#' w1 <- fetch_epicrop_weather_list(
#'   lonlat     = c(121.255669, 14.16742),
#'   start_date = "6/30",
#'   duration   = 120L,
#'   years      = 2000:2001
#' )
#'
#' # ISO start date
#' w2 <- fetch_epicrop_weather_list(
#'   lonlat     = c(121.255669, 14.16742),
#'   start_date = "2000-06-30",
#'   duration   = 120L
#' )
#'
#' # Multiple locations × multiple dates (cross mode), mixed inputs
#' locations <- rbind(
#'   "IRRI_ZES" = c(121.255669, 14.16742),
#'   "Metro_Manila" = c(120.985, 14.6042)
#' )
#' w3 <- fetch_epicrop_weather_list(
#'   lonlat     = locations,
#'   start_date = c("2000-06-30", "6-30"),
#'   duration   = 120L,
#'   years      = 2000:2001,
#'   mode       = "cross"
#' )
#'
#' # Multiple start dates for same location across years (efficient API calls)
#' w4 <- fetch_epicrop_weather_list(
#'   lonlat     = c(121.255669, 14.16742),
#'   start_date = c("-06-01", "-06-14", "-06-30"),
#'   duration   = 90L,
#'   years      = 2001:2003
#' )
#' # This makes only 3 API calls (one per year), fetching from earliest to
#' # latest start date + duration
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @export

fetch_epicrop_weather_list <- function(
  lonlat,
  start_date,
  duration,
  years = NULL,
  mode = "pairwise",
  location_names = NULL
) {
  # Validate inputs
  .validate_required_args(lonlat, start_date, duration)
  mode <- rlang::arg_match0(mode, c("pairwise", "cross"))

  # Normalize locations
  locations <- .norm_locations(lonlat)
  location_names <- .resolve_location_names(locations, location_names)

  # Process start dates
  sd_char <- .process_start_dates(start_date, years)

  # `mode` only controls how locations and dates are paired when building the
  # *candidate* set of date/duration windows (pairwise recycling vs. full
  # location x date cross join); it must not change how weather ends up
  # fetched and bundled.
  tasks <- .build_tasks(
    locations,
    location_names,
    sd_char,
    duration,
    years,
    mode
  )

  tasks <- .collapse_tasks_by_location(tasks)

  results <- lapply(tasks, .fetch_one_weather)

  return(.format_results(results, tasks))
}

#' Validate Required Arguments
#' @autoglobal
#' @dev
.validate_required_args <- function(lonlat, start_date, duration) {
  if (missing(lonlat)) {
    cli::cli_abort(c(
      "{.arg lonlat} is required.",
      i = "Provide a numeric vector: {.code c(longitude, latitude)}",
      i = "Example: {.code lonlat = c(121.255669, 14.16742)}"
    ))
  }

  if (missing(start_date)) {
    cli::cli_abort(c(
      "{.arg start_date} is required.",
      i = "Provide a date string or vector of dates.",
      i = "Examples: {.code \"2000-06-30\"} or
      {.code c(\"-06-01\", \"-06-14\")}"
    ))
  }

  if (missing(duration)) {
    cli::cli_abort(c(
      "{.arg duration} is required.",
      i = "Provide the number of days as an integer.",
      i = "Example: {.code duration = 120L}"
    ))
  }
}

#' Normalize Location Input to Standard Matrix Format
#'
#' Converts various location input formats (numeric vector, matrix, data frame,
#' or list of pairs) to a standard 2-column numeric matrix with columns LON and LAT.
#' Preserves row names where available and validates all coordinates are finite.
#'
#' @param ll Location coordinates in one of the following formats:
#'   * Numeric vector of length 2: `c(lon, lat)` for a single location
#'   * Numeric vector of length 2n: `c(lon1, lat1, lon2, lat2, ...)` for
#'     n locations
#'   * 2-column matrix or data frame: each row is `c(lon, lat)`
#'   * List of numeric vectors: each element is `c(lon, lat)`. If named,
#'     names are preserved as row names.
#'
#' @returns A numeric 2-column matrix with:
#'   * Column names: `"LON"`, `"LAT"`
#'   * Row names preserved from input if available
#'   * Storage mode: `"double"`
#'   * All values finite and valid coordinates
#'
#' @details
#' **Input processing order:**
#' 1. List (non-data.frame): Validate elements, extract names, convert to matrix
#' 2. Data frame: Validate 2 columns and numeric type, convert to matrix
#' 3. Matrix: Validate 2 columns and numeric type, set column names
#' 4. Numeric vector: Validate even length, reshape to 2 columns
#'
#' **Validation:**
#' * All coordinates must be finite (no NA, Inf, or NaN)
#' * Matrix must have exactly 2 columns
#' * Numeric vector must have even length (length >= 2)
#' * List elements must be numeric vectors of length 2 with finite values
#'
#' **Errors:** Aborts with informative messages if input format is invalid
#' or coordinates are non-finite.
#'
#' @examples
#' # Single location as vector
#' .norm_locations(c(121.255669, 14.16742))
#'
#' # Multiple locations as vector
#' .norm_locations(c(121.255669, 14.16742, 120.985, 14.6042))
#'
#' # Named matrix
#' locs <- rbind(
#'   "IRRI_ZES" = c(121.255669, 14.16742),
#'   "Metro_Manila" = c(120.985, 14.6042)
#' )
#' .norm_locations(locs)
#'
#' # Named list
#' .norm_locations(list(
#'   site1 = c(121.255669, 14.16742),
#'   site2 = c(120.985, 14.6042)
#' ))
#'
#' @autoglobal
#' @dev
.norm_locations <- function(ll) {
  if (is.list(ll) && !is.data.frame(ll)) {
    ll <- .process_location_list(ll)
  } else if (is.data.frame(ll)) {
    ll <- .process_location_dataframe(ll)
  } else if (is.matrix(ll)) {
    ll <- .process_location_matrix(ll)
  } else if (is.numeric(ll)) {
    ll <- .process_location_vector(ll)
  } else {
    .abort_invalid_location_type(ll)
  }

  # Finalize: ensure double storage mode and column names
  storage.mode(ll) <- "double"
  colnames(ll) <- c("LON", "LAT")
  ll
}

#' Process Location List Input
#'
#' Validates and converts a list of coordinate pairs to a matrix.
#'
#' @param ll A list where each element is a numeric vector of length 2.
#'
#' @returns A numeric matrix with row names preserved if available.
#'
#' @autoglobal
#' @dev
.process_location_list <- function(ll) {
  valid <- vapply(
    ll,
    function(x) {
      is.numeric(x) && length(x) == 2L && all(is.finite(x))
    },
    logical(1L),
    USE.NAMES = FALSE
  )

  if (!length(ll) || !all(valid)) {
    cli::cli_abort(c(
      "{.arg lonlat} list elements must be numeric longitude-latitude pairs.",
      x = "Each element must be a finite numeric vector of length 2.",
      i = "Example: {.code list(site = c(121.255669, 14.16742))}"
    ))
  }

  location_names <- names(ll)
  mat <- do.call(rbind, unname(ll))

  # Preserve rownames if all are non-empty
  if (
    !is.null(location_names) &&
      length(location_names) == nrow(mat) &&
      all(nzchar(location_names))
  ) {
    rownames(mat) <- location_names
  }

  mat
}

#' Process Location Data Frame Input
#'
#' Validates and converts a data frame to a location matrix.
#'
#' @param ll A data frame with exactly 2 numeric columns.
#'
#' @returns A numeric matrix.
#'
#' @autoglobal
#' @dev
.process_location_dataframe <- function(ll) {
  if (ncol(ll) != 2L) {
    cli::cli_abort(c(
      "{.arg lonlat} matrix/data.frame must have exactly 2 columns.",
      x = "You provided {ncol(ll)} column{?s}.",
      i = "Columns should be: [longitude, latitude]"
    ))
  }

  numeric_columns <- vapply(ll, is.numeric, logical(1L))

  if (!all(numeric_columns)) {
    cli::cli_abort(c(
      "{.arg lonlat} columns must be numeric.",
      x = "Non-numeric column{?s}: {paste(names(ll)[!numeric_columns], collapse = ', ')}.",
      i = "Columns should contain longitude and latitude values."
    ))
  }

  as.matrix(ll)
}

#' Process Location Matrix Input
#'
#' Validates a matrix of coordinate pairs.
#'
#' @param ll A numeric matrix with exactly 2 columns.
#'
#' @returns The validated matrix.
#'
#' @autoglobal
#' @dev
.process_location_matrix <- function(ll) {
  if (ncol(ll) != 2L) {
    cli::cli_abort(c(
      "{.arg lonlat} matrix/data.frame must have exactly 2 columns.",
      x = "You provided {ncol(ll)} column{?s}.",
      i = "Columns should be: [longitude, latitude]"
    ))
  }

  if (!is.numeric(ll)) {
    cli::cli_abort(c(
      "{.arg lonlat} matrix must be numeric.",
      x = "You provided a matrix with storage mode {.val {storage.mode(ll)}}.",
      i = "Columns should contain longitude and latitude values."
    ))
  }

  if (!all(is.finite(ll))) {
    cli::cli_abort(
      "{.arg lonlat} must contain only finite longitude and latitude values."
    )
  }

  ll
}

#' Process Location Numeric Vector Input
#'
#' Validates and reshapes a numeric vector of coordinate pairs.
#'
#' @param ll A numeric vector of even length (lon1, lat1, lon2, lat2, ...).
#'
#' @returns A 2-column matrix with one pair per row.
#'
#' @autoglobal
#' @dev
.process_location_vector <- function(ll) {
  n <- length(ll)

  if (n == 0L || n %% 2L != 0L) {
    cli::cli_abort(c(
      "{.arg lonlat} numeric vector must have positive even length.",
      x = "You provided length {n}.",
      i = "Expected pairs of (longitude, latitude)."
    ))
  }

  if (!all(is.finite(ll))) {
    cli::cli_abort(
      "{.arg lonlat} must contain only finite longitude and latitude values."
    )
  }

  matrix(ll, ncol = 2L, byrow = TRUE)
}

#' Abort on Invalid Location Type
#'
#' Helper to provide consistent error message for unsupported input types.
#'
#' @param ll The invalid input object.
#'
#' @autoglobal
#' @dev
.abort_invalid_location_type <- function(ll) {
  cli::cli_abort(c(
    "{.arg lonlat} must be numeric, not {.cls {class(ll)}}.",
    i = "Expected format: {.code c(longitude, latitude)}.",
    i = "Example: {.code lonlat = c(121.255669, 14.16742)}."
  ))
}

#' Resolve Location Names
#' @autoglobal
#' @dev
.resolve_location_names <- function(locations, location_names) {
  n_locations <- nrow(locations)

  if (!is.null(location_names)) {
    if (length(location_names) != n_locations) {
      cli::cli_abort(c(
        "{.arg location_names} length must equal number of locations.",
        x = "You provided {length(location_names)} name{?s} for
        {n_locations} location{?s}."
      ))
    }
    return(location_names)
  }

  # Use rownames if available
  rn <- rownames(locations)
  if (!is.null(rn) && all(nzchar(rn))) {
    return(rn)
  }

  # Generate default names
  sprintf("location%02d", seq_len(n_locations))
}

#' Check if Date String is Month-Day Only Format
#' @dev
.is_md_only <- function(x_chr) {
  grepl("^-?\\d{1,2}[-/]\\d{1,2}$", x_chr) &
    !grepl("^\\d{4}-\\d{2}-\\d{2}$", x_chr)
}

#' Normalize Month-Day Strings to MM-DD Format
#' @dev
.normalize_md <- function(x_chr) {
  vapply(
    strsplit(
      sub("^-?", "", gsub("/", "-", x_chr, fixed = TRUE)),
      "-",
      fixed = TRUE
    ),
    function(p) sprintf("%02d-%02d", as.integer(p[1L]), as.integer(p[2L])),
    character(1L),
    USE.NAMES = FALSE
  )
}

#' Process Start Dates to Character Format
#' @autoglobal
#' @dev
.process_start_dates <- function(start_date, years) {
  sd_char <- as.character(start_date)
  md_idx <- .is_md_only(sd_char)

  if (any(md_idx) && is.null(years)) {
    cli::cli_abort(c(
      "When {.arg start_date} is month-day only (e.g., '06-30'), you must
      provide {.arg years}.",
      i = "Example: {.code years = 2001:2003}"
    ))
  }

  if (any(md_idx)) {
    sd_char[md_idx] <- .normalize_md(sd_char[md_idx])
  }

  sd_char
}

#' Build Task List for Weather Fetching
#' @autoglobal
#' @dev
.build_tasks <- function(
  locations,
  location_names,
  sd_char,
  duration,
  years,
  mode
) {
  n_locations <- nrow(locations)
  n_dates <- length(sd_char)

  # Validate duration before any processing
  if (!rlang::is_integerish(duration, finite = TRUE) || any(duration <= 0L)) {
    cli::cli_abort("{.arg duration} must contain positive integers (days).")
  }

  # Create index combinations based on mode
  if (mode == "pairwise") {
    max_len <- max(n_locations, n_dates)
    location_idx <- rep_len(seq_len(n_locations), max_len)
    date_idx <- rep_len(seq_len(n_dates), max_len)
  } else {
    location_idx <- rep(seq_len(n_locations), each = n_dates)
    date_idx <- rep(seq_len(n_dates), times = n_locations)
  }

  # Recycle duration to match task count
  dur_vec <- rep_len(as.integer(duration), length(location_idx))

  # Build tasks
  Map(
    function(loc_i, date_i, dur) {
      .create_single_task(
        locations,
        location_names,
        loc_i,
        sd_char,
        date_i,
        dur,
        years
      )
    },
    location_idx,
    date_idx,
    dur_vec
  )
}

#' Create a Single Task for Weather Fetching
#' @autoglobal
#' @dev
.create_single_task <- function(
  locations,
  location_names,
  loc_idx,
  sd_char,
  date_idx,
  dur,
  years
) {
  resolved <- .resolve_date_and_duration(sd_char[date_idx], dur, years)

  list(
    location_id = loc_idx,
    location_name = location_names[loc_idx],
    lonlat = locations[loc_idx, ],
    start_chr = resolved$start_chr,
    duration = resolved$duration
  )
}

#' Resolve Date String and Calculate Duration
#' @autoglobal
#' @dev
.resolve_date_and_duration <- function(d_str, dur, years) {
  if (grepl("^\\d{2}-\\d{2}$", d_str)) {
    return(.cover_from_md_years(d_str, years, dur))
  }

  start_idate <- tryCatch(
    data.table::as.IDate(d_str),
    error = function(e) NA_integer_
  )

  if (is.na(start_idate)) {
    cli::cli_abort(c(
      x = "Cannot parse {.arg start_date} value {.val {d_str}} as a valid date.",
      i = "Expected format: YYYY-MM-DD",
      i = "Example: {.code \"2000-06-30\"}"
    ))
  }

  list(start_chr = as.character(start_idate), duration = as.integer(dur))
}

#' Calculate Date Coverage Window from Month-Day and Years
#' @autoglobal
#' @dev
.cover_from_md_years <- function(md_mmdd, yrs, window_days) {
  if (is.null(yrs) || length(yrs) == 0L) {
    cli::cli_abort(c(
      "When {.arg start_date} is month-day only, you must provide {.arg years}.",
      i = "Example: {.code years = 2001:2003}"
    ))
  }

  em <- tryCatch(
    data.table::as.IDate(sprintf("%04d-%s", as.integer(yrs), md_mmdd)),
    error = function(e) data.table::as.IDate(NA)
  )

  if (anyNA(em)) {
    cli::cli_abort(c(
      "Failed to construct valid dates from {.arg years} and month-day
      {.val {md_mmdd}}.",
      i = "Check that month-day values are valid (e.g., not 02-30)."
    ))
  }

  start <- min(em)
  end <- max(em) + (as.integer(window_days) - 1L)

  list(
    start_chr = as.character(start),
    duration = as.integer(end - start + 1L)
  )
}


#' Collapse Tasks for One Location
#' @autoglobal
#' @dev
.collapse_one_location <- function(tsks) {
  starts <- data.table::as.IDate(vapply(
    tsks,
    `[[`,
    character(1L),
    "start_chr",
    USE.NAMES = FALSE
  ))
  durations <- vapply(tsks, `[[`, integer(1L), "duration", USE.NAMES = FALSE)
  ends <- starts + (durations - 1L)

  s_min <- min(starts)
  e_max <- max(ends)

  t0 <- tsks[[1L]]
  list(
    location_id = t0$location_id,
    location_name = t0$location_name,
    lonlat = t0$lonlat,
    start_chr = as.character(s_min),
    duration = as.integer(e_max - s_min + 1L)
  )
}

#' Collapse Tasks by Location for Efficient API Calls
#' @autoglobal
#' @dev
.collapse_tasks_by_location <- function(tasks) {
  by_location <- split(
    tasks,
    vapply(tasks, `[[`, integer(1L), "location_id", USE.NAMES = FALSE)
  )
  unname(lapply(by_location, .collapse_one_location))
}

#' Fetch Weather Data for One Task
#' @dev
.fetch_one_weather <- function(task) {
  tab <- get_wth(
    lonlat = task$lonlat,
    dates = task$start_chr,
    duration = task$duration
  )

  out <- split(
    tab,
    data.table::year(tab$YYYYMMDD),
    keep.by = FALSE
  )

  class(out) <- c("epicrop.wth.list", "list")
  out
}

#' Format Results for Return
#' @dev
.format_results <- function(results, tasks) {
  if (length(results) == 1L) {
    return(results[[1L]])
  }

  keys <- vapply(
    tasks,
    function(tk) {
      sprintf("%s__%s__%dd", tk$location_name, tk$start_chr, tk$duration)
    },
    character(1L),
    USE.NAMES = FALSE
  )

  names(results) <- keys
  class(results) <- c("epicrop.wth.bundle", "epicrop.wth.list", "list")
  results
}
