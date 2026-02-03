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
  .validate_lonlat_type(lonlat)
  mode <- rlang::arg_match0(mode, c("pairwise", "cross"))

  # Normalize and validate locations
  locations <- .norm_locations(lonlat)
  location_names <- .resolve_location_names(locations, location_names)

  # Process start dates
  sd_char <- .process_start_dates(start_date, years)

  # Build and validate tasks
  tasks <- .build_tasks(
    locations,
    location_names,
    sd_char,
    duration,
    years,
    mode
  )

  # Collapse tasks by location for efficiency
  tasks <- .collapse_tasks_by_location(tasks)

  # Fetch and process weather data
  results <- .fetch_all_weather(tasks)

  # Format and return results
  return(.format_results(results, tasks))
}

#' Validate Required Arguments
#'
#' Checks that `lonlat`, `start_date`, and `duration` are provided and gives
#' helpful error messages with examples if any are missing.
#'
#' @param lonlat The lonlat argument from the parent function.
#' @param start_date The start_date argument from the parent function.
#' @param duration The duration argument from the parent function.
#'
#' @return NULL (invisible). Aborts with error if validation fails.
#'
#' @examples
#' # This will abort with helpful message
#' .validate_required_args(missing_arg(), "2000-06-01", 120)
#'
#' # This passes
#' .validate_required_args(c(121.25, 14.16), "2000-06-01", 120)
#' @dev
#' @autoglobal
.validate_required_args <- function(lonlat, start_date, duration) {
  if (missing(lonlat)) {
    cli::cli_abort(c(
      "{.arg lonlat} is required.",
      "i" = "Provide a numeric vector: {.code c(longitude, latitude)}",
      "i" = "Example: {.code lonlat = c(121.255669, 14.16742)}"
    ))
  }

  if (missing(start_date)) {
    cli::cli_abort(c(
      "{.arg start_date} is required.",
      "i" = "Provide a date string or vector of dates.",
      "i" = "Examples: {.code \"2000-06-30\"} or {.code c(\"-06-01\",
              \"-06-14\")}"
    ))
  }

  if (missing(duration)) {
    cli::cli_abort(c(
      "{.arg duration} is required.",
      "i" = "Provide the number of days as an integer.",
      "i" = "Example: {.code duration = 120L}"
    ))
  }
}

#' Validate lonlat Type and Catch Common Errors
#'
#' Validates that `lonlat` is an appropriate type (numeric, matrix, data.frame,
#' or list). Also detects common mistakes like passing a vector of years instead
#' of coordinates.
#'
#' @param lonlat The lonlat argument to validate.
#'
#' @return NULL (invisible). Aborts with error if validation fails.
#'
#' @examples
#' # Valid inputs
#' .validate_lonlat_type(c(121.25, 14.16))
#' .validate_lonlat_type(matrix(c(121.25, 14.16, 120.98, 14.60),
#'                                ncol = 2, byrow = TRUE))
#'
#' # This catches the common mistake of passing years
#' .validate_lonlat_type(2001:2003)  # Aborts with helpful message
#'
#' # This catches invalid types
#' .validate_lonlat_type("not coordinates")  # Aborts
#' @dev
#' @autoglobal
.validate_lonlat_type <- function(lonlat) {
  valid_types <- is.numeric(lonlat) ||
    is.matrix(lonlat) ||
    is.data.frame(lonlat) ||
    is.list(lonlat)

  if (!valid_types) {
    cli::cli_abort(c(
      "{.arg lonlat} must be numeric, not {.cls {class(lonlat)}}.",
      "x" = "You provided: {.val {lonlat}}",
      "i" = "Expected format: {.code c(longitude, latitude)}",
      "i" = "Example: {.code lonlat = c(121.255669, 14.16742)}"
    ))
  }
}


#' Normalize Location Input to Standard Matrix Format
#'
#' Converts various location input formats (numeric vector, matrix, data.frame,
#' or list) into a standardized 2-column numeric matrix with columns "LON" and
#' "LAT".
#'
#' @param ll Location input in one of several formats.
#'
#' @return A numeric matrix with 2 columns ("LON", "LAT") and n rows (one per
#'   location).
#'
#' @examples
#' # Single location as vector
#' .norm_locations(c(121.25, 14.16))
#' #      LON    LAT
#' # [1,] 121.25 14.16
#'
#' # Multiple locations as flat numeric vector
#' .norm_locations(c(121.25, 14.16, 120.98, 14.60))
#' #      LON    LAT
#' # [1,] 121.25 14.16
#' # [2,] 120.98 14.60
#'
#' # As matrix
#' .norm_locations(matrix(c(121.25, 14.16, 120.98, 14.60), ncol = 2,
#'                          byrow = TRUE))
#'
#' # As data.frame
#' .norm_locations(data.frame(lon = c(121.25, 120.98), lat = c(14.16, 14.60)))
#'
#' # As list
#' .norm_locations(list(c(121.25, 14.16), c(120.98, 14.60)))
#' @dev
#' @autoglobal
.norm_locations <- function(ll) {
  # Convert list to data.frame
  if (is.list(ll) && !is.data.frame(ll)) {
    ll <- data.table::rbindlist(ll)
  }

  # Convert data.frame to matrix
  if (is.data.frame(ll)) {
    ll <- as.matrix(ll)
  }

  # Validate and return matrix
  if (is.matrix(ll)) {
    return(.validate_location_matrix(ll))
  }

  # Convert numeric vector to matrix
  if (is.numeric(ll)) {
    return(.numeric_to_location_matrix(ll))
  }

  # If we get here, invalid input
  cli::cli_abort(c(
    "{.arg lonlat} must be in one of these formats:",
    "i" = "A numeric vector: {.code c(lon, lat)}",
    "i" = "A 2-column matrix or data.frame",
    "i" = "A list of length-2 numeric vectors",
    "x" = "You provided a {.cls {class(ll)}} of length {length(ll)}"
  ))
}

#' Validate Location Matrix Has 2 Columns
#'
#' Ensures a location matrix has exactly 2 columns (longitude, latitude) and
#' sets appropriate column names and storage mode.
#'
#' @param ll A matrix to validate.
#'
#' @return A validated numeric matrix with columns "LON" and "LAT".
#'
#' @examples
#' # Valid matrix
#' m <- matrix(c(121.25, 14.16, 120.98, 14.60), ncol = 2, byrow = TRUE)
#' .validate_location_matrix(m)
#'
#' # Invalid - 3 columns
#' m_bad <- matrix(1:9, ncol = 3)
#' .validate_location_matrix(m_bad)  # Aborts with error
#' @dev
#' @autoglobal
.validate_location_matrix <- function(ll) {
  if (ncol(ll) != 2L) {
    cli::cli_abort(c(
      "{.arg lonlat} matrix/data.frame must have exactly 2 columns.",
      "x" = "You provided {ncol(ll)} column{?s}.",
      "i" = "Columns should be: [longitude, latitude]"
    ))
  }
  storage.mode(ll) <- "double"
  colnames(ll) <- c("LON", "LAT")
  return(ll)
}

#' Convert Numeric Vector to Location Matrix
#'
#' Converts a numeric vector to a 2-column location matrix. Accepts length-2
#' vectors (single location) or even-length vectors (multiple locations, treated
#' as pairs).
#'
#' @param ll A numeric vector of length 2 or even length > 2.
#'
#' @return A numeric matrix with 2 columns ("LON", "LAT").
#'
#' @examples
#' # Single location
#' .numeric_to_location_matrix(c(121.25, 14.16))
#'
#' # Multiple locations (flat vector)
#' .numeric_to_location_matrix(c(121.25, 14.16, 120.98, 14.60))
#'
#' # Odd length - error
#' .numeric_to_location_matrix(c(121.25, 14.16, 120.98))  # Aborts
#' @dev
#' @autoglobal
.numeric_to_location_matrix <- function(ll) {
  if (length(ll) == 2L) {
    ll <- matrix(ll, ncol = 2L)
  } else if (length(ll) %% 2L == 0L) {
    ll <- matrix(ll, ncol = 2L, byrow = TRUE)
  } else {
    cli::cli_abort(c(
      "{.arg lonlat} numeric vector must have even length.",
      "x" = "You provided length {length(ll)}",
      "i" = "Expected pairs of (longitude, latitude)"
    ))
  }
  storage.mode(ll) <- "double"
  colnames(ll) <- c("LON", "LAT")
  return(ll)
}

#' Resolve Location Names
#'
#' Determines location names from explicit argument, matrix rownames, or
#' generates default names.
#'
#' @param locations A normalized location matrix (from `.norm_locations()`).
#' @param location_names Optional character vector of names, or NULL.
#'
#' @return A character vector of location names (length = number of locations).
#'
#' @examples
#' # With explicit names
#' locs <- matrix(c(121.25, 14.16, 120.98, 14.60), ncol = 2, byrow = TRUE)
#' colnames(locs) <- c("LON", "LAT")
#' .resolve_location_names(locs, c("Site_A", "Site_B"))
#' # [1] "Site_A" "Site_B"
#'
#' # With rownames
#' rownames(locs) <- c("IRRI", "Manila")
#' .resolve_location_names(locs, NULL)
#' # [1] "IRRI"   "Manila"
#'
#' # Auto-generated
#' .resolve_location_names(locs, NULL)
#' # [1] "location01" "location02"
#' @dev
#' @autoglobal
.resolve_location_names <- function(locations, location_names) {
  n_locations <- nrow(locations)

  if (!is.null(location_names)) {
    if (length(location_names) != n_locations) {
      cli::cli_abort(c(
        "{.arg location_names} length must equal number of locations.",
        "x" = "You provided {length(location_names)} name{?s} for {n_locations}
        location{?s}."
      ))
    }
    return(location_names)
  }

  # Use rownames if available
  if (!is.null(rownames(locations)) && all(nzchar(rownames(locations)))) {
    return(rownames(locations))
  }

  # Generate default names
  return(sprintf("location%02d", seq_len(n_locations)))
}

#' Check if Date String is Month-Day Only Format
#'
#' Determines if a date string is in month-day format (e.g., "06-30", "-6-30",
#' "6/30") rather than full ISO format (YYYY-MM-DD).
#'
#' @param x_chr Character vector of date strings.
#'
#' @return Logical vector indicating which elements are month-day only.
#'
#' @examples
#' .is_md_only(c("2000-06-30", "-06-30", "6/15", "12-25"))
#' # [1] FALSE  TRUE  TRUE  TRUE
#'
#' .is_md_only("2000-06-30")
#' # [1] FALSE
#'
#' .is_md_only("-06-30")
#' # [1] TRUE
#' @dev
#' @autoglobal
.is_md_only <- function(x_chr) {
  !grepl("^\\d{4}-\\d{2}-\\d{2}$", x_chr) &
    grepl("^\\-?\\d{1,2}[-/]\\d{1,2}$", x_chr)
}

#' Normalize Month-Day Strings to MM-DD Format
#'
#' Converts various month-day formats (e.g., "6-30", "-06-30", "6/15") to
#' standardized "MM-DD" format with zero-padding.
#'
#' @param x_chr Character vector of month-day strings.
#'
#' @return Character vector of normalized "MM-DD" strings.
#'
#' @examples
#' .normalize_md(c("-06-30", "6-15", "12/1"))
#' # [1] "06-30" "06-15" "12-01"
#'
#' .normalize_md("-6-1")
#' # [1] "06-01"
#' @dev
#' @autoglobal
.normalize_md <- function(x_chr) {
  vapply(
    strsplit(gsub("^\\-?", "", gsub("/", "-", x_chr)), "-", fixed = TRUE),
    function(p) sprintf("%02d-%02d", as.integer(p[1]), as.integer(p[2])),
    character(1)
  )
}

#' Process Start Dates to Character Format
#'
#' Converts start_date input to character format and normalizes any month-day
#' only values. Validates that years are provided when needed.
#'
#' @param start_date Date input (character, Date, or IDate), possibly containing
#'   month-day only values.
#' @param years Optional integer vector of years (required for month-day
#'   values).
#'
#' @return Character vector of processed date strings.
#'
#' @examples
#' # ISO dates pass through
#' .process_start_dates("2000-06-30", NULL)
#' # [1] "2000-06-30"
#'
#' # Month-day format normalized
#' .process_start_dates(c("-06-01", "6/15"), 2000:2001)
#' # [1] "06-01" "06-15"
#'
#' # Error if years missing
#' .process_start_dates("-06-30", NULL)  # Aborts
#' @dev
#' @autoglobal
.process_start_dates <- function(start_date, years) {
  sd_char <- as.character(start_date)
  md_idx <- .is_md_only(sd_char)

  if (any(md_idx) && is.null(years)) {
    cli::cli_abort(c(
      "When {.arg start_date} is month-day only (e.g., '06-30'), you must
      provide {.arg years}.",
      "i" = "Example: {.code years = 2001:2003}"
    ))
  }

  if (any(md_idx)) {
    sd_char[md_idx] <- .normalize_md(sd_char[md_idx])
  }

  return(sd_char)
}

#' Build Task List for Weather Fetching
#'
#' Creates a list of tasks for fetching weather data based on locations, dates,
#' durations, and mode (pairwise or cross).
#'
#' @param locations Normalized location matrix (from `.norm_locations()`).
#' @param location_names Character vector of location names.
#' @param sd_char Character vector of processed start dates.
#' @param duration Integer(s) for duration in days.
#' @param years Integer vector of years (for month-day dates).
#' @param mode Character: "pairwise" or "cross".
#'
#' @return List of tasks, each containing location_id, location_name, lonlat,
#'   start_chr, and duration.
#'
#' @examples
#' locs <- matrix(c(121.25, 14.16, 120.98, 14.60), ncol = 2, byrow = TRUE)
#' colnames(locs) <- c("LON", "LAT")
#' names <- c("Site_A", "Site_B")
#' dates <- c("06-01", "06-15")
#'
#' # Pairwise: Site_A with 06-01, Site_B with 06-15
#' tasks <- .build_tasks(locs, names, dates, 120L, 2000:2001, "pairwise")
#'
#' # Cross: All combinations
#' tasks <- .build_tasks(locs, names, dates, 120L, 2000:2001, "cross")
#' @dev
#' @autoglobal
.build_tasks <- function(
  locations,
  location_names,
  sd_char,
  duration,
  years,
  mode
) {
  n_locations <- nrow(locations)
  date_vec <- sd_char

  # Create combinations based on mode
  if (mode == "pairwise") {
    # Recycle to match longer vector
    max_len <- max(n_locations, length(date_vec))
    location_idx <- rep_len(seq_len(n_locations), max_len)
    date_idx <- rep_len(seq_along(date_vec), max_len)
  } else {
    # Cross: all combinations
    location_idx <- rep(seq_len(n_locations), each = length(date_vec))
    date_idx <- rep(seq_along(date_vec), times = n_locations)
  }

  dur_vec <- rep_len(as.integer(duration), length.out = length(date_idx))

  # Validate duration
  if (any(!rlang::is_integerish(dur_vec, finite = TRUE) | dur_vec <= 0L)) {
    cli::cli_abort("{.arg duration} must contain positive integers (days).")
  }

  # Build task list
  tasks <- vector("list", length(date_idx))
  for (k in seq_along(tasks)) {
    tasks[[k]] <- .create_single_task(
      locations,
      location_names,
      location_idx[k],
      sd_char,
      date_idx[k],
      dur_vec[k],
      years
    )
  }

  return(tasks)
}

#' Create a Single Task for Weather Fetching
#'
#' Builds one task structure containing all information needed to fetch weather
#' for a specific location, date, and duration combination.
#'
#' @param locations Normalized location matrix.
#' @param location_names Character vector of location names.
#' @param loc_idx Integer index for which location to use.
#' @param sd_char Character vector of all start dates.
#' @param date_idx Integer index for which date to use.
#' @param dur Integer duration in days.
#' @param years Integer vector of years (for month-day dates).
#'
#' @return A list with elements: location_id, location_name, lonlat, start_chr,
#'   duration.
#'
#' @examples
#' locs <- matrix(c(121.25, 14.16), ncol = 2)
#' colnames(locs) <- c("LON", "LAT")
#' task <- .create_single_task(locs, "Site_A", 1L, c("06-01", "06-15"), 1L,
#'                             120L, 2000:2001)
#' # $location_id: 1
#' # $location_name: "Site_A"
#' # $lonlat: c(121.25, 14.16)
#' # $start_chr: "2000-06-01"
#' # $duration: 485 (covers 2000 and 2001)
#' @dev
#' @autoglobal
.create_single_task <- function(
  locations,
  location_names,
  loc_idx,
  sd_char,
  date_idx,
  dur,
  years
) {
  lon <- locations[loc_idx, "LON"]
  lat <- locations[loc_idx, "LAT"]
  d_str <- sd_char[date_idx]

  # Resolve date and duration
  resolved <- .resolve_date_and_duration(d_str, dur, years)

  return(list(
    location_id = loc_idx,
    location_name = location_names[loc_idx],
    lonlat = c(lon, lat),
    start_chr = resolved$start_chr,
    duration = resolved$duration
  ))
}

#' Resolve Date String and Calculate Duration
#'
#' Converts a date string (either ISO YYYY-MM-DD or month-day MM-DD) to a final
#' start date and duration, expanding month-day values across years if needed.
#'
#' @param d_str Character date string.
#' @param dur Integer duration in days.
#' @param years Integer vector of years (for month-day dates).
#'
#' @return A list with elements: start_chr (character date), duration (integer).
#'
#' @examples
#' # ISO date - pass through
#' .resolve_date_and_duration("2000-06-30", 120L, NULL)
#' # $start_chr: "2000-06-30"
#' # $duration: 120
#'
#' # Month-day across multiple years
#' .resolve_date_and_duration("06-30", 120L, 2000:2002)
#' # $start_chr: "2000-06-30"
#' # $duration: 850 (2000-06-30 to 2002-10-27)
#' @dev
#' @autoglobal
.resolve_date_and_duration <- function(d_str, dur, years) {
  # Check if month-day only format
  if (grepl("^\\d{2}-\\d{2}$", d_str)) {
    return(.cover_from_md_years(d_str, years, dur))
  }

  # Parse as full date
  start_idate <- data.table::as.IDate(d_str)
  if (is.na(start_idate)) {
    cli::cli_abort(c(
      "Cannot parse {.arg start_date} value {.val {d_str}} as a valid date.",
      "i" = "Expected format: YYYY-MM-DD",
      "i" = "Example: {.code \"2000-06-30\"}"
    ))
  }

  return(list(
    start_chr = as.character(start_idate),
    duration = as.integer(dur)
  ))
}

#' Calculate Date Coverage Window from Month-Day and Years
#'
#' Given a month-day string and a vector of years, calculates the earliest start
#' date and total duration needed to cover all year × month-day combinations
#' plus the growing season duration.
#'
#' @param md_mmdd Character month-day string in "MM-DD" format.
#' @param yrs Integer vector of years.
#' @param window_days Integer growing season duration in days.
#'
#' @return A list with elements: start_chr (earliest date), duration
#' (total days).
#'
#' @examples
#' # Single year
#' .cover_from_md_years("06-30", 2000L, 120L)
#' # $start_chr: "2000-06-30"
#' # $duration: 120
#'
#' # Multiple years - spans from first to last + duration
#' .cover_from_md_years("06-30", 2000:2002, 120L)
#' # $start_chr: "2000-06-30"
#' # $duration: 850 (from 2000-06-30 to 2002-10-27)
#' @dev
#' @autoglobal
.cover_from_md_years <- function(md_mmdd, yrs, window_days) {
  if (is.null(yrs) || length(yrs) == 0L) {
    cli::cli_abort(c(
      "When {.arg start_date} is month-day only, you must provide
      {.arg years}.",
      "i" = "Example: {.code years = 2001:2003}"
    ))
  }

  em <- data.table::as.IDate(sprintf("%04d-%s", as.integer(yrs), md_mmdd))
  if (anyNA(em)) {
    cli::cli_abort(c(
      "Failed to construct valid dates from {.arg years} and month-day
      {.val {md_mmdd}}.",
      "i" = "Check that month-day values are valid (e.g., not 02-30)."
    ))
  }

  start <- min(em)
  end <- max(em) + (as.integer(window_days) - 1L)

  return(list(
    start_chr = as.character(start),
    duration = as.integer(end - start + 1L)
  ))
}

#' Collapse Tasks by Location for Efficient API Calls
#'
#' Groups tasks by location and collapses overlapping date ranges to minimize
#' the number of API calls needed.
#'
#' @param tasks List of tasks from `.build_tasks()`.
#'
#' @return List of collapsed tasks (one per location) with extended durations.
#'
#' @examples
#' # If you have 3 tasks for same location with dates:
#' # Task 1: 2000-06-01 for 120 days
#' # Task 2: 2000-06-15 for 120 days
#' # Task 3: 2000-06-30 for 120 days
#' # This collapses to:
#' # Task: 2000-06-01 for 149 days (covers all three + durations)
#' @dev
#' @autoglobal
.collapse_tasks_by_location <- function(tasks) {
  by_location <- split(tasks, vapply(tasks, `[[`, integer(1L), "location_id"))
  unname(lapply(by_location, .collapse_one_location))
}

#' Collapse Tasks for One Location
#'
#' Takes all tasks for a single location and finds the minimum start date and
#' maximum end date to create one efficient fetch covering all requests.
#'
#' @param tsks List of tasks for one location.
#'
#' @return A single task covering the entire date range needed.
#'
#' @examples
#' tasks <- list(
#'   list(location_id = 1L, start_chr = "2000-06-01", duration = 120L),
#'   list(location_id = 1L, start_chr = "2000-06-30", duration = 120L)
#' )
#' .collapse_one_location(tasks)
#' # Single task: start = "2000-06-01", duration = 149
#' @dev
#' @autoglobal
.collapse_one_location <- function(tsks) {
  starts <- data.table::as.IDate(vapply(
    tsks,
    `[[`,
    character(1L),
    "start_chr"
  ))
  ends <- starts +
    (vapply(tsks, function(t) as.integer(t$duration), integer(1L)) - 1L)

  s_min <- min(starts)
  e_max <- max(ends)

  t0 <- tsks[[1L]]
  return(list(
    location_id = t0$location_id,
    location_name = t0$location_name,
    lonlat = t0$lonlat,
    start_chr = as.character(s_min),
    duration = as.integer(e_max - s_min + 1L)
  ))
}

#' Fetch Weather Data for All Tasks
#'
#' Executes weather fetching for each task in the list.
#'
#' @param tasks List of tasks (typically collapsed by location).
#'
#' @return List of weather data results, one per task.
#'
#' @examples
#' tasks <- list(
#'   list(lonlat = c(121.25, 14.16), start_chr = "2000-06-01", duration = 120L)
#' )
#' results <- .fetch_all_weather(tasks)
#' @dev
#' @autoglobal
.fetch_all_weather <- function(tasks) {
  lapply(tasks, .fetch_one_weather)
}

#' Fetch Weather Data for One Task
#'
#' Fetches weather data for a single location/date/duration combination and
#' splits the result by calendar year.
#'
#' @param task A task list with elements: lonlat, start_chr, duration.
#'
#' @return An `epicrop.wth.list` object containing weather data split by year.
#'
#' @examples
#' task <- list(
#'   lonlat = c(121.25, 14.16),
#'   start_chr = "2000-06-01",
#'   duration = 120L
#' )
#' wth <- .fetch_one_weather(task)
#' # Returns list with elements: $`2000`, potentially $`2001`
#' @dev
#' @autoglobal
.fetch_one_weather <- function(task) {
  tab <- get_wth(
    lonlat = task$lonlat,
    dates = task$start_chr,
    duration = task$duration
  )

  # Split by calendar year
  yrs <- sort(unique(format(tab[["YYYYMMDD"]], "%Y")))
  out <- lapply(yrs, function(y) {
    s <- data.table::as.IDate(sprintf("%s-01-01", y))
    e <- data.table::as.IDate(sprintf("%s-12-31", y))
    tab[YYYYMMDD %between% c(s, e)]
  })
  names(out) <- yrs
  class(out) <- c("epicrop.wth.list", "list")
  return(out)
}

#' Format Results for Return
#'
#' Formats the final results list, returning a single `epicrop.wth.list` for
#' one request or an `epicrop.wth.bundle` for multiple requests with descriptive
#' names.
#'
#' @param results List of weather data results from `.fetch_all_weather()`.
#' @param tasks List of tasks used to generate results.
#'
#' @return Either a single `epicrop.wth.list` or an `epicrop.wth.bundle`.
#'
#' @examples
#' # Single result - returns unwrapped
#' .format_results(list(wth_data), list(task1))
#' # Returns: wth_data (class: epicrop.wth.list)
#'
#' # Multiple results - returns named bundle
#' .format_results(list(wth1, wth2), list(task1, task2))
#' # Returns: named list (class: epicrop.wth.bundle, epicrop.wth.list, list)
#' @dev
#' @autoglobal
.format_results <- function(results, tasks) {
  if (length(results) == 1L) {
    return(results[[1L]])
  }

  keys <- vapply(
    tasks,
    function(tk) {
      sprintf(
        "%s__%s__%dd",
        tk$location_name,
        tk$start_chr,
        as.integer(tk$duration)
      )
    },
    character(1)
  )

  names(results) <- keys
  class(results) <- c("epicrop.wth.bundle", "epicrop.wth.list", "list")
  return(results)
}
