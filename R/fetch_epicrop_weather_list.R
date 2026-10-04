#' Get Weather for One or More Locations and Split by Calendar Year
#'
#' Retrieves one or more continuous blocks of weather data for one or more
#' locations from given start dates and durations, then splits each result into
#' a named list of per-year tables. Each (location, start, duration) result is
#' an `epicrop.wth.list`. The top-level return is an `epicrop.wth.bundle` when
#' multiple requests are processed, otherwise a single `epicrop.wth.list`.
#'
#' @section Flexible Input Formats:
#'
#' **Locations (`lonlat`):**
#' - Single numeric vector: `c(lon, lat)`
#' - Multiple locations as: numeric vector of length `2n`, a 2-column
#'   matrix/data.frame, or a list of `list(c(lon, lat))`
#'
#' **Start dates (`start_date`):**
#' - Full ISO date: `"2000-06-30"` or `Date`/`IDate` object
#' - Month-day string: `"06-30"`, `"6-30"`, `"-06-30"` (with `years` parameter)
#' - Vector of dates (recycled in pairwise mode or crossed in cross mode)
#'
#' **Duration:**
#' - Single integer or vector (recycled)
#' - Interpreted as inclusive days: `end = start + duration - 1`
#'
#' **Years (optional):**
#' - Only used when `start_date` contains month-day values without a year
#' - Computes a **cover window** per distinct month-day:
#'   - `start = min(paste(years, md))`
#'   - `end = max(paste(years, md)) + (duration - 1)`.
#'
#' @param lonlat Numeric length-2 vector `c(lon, lat)`, OR multiple locations
#'   as: numeric vector of length `2n`, a 2-column matrix/data.frame, or a list
#'   of numeric length-2 vectors.
#' @param start_date A starting date (character `"YYYY-MM-DD"`, [Date], or
#'   [data.table::IDate]) **or** a month-day string like `"06-30"` (leading
#'   `"-"` optional) used together with `years`.
#' @param duration Integer(s). Number of days to retrieve **from** `start_date`
#'   (inclusive). Single value or vector (recycled).
#' @param years Optional integer vector of years. **Required** if `start_date`
#'   contains any month-day values without a year.
#' @param mode Character scalar: `"pairwise"` (default) pairs locations and
#'   dates by position (recycling length-1), or `"cross"` forms all combinations
#'   (locations × dates).
#' @param location_names Optional character names for locations (length = number
#'   of unique locations).
#' @param source Character scalar: weather data source, `"power"` (default, NASA
#'   POWER) or `"era5"` (ERA5 reanalysis via Open-Meteo). See [get_wth()] for
#'   details on the differences between sources.
#'
#' @returns
#' - If only one (location, start, duration) is requested: an `epicrop.wth.list`.
#' - Otherwise: an `epicrop.wth.bundle` (a named list of `epicrop.wth.list`).
#'
#' @examplesIf interactive()
#' # Single location, month-day start date
#' w1 <- get_epicrop_weather_list(
#'   lonlat = c(121.255669, 14.16742),
#'   start_date = "6/30",
#'   duration = 120L,
#'   years = 2000:2001
#' )
#'
#' # ISO start date
#' w2 <- get_epicrop_weather_list(
#'   lonlat = c(121.255669, 14.16742),
#'   start_date = "2000-06-30",
#'   duration = 120L
#' )
#'
#' # Multiple locations × multiple dates (cross mode)
#' locations <- rbind(
#'   "IRRI_ZES" = c(121.255669, 14.16742),
#'   "Metro_Manila" = c(120.985, 14.6042)
#' )
#' w3 <- get_epicrop_weather_list(
#'   lonlat = locations,
#'   start_date = c("2000-06-30", "6-30"),
#'   duration = 120L,
#'   years = 2000:2001,
#'   mode = "cross"
#' )
#'
#' # Multiple start dates for same location
#' w4 <- get_epicrop_weather_list(
#'   lonlat = c(121.255669, 14.16742),
#'   start_date = c("-06-01", "-06-14", "-06-30"),
#'   duration = 90L,
#'   years = 2001:2003
#' )
#' # ERA5 instead of NASA POWER
#' w5 <- get_epicrop_weather_list(
#'   lonlat = c(121.255669, 14.16742),
#'   start_date = "2000-06-30",
#'   duration = 120L,
#'   source = "era5"
#' )
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @export
get_epicrop_weather_list <- function(
  lonlat,
  start_date,
  duration,
  years = NULL,
  mode = "pairwise",
  location_names = NULL,
  source = c("power", "era5")
) {
  # Validate inputs
  .validate_required_args(lonlat, start_date, duration)
  mode <- rlang::arg_match0(mode, c("pairwise", "cross"))
  source <- rlang::arg_match(source)

  # Normalize locations
  locations <- .norm_locations(lonlat)
  location_names <- .resolve_location_names(locations, location_names)

  # Process start dates
  sd_char <- .process_start_dates(start_date, years)

  # Build tasks: mode controls location/date pairing in candidate set
  tasks <- .build_tasks(
    locations,
    location_names,
    sd_char,
    duration,
    years,
    mode
  )

  # Collapse tasks by location for efficient API calls
  tasks <- .collapse_tasks_by_location(tasks)

  # Fetch weather data
  results <- lapply(tasks, .get_one_weather, source = source)

  return(.format_results(results, tasks))
}

#' Validate Required Arguments for Weather Fetching
#'
#' Checks that `lonlat`, `start_date`, and `duration` are provided and aborts
#' with helpful error messages if any are missing. Called early in
#' [get_epicrop_weather_list()] to fail fast.
#'
#' @param lonlat Numeric coordinates or matrix of coordinates (required).
#' @param start_date A date string or vector of dates (required).
#' @param duration Integer or vector of integers specifying days (required).
#'
#' @returns NULL (invisibly). Called for side effects (validation/abort).
#'
#' @examplesIf interactive()
#' # This would trigger validation (not normally called directly)
#' .validate_required_args(c(121.255669, 14.16742), "2000-06-30", 120L)
#'
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
      i = "Examples: {.code \"2000-06-30\"} or {.code c(\"-06-01\", \"-06-14\")}"
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
#' or list of pairs) to a standard 2-column numeric matrix with columns LON and
#' LAT. Preserves row names where available and validates all coordinates are
#' finite.
#'
#' @section Input Processing:
#' 1. **List** (non-data.frame): Validate elements, extract names, convert to matrix
#' 2. **Data frame**: Validate 2 columns and numeric type, convert to matrix
#' 3. **Matrix**: Validate 2 columns and numeric type, set column names
#' 4. **Numeric vector**: Validate even length, reshape to 2 columns
#'
#' @section Validation:
#' - All coordinates must be finite (no NA, Inf, or NaN)
#' - Matrix must have exactly 2 columns
#' - Numeric vector must have even length (length >= 2)
#' - List elements must be numeric vectors of length 2 with finite values
#'
#' @param ll Location coordinates in one of the following formats:
#'   * Numeric vector of length 2: `c(lon, lat)` for a single location
#'   * Numeric vector of length 2n: `c(lon1, lat1, lon2, lat2, ...)` for
#'     n locations
#'   * 2-column matrix or data frame: each row is `c(lon, lat)`
#'   * List of numeric vectors: each element is `c(lon, lat)`. If named,
#'     names are preserved as row names.
#'
#' @returns
#' A numeric 2-column matrix with:
#'   * Column names: `"LON"`, `"LAT"`
#'   * Row names preserved from input if available
#'   * Storage mode: `"double"`
#'   * All values finite and valid coordinates
#'
#' @examplesIf interactive()
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
#' Validates and converts a list of coordinate pairs to a matrix. Each element
#' must be a numeric vector of length 2 with finite values.
#'
#' @param ll A list where each element is a numeric vector of length 2.
#'
#' @returns A numeric matrix with row names preserved if available.
#'
#' @details
#' Errors if any element is not a length-2 numeric vector with finite values.
#' Row names are preserved if the list is named and all names are non-empty.
#'
#' @examplesIf interactive()
#' .process_location_list(list(
#'   IRRI = c(121.255669, 14.16742),
#'   Manila = c(120.985, 14.6042)
#' ))
#'
#' @autoglobal
#' @dev
.process_location_list <- function(ll) {
  valid <- vapply(
    ll,
    function(x) is.numeric(x) && length(x) == 2L && all(is.finite(x)),
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
#' Validates and converts a data frame to a location matrix. Ensures exactly 2
#' numeric columns (longitude, latitude).
#'
#' @param ll A data frame with exactly 2 numeric columns.
#'
#' @returns A numeric matrix with row names (if any) preserved.
#'
#' @examplesIf interactive()
#' locations <- data.frame(
#'   lon = c(121.255669, 120.985),
#'   lat = c(14.16742, 14.6042)
#' )
#' .process_location_dataframe(locations)
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
#' Validates a matrix of coordinate pairs. Checks for exactly 2 columns, numeric
#' storage, and finite coordinate values.
#'
#' @param ll A numeric matrix with exactly 2 columns.
#'
#' @returns The validated matrix unchanged.
#'
#' @examplesIf interactive()
#' locations <- rbind(c(121.255669, 14.16742), c(120.985, 14.6042))
#' .process_location_matrix(locations)
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
#' Validates and reshapes a numeric vector of coordinate pairs into a matrix.
#' Vector must have even length (lon1, lat1, lon2, lat2, ...).
#'
#' @param ll A numeric vector of even length with finite values.
#'
#' @returns A 2-column matrix with one coordinate pair per row.
#'
#' @examplesIf interactive()
#' # Single location
#' .process_location_vector(c(121.255669, 14.16742))
#'
#' # Multiple locations
#' .process_location_vector(c(121.255669, 14.16742, 120.985, 14.6042))
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
#' Provides a consistent error message for unsupported location input types.
#'
#' @param ll The invalid input object.
#'
#' @returns Does not return; aborts with error message.
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

#' Resolve Location Names from Matrix and Inputs
#'
#' Determines appropriate names for locations by checking user-provided names,
#' rownames from the location matrix, or generating default names.
#'
#' @section Priority:
#' 1. User-provided `location_names` (must match number of locations)
#' 2. Non-empty rownames from `locations` matrix
#' 3. Auto-generated names: `location01`, `location02`, ...
#'
#' @param locations A numeric 2-column matrix (LON, LAT).
#' @param location_names Optional character vector of location names.
#'
#' @returns
#' A character vector of location names with length equal to the number of rows
#' in `locations`.
#'
#' @examplesIf interactive()
#' locations <- rbind(
#'   "IRRI_ZES" = c(121.255669, 14.16742),
#'   "Metro_Manila" = c(120.985, 14.6042)
#' )
#'
#' # Uses rownames by default
#' .resolve_location_names(locations, NULL)
#'
#' # Overrides with user names
#' .resolve_location_names(locations, c("Site A", "Site B"))
#'
#' @autoglobal
#' @dev
.resolve_location_names <- function(locations, location_names) {
  n_locations <- nrow(locations)

  if (!is.null(location_names)) {
    if (length(location_names) != n_locations) {
      cli::cli_abort(c(
        "{.arg location_names} length must equal number of locations.",
        x = "You provided {length(location_names)} name{?s} for {n_locations} location{?s}."
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

#' Detect Month-Day Only Date Strings
#'
#' Identifies date strings that contain only month and day without a year, in
#' formats like `"06-30"`, `"-06-30"`, or `"6/30"`.
#'
#' @section Patterns Matched:
#' - `"06-30"` or `"-06-30"` (with optional leading dash)
#' - `"6-30"` or `"-6-30"` (single-digit month/day)
#' - Regex: `^-?\\d{1,2}[-/]\\d{1,2}$`
#' - Excludes full ISO dates (`YYYY-MM-DD`)
#'
#' @param x_chr Character vector of date strings.
#'
#' @returns Logical vector indicating which elements are month-day only.
#'
#' @examplesIf interactive()
#' .is_md_only(c("06-30", "2000-06-30", "6/30", "-06-30"))
#' # [1]  TRUE FALSE  TRUE  TRUE
#'
#' @autoglobal
#' @dev
.is_md_only <- function(x_chr) {
  grepl("^-?\\d{1,2}[-/]\\d{1,2}$", x_chr) &
    !grepl("^\\d{4}-\\d{2}-\\d{2}$", x_chr)
}

#' Normalize Month-Day Strings to Standard MM-DD Format
#'
#' Converts month-day strings in various formats (`"6/30"`, `"-06-30"`, etc.) to
#' a consistent `"MM-DD"` format with zero-padded digits.
#'
#' @section Transformations:
#' - Slashes or dashes as separators (normalized to dashes)
#' - Optional leading dash (removed)
#' - Single or double-digit month/day (zero-padded)
#'
#' @param x_chr Character vector of month-day strings (with optional leading
#'   dash or slash separator).
#'
#' @returns Character vector in `"MM-DD"` format (*e.g.*, `"06-30"`).
#'
#' @examplesIf interactive()
#' .normalize_md(c("6/30", "-6-30", "06-30"))
#' # [1] "06-30" "06-30" "06-30"
#'
#' @autoglobal
#' @dev
.normalize_md <- function(x_chr) {
  # Remove leading dash if present, normalize separators
  cleaned <- sub("^-", "", x_chr)
  cleaned <- gsub("/", "-", cleaned, fixed = TRUE)

  # Split on dash and pad with zeros
  vapply(
    strsplit(cleaned, "-", fixed = TRUE),
    function(p) sprintf("%02d-%02d", as.integer(p[1L]), as.integer(p[2L])),
    character(1L),
    USE.NAMES = FALSE
  )
}

#' Process Start Dates to Character Format
#'
#' Converts start dates to character format and normalizes month-day strings.
#' Month-day dates require the `years` parameter to construct complete date
#' ranges.
#'
#' @param start_date Character, `Date`, or `IDate` vector of start dates.
#' @param years Optional integer vector of years. Required if `start_date`
#'   contains month-day values.
#'
#' @returns
#' Character vector of start dates in `"YYYY-MM-DD"` or `"MM-DD"` format.
#'
#' @details
#' Month-day strings are normalized to `"MM-DD"` format. Full dates are
#' converted to character strings. If month-day values are found but `years` is
#' NULL, an error is raised.
#'
#' @examplesIf interactive()
#' .process_start_dates(c("2000-06-30", "-06-30"), c(2000, 2001))
#'
#' @autoglobal
#' @dev
.process_start_dates <- function(start_date, years) {
  sd_char <- as.character(start_date)
  md_idx <- .is_md_only(sd_char)

  if (any(md_idx) && is.null(years)) {
    cli::cli_abort(c(
      "When {.arg start_date} is month-day only (e.g., '06-30')
       you must provide {.arg years}.",
      i = "Example: {.code years = 2001:2003}"
    ))
  }

  if (any(md_idx)) {
    sd_char[md_idx] <- .normalize_md(sd_char[md_idx])
  }

  sd_char
}

#' Build Task List for Weather Fetching
#'
#' Constructs a list of (location, start_date, duration) tasks based on input
#' locations, dates, and pairing mode.
#'
#' @section Pairing Modes:
#' - **pairwise** (default): Recycles locations and dates by position.
#'   - `locations[1]` × `dates[1]`, `locations[2]` × `dates[2]`, etc.
#'   - Excess elements are recycled.
#' - **cross**: Forms all combinations.
#'   - Every location × every date combination.
#'
#' @param locations A numeric 2-column matrix (LON, LAT).
#' @param location_names Character vector of location names.
#' @param sd_char Character vector of start dates.
#' @param duration Integer or vector of durations.
#' @param years Optional integer vector of years.
#' @param mode Pairing mode: `"pairwise"` or `"cross"`.
#'
#' @returns
#' A list of task objects (lists) with elements:
#'   * `location_id`: integer index
#'   * `location_name`: character name
#'   * `lonlat`: numeric vector c(lon, lat)
#'   * `start_chr`: character start date
#'   * `duration`: integer duration (days)
#'
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
    # cross mode
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
#'
#' Constructs a single task object with resolved date and duration information.
#'
#' @param locations A numeric 2-column matrix (LON, LAT).
#' @param location_names Character vector of location names.
#' @param loc_idx Integer location index.
#' @param sd_char Character vector of start dates.
#' @param date_idx Integer date index.
#' @param dur Integer duration (days).
#' @param years Optional integer vector of years.
#'
#' @returns
#' A list with task information:
#'   * `location_id`: integer index
#'   * `location_name`: character name
#'   * `lonlat`: numeric vector c(lon, lat)
#'   * `start_chr`: character start date
#'   * `duration`: integer duration
#'
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
#'
#' Converts date strings to a standard start date character and duration.
#' Handles both full ISO dates and month-day strings with year ranges.
#'
#' @param d_str Character start date (ISO format or month-day only).
#' @param dur Integer duration (days).
#' @param years Optional integer vector of years.
#'
#' @returns
#' A list with:
#'   * `start_chr`: character start date in `"YYYY-MM-DD"` format
#'   * `duration`: integer duration (days)
#'
#' @details
#' If `d_str` is in `"MM-DD"` format, constructs a cover window using `years`.
#' Otherwise, parses as ISO date using [data.table::as.IDate()].
#'
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
#'
#' Constructs a date range that covers all occurrences of a given month-day
#' across a range of years, plus the specified duration.
#'
#' @section Logic:
#' 1. Paste year with month-day string to create dates for each year
#' 2. Find earliest start date: `min(all_dates)`
#' 3. Find latest end date: `max(all_dates) + (duration - 1)`
#' 4. Return: `list(start_chr, duration_to_cover_window)`
#'
#' @param md_mmdd Character month-day in `"MM-DD"` format.
#' @param yrs Integer vector of years.
#' @param window_days Integer duration (days) from each start date.
#'
#' @returns
#' A list with:
#'   * `start_chr`: character earliest start date in `"YYYY-MM-DD"` format
#'   * `duration`: integer days needed to cover all years and duration
#'
#' @details
#' This is used to efficiently fetch weather in a single API call that covers
#' multiple year windows. For example, fetching "06-01 to 08-30" for years
#' 2000:2002 requires one call from 2000-06-01 to 2002-08-30.
#'
#' @examplesIf interactive()
#' .cover_from_md_years("06-30", 2000:2001, 90L)
#'
#' @autoglobal
#' @dev
.cover_from_md_years <- function(md_mmdd, yrs, window_days) {
  if (is.null(yrs) || length(yrs) == 0L) {
    cli::cli_abort(c(
      "When {.arg start_date} is month-day only,
       you must provide {.arg years}.",
      i = "Example: {.code years = 2001:2003}"
    ))
  }

  em <- tryCatch(
    data.table::as.IDate(sprintf("%04d-%s", as.integer(yrs), md_mmdd)),
    error = function(e) data.table::as.IDate(NA)
  )

  if (anyNA(em)) {
    cli::cli_abort(c(
      "Failed to construct valid dates from 
       {.arg years} and month-day {.val {md_mmdd}}.",
      i = "Check that month-day values are valid (e.g., not 02-30)."
    ))
  }

  start <- min(em)
  end <- max(em) + (as.integer(window_days) - 1L)

  list(start_chr = as.character(start), duration = as.integer(end - start + 1L))
}

#' Collapse Multiple Tasks for One Location
#'
#' Merges multiple tasks (start_date/duration combinations) for a single
#' location into one consolidated task with the minimum start date and maximum
#' end date. This ensures efficient API calls.
#'
#' @param tsks A list of task objects for the same location.
#'
#' @returns
#' A single task list with:
#'   * `location_id`: unchanged
#'   * `location_name`: unchanged
#'   * `lonlat`: unchanged
#'   * `start_chr`: earliest start date
#'   * `duration`: days to cover latest end date
#'
#' @details
#' Used to combine multiple requested date ranges into one API fetch. Reduces
#' redundant requests for the same location.
#'
#' @examplesIf interactive()
#' # Multiple tasks for same location across years
#' tasks <- list(
#'   list(
#'     location_id = 1,
#'     location_name = "Site1",
#'     lonlat = c(0, 0),
#'     start_chr = "2000-06-01",
#'     duration = 90L
#'   ),
#'   list(
#'     location_id = 1,
#'     location_name = "Site1",
#'     lonlat = c(0, 0),
#'     start_chr = "2001-06-01",
#'     duration = 90L
#'   )
#' )
#' .collapse_one_location(tasks)
#'
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
#'
#' Groups tasks by location and collapses each group into a single task. This
#' minimizes the number of API calls required to fetch all data.
#'
#' @param tasks A list of task objects (from [.build_tasks()]).
#'
#' @returns A list of collapsed tasks, one per unique location.
#'
#' @details
#' By combining all date ranges for a single location, we can fetch the entire
#' date span in one API call, then extract individual results later. This is
#' especially efficient when fetching multiple years or multiple start dates for
#' the same location.
#'
#' @autoglobal
#' @dev
.collapse_tasks_by_location <- function(tasks) {
  by_location <- split(
    tasks,
    vapply(tasks, `[[`, integer(1L), "location_id", USE.NAMES = FALSE)
  )
  unname(lapply(by_location, .collapse_one_location))
}

#' Get Weather Data for One Task
#'
#' Calls the API to fetch weather data for a single (location, start, duration)
#' task and splits the result by calendar year into a named list.
#'
#' @section Return Structure:
#' The weather table is split by `data.table::year(YYYYMMDD)` into a list. Each
#' list element is a table for one calendar year, named by the year. The result
#' is classed as `epicrop.wth.list`.
#'
#' @param task A task object (from [.create_single_task()]) with:
#'   * `lonlat`: numeric vector c(lon, lat)
#'   * `start_chr`: character start date
#'   * `duration`: integer duration
#   #' @param source Character scalar passed to [get_wth()]: `"power"` (default)
#'   or `"era5"`.'
#' @returns
#' An `epicrop.wth.list` object: a named list of weather tables, one per year.
#' Names are year strings (*e.g.*, "2000", "2001").
#'
#' @details
#' This function calls [get_wth()] to fetch data, then splits by year. It is
#' called internally by [get_epicrop_weather_list()].
#'
#' @autoglobal
#' @dev
.get_one_weather <- function(task, source = "power") {
  tab <- get_wth(
    lonlat = task$lonlat,
    dates = task$start_chr,
    duration = task$duration,
    source = source
  )

  out <- split(tab, data.table::year(tab$YYYYMMDD), keep.by = FALSE)

  class(out) <- c("epicrop.wth.list", "list")
  out
}

#' Format Results for Return
#'
#' Prepares final results in the appropriate class and structure.
#'
#' @section Output Structure:
#' - If only one (location, start, duration) combination:
#'   returns the `epicrop.wth.list` directly.
#' - If multiple combinations:
#'   returns an `epicrop.wth.bundle` (a named list of `epicrop.wth.list`)
#'   with keys like `"location01__2000-01-01__365d"`.
#'
#' @param results A list of `epicrop.wth.list` objects (from
#'   [.get_one_weather()]).
#' @param tasks A list of task objects (from [.collapse_tasks_by_location()]).
#'
#' @returns Either an `epicrop.wth.list` or an `epicrop.wth.bundle` object.
#'
#' @autoglobal
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
