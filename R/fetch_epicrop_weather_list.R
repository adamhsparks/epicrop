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
#' * Ifhhhly one (location, start, duration) is requested: an
#'   `epicrop.wth.list`.
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
  mode <- rlang::arg_match0(mode, c("pairwise", "cross"))

  norm_locations <- function(ll) {
    if (is.list(ll)) {
      ll <- data.table::rbindlist(ll) # FIXED: was "datat.table"
    }
    if (is.data.frame(ll)) {
      ll <- as.matrix(ll)
    }
    if (is.matrix(ll)) {
      if (ncol(ll) != 2L) {
        cli::cli_abort("{.arg lonlat} must be 2 columns: lon, lat.")
      }
      storage.mode(ll) <- "double"
      colnames(ll) <- c("LON", "LAT")
      return(ll)
    }
    if (is.numeric(ll) && length(ll) %% 2L == 0L && length(ll) > 2L) {
      ll <- matrix(ll, ncol = 2L, byrow = TRUE)
    } else if (is.numeric(ll) && length(ll) == 2L) {
      ll <- matrix(ll, ncol = 2L)
    } else {
      cli::cli_abort(
        "{.arg lonlat} must be c(lon, lat), a 2-column matrix/data.frame, or a
        list of length-2 numerics."
      )
    }
    storage.mode(ll) <- "double"
    colnames(ll) <- c("LON", "LAT")
    ll
  }

  is_md_only_vec <- function(x_chr) {
    !grepl("^\\d{4}-\\d{2}-\\d{2}$", x_chr) &
      grepl("^\\-?\\d{1,2}[-/]\\d{1,2}$", x_chr)
  }

  normalize_md_vec <- function(x_chr) {
    vapply(
      strsplit(gsub("^\\-?", "", gsub("/", "-", x_chr)), "-", fixed = TRUE),
      function(p) sprintf("%02d-%02d", as.integer(p[1]), as.integer(p[2])),
      character(1)
    )
  }

  cover_from_md_years_cached <- local({
    cache <- new.env(parent = emptyenv())
    function(md_mmdd, yrs, window_days) {
      key <- paste(
        md_mmdd,
        paste0(yrs, collapse = ","),
        as.integer(window_days),
        sep = "|"
      )
      if (!exists(key, envir = cache, inherits = FALSE)) {
        if (is.null(yrs) || length(yrs) == 0L) {
          cli::cli_abort(
            "When {.arg start_date} is month-day only, you must provide
            {.arg years}.",
            .envir = parent.frame()
          )
        }
        # Use data.table::as.IDate for date construction
        em <- data.table::as.IDate(sprintf("%04d-%s", as.integer(yrs), md_mmdd))
        if (anyNA(em)) {
          cli::cli_abort(
            "Failed to construct emergence dates from {.arg years} and month-day
            {.val {md_mmdd}}.",
            .envir = parent.frame()
          )
        }
        start <- min(em)
        end <- max(em) + (as.integer(window_days) - 1L)
        assign(
          key,
          list(start = start, duration = as.integer(end - start + 1L)),
          envir = cache
        )
      }
      get(key, envir = cache, inherits = FALSE)
    }
  })

  locations <- norm_locations(lonlat)
  n_locations <- nrow(locations)

  if (!missing(location_names) && !is.null(location_names)) {
    if (length(location_names) != n_locations) {
      cli::cli_abort(
        "{.arg location_names} length must equal number of locations
        ({n_locations})."
      )
    }
  } else {
    if (!is.null(rownames(locations)) && all(nzchar(rownames(locations)))) {
      location_names <- rownames(locations)
    } else {
      location_names <- sprintf("location%02d", seq_len(n_locations))
    }
  }

  if (missing(start_date)) {
    cli::cli_abort("Please supply {.arg start_date}.", .envir = parent.frame())
  }
  if (missing(duration)) {
    cli::cli_abort(
      "Please supply {.arg duration} (inclusive days).",
      .envir = parent.frame()
    )
  }

  sd_char <- as.character(start_date)
  md_idx <- is_md_only_vec(sd_char)
  if (any(md_idx)) {
    if (is.null(years)) {
      cli::cli_abort(
        "When {.arg start_date} is month-day only (e.g., '06-30'), you must
        provide {.arg years}.",
        .envir = parent.frame()
      )
    }
    sd_char[md_idx] <- normalize_md_vec(sd_char[md_idx])
  }

  # Always build tasks for all location × date combinations
  date_vec <- sd_char
  location_idx <- rep(seq_len(n_locations), each = length(date_vec))
  date_idx <- rep(seq_along(date_vec), times = n_locations)

  dur_vec <- rep_len(as.integer(duration), length.out = length(date_idx))
  if (any(!rlang::is_integerish(dur_vec, finite = TRUE) | dur_vec <= 0L)) {
    cli::cli_abort("{.arg duration} must contain positive integers (days).")
  }

  tasks <- vector("list", length(date_idx))
  for (k in seq_along(tasks)) {
    i <- location_idx[k]
    j <- date_idx[k]
    lon <- locations[i, "LON"]
    lat <- locations[i, "LAT"]
    d_str <- sd_char[j]
    dur <- dur_vec[k]

    if (grepl("^\\d{2}-\\d{2}$", d_str)) {
      cov <- cover_from_md_years_cached(d_str, years, window_days = dur)
      start_idate <- cov$start
      dur_use <- cov$duration
      start_chr <- as.character(start_idate)
    } else {
      start_idate <- data.table::as.IDate(d_str)
      if (is.na(start_idate)) {
        cli::cli_abort(
          "Cannot coerce {.arg start_date} value {d_str} to a valid date
          (YYYY-MM-DD).",
          .envir = parent.frame()
        )
      }
      dur_use <- as.integer(dur)
      start_chr <- as.character(start_idate)
    }

    tasks[[k]] <- list(
      location_id = i,
      location_name = location_names[i],
      lonlat = c(lon, lat),
      start_chr = start_chr,
      duration = dur_use
    )
  }

  # Group tasks by location
  by_location <- split(tasks, vapply(tasks, `[[`, integer(1L), "location_id"))

  # Collapse overlapping/adjacent date ranges per location
  # This is the KEY function that makes API calls efficient!
  collapse_one <- function(tsks) {
    # Use data.table::as.IDate for all date operations
    starts <- data.table::as.IDate(vapply(
      tsks,
      `[[`,
      character(1L),
      "start_chr"
    ))
    ends <- starts +
      (vapply(tsks, function(t) as.integer(t$duration), integer(1L)) - 1L)

    # Find the earliest start and latest end
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

  tasks <- unname(lapply(by_location, collapse_one))

  run_one <- function(task) {
    tab <- get_wth(
      lonlat = task$lonlat,
      dates = task$start_chr,
      duration = task$duration
    )

    # Split by calendar year; ensure 'YYYYMMDD' exists and is IDate
    yrs <- sort(unique(format(tab[["YYYYMMDD"]], "%Y")))
    out <- lapply(yrs, function(y) {
      s <- data.table::as.IDate(sprintf("%s-01-01", y))
      e <- data.table::as.IDate(sprintf("%s-12-31", y))
      tab[YYYYMMDD %between% c(s, e)]
    })
    names(out) <- yrs
    class(out) <- c("epicrop.wth.list", "list")
    out
  }

  results <- lapply(tasks, run_one)

  make_key <- function(tk) {
    sprintf(
      "%s__%s__%dd",
      tk$location_name,
      tk$start_chr,
      as.integer(tk$duration)
    )
  }
  keys <- vapply(tasks, make_key, character(1))

  if (length(results) == 1L) {
    return(results[[1L]])
  }

  names(results) <- keys
  class(results) <- c("epicrop.wth.bundle", "epicrop.wth.list", "list")
  results
}
