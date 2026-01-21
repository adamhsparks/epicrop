#' Fetch weather once (single or multiple sites) and split by calendar year
#'
#' Retrieve one or more continuous blocks of weather for one or more locations
#' from given start dates and durations, then split each result into a named
#' list of per‑year tables. Each (site, start, duration) result is an
#' `epicrop.wth.list`. The top-level return is an `epicrop.wth.bundle` when
#' multiple requests are processed, otherwise a single `epicrop.wth.list`.
#'
#' ## Flexible inputs
#' * `lonlat`: a single `c(lon, lat)`; or multiple sites as a numeric vector of
#'   length `2n`, or a 2‑column matrix/data.frame, or a list of `c(lon, lat)`.
#' * `start_date`: a single `"YYYY-MM-DD"` (or `Date`/`IDate`), or a vector of
#'   the same; or a month–day string like `"06-30"` or `"6-30"` (leading `"-"`
#'   optional) provided together with `years`.
#' * `duration`: single integer or a vector (recycled); interpreted as inclusive
#'   days to fetch (`end = start + duration - 1`).
#' * `years`: only used when `start_date` lacks a year. We compute a **cover
#'   window** per distinct month–day: `start = min(paste(years, md))`,
#'   `end = max(paste(years, md)) + (duration - 1)`.
#'
#' @param lonlat Numeric length‑2 vector `c(lon, lat)`, OR multiple sites as:
#'   numeric vector of length `2n`, a 2‑column matrix/data.frame, or a list of
#'   numeric length‑2 vectors.
#' @param start_date A starting date (character `"YYYY-MM-DD"`, [Date], or
#'   [data.table::IDate]) **or** a month–day string like `"06-30"` (leading
#'   `"-"` optional) used together with `years`.
#' @param duration Integer(s). Number of days to retrieve **from** `start_date`
#'   (inclusive). Single value or vector (recycled).
#' @param years Optional integer vector of years. **Required** if `start_date`
#'   contains any month–day values without a year.
#' @param mode Character scalar: `"pairwise"` (default) pairs sites and dates by
#'   position (recycling length-1), or `"cross"` forms all combinations
#'   (sites × dates).
#' @param site_names Optional character names for sites (length = number of
#'   unique sites).
#'
#' @return
#' * If only one (site, start, duration) is requested: an `epicrop.wth.list`.
#' * Otherwise: an `epicrop.wth.bundle` (a named list of `epicrop.wth.list`).
#'
#' @examplesIf interactive()
#' # Single site, month-day start date (any of "6/30", "-06-30", "06-30")
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
#' # Multiple sites × multiple dates (cross mode), mixed inputs
#' sites <- rbind(
#'   c(121.255669, 14.16742),
#'   c(120.985,    14.6042)
#' )
#' w3 <- fetch_epicrop_weather_list(
#'   lonlat     = sites,
#'   start_date = c("2000-06-30", "6-30"),
#'   duration   = 120L,
#'   years      = 2000:2001,
#'   mode       = "cross",
#'   site_names = c("IRRI_ZES", "MetroManila")
#' )
#'
#' @export
fetch_epicrop_weather_list <- function(
  lonlat,
  start_date,
  duration,
  years = NULL,
  mode = c("pairwise", "cross"),
  site_names = NULL
) {
  mode <- match.arg(mode)

  # -- Helpers (local, not exported) ------------------------------------------
  norm_sites <- function(ll) {
    if (is.list(ll)) {
      ll <- do.call(rbind, ll)
    }
    if (is.numeric(ll) && length(ll) %% 2L == 0L && length(ll) > 2L) {
      ll <- matrix(ll, ncol = 2L, byrow = TRUE)
    }
    if (is.numeric(ll) && length(ll) == 2L) {
      ll <- matrix(ll, ncol = 2L)
    }
    if (is.data.frame(ll)) {
      ll <- as.matrix(ll)
    }
    if (!is.matrix(ll) || ncol(ll) != 2L) {
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
    # TRUE for elements that are month-day like: "6-30", "-06-30", "06/30"
    !grepl("^\\d{4}-\\d{2}-\\d{2}$", x_chr) &
      grepl("^\\-?\\d{1,2}[-/]\\d{1,2}$", x_chr)
  }

  normalize_md_vec <- function(x_chr) {
    # Normalize to "MM-DD" element-wise
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

  sites <- norm_sites(lonlat)
  n_sites <- nrow(sites)

  if (!missing(site_names) && !is.null(site_names)) {
    if (length(site_names) != n_sites) {
      cli::cli_abort(
        "{.arg site_names} must have length equal to number of sites
        ({n_sites})."
      )
    }
  } else {
    site_names <- sprintf("site%02d", seq_len(n_sites))
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

  # Coerce start_date to character for pattern analysis; Dates/IDates pass through
  sd_char <- as.character(start_date)
  md_idx <- is_md_only_vec(sd_char)

  # If ANY month-day present, normalize those elements; enforce years present
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
  if (mode == "cross") {
    date_vec <- sd_char
    idx <- expand.grid(
      site = seq_len(n_sites),
      date = seq_along(date_vec),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    site_idx <- idx$site
    date_idx <- idx$date
  } else {
    site_idx <- seq_len(n_sites)
    date_idx <- rep_len(seq_along(sd_char), length.out = n_sites)
  }

  # Recycle duration to align with date_idx (then with tasks)
  dur_vec <- rep_len(as.integer(duration), length.out = length(date_idx))
  if (any(!rlang::is_integerish(dur_vec, finite = TRUE) | dur_vec <= 0L)) {
    cli::cli_abort("{.arg duration} must contain positive integers (days).")
  }

  tasks <- vector("list", length(date_idx))

  for (k in seq_along(tasks)) {
    i <- site_idx[k]
    j <- date_idx[k]
    lon <- sites[i, 1]
    lat <- sites[i, 2]
    d_str <- sd_char[j]
    dur <- dur_vec[k]

    # Path A: month-day only (now in "MM-DD")
    if (grepl("^\\d{2}-\\d{2}$", d_str)) {
      cov <- cover_from_md_years_cached(d_str, years, window_days = dur)
      start_idate <- cov$start
      dur_use <- cov$duration
      start_chr <- as.character(start_idate)

      # Path B: full ISO or coercible Date string ("YYYY-MM-DD")
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
      site_id = i,
      site_name = site_names[i],
      lonlat = c(lon, lat),
      start_chr = start_chr,
      duration = dur_use
    )
  }

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
    sprintf("%s__%s__%dd", tk$site_name, tk$start_chr, as.integer(tk$duration))
  }
  keys <- vapply(tasks, make_key, character(1))

  # If only one element, return it directly for backward-compatibility
  if (length(results) == 1L) {
    return(results[[1L]])
  }

  names(results) <- keys
  class(results) <- c("epicrop.wth.bundle", "epicrop.wth.list", "list")
  results
}

#' Compute a cover window from emergence dates and window length
#'
#' Given a set of emergence dates and a fixed window length (days after
#' emergence, inclusive), compute a single continuous **cover window** that
#' spans all seasonal runs. The cover window is defined as:
#'
#' \preformatted{
#' start_date = min(emergence_dates)
#' end_date   = max(emergence_dates) + (window_days - 1)
#' duration   = as.integer(end_date - start_date + 1)
#' }
#'
#' This helper is designed to be used with
#' `fetch_epicrop_weather_list(lonlat, start_date, duration)`, so you fetch
#' **once** for a single site and then let [run_epicrop_model()] slice within
#' and across years as needed.
#'
#' @param emergence_dates A vector of dates (character `"YYYY-MM-DD"`, [Date],
#'   [data.table::IDate]), or an object of class `epicrop.emerge` (as returned
#'   by [build_epicrop_emergence()]).
#' @param window_days Integer. Number of days after each emergence to include
#'   for modelling (inclusive).
#'
#' @return A named list with components:
#' \itemize{
#'   \item `start_date` ([data.table::IDate]): the earliest emergence date.
#'   \item `end_date`   ([data.table::IDate]): the latest emergence date plus
#'         `window_days - 1`.
#'   \item `duration`   (integer): `end_date - start_date + 1` (days).
#' }
#'
#' @seealso [build_epicrop_emergence()], [fetch_epicrop_weather_list()],
#'   [run_epicrop_model()]
#'
#' @examplesIf interactive()
#' # Example: multiple emergences across years; compute a single cover window
#' years <- 2001:2003
#' month_day <- c("-06-01", "-06-14", "-06-30")
#' window_days <- 120L
#'
#' em <- build_epicrop_emergence(years, month_day)
#' cov <- compute_cover_window(em, window_days)
#' cov$start_date  # IDate
#' cov$end_date    # IDate
#' cov$duration    # integer
#'
#' # Fetch once using the cover window and run a model
#' # (Assumes get_wth() is available and returns `YYYYMMDD` as IDate.)
#' lonlat <- c(121.255669, 14.16742)
#' wth_list <- fetch_epicrop_weather_list(
#'   lonlat     = lonlat,
#'   start_date = cov$start_date,
#'   duration   = cov$duration
#' )
#'
#' @autoglobal
#' @export
compute_cover_window <- function(emergence_dates, window_days) {
  if (
    !rlang::is_integerish(window_days, n = 1, finite = TRUE) || window_days <= 0
  ) {
    cli::cli_abort(
      "{.arg window_days} must be a single positive integer.",
      .envir = parent.frame()
    )
  }
  window_days <- as.integer(window_days)

  if (!rlang::inherits_any(emergence_dates, "epicrop.emerge")) {
    emergence_dates <- data.table::as.IDate(emergence_dates)
    if (anyNA(emergence_dates)) {
      cli::cli_abort(
        "Failed to coerce {.arg emergence_dates} to {.cls IDate}.",
        .envir = parent.frame()
      )
    }
  }
  if (length(emergence_dates) == 0L) {
    cli::cli_abort(
      "{.arg emergence_dates} cannot be empty.",
      .envir = parent.frame()
    )
  }

  start_date <- min(emergence_dates)
  end_date <- max(emergence_dates) + (window_days - 1L)
  duration <- as.integer(end_date - start_date + 1L)

  return(list(
    start_date = start_date,
    end_date = end_date,
    duration = duration
  ))
}
