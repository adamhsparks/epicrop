#' Run an epicrop disease model across emergence dates (sequential or futureverse)
#'
#' A unified helper that executes any \pkg{epicrop} model (*e.g.*,
#' [bacterial_blight()], [brown_spot()], [leaf_blast()], [sheath_blight()],
#' [tungro()], [modified_kim_leaf_blast()], [modified_kim_sheath_blight()],
#' [leaf_rust()], or [s_tritici_blotch()]) over a set of emergence dates, using
#' a list of per-year weather `data.table`s. Handles within-year and cross-year
#' windows without globally binding weather.
#'
#' @param model_fun Function. An \pkg{epicrop} model function (*e.g.*,
#'    [bacterial_blight()]).
#' @param emergence_dates Vector of dates (character "YYYY-MM-DD", Date, or
#'    IDate).
#' @param wth_list Named list of weather [data.table::data.table]s with one per
#'   year;
#'   `names(wth_list)` must be year strings (e.g., "2001"). **Each table must
#'   have a keyed `date` column** (IDate/Date) as emitted by [get_wth()].
#' @param window_days Integer. Days after emergence to include. The slicer
#'    returns exactly `window_days` rows when data are available.  This is
#'    necessary when the model run spans multiple years, otherwise it just
#'    passes through.
#' @param mode Character. "sequential" for sequential (default) or "future" for
#'   parallel.
#' @param cores Integer. Number of workers when `mode = "future"`.
#'   Default: `max(1L, parallel::detectCores() - 1L)`.
#' @param chunk Either "auto", "none", or a positive integer chunk size
#'   for batching emergence dates per future. "auto" ~ 4 chunks/worker.
#' @param output Character. "audpc" (compact summary using [get_audpc()])
#'   or "full" (return full model outputs per date, with `emergence` added).
#' @param strict Logical. If TRUE (default), abort via [cli::cli_abort()] when
#'   required year weather is missing; if FALSE, skip that date with
#'   [cli::cli_warn()].
#' @param dt_threads Integer or NULL. \CRANpkg{data.table} threads per process.
#'   In parallel, this is set **inside each worker** (default 1). In sequential,
#'   it is set in the main process if provided.
#' @param seed `NULL`` or integer. When supplied, makes results **fully
#'   reproducible** across sequential and future modes. In future mode, uses
#'   L'Ecuyer-CMRG with a deterministic per-emergence seed. Default: `NULL` (no
#'   seeding).
#' @param ... Additional arguments forwarded to `model_fun`.
#'
#' @return A [data.table::data.table]. When `output = "audpc"`:
#'   columns `emergence` (IDate) and `AUDPC` (numeric).
#'   When `output = "full"`: full model output columns plus `emergence`, and
#'   an `AUDPC` column preserved per emergence date.
#'
#' @examplesIf interactive()
#' years <- 2001:2003
#' month_day <- c("-06-01", "-06-14", "-06-30")
#' lonlat <- c(121.255669, 14.16742)
#'
#' emergence <- build_epicrop_emergence(years, month_day)
#'
#' # select 200 days duration to allow for multiple seasonal runs on same data
#' wth_list <- fetch_epicrop_weather_list(years,
#'                                        lonlat,
#'                                        start_md = month_day[1],
#'                                        duration = 200L)
#'
#' # Sequential AUDPC for bacterial blight
#' run_epicrop_model(bacterial_blight,
#'                   emergence,
#'                   wth_list,
#'                   window_days = 120L,
#'                   output = "audpc")
#'
#' # Parallel (futureverse) AUDPC for BB on 4 workers
#' run_epicrop_model(bacterial_blight,
#'                   emergence,
#'                   wth_list,
#'                   window_days = 120L,
#'                   mode = "future",
#'                   cores = 4,
#'                   chunk = "auto",
#'                   output = "audpc")
#'
#' @seealso fetch_epicrop_weather_list, build_epicrop_emergence
#' @autoglobal
#' @export
run_epicrop_model <- function(
  model_fun,
  emergence_dates,
  wth_list,
  window_days = 120L,
  mode = c("sequential", "future"),
  cores = NULL,
  chunk = c("auto", "none"), # also accepts numeric
  output = c("audpc", "full"),
  strict = TRUE,
  dt_threads = NULL,
  seed = NULL,
  ...
) {
  mode <- match.arg(mode)
  output <- match.arg(output)
  chunk <- .normalize_chunk(chunk) # supports "auto", "none", or numeric

  if (!rlang::inherits_any(emergence_dates, "epicrop.emerge")) {
    cli::cli_warn(
      "{.arg emergence_dates} is not of class {.cls epicrop.emerge}.",
      "Consider using {.fn build_epicrop_emergence} to construct it."
    )
    emergence_dates <- data.table::as.IDate(emergence_dates)
    if (anyNA(emergence_dates)) {
      cli::cli_abort("Failed to coerce {.arg emergence_dates} to {.cls IDate}.")
    }
  }

  if (!rlang::inherits_any(wth_list, "epicrop.wth.list")) {
    cli::cli_abort(
      "{.arg wth_list} is not of class {.cls epicrop.wth.list}.",
      "Please use {.fn fetch_epicrop_weather_list} to construct it."
    )
  }

  # Threads policy (parent)
  if (mode == "future") {
    data.table::setdtthreads(
      if (is.null(dt_threads)) 1L else as.integer(dt_threads)
    )
  } else if (!is.null(dt_threads)) {
    data.table::setdtthreads(as.integer(dt_threads))
  }

  wd <- as.integer(window_days)
  slicer <- .make_slicer(wth_list, wd, strict)
  runner <- .make_runner(model_fun, output, slicer, ..., window_days = wd)
  applier <- .select_applier(
    mode = mode,
    cores = cores,
    chunk = chunk,
    dt_threads = dt_threads,
    seed = seed
  )

  pieces <- applier(emergence_dates, runner)
  .assemble_results(pieces, output)
}


#' Normalize the `chunk` option ("auto"/"none" or a positive integer)
#' @param chunk Character or numeric. Accepted: "auto", "none", or >0 integer.
#' @return Either "auto", "none", or a scalar positive integer.
#' @dev
.normalize_chunk <- function(chunk) {
  if (is.numeric(chunk)) {
    chk <- as.integer(chunk[1L])
    if (!isTRUE(chk > 0L)) {
      cli::cli_abort("{.arg chunk} must be a positive integer.")
    }
    return(chk)
  }
  match.arg(chunk, c("auto", "none"))
}

#' Create a cross‑year‑aware weather slicing function (uses YYYYMMDD IDate)
#'
#' @param wth_list Named per-year weather tables containing an `IDate` column named `YYYYMMDD`.
#' @param window_days Integer number of days to include from emergence.
#' @param strict Logical; error if missing year, else warn and skip.
#' @return A function(ed) -> data.table or NULL
#' @dev
.make_slicer <- function(wth_list, window_days, strict) {
  year_of <- function(id) as.integer(format(id, "%Y"))

  function(ed) {
    start <- ed
    # inclusive end so slice length == window_days
    end <- ed + (as.integer(window_days) - 1L)
    ys <- seq.int(year_of(start), year_of(end))

    segs <- vector("list", length(ys))
    n <- 0L

    for (y in ys) {
      yn <- as.character(y)
      tab <- wth_list[[yn]]
      if (is.null(tab)) {
        if (strict) {
          cli::cli_abort(
            "Missing weather for required year {y} (window {start}–{end})."
          )
        }
        cli::cli_warn("Skipping {start}: no weather for required year {y}.")
        next
      }

      s <- max(start, data.table::as.IDate(sprintf("%d-01-01", y)))
      e <- min(end, data.table::as.IDate(sprintf("%d-12-31", y)))

      # Since 'YYYYMMDD' is a non-syntactic name, use backticks or get()
      seg <- tab[`YYYYMMDD` %between% c(s, e)]

      if (nrow(seg)) {
        n <- n + 1L
        segs[[n]] <- seg
      }
    }

    if (n == 0L) {
      return(NULL)
    }
    if (n == 1L) {
      return(segs[[1L]])
    }
    data.table::rbindlist(segs[seq_len(n)], use.names = TRUE)
  }
}

#' Create a per‑emergence model‑execution function
#' @param model_fun The epicrop model function to call.
#' @param output "audpc" or "full".
#' @param slicer A function that returns a weather slice for a given emergence date.
#' @param ... Extra args forwarded to `model_fun`.
#' @param window_days Integer duration passed for clarity (optional usage).
#' @return A function(ed) -> data.table (AUDPC summary or full model output)
#' @dev
.make_runner <- function(model_fun, output, slicer, ..., window_days = NULL) {
  force(model_fun)
  force(output)
  force(slicer)
  dots <- list(...)

  has_user_duration <- (function() {
    dn <- names(dots)

    # If any unnamed args exist, be conservative: they could bind by position.
    if (is.null(dn) || any(is.na(dn) | dn == "")) {
      return(TRUE)
    }

    if ("duration" %in% dn) {
      return(TRUE)
    }

    fml_names <- tryCatch(names(formals(model_fun)), error = function(e) NULL)
    if (is.null(fml_names)) {
      return(FALSE)
    }
    fml_no_dots <- setdiff(fml_names, "...")

    any(vapply(
      dn,
      function(nm) {
        if (!nzchar(nm)) {
          return(FALSE)
        }
        hit <- startsWith(fml_no_dots, nm)
        sum(hit) == 1L && fml_no_dots[which(hit)] == "duration"
      },
      logical(1)
    ))
  })()

  function(ed) {
    w_sub <- slicer(ed)
    if (is.null(w_sub) || nrow(w_sub) == 0L) {
      if (identical(output, "audpc")) {
        return(data.table::data.table(emergence = ed, AUDPC = NA_real_))
      }
      return(NULL)
    }

    # sever by-reference ties exactly once per emergence
    w_sub <- data.table::copy(w_sub)

    extra_args <- if (!has_user_duration) {
      list(duration = as.integer(nrow(w_sub)))
    } else {
      list()
    }

    out <- do.call(
      model_fun,
      # Keep user args before our auto-injected duration
      c(list(wth = w_sub, emergence = ed), dots, extra_args)
    )

    if (identical(output, "audpc")) {
      aud <- attr(out, "AUDPC", exact = TRUE)
      if (is.null(aud)) {
        y <- out[["intensity"]]
        if (is.null(y)) {
          cli::cli_abort(
            "Model output lacks {.field intensity} column required for AUDPC."
          )
        }
        aud <- sum(y[-1L] + y[-length(y)]) * 0.5 # trapezoid rule, dt = 1
      }
      data.table::data.table(emergence = ed, AUDPC = aud)
    } else {
      data.table::set(out, j = "emergence", value = ed)
      data.table::set(
        out,
        j = "AUDPC",
        value = attr(out, "AUDPC", exact = TRUE)
      )
      out
    }
  }
}


#' Select sequential or futureverse parallel application strategy
#' @param mode "sequential" or "future".
#' @param cores Integer worker count (only for future).
#' @param chunk "auto", "none", or numeric chunk size (≈ elements per
#'    scheduling unit).
#' @param dt_threads data.table threads per worker (default 1).
#' @param seed `NULL` or integer. When set, results are reproducible across
#'    modes.
#' @return A function(x, fun) applying `fun` over `x` (possibly in parallel).
#' @dev
.select_applier <- function(
  mode = c("sequential", "future"),
  cores = NULL,
  chunk = "auto",
  dt_threads = NULL,
  seed = NULL
) {
  mode <- match.arg(mode)

  if (identical(mode, "sequential")) {
    # Sequential: set seed if provided; then plain lapply
    return(function(x, fun) {
      if (!is.null(seed)) {
        # Base RNG is fine in single process
        set.seed(as.integer(seed))
      }
      lapply(X = x, FUN = fun)
    })
  }

  if (
    !requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("future.apply", quietly = TRUE)
  ) {
    cli::cli_abort(
      "Parallel mode requires {.pkg future} and {.pkg future.apply}."
    )
  }

  map_scheduling <- function(chunk, n, workers) {
    if (is.numeric(chunk)) {
      return(max(1L, as.integer(chunk)))
    }
    if (identical(chunk, "none")) {
      return(1L)
    }
    4L # "auto"
  }

  return(function(x, fun) {
    n <- length(x)
    wrk <- if (is.null(cores)) {
      max(1L, parallel::detectCores() - 1L)
    } else {
      as.integer(cores)
    }
    sched <- map_scheduling(chunk, n, wrk)

    # Deterministic across workers/scheduling:
    # Use L'Ecuyer-CMRG + a single user seed in the main R session
    if (!is.null(seed)) {
      suppressWarnings(RNGkind("L'Ecuyer-CMRG"))
      set.seed(as.integer(seed))
    }

    oplan <- future::plan(future::multisession, workers = wrk)
    on.exit(try(future::plan(oplan), silent = TRUE), add = TRUE)
    cli::cli_inform("Running in parallel with {.val {wrk}} worker{?s}.")

    future.apply::future_lapply(
      X = x,
      FUN = function(ed) {
        data.table::setdtthreads(
          if (is.null(dt_threads)) 1L else as.integer(dt_threads)
        )
        fun(ed)
      },
      future.seed = TRUE, # stable seed per element
      future.scheduling = sched
    )
  })
}

#' Assemble model outputs into final result tables
#' @param pieces List of per-emergence outputs (summaries or full tables).
#' @param output "audpc" or "full".
#' @return data.table with final results.
#' @dev
.assemble_results <- function(pieces, output) {
  if (identical(output, "audpc")) {
    res <- data.table::rbindlist(pieces, use.names = TRUE)
    data.table::setkey(res, emergence, AUDPC)
    unique(res, by = c("emergence", "AUDPC"))
  } else {
    data.table::rbindlist(
      Filter(Negate(is.null), pieces),
      use.names = TRUE,
      fill = TRUE
    )
  }
}

#' Build emergence dates from year × month_day combinations
#'
#' @return A sorted vector of `IDate` emergence dates with class
#'  `epicrop.emerge`.
#' @seealso run_epicrop_model, fetch_epicrop_weather_list
#'
#' @examples
#' years <- 2001:2002
#' month_day <- c("-06-01", "-06-14", "-06-30")
#'
#' em <- build_epicrop_emergence(years, month_day)
#'
#' run_epicrop_model(bacterial_blight, em, wth_list, output = "audpc")
#'
#' @export
build_epicrop_emergence <- function(years, month_day) {
  g <- data.table::CJ(years = years, month_day = month_day, sorted = FALSE)
  g <- sort(
    data.table::as.IDate(
      sprintf("%s%s", g$years, g$month_day),
      format = "%Y-%m-%d"
    )
  )
  class(g) <- union("epicrop.emerge", class(g))
  g
}


#' Fetch per-year weather tables for use with epicrop models (polite to upstream APIs)
#'
#' @param years Integer vector of years to fetch.
#' @param lonlat Numeric vector of length 2: `c(lon, lat)` or a 2 column matrix
#'   named "lon" and "lat", respectively.  See examples.
#' @param start_date Character string of month-day in "-MM-DD" format indicating
#'   the month and day to start fetching weather for each year.
#' @param duration Integer. Number of days of weather data to fetch per season.
#' @param sleep_jitter Numeric vector of length 2. Minimum and maximum seconds.
#'   Defaults to `c(0, 0)` (no sleep).  When set to positive values, a random
#'   value in this range is used to pause between requests to the
#'   \acronym{POWER} API.
#'
#' @seealso run_epicrop_model, build_epicrop_emergence
#'
#' @examplesIf interactive()
#' # Minimal example: fetch two years at a given location
#' yrs <- 2001:2002
#' lonlat <- c(121.255669, 14.16742) # (lon, lat)
#'
#' # Start month-day and duration (days) per year
#' wth_list <- fetch_epicrop_weather_list(
#'   years     = yrs,
#'   lonlat    = lonlat,
#'   start_md  = "-06-01",
#'   duration  = 120L,
#'   sleep_jitter = c(0.1, 0.5)          # be polite to upstream APIs
#' )
#'
#' em <- build_epicrop_emergence(yrs, "-06-01")
#'
#' # using a matrix of multiple locations
#' lonlat <- matrix(
#' c(
#'   115.8600, -31.9500,   # Perth
#'   117.0000, -30.7500,   # Wheatbelt example
#'   121.2556,  14.6774    # IRRI ZEIGLER
#' ),
#' ncol = 2,
#' byrow = TRUE,
#' dimnames = list(NULL, c("lon", "lat"))
#' )
#'
#' wth_list <- fetch_epicrop_weather_list(
#'  years = 2020:2021,
#'  lonlat = lonlat,
#'  start_date = c("04-15", "05-01"),
#'  duration = 120L
#'  )
#'
#' @autoglobal
#' @export

fetch_epicrop_weather_list <- function(
  years,
  lonlat,
  start_date,
  duration,
  sleep_jitter = c(0, 0)
) {
  # coerce lonlat to 2-col matrix [lon, lat]
  if (is.numeric(lonlat) && length(lonlat) == 2L) {
    lonlat <- matrix(lonlat, nrow = 1L, dimnames = list(NULL, c("lon", "lat")))
  } else {
    lonlat <- as.matrix(lonlat)
    if (ncol(lonlat) != 2L) {
      cli::cli_abort("{.var lonlat} must have 2 columns: lon, lat")
    }
    colnames(lonlat) <- c("lon", "lat")
  }

  # normalise start_date to "-MM-DD"
  start_date <- ifelse(
    substr(start_date, 1L, 1L) == "-",
    start_date,
    paste0("-", start_date)
  )

  # fast Cartesian product
  dt <- data.table::CJ(
    year = as.integer(years),
    md = start_date,
    idx = seq_len(nrow(lonlat)),
    sorted = FALSE
  )
  dt[, `:=`(lon = lonlat[idx, 1], lat = lonlat[idx, 2])]
  dt[, full_date := paste0(year, md)]

  n <- nrow(dt)
  out <- vector("list", n)
  nm <- sprintf("%s|%0.6f,%0.6f", dt$full_date, dt$lon, dt$lat)

  for (i in seq_len(n)) {
    ll <- c(dt$lon[i], dt$lat[i])
    # IMPORTANT: match get_wth signature (lonlat, dates, duration)
    out[[i]] <- get_wth(
      lonlat = ll,
      dates = dt$full_date[i],
      duration = duration
    )

    if (
      length(sleep_jitter) >= 2L &&
        is.finite(sleep_jitter[2]) &&
        sleep_jitter[2] > 0
    ) {
      lo <- min(sleep_jitter[1], sleep_jitter[2])
      hi <- max(sleep_jitter[1], sleep_jitter[2])
      Sys.sleep(runif(1, lo, hi))
    }
  }

  names(out) <- nm
  class(out) <- c("epicrop.wth.list", "list")
  out
}
