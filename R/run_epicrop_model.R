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
#'  [bacterial_blight()]).
#' @param emergence_dates Vector of dates (character "YYYY-MM-DD", Date, or
#'  IDate).
#' @param wth_list Named list of weather [data.table::data.table]s with
#'   one per year; `names(wth_list)` must be year strings (*e.g.*, "2001").
#'   Each table must have a \code{YYYYMMDD} column (Date/IDate/ISO/"YYYYMMDD").
#' @param window_days Integer. Days after emergence to include (default 120L).
#'   The slicer returns exactly `window_days` rows when data are available.
#' @param mode Character. "sequential" for sequential (default) or "future" for
#'   parallel. Defaults to "sequential".
#' @param cores Integer. Number of workers when `mode = "future"`.
#'   Default: `max(1L, parallel::detectCores() - 1L)`.
#' @param chunk Either "auto", "none", or a positive integer chunk size
#'   for batching emergence dates per future. "auto" ~ 4 chunks/worker. Defaults
#'   to "auto".
#' @param output Character. "audpc" (compact summary using [get_audpc()])
#'   or "full" (return full model outputs per date, with `emergence` added).
#'   Defaults to "audpc".
# ' @param strict Logical. If TRUE (default), abort via [cli::cli_abort()] when
#'   required year weather is missing; if FALSE, skip that date with
#'   [cli::cli_warn()].
#' @param dt_threads Integer or NULL. \CRANpkg{data.table} threads per process.
#'   In parallel, this is set **inside each worker** (default 1). In sequential,
#'   it is set in the main process if provided.
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
#' # Parallel (futureverse) AUDPC for STB on 4 workers
#' # requires {future} and {future.apply} packages to be installed
#' run_epicrop_model(stb,
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
  mode = "sequential",
  cores = NULL,
  chunk = "auto",
  output = "audpc",
  strict = TRUE,
  dt_threads = NULL,
  ...
) {
  mode <- rlang::arg_match(mode, c("sequential", "future"))
  output <- rlang::arg_match(output, c("audpc", "full"))
  chunk <- rlang::arg_match(chunk, c("auto", "none"))
  chunk <- .normalize_chunk(chunk)

  if (!rlang::inherits_any(emergence_dates, "epicrop.emerge")) {
    cli::cli_abort(
      c(
        "{.arg emergence_dates} is not of class {.cls epicrop.emerge}.",
        i = "Please use {.fn build_epicrop_emergence} to construct it."
      )
    )
  }

  if (!rlang::inherits_any(wth_list, "epicrop.wth.list")) {
    cli::cli_abort(
      c(
        "{.arg wth_list} is not of class {.cls epicrop.wth.list}.",
        i = "Please use {.fn fetch_epicrop_weather_list} to construct it."
      )
    )
  }

  # data.table threads policy (parent)
  if (mode == "future") {
    data.table::setDTthreads(if (is.null(dt_threads)) 1L else dt_threads)
  } else if (!is.null(dt_threads)) {
    data.table::setDTthreads(dt_threads)
  }

  # Slicer trusts S3 contract: `YYYYMMDD` exists and is IDate
  slicer <- .make_slicer(wth_list, as.integer(window_days), strict)

  runner <- .make_runner(
    model_fun,
    output,
    slicer,
    ...,
    window_days = as.integer(window_days)
  )

  applier <- .select_applier(
    mode = mode,
    cores = cores,
    chunk = chunk,
    dt_threads = dt_threads
  )

  pieces <- applier(emergence_dates, runner)
  .assemble_results(pieces, output)
}


#' Cross‑year weather slicer (assumes `epicrop.wth.list` contract)
#'
#' Returns a function that slices per-year weather tables using the IDate column
#' `YYYYMMDD`. No format parsing or coercion is performed—this trusts the S3
#' class contract that tables are well-formed.
#'
#' @param wth_list An `epicrop.wth.list` (named by year strings).
#' @param window_days Integer number of days after emergence to include
#'  (inclusive).
#' @param strict Logical; TRUE to abort on missing year/rows, FALSE to warn and
#'   skip. Defaults to `TRUE`.
#' @return A function(ed) -> data.table or NULL
#' @autoglobal
#' @dev
.make_slicer <- function(wth_list, window_days, strict = TRUE) {
  year_of <- function(id) as.integer(format(id, "%Y"))

  function(ed) {
    start <- ed
    end <- ed + (as.integer(window_days) - 1L)
    ys <- seq.int(year_of(start), year_of(end))

    segs <- vector("list", length(ys))
    n_seg <- 0L

    for (y in ys) {
      tab <- wth_list[[as.character(y)]]
      if (is.null(tab)) {
        if (strict) {
          cli::cli_abort(
            "Missing weather for required year {y} (window {start}–{end}).",
            .envir = parent.frame()
          )
        }
        cli::cli_warn(
          "Skipping {start}: no weather for required year {y}.",
          .envir = parent.frame()
        )
        next
      }

      s <- max(start, data.table::as.IDate(sprintf("%d-01-01", y)))
      e <- min(end, data.table::as.IDate(sprintf("%d-12-31", y)))

      # Direct slice on non‑syntactic column; no copies here
      seg <- tab[`YYYYMMDD` %between% c(s, e)]
      if (nrow(seg)) {
        n_seg <- n_seg + 1L
        segs[[n_seg]] <- seg
      } else if (strict) {
        cli::cli_abort(
          "Missing weather rows for required year {y} within window {start}–{end}.",
          .envir = parent.frame()
        )
      } else {
        cli::cli_warn(
          "Skipping {start}: no rows in year {y} within {s}–{e}.",
          .envir = parent.frame()
        )
      }
    }

    if (n_seg == 0L) {
      return(NULL)
    }
    if (n_seg == 1L) {
      return(segs[[1L]])
    }
    data.table::rbindlist(segs[seq_len(n_seg)], use.names = TRUE)
  }
}

#' Create a per‑emergence model‑execution function
#'
#' Ensures we only auto‑inject `duration` when the user has *not* supplied it
#' (including unique partial matches or positional args).
#'
#' @param model_fun Epicrop model function to call.
#' @param output "audpc" or "full".
#' @param slicer Slicer function producing a weather slice for an emergence
#'  date.
#' @param ... Extra args forwarded to `model_fun`.
#' @param window_days Integer; passed for reference only.
#' @return A function(ed) -> data.table (summary or full)
#' @autoglobal
#' @dev
.make_runner <- function(model_fun, output, slicer, ..., window_days = NULL) {
  force(model_fun)
  force(output)
  force(slicer)
  dots <- list(...)

  has_user_duration <- (function() {
    # CHANGED
    dn <- names(dots)
    if (is.null(dn) || any(is.na(dn) | dn == "")) {
      return(TRUE)
    } # unnamed: could bind by position
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

    # sever by-reference ties
    w_sub <- data.table::copy(w_sub)

    # Respect user-provided duration if present; otherwise use the slice length
    extra_args <- if (!has_user_duration) {
      list(duration = as.integer(nrow(w_sub)))
    } else {
      list()
    }

    out <- do.call(
      model_fun,
      c(list(wth = w_sub, emergence = ed), dots, extra_args)
    )

    if (identical(output, "audpc")) {
      aud <- attr(out, "AUDPC", exact = TRUE)
      if (is.null(aud)) {
        y <- out[["intensity"]]
        if (is.null(y)) {
          cli::cli_abort(
            "Model output lacks {intensity} column required for AUDPC.",
            .envir = parent.frame()
          )
        }
        aud <- sum((head(y, -1L) + tail(y, -1L)) / 2) # dt = 1
      }
      data.table::data.table(emergence = ed, AUDPC = aud)
    } else {
      out[, emergence := ed]
      # Preserve AUDPC even after rbindlist
      out[, AUDPC := attr(out, "AUDPC", exact = TRUE)]
      out
    }
  }
}


#' Select sequential or futureverse application strategy
#'
#' Applies `fun` over emergence dates either sequentially or via {future.apply}.
#' In parallel, uses `future.seed = TRUE` to give each element a stable RNG
#'   stream.
#'
#' @param mode "sequential" or "future".
#' @param cores Integer worker count (future).
#' @param chunk "auto", "none", or numeric (mapped to future.scheduling).
#' @param dt_threads Integer data.table threads per worker (default 1 in
#'  parallel).
#' @return function(x, fun) -> list of results
#' @autoglobal
#' @dev
.select_applier <- function(
  mode = c("sequential", "future"),
  cores = NULL,
  chunk = c("auto", "none"),
  dt_threads = NULL
) {
  mode <- rlang::arg_match(mode, c("sequential", "future"))
  chunk <- .normalize_chunk(chunk)

  if (mode == "sequential") {
    return(function(x, fun) lapply(X = x, FUN = fun))
  }

  if (
    !requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("future.apply", quietly = TRUE)
  ) {
    cli::cli_abort(
      "Parallel mode requires {.pkg future} and {.pkg future.apply}.",
      .envir = parent.frame()
    )
  }

  map_scheduling <- function(chunk, n, workers) {
    if (is.numeric(chunk)) {
      return(max(1L, as.integer(chunk)))
    }
    if (identical(chunk, "none") || n <= workers) {
      return(1L)
    }
    4L # "auto"
  }

  function(x, fun) {
    n <- length(x)
    wrk <- if (is.null(cores)) {
      max(1L, parallel::detectCores() - 1L)
    } else {
      as.integer(cores)
    }
    sch <- map_scheduling(chunk, n, wrk)

    old <- future::plan()
    on.exit(try(future::plan(old), silent = TRUE), add = TRUE)
    future::plan(future::multisession, workers = wrk)
    cli::cli_inform(
      "Running in parallel with {.val {wrk}} worker{?s}.",
      .envir = parent.frame()
    )

    future.apply::future_lapply(
      X = x,
      FUN = function(ed) {
        data.table::setDTthreads(
          if (is.null(dt_threads)) 1L else as.integer(dt_threads)
        )
        fun(ed)
      },
      future.seed = TRUE,
      future.scheduling = sch
    )
  }
}


#' Assemble model outputs into final result tables
#'
#' Combines per-emergence outputs into the requested shape.
#' For "audpc", returns unique rows by (emergence, AUDPC).
#'
#' @param pieces List of per-emergence outputs.
#' @param output "audpc" or "full".
#' @return data.table
#' @autoglobal
#' @dev

.assemble_results <- function(pieces, output) {
  if (identical(output, "audpc")) {
    # Normalize each piece to have (emergence, AUDPC) or a NA row
    norm_piece <- function(x) {
      if (is.null(x)) {
        return(data.table::data.table(
          emergence = data.table::as.IDate(NA),
          AUDPC = as.numeric(NA)
        ))
      }
      # Coerce to data.table
      if (!data.table::is.data.table(x)) {
        x <- data.table::as.data.table(x)
      }

      # Normalize likely variants of names
      nms <- names(x)
      if (!"emergence" %in% nms) {
        if ("Emergence" %in% nms) {
          data.table::setnames(x, "Emergence", "emergence")
        }
        if ("emergence_date" %in% nms) {
          data.table::setnames(x, "emergence_date", "emergence")
        }
      }
      if (!"AUDPC" %in% nms) {
        if ("audpc" %in% nms) data.table::setnames(x, "audpc", "AUDPC")
      }

      # Ensure both columns exist (fill-with-NA if absent)
      if (!"emergence" %in% names(x)) {
        x[, emergence := data.table::as.IDate(NA)]
      }
      if (!"AUDPC" %in% names(x)) {
        x[, AUDPC := as.numeric(NA)]
      }

      # Keep only what we need for the compact result
      x[, .(emergence, AUDPC)]
    }

    # Handle totally empty inputs robustly
    if (length(pieces) == 0L) {
      res <- data.table::data.table(
        emergence = data.table::as.IDate(NA),
        AUDPC = as.numeric(NA)
      )
      data.table::setkey(res, emergence, AUDPC)
      return(res)
    }

    pieces2 <- lapply(pieces, norm_piece)
    res <- data.table::rbindlist(pieces2, use.names = TRUE, fill = TRUE)
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

#' Normalize the `chunk` option
#'
#' Accepts "auto"/"none" or a positive integer. Returns a scalar value suitable
#' for mapping to scheduling behavior.
#'
#' @param chunk Character or numeric.
#' @return "auto", "none", or integer > 0.
#' @autoglobal
#' @dev
.normalize_chunk <- function(chunk) {
  if (is.numeric(chunk)) {
    chk <- as.integer(chunk[1L])
    if (!is.finite(chk) || chk <= 0L) {
      cli::cli_abort(
        "{.arg chunk} must be a positive integer when numeric.",
        .envir = parent.frame()
      )
    }
    return(chk)
  }
  rlang::arg_match(chunk, c("auto", "none"))
}
