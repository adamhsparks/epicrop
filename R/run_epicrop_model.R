#' Run an epicrop disease model across emergence dates and/or locations
#'
#' A unified helper that executes any \pkg{epicrop} model (*e.g.*,
#' [bacterial_blight()], [brown_spot()], [leaf_blast()], [sheath_blight()],
#' [tungro()], [modified_kim_leaf_blast()], [modified_kim_sheath_blight()],
#' [leaf_rust()], or [s_tritici_blotch()]) over a set of emergence dates, using
#' a list of per-year weather `data.table`s. Handles within-year and cross-year
#' windows without globally binding weather.
#'
#' @param model_fun Function. An \pkg{epicrop} model function (*e.g.*,
#'   [bacterial_blight()]).
#' @param emergence_dates Vector of dates (character "YYYY-MM-DD", Date, or
#'   IDate).
#' @param wth_list Named list of weather [data.table::data.table]s with
#'   one per year; `names(wth_list)` must be year strings (*e.g.*, "2001").
#'   Each table must have a \code{YYYYMMDD} column (Date/IDate/ISO/"YYYYMMDD").
#' @param window_days Integer. Days after emergence to include (default 120L).
#'   The slicer returns exactly `window_days` rows when data are available.
#' @param output Character. "audpc" (compact summary using [get_audpc()])
#'   or "full" (return full model outputs per date, with `emergence` added).
#'   Defaults to "audpc".
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
#' duration <- 120L
#' emergence <- build_epicrop_emergence(years, month_day)
#'
#' wth_list <- fetch_epicrop_weather_list(years,
#'                                        lonlat,
#'                                        start_date = month_day,
#'                                        duration = duration)
#'
#' # AUDPC for bacterial blight
#' run_epicrop_model(bacterial_blight,
#'                   emergence,
#'                   wth_list,
#'                   window_days = 120L,
#'                   output = "audpc")
#'
#' @seealso fetch_epicrop_weather_list, build_epicrop_emergence
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @export
run_epicrop_model <- function(
  model_fun,
  emergence_dates,
  wth_list,
  window_days = 120L,
  output = "audpc",
  ...
) {
  output <- rlang::arg_match(output, c("audpc", "full"))

  # Validate inputs
  .validate_inputs(emergence_dates, wth_list)

  # Handle bundle (multiple locations)
  if (.is_bundle(wth_list)) {
    return(.process_bundle(
      model_fun,
      emergence_dates,
      wth_list,
      window_days,
      output,
      ...
    ))
  }

  # Single location processing
  slicer <- .make_slicer(wth_list, window_days)
  runner <- .make_runner(model_fun, output, slicer, window_days, ...)

  pieces <- lapply(emergence_dates, runner)
  out <- .assemble_results(pieces, output)

  data.table::setattr(
    out,
    "class",
    c("epicrop.sim", "data.table", "data.frame")
  )
}

#' Validate input arguments
#' @noRd
.validate_inputs <- function(emergence_dates, wth_list) {
  if (!inherits(emergence_dates, "epicrop.emerge")) {
    cli::cli_abort(c(
      "{.arg emergence_dates} is not of class {.cls epicrop.emerge}.",
      i = "Construct it via {.fn build_epicrop_emergence}."
    ))
  }

  if (
    !rlang::inherits_any(wth_list, c("epicrop.wth.list", "epicrop.wth.bundle"))
  ) {
    cli::cli_abort(c(
      "{.arg wth_list} must be {.cls epicrop.wth.list} or {.cls epicrop.wth.bundle}.",
      i = "Use {.fn fetch_epicrop_weather_list}."
    ))
  }
}

#' Check if wth_list is a bundle (multiple locations)
#' @noRd
.is_bundle <- function(wth_list) {
  inherits(wth_list, "epicrop.wth.bundle") ||
    !all(grepl("^\\d{4}$", rlang::names2(wth_list)))
}

#' Process bundle of weather lists across multiple locations
#' @noRd
.process_bundle <- function(
  model_fun,
  emergence_dates,
  wth_list,
  window_days,
  output,
  ...
) {
  location_keys <- rlang::names2(wth_list)
  location_labels <- sub("__.*$", "", location_keys)
  location_labels[!nzchar(location_labels)] <- sprintf(
    "location%02d",
    seq_along(wth_list)
  )

  pieces <- Map(
    function(location_w, location_nm) {
      ans <- run_epicrop_model(
        model_fun = model_fun,
        emergence_dates = emergence_dates,
        wth_list = location_w,
        window_days = window_days,
        output = output,
        ...
      )
      ans[, location := location_nm]
    },
    wth_list,
    location_labels
  )

  data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
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
#' @return A function(ed) -> data.table or NULL
#' @noRd
.make_slicer <- function(wth_list, window_days) {
  window_days <- as.integer(window_days)

  function(ed) {
    start <- ed
    end <- ed + (window_days - 1L)
    years <- seq.int(data.table::year(start), data.table::year(end))

    segs <- vector("list", length(years))

    for (i in seq_along(years)) {
      y <- years[i]
      tab <- wth_list[[as.character(y)]]

      if (is.null(tab)) {
        cli::cli_warn(
          "Skipping {start}: no weather for required year {y} (window {start}\u2013{end})."
        )
        next
      }

      year_start <- data.table::as.IDate(sprintf("%d-01-01", y))
      year_end <- data.table::as.IDate(sprintf("%d-12-31", y))

      seg_start <- max(start, year_start)
      seg_end <- min(end, year_end)

      seg <- tab[YYYYMMDD %between% c(seg_start, seg_end)]

      if (nrow(seg) == 0L) {
        cli::cli_warn(
          "Skipping {start}: no rows in year {y} within {seg_start}\u2013{seg_end}."
        )
        next
      }

      segs[[i]] <- seg
    }

    segs <- Filter(Negate(is.null), segs)

    if (length(segs) == 0L) {
      return(NULL)
    }
    if (length(segs) == 1L) {
      return(segs[[1L]])
    }

    data.table::rbindlist(segs, use.names = TRUE)
  }
}

#' Create a per‑emergence model‑execution function
#'
#' Ensures we only auto‑inject `duration` when the user has *not* supplied it
#' (including unique partial matches or positional args).
#'
#' @param model_fun \pkg{epicrop} model function to call.
#' @param output "audpc" or "full".
#' @param slicer Slicer function producing a weather slice for an emergence date.
#' @param window_days Integer; passed for reference only.
#' @param ... Extra args forwarded to `model_fun`.
#' @return A function(ed) -> data.table (summary or full)
#' @noRd
.make_runner <- function(model_fun, output, slicer, window_days, ...) {
  dots <- rlang::dots_list(..., .named = TRUE)
  has_user_duration <- .check_user_duration(dots, model_fun)

  function(ed) {
    w_sub <- slicer(ed)

    if (is.null(w_sub) || nrow(w_sub) == 0L) {
      if (output == "audpc") {
        return(data.table::data.table(emergence = ed, AUDPC = NA_real_))
      }
      return(NULL)
    }

    w_sub <- data.table::copy(w_sub)

    # Inject duration only if user didn't provide it
    extra <- if (!has_user_duration) list(duration = nrow(w_sub)) else list()

    out <- rlang::exec(
      model_fun,
      wth = w_sub,
      emergence = ed,
      !!!dots,
      !!!extra
    )

    if (output == "audpc") {
      data.table::data.table(
        emergence = ed,
        AUDPC = attr(out, "AUDPC", exact = TRUE)
      )
    } else {
      audpc_val <- attr(out, "AUDPC", exact = TRUE)
      out[, `:=`(emergence = ed, AUDPC = audpc_val)][]
    }
  }
}

#' Check if user provided duration argument
#' @noRd
.check_user_duration <- function(dots, model_fun) {
  dot_names <- rlang::names2(dots)

  if (length(dot_names) == 0L || !any(nzchar(dot_names))) {
    return(FALSE)
  }

  if ("duration" %in% dot_names) {
    return(TRUE)
  }

  fmls <- rlang::fn_fmls_names(model_fun)
  if (is.null(fmls)) {
    return(FALSE)
  }

  fml_no_dots <- setdiff(fmls, "...")

  any(vapply(
    dot_names[nzchar(dot_names)],
    function(nm) {
      hit <- startsWith(fml_no_dots, nm)
      sum(hit) == 1L && fml_no_dots[which(hit)] == "duration"
    },
    logical(1),
    USE.NAMES = FALSE
  ))
}

#' Assemble model outputs into final result tables
#'
#' Combines per-emergence outputs into the requested shape.
#' For "audpc", returns unique rows by (emergence, AUDPC).
#'
#' @param pieces List of per-emergence outputs.
#' @param output "audpc" or "full".
#' @return data.table
#' @noRd
.assemble_results <- function(pieces, output) {
  if (output == "audpc") {
    if (length(pieces) == 0L) {
      res <- data.table::data.table(
        emergence = data.table::as.IDate(NA),
        AUDPC = NA_real_
      )
      return(data.table::setkey(res, emergence, AUDPC))
    }

    # Normalize pieces
    pieces <- lapply(pieces, function(x) {
      if (is.null(x)) {
        return(data.table::data.table(
          emergence = data.table::as.IDate(NA),
          AUDPC = NA_real_
        ))
      }
      x[, .(emergence, AUDPC)]
    })

    res <- data.table::rbindlist(pieces, use.names = TRUE)
    data.table::setkey(res, emergence, AUDPC)
    unique(res)
  } else {
    data.table::rbindlist(
      Filter(Negate(is.null), pieces),
      use.names = TRUE,
      fill = TRUE
    )
  }
}
