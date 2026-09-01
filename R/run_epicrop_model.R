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
#' run_epicrop_model(bacterial_blight, emergence, wth_list)
#'
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
  .validate_run_inputs(emergence_dates, wth_list)

  if (.is_wth_bundle(wth_list)) {
    return(.run_bundle(
      model_fun,
      emergence_dates,
      wth_list,
      window_days,
      output,
      ...
    ))
  }

  dots <- rlang::dots_list(..., .named = TRUE)

  # Auto-fill duration from window_days if the model exposes it as a formal
  if (.model_has_duration_formal(model_fun) && !("duration" %in% names(dots))) {
    dots$duration <- window_days
  }
  pieces <- lapply(emergence_dates, function(ed) {
    .run_one(
      ed,
      model_fun,
      wth_list,
      window_days,
      dots,
      output
    )
  })

  .assemble(pieces, output)
}

.validate_run_inputs <- function(emergence_dates, wth_list) {
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
      "{.arg wth_list} must be {.cls epicrop.wth.list} or
        {.cls epicrop.wth.bundle}.",
      i = "Use {.fn fetch_epicrop_weather_list}."
    ))
  }
}

.is_wth_bundle <- function(wth_list) {
  inherits(wth_list, "epicrop.wth.bundle") ||
    !all(grepl("^\\d{4}$", rlang::names2(wth_list)))
}

#' Run epicrop Model Across a Weather Bundle
#'
#' Internal helper that processes a weather bundle (multiple locations or
#' location-date combinations) by executing [run_epicrop_model()] separately
#' for each bundled weather list and combining results with location labels.
#'
#' @param model_fun Function. An \pkg{epicrop} model function.
#' @param emergence_dates Vector of emergence dates (class `epicrop.emerge`).
#' @param wth_list An `epicrop.wth.bundle`; a named list of `epicrop.wth.list`
#'   objects.
#' @param window_days Integer. Days after emergence to include (default 120L).
#' @param output Character. `"audpc"` or `"full"`.
#' @param ... Additional arguments forwarded to `model_fun`.
#'
#' @return A [data.table::data.table] with results from all bundled locations
#'   combined row-wise, with an additional `location` column identifying the
#'   source bundle member.
#'
#' @details
#' The function extracts location labels from the bundle's names (text before
#' `"__"` delimiter) and generates default labels (`location01`, `location02`,
#' etc.) for any unnamed or empty-named entries. Each bundled weather list is
#' processed independently via [run_epicrop_model()], then results are
#' combined using [data.table::rbindlist()].
#'
#' @seealso [run_epicrop_model()], [fetch_epicrop_weather_list()]
#'
#' @autoglobal
#' @dev
.run_bundle <- function(
  model_fun,
  emergence_dates,
  wth_list,
  window_days,
  output,
  ...
) {
  nms <- rlang::names2(wth_list)
  labels <- sub("__.*$", "", nms)
  labels[!nzchar(labels)] <- sprintf("location%02d", seq_along(wth_list))

  pieces <- Map(
    function(w, nm) {
      out <- run_epicrop_model(
        model_fun,
        emergence_dates,
        w,
        window_days,
        output,
        ...
      )
      out[, location := nm]
    },
    wth_list,
    labels
  )

  data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
}


.has_duration_arg <- function(dots, model_fun) {
  nms <- rlang::names2(dots)[nzchar(rlang::names2(dots))]
  fmls <- setdiff(rlang::fn_fmls_names(model_fun) %||% character(), "...")
  if ("duration" %in% nms) {
    return(TRUE)
  }
  any(vapply(
    nms,
    function(nm) {
      hits <- startsWith(fmls, nm)
      sum(hits) == 1L && fmls[hits] == "duration"
    },
    logical(1),
    USE.NAMES = FALSE
  ))
}

.model_has_duration_formal <- function(model_fun) {
  # Check if 'duration' is an explicit formal parameter
  fmls <- rlang::fn_fmls_names(model_fun) %||% character()
  "duration" %in% fmls
}

.slice_weather <- function(ed, wth_list, window_days) {
  start <- ed
  end <- ed + (window_days - 1L)
  years <- seq.int(data.table::year(start), data.table::year(end))

  segs <- Filter(
    Negate(is.null),
    lapply(years, function(y) {
      tab <- wth_list[[as.character(y)]]
      if (is.null(tab)) {
        cli::cli_warn("Skipping {ed}: no weather for year {y}.")
        return(NULL)
      }
      seg <- tab[
        YYYYMMDD %between%
          c(
            max(start, data.table::as.IDate(sprintf("%d-01-01", y))),
            min(end, data.table::as.IDate(sprintf("%d-12-31", y)))
          )
      ]
      if (nrow(seg) == 0L) {
        cli::cli_warn("Skipping {ed}: no rows in year {y}.")
        return(NULL)
      }
      seg
    })
  )

  if (!length(segs)) {
    return(NULL)
  }
  if (length(segs) == 1L) {
    segs[[1L]]
  } else {
    data.table::rbindlist(segs, use.names = TRUE)
  }
}

.run_one <- function(
  ed,
  model_fun,
  wth_list,
  window_days,
  dots,
  output
) {
  w_sub <- .slice_weather(ed, wth_list, window_days)

  if (is.null(w_sub)) {
    return(
      if (output == "audpc") {
        data.table::data.table(
          emergence = ed,
          AUDPC = NA_real_
        )
      } else {
        NULL
      }
    )
  }

  result <- rlang::exec(
    model_fun,
    wth = data.table::copy(w_sub),
    emergence = ed,
    !!!dots
  )

  audpc <- attr(result, "AUDPC", exact = TRUE)

  if (output == "audpc") {
    return(data.table::data.table(
      emergence = ed,
      AUDPC = audpc
    ))
  }

  result <- data.table::copy(result)
  result[, `:=`(
    emergence = ed,
    AUDPC = audpc
  )]

  result[]
}

.assemble <- function(pieces, output) {
  if (output != "audpc") {
    out <- data.table::rbindlist(
      Filter(Negate(is.null), pieces),
      use.names = TRUE,
      fill = TRUE
    )
    return(data.table::setattr(
      out,
      "class",
      c("epicrop.sim", "data.table", "data.frame")
    ))
  }

  empty <- data.table(
    emergence = data.table::as.IDate(NA),
    AUDPC = NA_real_
  )
  if (!length(Filter(Negate(is.null), pieces))) {
    return(data.table::setkeyv(empty, c("emergence", "AUDPC")))
  }

  res <- data.table::rbindlist(
    lapply(pieces, function(x) x %||% empty),
    use.names = TRUE
  )
  unique(data.table::setkeyv(res, c("emergence", "AUDPC")))
}
