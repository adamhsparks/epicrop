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
# ' @param strict Logical. If TRUE (default), abort via [cli::cli_abort()] when
#'   required year weather is missing; if FALSE, skip that date with
#'   [cli::cli_warn()].
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

  if (!rlang::inherits_any(emergence_dates, "epicrop.emerge")) {
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

  is_bundle <- rlang::inherits_any(wth_list, "epicrop.wth.bundle") ||
    !all(grepl("^\\d{4}$", rlang::names2(wth_list)))

  if (is_bundle) {
    site_keys <- rlang::names2(wth_list)
    site_labels <- sub("__.*$", "", site_keys)
    site_labels[!nzchar(site_labels)] <- paste0(
      "site",
      sprintf("%02d", seq_along(wth_list))
    )

    pieces <- Map(
      function(site_w, site_nm) {
        ans <- run_epicrop_model(
          model_fun = model_fun,
          emergence_dates = emergence_dates,
          wth_list = site_w,
          window_days = window_days,
          output = output,
          ...
        )
        ans[, site := site_nm]
        ans
      },
      wth_list,
      site_labels
    )

    return(data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE))
  }

  slicer <- .make_slicer(wth_list, as.integer(window_days))

  runner <- .make_runner(
    model_fun = model_fun,
    output = output,
    slicer = slicer,
    ...,
    window_days = as.integer(window_days)
  )

  # Always sequential
  applier <- function(x, fun) lapply(X = x, FUN = fun)

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
.make_slicer <- function(wth_list, window_days) {
  year_of <- function(id) as.integer(format(id, "%Y"))

  function(ed) {
    start <- ed
    end <- ed + (as.integer(window_days) - 1L)
    ys <- seq.int(year_of(start), year_of(end))

    .env <- rlang::current_env()

    segs <- vector("list", length(ys))
    n_seg <- 0L

    for (y in ys) {
      tab <- wth_list[[as.character(y)]]
      if (is.null(tab)) {
        cli::cli_warn(
          "Skipping {start}: no weather for required year {y}
          (window {start}–{end}).",
          .envir = .env
        )
        next
      }

      s <- max(start, data.table::as.IDate(sprintf("%d-01-01", y)))
      e <- min(end, data.table::as.IDate(sprintf("%d-12-31", y)))

      seg <- tab[`YYYYMMDD` %between% c(s, e)]
      if (nrow(seg)) {
        n_seg <- n_seg + 1L
        segs[[n_seg]] <- seg
      } else {
        cli::cli_warn(
          "Skipping {start}: no rows in year {y} within {s}–{e}.",
          .envir = .env
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
  dots <- rlang::dots_list(..., .named = TRUE)

  # Determine if user *supplied* duration (exact or unique partial)
  has_user_duration <- local({
    dn <- rlang::names2(dots)
    if (length(dn) && any(nzchar(dn))) {
      if ("duration" %in% dn) {
        TRUE
      } else {
        fmls <- rlang::fn_fmls_names(model_fun)
        if (is.null(fmls)) {
          FALSE
        } else {
          fml_no_dots <- setdiff(fmls, "...")
          # unique partial match to "duration"
          any(vapply(
            dn,
            function(nm) {
              if (!nzchar(nm)) {
                return(FALSE)
              }
              hit <- startsWith(fml_no_dots, nm)
              sum(hit) == 1L && identical(fml_no_dots[which(hit)], "duration")
            },
            logical(1)
          ))
        }
      }
    } else {
      # unnamed ... could bind by position; be conservative
      TRUE
    }
  })

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

    # Inject duration only if user didn't provide it
    extra <- if (!has_user_duration) {
      list(duration = as.integer(nrow(w_sub)))
    } else {
      list()
    }

    out <- rlang::exec(
      model_fun,
      wth = w_sub,
      emergence = ed,
      !!!dots,
      !!!extra
    )

    if (identical(output, "audpc")) {
      aud <- attr(out, "AUDPC", exact = TRUE)
      data.table::data.table(emergence = ed, AUDPC = aud)
    } else {
      out[, emergence := ed]
      out[, AUDPC := attr(out, "AUDPC", exact = TRUE)]
      out
    }
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
          AUDPC = NA_real_
        ))
      }
      # Coerce to data.table
      if (!data.table::is.data.table(x)) {
        x <- data.table::as.data.table(x)
      }

      # Keep only what we need for the compact result
      x[, .(emergence, AUDPC)]
    }

    # Handle totally empty inputs robustly
    if (length(pieces) == 0L) {
      res <- data.table::data.table(
        emergence = data.table::as.IDate(NA),
        AUDPC = NA_real_
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
