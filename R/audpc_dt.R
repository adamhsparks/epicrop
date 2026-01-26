#' Create an AUDPC data frame for locations and emergence dates
#' @param dt A `data.table` created by `run_epicrop_model()`.
#' @returns A [data.table::data.table()] suitable for mapping or other analysis.
#'  If `dt` is from `run_epicrop_model()` directly, the output will be a
#'  `data.table` with five columns: `lat`, `lon`, `AUDPC`, `location`, and
#'  `emergence`. If `dt` is from any of the helper functions or directly from
#'   `epiccrop_sim()`, the output will be a `data.table` with one column:
#'   `AUDPC`.
#' @autoglobal
#' @export

audpc_dt <- function(dt) {
  if (!rlang::inherits_any(x = dt, class = "epicrop.sim")) {
    cli::cli_abort(
      "{.var dt} must be an `epicrop.sim` object created by
      `run_epicrop_model()` or any of the other helper functions that create
      an `epicrop.sim` object."
    )
  }

  if (all(names(dt) %in% c("lat", "lon", "AUDPC", "location", "emergence"))) {
    dt <- dt[, .SD, .SDcols = c("lat", "lon", "AUDPC", "location", "emergence")]
    data.table::setkey(dt, location, emergence)
  } else {
    x <- get_audpc(dt)
    dt <- data.table::as.data.table(x)
    data.table::setnames(dt, "x", "AUCPC")
  }
  return(unique(dt))
}
