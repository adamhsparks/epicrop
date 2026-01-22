#' Create an AUDPC data frame for locations and emergence dates
#' @param dt A `data.table` created by `run_epicrop_model()`.
#' @returns A [data.table::data.table()] suitable for mapping or other analysis
#'  with five columns: `lat`, `lon`, `AUDPC`, `location`, and `emergence`.
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
  dt <- dt[, .SD, .SDcols = c("lat", "lon", "AUDPC", "location", "emergence")]
  data.table::setkey(dt, location, emergence)
  return(unique(dt))
}
