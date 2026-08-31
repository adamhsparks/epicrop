#' Retrieve internal parameter set for a crop/disease combination
#'
#' @param crop Character. One of `names(.epi_params)`, e.g. "rice",
#'   "rice_modified_kim", "wheat".
#' @param disease Character. One of `names(.epi_params[[crop]])`.
#'
#' @return A named list of parameters (`RcA`, `RcT`, `RRG`, and either `RRS`
#'   or `RRPSN` depending on crop). Matrix-valued entries are 2-column
#'   (`x`, `y`) interpolation tables; scalar entries are constants.
#' @keywords internal
#' @dev
epi_get_params <- function(crop, disease) {
  crop <- tolower(crop)
  disease <- tolower(disease)

  if (!crop %in% names(.epi_params)) {
    cli::cli_abort(
      c(
        "Unknown crop: {.val {crop}}.",
        "i" = "Available crops: {.val {names(.epi_params)}}."
      )
    )
  }

  crop_params <- .epi_params[[crop]]

  if (!disease %in% names(crop_params)) {
    cli::cli_abort(
      c(
        "Unknown disease {.val {disease}} for crop {.val {crop}}.",
        "i" = "Available diseases: {.val {names(crop_params)}}."
      )
    )
  }

  crop_params[[disease]]
}

#' Evaluate a parameter that may be a constant or an interpolation table
#'
#' Wraps [stats::approx()] so that scalar parameters (e.g. a constant `RRG`)
#' and matrix parameters (e.g. a temperature-dependent `RcT` table) can be
#' queried through the same interface.
#'
#' @param param Either a numeric scalar, or a 2-column matrix/data.frame with
#'   columns `x` (driver value) and `y` (modifier or rate value), as stored in
#'   `.epi_params`.
#' @param at Numeric. The driver value(s) to evaluate at (e.g. current
#'   temperature or DVS). Ignored if `param` is a scalar.
#' @param rule Passed to [stats::approx()]; default `2` extends the boundary
#'   value beyond the table's range rather than returning `NA`.
#'
#' @return Numeric value(s), same length as `at` (or length 1 for a scalar
#'   `param`).
#' @keywords internal
#' @dev
epi_eval_param <- function(param, at, rule = 2L) {
  if (is.matrix(param) || is.data.frame(param)) {
    stats::approx(x = param[, "x"], y = param[, "y"], xout = at, rule = rule)$y
  } else if (is.numeric(param) && length(param) == 1L) {
    rep(param, length(at))
  } else {
    cli::cli_abort(
      "{.arg param} must be a numeric scalar or a 2-column (x, y) matrix."
    )
  }
}
