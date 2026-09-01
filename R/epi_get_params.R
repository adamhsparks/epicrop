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
