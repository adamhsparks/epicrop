#' Interpolate Parameter Values
#'
#' Interpolates or evaluates parameter definitions to produce a vector of daily
#' values for use in the simulation.  Handles scalars, vectors, matrices, data
#' frames, and functions.
#'
#' @param def Parameter definition.  Can be:
#'   * Scalar numeric:  Replicated to length of `x`
#'   * Numeric vector: Must have same length as `x`
#'   * Matrix/data.frame: 2-column table \[x_values, y_values\] for linear
#'      interpolation
#'   * Function: Evaluated at each value of `x`
#' @param x Numeric vector of x-values at which to interpolate (e.g.,
#'     days, temperatures, DVS).
#'
#' @returns A numeric vector of length `length(x)` with interpolated/evaluated
#'    values.
#'
#' @details
#' For tables/matrices:
#' * First column is x-values (e.g., days, °C)
#' * Second column is y-values (modifier from 0 to 1)
#' * Linear interpolation between points
#' * Values outside range use first/last y-value (no extrapolation)
#'
#' For functions:
#' * Called with vector `x` as argument
#' * Must return numeric vector of same length
#' * Negative values are set to 0
#'
#' @autoglobal
#' @noRd
.interp <- function(def, x) {
  # Handle functions
  if (is.function(def)) {
    result <- tryCatch(
      def(x),
      error = function(e) {
        cli::cli_abort(c(
          "Function evaluation failed:",
          "x" = conditionMessage(e)
        ))
      }
    )
    return(pmax(0, as.numeric(result)))
  }

  # Handle numeric scalars and vectors
  if (is.numeric(def)) {
    if (length(def) == 1) {
      return(rep(def, length(x)))
    }
    if (length(def) == length(x)) {
      return(as.numeric(def))
    }
    cli::cli_abort(c(
      "Vector length mismatch:",
      "x" = "Got {.val {length(def)}}, expected {.val {length(x)}}"
    ))
  }

  # Handle tables/matrices
  if (is.matrix(def) || is.data.frame(def)) {
    if (ncol(def) != 2) {
      cli::cli_abort(c(
        "Table must have 2 columns:  [x, value]",
        "x" = "Got {.val {ncol(def)}} columns"
      ))
    }

    return(
      stats::approx(
        x = def[, 1],
        y = def[, 2],
        xout = x,
        method = "linear",
        yleft = def[1, 2],
        yright = def[nrow(def), 2]
      )$y
    )
  }

  cli::cli_abort("Unsupported parameter type: {.cls {class(def)}}")
}
