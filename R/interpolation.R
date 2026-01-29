#' Interpolate parameter values
#'
#' Internal function to handle different parameter input types (scalar, vector,
#' matrix lookup table, or function) and interpolate to daily values.
#'
#' @param param Parameter value(s). Can be:
#'   \itemize{
#'     \item Scalar:  single value used for all days
#'     \item Vector: must match length of `age_vec`
#'     \item Matrix: 2-column lookup table (column 1 = age, column 2 = modifier)
#'     \item Function: function of age that returns modifier values
#'   }
#' @param age_vec Numeric vector of ages (days or DVS) to interpolate over
#'
#' @returns Numeric vector of length `length(age_vec)` with interpolated values
#'
#' @details
#' For matrix input, uses linear interpolation via [stats::approx()] with
#' `rule = 2` (constant extrapolation at boundaries).
#'
#' For function input, applies function to `age_vec` and sets any negative
#' values to 0.
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @keywords internal
#' @dev
.interp <- function(param, age_vec) {
  # Function - apply to age_vec (check this FIRST before length checks)
  if (is.function(param)) {
    tryCatch(
      {
        result <- param(age_vec)
        # Set negative values to 0
        result[result < 0] <- 0
        return(result)
      },
      error = function(e) {
        cli::cli_abort(
          call = rlang::caller_env(),
          c(
            "Function evaluation failed:",
            "x" = conditionMessage(e)
          )
        )
      }
    )
  }

  # Matrix or data.frame - lookup table with interpolation
  if (is.matrix(param) || is.data.frame(param)) {
    if (ncol(param) != 2L) {
      cli::cli_abort(
        call = rlang::caller_env(),
        c(
          "Matrix/data.frame must have exactly 2 columns",
          "x" = "Got {ncol(param)} columns",
          "i" = "Column 1 = age values, Column 2 = modifier values"
        )
      )
    }

    # Check that values are numeric
    if (!is.numeric(param[, 1]) || !is.numeric(param[, 2])) {
      cli::cli_abort(
        call = rlang::caller_env(),
        "Both columns of lookup table must be numeric"
      )
    }

    # Linear interpolation with constant extrapolation
    interpolated <- stats::approx(
      x = param[, 1],
      y = param[, 2],
      xout = age_vec,
      method = "linear",
      rule = 2 # Use edge values for extrapolation
    )$y

    return(interpolated)
  }

  # Scalar - replicate for all ages
  if (length(param) == 1L) {
    return(rep(param, length(age_vec)))
  }

  # Vector - must match age_vec length
  if (is.vector(param) && !is.list(param) && length(param) > 1L) {
    if (length(param) != length(age_vec)) {
      cli::cli_abort(
        call = rlang::caller_env(),
        c(
          "Vector length mismatch:",
          "x" = "Got {length(param)}, expected {length(age_vec)}"
        )
      )
    }
    return(param)
  }

  # Unsupported type
  cli::cli_abort(
    call = rlang::caller_env(),
    c(
      "Unsupported parameter type:",
      "x" = "Got {class(param)[1]}",
      "i" = "Must be scalar, vector, matrix, data.frame, or function"
    )
  )
}
