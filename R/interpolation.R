#' Interpolate parameter values
#'
#' Internal function to handle different parameter input types (scalar, vector,
#' matrix lookup table, or function) and interpolate to daily values.
#'
#' @param param Parameter value(s). Can be:
#'   * Scalar: single value used for all days
#'   * Vector: must match length of `age_vec`
#'   * Matrix: 2-column lookup table (column 1 = age, column 2 = modifier)
#'   * Function: function of age that returns modifier values
#' @param age_vec Numeric vector of ages (days or DVS) to interpolate over
#'
#' @returns Numeric vector of length `length(age_vec)` with interpolated values
#'
#' @details
#' For matrix input, uses fast linear interpolation with constant extrapolation
#' at boundaries. For function input, applies function to `age_vec` and clamps
#' negative values to 0.
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @keywords internal
#' @dev

.interp <- function(param, age_vec) {
  # Handle NULL
  if (is.null(param)) {
    cli::cli_abort("Parameter is NULL", call = rlang::caller_env())
  }

  # Handle functions
  if (is.function(param)) {
    result <- param(age_vec)
    return(pmax(0, result)) # Clamp negative values to zero
  }

  # Handle scalars
  if (length(param) == 1L && is.numeric(param)) {
    return(rep(param, length(age_vec)))
  }

  # Handle data.frames and matrices
  if (is.data.frame(param) || is.matrix(param)) {
    return(.interp_table(param, age_vec))
  }

  # Handle vectors
  if (is.numeric(param) && length(param) > 1L) {
    return(.interp_vector(param, age_vec))
  }

  # Unsupported type
  cli::cli_abort(
    c(
      "Parameter must be a scalar, matrix, vector, or function",
      "x" = "Got {.cls {class(param)}} of length {length(param)}"
    ),
    call = rlang::caller_env()
  )
}

#' Interpolate from lookup table (matrix or data.frame)
#' @autoglobal
#' @dev
.interp_table <- function(param, age_vec) {
  # Convert data.frame to matrix
  if (is.data.frame(param)) {
    param <- as.matrix(param)
  }

  # Validate structure
  if (ncol(param) != 2L) {
    cli::cli_abort(
      c(
        "Lookup table must have 2 columns [age, value]",
        "x" = "Got {ncol(param)} column{?s}"
      ),
      call = rlang::caller_env()
    )
  }

  # Validate numeric content
  if (!is.numeric(param[1, 1]) || !is.numeric(param[1, 2])) {
    cli::cli_abort(
      c(
        "Lookup table must contain numeric values",
        "x" = "Got {.cls {class(param[1, 1])}} in first column or
        {.cls {class(param[1, 2])}} in second column"
      ),
      call = rlang::caller_env()
    )
  }

  .fast_interp(param, age_vec)
}

#' Interpolate from numeric vector
#' @autoglobal
#' @dev
.interp_vector <- function(param, age_vec) {
  # If vector length matches age_vec, return as-is (pre-computed values)
  if (length(param) == length(age_vec)) {
    return(param)
  }

  # Check if it's a valid lookup table (even length for age/value pairs)
  if (length(param) %% 2L != 0L) {
    cli::cli_abort(
      c(
        "Vector length mismatch.",
        "i" = "Expected length {length(age_vec)} to match age vector, or even length for lookup table.",
        "x" = "Got length {length(param)}."
      ),
      call = rlang::caller_env()
    )
  }

  # Convert to matrix and interpolate
  mat <- matrix(param, ncol = 2L, byrow = TRUE)
  .fast_interp(mat, age_vec)
}

#' Fast linear interpolation
#'
#' Vectorized linear interpolation using findInterval(). Handles unsorted and
#' duplicate x values automatically. Faster than stats::approx() for repeated
#' calls with similar data.
#'
#' @param mat Two-column numeric matrix \[x, y\]
#' @param xout Vector of x values to interpolate at
#' @returns Vector of interpolated y values
#'
#' @details
#' Automatically sorts by x column if unsorted, removes duplicates (keeping
#' first occurrence), and uses constant extrapolation at boundaries.
#'
#' @autoglobal
#' @keywords internal
#' @dev
.fast_interp <- function(mat, xout) {
  if (length(xout) == 0L) {
    return(numeric(0))
  }

  x <- mat[, 1L]
  y <- mat[, 2L]
  n <- length(x)

  # Edge case: single row
  if (n == 1L) {
    return(rep(y[1L], length(xout)))
  }

  # Sort if needed (check is cheap)
  if (is.unsorted(x)) {
    ord <- order(x)
    x <- x[ord]
    y <- y[ord]
  }

  # Remove duplicates (keep first occurrence)
  dup <- duplicated(x)
  if (any(dup)) {
    x <- x[!dup]
    y <- y[!dup]
    n <- length(x)
  }

  # Re-check after deduplication
  if (n == 1L) {
    return(rep(y[1L], length(xout)))
  }

  # Find intervals (returns 0 for xout < min(x), n for xout >= max(x))
  i <- findInterval(xout, x, left.open = FALSE)

  # Clamp to valid range [1, n-1] for interpolation
  i <- pmax(1L, pmin(i, n - 1L))

  # Calculate interpolation fraction
  dx <- x[i + 1L] - x[i]
  frac <- (xout - x[i]) / dx

  # Linear interpolation
  result <- y[i] + frac * (y[i + 1L] - y[i])

  # Handle extrapolation (use edge values)
  result[xout <= x[1L]] <- y[1L]
  result[xout >= x[n]] <- y[n]

  return(result)
}
