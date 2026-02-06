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
#' For matrix input, uses fast linear interpolation that exactly matches
#' [stats::approx()] with rule = 2 (constant extrapolation at boundaries).
#' For function input, applies function to `age_vec` and clamps negative
#' values to 0.
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @dev
.interp <- function(param, age_vec) {
  # Handle NULL
  if (is.null(param)) {
    cli::cli_abort("Parameter is NULL", call = rlang::caller_env())
  }

  # Handle functions (check FIRST before length checks)
  if (is.function(param)) {
    result <- tryCatch(
      param(age_vec),
      error = function(e) {
        cli::cli_abort(
          c(
            "Function evaluation failed",
            "x" = conditionMessage(e)
          ),
          call = rlang::caller_env()
        )
      }
    )
    result[result < 0] <- 0
    return(result)
  }

  # Handle scalars
  if (length(param) == 1L && is.numeric(param)) {
    return(rep(param, length(age_vec)))
  }

  # Handle data.frames and matrices
  if (is.data.frame(param) || is.matrix(param)) {
    return(.interp_table(param, age_vec))
  }

  # Handle vectors - MATCH SECOND VERSION (exact length match only)
  if (is.numeric(param) && length(param) > 1L) {
    if (length(param) != length(age_vec)) {
      cli::cli_abort(
        c(
          "Vector length mismatch:",
          "x" = "Got {length(param)}, expected {length(age_vec)}"
        ),
        call = rlang::caller_env()
      )
    }
    return(param)
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

#' Interpolate from lookup table (matrix or data.frame) with digest memoisation
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
        "Lookup table must have exactly 2 columns [age, value].",
        "x" = "Got {ncol(param)} column{?s}."
      ),
      call = rlang::caller_env()
    )
  }

  # Validate numeric content
  if (!is.numeric(param[, 1]) || !is.numeric(param[, 2])) {
    cli::cli_abort(
      c(
        "Lookup table must contain numeric values",
        "x" = "Got {.cls {class(param[, 1])}} in first column or
        {.cls {class(param[, 2])}} in second column"
      ),
      call = rlang::caller_env()
    )
  }

  # Check for empty or NA values
  if (nrow(param) == 0L) {
    cli::cli_abort(
      "Lookup table must have at least 1 row",
      call = rlang::caller_env()
    )
  }

  if (anyNA(param)) {
    cli::cli_warn(
      c(
        "Lookup table contains NA values",
        "i" = "Removing rows with NA"
      ),
      call = rlang::caller_env()
    )
    param <- param[!is.na(param[, 1L]) & !is.na(param[, 2L]), , drop = FALSE]

    if (nrow(param) == 0L) {
      cli::cli_abort(
        "No valid rows after removing NAs",
        call = rlang::caller_env()
      )
    }
  }

  # Use content-based memoised fast interpolation
  .memo_fast_interp_approx_digest(param, age_vec)
}

#' Fast linear interpolation matching stats::approx()
#'
#' Vectorized linear interpolation that produces identical results to
#' stats::approx(method = "linear", rule = 2) but with better performance
#' for repeated calls.
#'
#' @param mat Two-column numeric matrix \[x, y\]
#' @param xout Vector of x values to interpolate at
#' @returns Vector of interpolated y values (identical to `stats::approx()`
#' output)
#'
#' @details
#' Automatically sorts by x column if unsorted, handles duplicates using
#' `approx()`'s tie handling (keeps first value), and uses constant
#' extrapolation at boundaries (rule = 2).
#'
#' @autoglobal
#' @dev
.fast_interp_approx_internal <- function(mat, xout) {
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

  # Sort if needed (approx requires sorted x)
  if (is.unsorted(x)) {
    ord <- order(x)
    x <- x[ord]
    y <- y[ord]
  }

  # Handle duplicates - approx() uses first occurrence for ties
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

  # Find intervals - matches approx() behavior
  # findInterval with left.open = FALSE matches approx()'s bin assignment
  i <- findInterval(xout, x, left.open = FALSE)

  # Initialize result with NA (like approx does internally)
  result <- numeric(length(xout))

  # Handle left extrapolation (xout < min(x)) - use first y value
  left_extrap <- i == 0L
  if (any(left_extrap)) {
    result[left_extrap] <- y[1L]
  }

  # Handle right extrapolation (xout >= max(x)) - use last y value
  right_extrap <- i >= n
  if (any(right_extrap)) {
    result[right_extrap] <- y[n]
  }

  # Handle interpolation (values within range)
  interp_idx <- !left_extrap & !right_extrap
  if (any(interp_idx)) {
    i_interp <- i[interp_idx]
    xout_interp <- xout[interp_idx]

    # Linear interpolation formula (same as approx)
    x0 <- x[i_interp]
    x1 <- x[i_interp + 1L]
    y0 <- y[i_interp]
    y1 <- y[i_interp + 1L]

    # Interpolation weight
    w <- (xout_interp - x0) / (x1 - x0)

    # Linear interpolation: y = y0 + w * (y1 - y0)
    result[interp_idx] <- y0 + w * (y1 - y0)
  }

  return(result)
}

#' Memoised wrapper for interpolation using content-based hashing
#' @autoglobal
#' @dev
.memo_fast_interp_approx_digest <- memoise::memoise(
  function(mat, xout) {
    mat_hash <- digest::digest(as.numeric(mat))
    xout_hash <- digest::digest(as.numeric(xout))
    # Only used for cache key; actual computation does not use hashes
    .fast_interp_approx_internal(mat, xout)
  }
)
