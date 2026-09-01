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
  if (is.null(param)) {
    cli::cli_abort("Parameter is NULL", call = rlang::caller_env())
  }
  if (is.function(param)) {
    return(.interp_fn(param, age_vec))
  }
  if (length(param) == 1L && is.numeric(param)) {
    return(rep(param, length(age_vec)))
  }
  if (is.data.frame(param) || is.matrix(param)) {
    return(.interp_table(param, age_vec))
  }
  if (is.numeric(param) && length(param) > 1L) {
    return(.interp_vec(param, age_vec))
  }
  cli::cli_abort(
    c(
      "Parameter must be a scalar, matrix, vector, or function",
      x = "Got {.cls {class(param)}} of length {length(param)}"
    ),
    call = rlang::caller_env()
  )
}

.interp_fn <- function(param, age_vec) {
  result <- tryCatch(
    param(age_vec),
    error = function(e) {
      cli::cli_abort(
        c("Function evaluation failed", x = conditionMessage(e)),
        call = rlang::caller_env()
      )
    }
  )
  result[result < 0] <- 0
  result
}

.interp_vec <- function(param, age_vec) {
  if (length(param) != length(age_vec)) {
    cli::cli_abort(
      c(
        "Vector length mismatch:",
        x = "Got {length(param)}, expected {length(age_vec)}"
      ),
      call = rlang::caller_env()
    )
  }
  param
}

.interp_table <- function(param, age_vec) {
  if (is.data.frame(param)) {
    param <- as.matrix(param)
  }
  .validate_lookup(param)
  param <- .drop_na_rows(param)
  .memo_fast_interp_approx_digest(param, age_vec)
}

.validate_lookup <- function(param) {
  if (ncol(param) != 2L) {
    cli::cli_abort(
      c(
        "Lookup table must have exactly 2 columns [age, value].",
        x = "Got {ncol(param)} column{?s}."
      ),
      call = rlang::caller_env()
    )
  }
  if (!is.numeric(param[, 1]) || !is.numeric(param[, 2])) {
    cli::cli_abort(
      c(
        "Lookup table must contain numeric values",
        x = "Got {.cls {class(param[, 1])}} in col 1 or
               {.cls {class(param[, 2])}} in col 2"
      ),
      call = rlang::caller_env()
    )
  }
  if (nrow(param) == 0L) {
    cli::cli_abort(
      "Lookup table must have at least 1 row",
      call = rlang::caller_env()
    )
  }
}

.drop_na_rows <- function(param) {
  if (!anyNA(param)) {
    return(param)
  }
  cli::cli_warn(
    c("Lookup table contains NA values", i = "Removing rows with NA"),
    call = rlang::caller_env()
  )
  param <- param[!is.na(param[, 1L]) & !is.na(param[, 2L]), , drop = FALSE]
  if (nrow(param) == 0L) {
    cli::cli_abort(
      "No valid rows after removing NAs",
      call = rlang::caller_env()
    )
  }
  param
}

.fast_interp_approx_internal <- function(mat, xout) {
  if (length(xout) == 0L) {
    return(numeric(0))
  }
  xy <- .prep_xy(mat)
  x <- xy$x
  y <- xy$y
  n <- length(x)
  if (n == 1L) {
    return(rep(y[1L], length(xout)))
  }
  .interpolate(x, y, n, xout)
}

.prep_xy <- function(mat) {
  x <- mat[, 1L]
  y <- mat[, 2L]
  if (length(x) == 1L) {
    return(list(x = x, y = y))
  }
  if (is.unsorted(x)) {
    ord <- order(x)
    x <- x[ord]
    y <- y[ord]
  }
  if (any(dup <- duplicated(x))) {
    x <- x[!dup]
    y <- y[!dup]
  }
  list(x = x, y = y)
}

.interpolate <- function(x, y, n, xout) {
  i <- findInterval(xout, x, left.open = FALSE)
  result <- numeric(length(xout))

  left <- i == 0L
  if (any(left)) {
    result[left] <- y[1L]
  }
  right <- i >= n
  if (any(right)) {
    result[right] <- y[n]
  }

  mid <- !left & !right
  if (any(mid)) {
    im <- i[mid]
    xm <- xout[mid]
    w <- (xm - x[im]) / (x[im + 1L] - x[im])
    result[mid] <- y[im] + w * (y[im + 1L] - y[im])
  }
  result
}

#' Memoised Wrapper for Fast Interpolation with Content-Based Hashing
#'
#' A memoised version of linear interpolation that caches results based on
#' the content (values) of the input matrix and x-coordinates, not their
#' object identity. Uses [digest::digest()] to compute content hashes,
#' enabling cache hits across function calls even when inputs are
#' reconstructed.
#'
#' @param mat A numeric 2-column matrix with x-values (column 1) and
#'   y-values (column 2) defining the lookup table.
#' @param xout Numeric vector of x-coordinates to interpolate at.
#'
#' @returns A numeric vector of interpolated y-values, computed via
#'   `.fast_interp_approx_internal()`.
#'
#' @details
#' **Caching strategy:**
#' * Converts `mat` to numeric vector and computes SHA1 hash via
#'   [digest::digest()]
#' * Converts `xout` to numeric vector and computes SHA1 hash
#' * Combines hashes as memo key; cache hit if both match previous call
#' * Fallback to `.fast_interp_approx_internal()` if miss
#'
#' **Performance:** Useful when:
#' * The same lookup tables and x-coordinates are reused across many calls
#' * Object identity changes but values remain constant
#' * Interpolation cost is high relative to hashing overhead
#'
#' **Limitations:**
#' * Hashing overhead (~1-10 µs per call) may exceed interpolation time
#'   for small inputs
#' * Memo cache grows unbounded (no size limit by default)
#' * Not thread-safe with default memoise settings
#'
#' @seealso
#' * [memoise::memoise()] for memoisation mechanics
#' * [digest::digest()] for content-based hashing
#' * `.fast_interp_approx_internal()` for the underlying interpolation
#'
#' @importFrom memoise memoise
#' @importFrom digest digest
#' @autoglobal
#' @dev
.memo_fast_interp_approx_digest <- memoise::memoise(
  function(mat, xout) {
    mat_hash <- digest::digest(as.numeric(mat))
    xout_hash <- digest::digest(as.numeric(xout))
    .fast_interp_approx_internal(mat, xout)
  }
)
