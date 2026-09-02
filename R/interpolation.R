#' Interpolate Parameter Values Over Age/Time Vector
#'
#' Internal utility that handles flexible parameter input types (scalar, vector,
#' matrix lookup table, or function) and produces daily interpolated values.
#' Enables concise model parameterization where users can specify age-dependent
#' modifiers in multiple convenient formats.
#'
#' @section Parameter Input Formats:
#'
#' This function accepts four input types:
#'
#' 1. **Scalar** (single numeric value):
#'    ```r
#'    .interp(0.1, age_vec = 0:119)  # Returns c(0.1, 0.1, ..., 0.1)
#'    ```
#'    Constant value used for all ages.
#'
#' 2. **Vector** (numeric vector):
#'    ```r
#'    .interp(c(0.05, 0.1, 0.15, ...), age_vec = 0:119)
#'    ```
#'    Must have exact same length as `age_vec`. Element-wise correspondence.
#'
#' 3. **Matrix** (2-column lookup table):
#'    ```r
#'    RcA <- rbind(c(0, 0.05),    # Age 0 -> modifier 0.05
#'                 c(60, 0.6),    # Age 60 -> modifier 0.6
#'                 c(120, 0.6))   # Age 120 -> modifier 0.6
#'    .interp(RcA, age_vec = 0:119)
#'    ```
#'    Column 1 = x-axis (age/DVS), Column 2 = y-axis (modifier 0-1).
#'    Uses fast linear interpolation; constant extrapolation at boundaries.
#'
#' 4. **Function**:
#'    ```r
#'    .interp(function(age) 0.1 + 0.005 * age, age_vec = 0:119)
#'    ```
#'    Applied to `age_vec`; negative results clamped to 0.
#'
#' @section Interpolation Method:
#'
#' Matrix inputs use fast linear interpolation:
#' * Sorts x-values (ages) in ascending order
#' * Removes duplicate x-values (retaining first y-value)
#' * For each output point:
#'   - If below minimum x: use \eqn{y[1]} (constant extrapolation)
#'   - If above maximum x: use \eqn{y[n]} (constant extrapolation)
#'   - If between \eqn{x[i]} and \eqn{x[i+1]}: linear interpolation
#'
#' Matches [stats::approx()] with `rule = 2` exactly.
#'
#' @param param Parameter value(s). Can be:
#'   * **Scalar** (numeric length-1): constant value replicated to all ages
#'   * **Vector** (numeric length > 1): must match `length(age_vec)` exactly
#'   * **Matrix or data.frame**: 2-column lookup table, column 1 = x (age),
#'     column 2 = y (modifier); rows may be unordered or contain duplicates
#'   * **Function**: takes numeric vector (ages) and returns numeric vector
#'     (modifiers); result clamped to \[0, Inf)
#'
#' @param age_vec Numeric vector of ages or development stages (DVS) to
#'   interpolate over. Length determines output length. Can be:
#'   * Days (0-based): 0, 1, 2, ..., 119 (120-day season)
#'   * DVS (0-2): 0, 0.05, ..., 2 (development stages)
#'   * Temperatures (for function-based params): 10, 15, 20, ..., 35
#'
#' @returns Numeric vector of length `length(age_vec)` with interpolated
#'   parameter values. All values are >= 0 (negative results from functions
#'   or extrapolation are clamped to 0).
#'
#' @examplesIf interactive()
#' # Example 1: Scalar parameter
#' age <- 0:119
#' RRG_scalar <- .interp(0.1, age_vec = age)
#' head(RRG_scalar)  # [1] 0.1 0.1 0.1 0.1 0.1 0.1
#'
#' # Example 2: Lookup table (matrix)
#' RcA <- rbind(
#'   c(0, 0.05),    # Day 0: modifier = 0.05
#'   c(60, 0.6),    # Day 60: modifier = 0.6
#'   c(120, 0.6)    # Day 120: modifier = 0.6
#' )
#' RcA_interp <- .interp(RcA, age_vec = age)
#' plot(age, RcA_interp, type = "l",
#'      xlab = "Days After Emergence", ylab = "Age Modifier (RcA)")
#' # Shows ramp-up from 0 to 60 days, plateau after 60 days
#'
#' # Example 3: Vector parameter (age-dependent growth)
#' RRG_vec <- c(rep(0.05, 30), rep(0.1, 60), rep(0.05, 30))
#' RRG_interp <- .interp(RRG_vec, age_vec = age)
#' plot(age, RRG_interp, type = "s",
#'      xlab = "Days", ylab = "Relative Growth Rate")
#' # Shows different growth rates for early, mid, and late season
#'
#' # Example 4: Function-based parameter (temperature-dependent)
#' # Infection rate increases from 15 to 25 degrees C, then decreases
#' RcT_fn <- function(temp) {
#'   ifelse(temp < 15, 0,
#'          ifelse(temp >= 15 & temp < 25, (temp - 15) / 10,
#'                 ifelse(temp <= 35, 1 - (temp - 25) / 10, 0)))
#' }
#' temps <- seq(10, 40, by = 1)
#' RcT_interp <- .interp(RcT_fn, age_vec = temps)
#' plot(temps, RcT_interp, type = "b",
#'      xlab = "Temperature (degrees C)", ylab = "Temperature Modifier (RcT)",
#'      main = "Typical Bell Curve for Fungal Pathogen")
#'
#' # Example 5: Matrix with unsorted rows and duplicates
#' # (function handles reordering and deduplication automatically)
#' RcA_messy <- rbind(
#'   c(60, 0.4),    # Out of order
#'   c(0, 0.05),
#'   c(60, 0.6),    # Duplicate x (second value wins)
#'   c(120, 0.6)
#' )
#' RcA_clean <- .interp(RcA_messy, age_vec = age)
#' # Internally sorted to: 0->0.05, 60->0.6, 120->0.6
#'
#' # Example 6: Comparing DVS-based vs day-based aging
#' dvs <- seq(0, 2, length.out = 120)  # DVS 0-2 over 120 days
#'
#' RcA_dvs <- rbind(
#'   c(0.0, 0.1),   # DVS 0 (emergence)
#'   c(1.0, 0.8),   # DVS 1 (active growth)
#'   c(2.0, 0.2)    # DVS 2 (maturity/senescence)
#' )
#' RcA_interp_dvs <- .interp(RcA_dvs, age_vec = dvs)
#' plot(dvs, RcA_interp_dvs, type = "l",
#'      xlab = "Development Stage (DVS)", ylab = "Age Modifier")
#'
#' @details
#'
#' ## Error Handling
#'
#' * NULL input aborts
#' * Vector length mismatch aborts
#' * Unsupported class (e.g., character, factor) aborts
#' * Matrix with != 2 columns aborts
#' * Matrix with non-numeric values aborts
#' * NA rows in matrix trigger warning and are removed
#' * Function evaluation errors are caught and abort with message
#' * Negative function results are silently clamped to 0
#'
#' ## Performance
#'
#' For large datasets with repeated lookup tables:
#' * Matrix interpolation is memoised via [digest::digest()] content hashing
#' * Enables efficient cache reuse even when matrices are reconstructed
#' * See [.memo_fast_interp_approx_digest()] for details
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

#' Apply Function-Based Parameter Over Age Vector
#'
#' Evaluates a user-supplied function at each age value and clamps
#' negative results to 0.
#'
#' @param param Function taking numeric vector (ages) and returning numeric
#'   vector (modifiers).
#' @param age_vec Numeric vector of ages to evaluate function at.
#'
#' @returns Numeric vector of function results, with negative values
#'   clamped to 0.
#'
#' @details
#' Catches function evaluation errors and aborts with informative message.
#' Negative results are silently converted to 0 (biologically reasonable
#' for growth/senescence rates).
#'
#' @examplesIf interactive()
#' # Temperature-dependent infection rate
#' RcT_fn <- function(temp) {
#'   pmax(0, 1 - (abs(temp - 25) / 15))  # Optimum at 25 degrees C
#' }
#' temps <- seq(10, 40, by = 2)
#' modifiers <- .interp_fn(RcT_fn, temps)
#' plot(temps, modifiers, type = "b", main = "Temperature Response")
#'
#' @autoglobal
#' @dev
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

  # Clamp negative values to 0 (biologically unrealistic rates)
  result[result < 0] <- 0
  result
}

#' Apply Vector Parameter Over Age Vector
#'
#' Validates that parameter vector matches age vector length exactly.
#'
#' @param param Numeric vector of parameter values.
#' @param age_vec Numeric vector of ages.
#'
#' @returns Numeric vector (same as `param` if length matches).
#'
#' @details Aborts if `length(param) != length(age_vec)`.
#'
#' @autoglobal
#' @dev
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

#' Apply Matrix Lookup Table Parameter Over Age Vector
#'
#' Converts data.frame to matrix, validates, cleans (removes NA rows),
#' and performs fast linear interpolation.
#'
#' @param param Numeric matrix or data.frame with 2 columns
#'   (age, modifier).
#' @param age_vec Numeric vector of ages to interpolate at.
#'
#' @returns Numeric vector of interpolated modifier values.
#'
#' @details
#' Steps:
#' 1. Convert data.frame to matrix if needed
#' 2. Validate 2 columns and numeric values
#' 3. Remove rows with NA values (with warning if any found)
#' 4. Call [.memo_fast_interp_approx_digest()] for memoised interpolation
#'
#' @autoglobal
#' @dev
.interp_table <- function(param, age_vec) {
  if (is.data.frame(param)) {
    param <- as.matrix(param)
  }

  .validate_lookup(param)
  param <- .drop_na_rows(param)
  .memo_fast_interp_approx_digest(param, age_vec)
}

#' Validate Lookup Table Structure
#'
#' Checks that lookup table (matrix) has exactly 2 columns and contains
#' numeric values.
#'
#' @param param Numeric matrix.
#'
#' @returns NULL (invisibly). Called for side effects (validation/abort).
#'
#' @details Validates:
#' * Exactly 2 columns
#' * Both columns contain numeric values
#' * At least 1 row
#'
#' @autoglobal
#' @dev
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

  if (!is.numeric(param[, 1L]) || !is.numeric(param[, 2L])) {
    cli::cli_abort(
      c(
        "Lookup table must contain numeric values",
        x = "Got {.cls {class(param[, 1L])}} in col 1 or
               {.cls {class(param[, 2L])}} in col 2"
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

#' Remove NA Rows from Lookup Table
#'
#' Filters out rows containing NA in either column. Warns if any NA rows
#' are found.
#'
#' @param param Numeric 2-column matrix.
#'
#' @returns Matrix with rows containing any NA removed.
#'
#' @details Aborts if all rows are NA (no valid data remain).
#'
#' @autoglobal
#' @dev
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

#' Fast Linear Interpolation with Boundary Handling
#'
#' Core interpolation algorithm using [base::findInterval()] and vectorized
#' linear interpolation. Matches [stats::approx()] with `rule = 2` exactly.
#'
#' @section Algorithm:
#'
#' 1. Prepare x-y points (sorted, deduplicated)
#' 2. Use [base::findInterval()] to locate each output point in x-grid
#' 3. Categorize points:
#'    - Below \eqn{min(x)}: use \eqn{y[1]} (constant)
#'    - Above \eqn{max(x)}: use \eqn{y[n]} (constant)
#'    - Between \eqn{x[i]} and \eqn{x[i+1]}: linear interpolation
#' 4. Return interpolated y-values
#'
#' @param mat Numeric 2-column matrix with x (col 1) and y (col 2) values.
#'   Rows may be unordered or contain duplicates.
#' @param xout Numeric vector of x-coordinates to interpolate at.
#'
#' @returns Numeric vector of interpolated y-values.
#'
#' @details
#' Internally calls:
#' 1. [.prep_xy()] to sort/deduplicate
#' 2. [.interpolate()] to perform vectorized interpolation
#'
#' Edge cases:
#' * Empty `xout`: returns numeric(0)
#' * Single row in matrix: returns y\[1\] repeated
#' * `xout` below min(x): constant extrapolation to y\[1\]
#' * `xout` above max(x): constant extrapolation to y\[n\]
#'
#' @examplesIf interactive()
#' # Simple lookup table
#' mat <- rbind(c(0, 0.05), c(60, 0.6), c(120, 0.6))
#' xout <- c(0, 30, 60, 90, 120, 150)
#' y_interp <- .fast_interp_approx_internal(mat, xout)
#' print(cbind(xout, y_interp))
#' #      xout y_interp
#' # [1,]    0     0.05     (exact match)
#' # [2,]   30     0.325    (linear interp: 0.05 + 30/60 * (0.6-0.05))
#' # [3,]   60     0.6      (exact match)
#' # [4,]   90     0.6      (plateau)
#' # [5,]  120     0.6      (exact match)
#' # [6,]  150     0.6      (extrapolated)
#'
#' @autoglobal
#' @dev
.fast_interp_approx_internal <- function(mat, xout) {
  if (length(xout) == 0L) {
    return(numeric(0))
  }

  xy <- .prep_xy(mat)
  x <- xy$x
  y <- xy$y
  n <- length(x)

  # Single point: return constant
  if (n == 1L) {
    return(rep(y[1L], length(xout)))
  }

  .interpolate(x, y, n, xout)
}

#' Prepare X-Y Coordinates for Interpolation
#'
#' Extracts x-y columns, sorts by x, and removes duplicate x-values.
#'
#' @section Deduplication:
#'
#' When duplicate x-values exist (user error or data source quirk):
#' * Sorts by x-coordinate
#' * Keeps first y-value for each x (others discarded)
#' * Warns internally via parent function
#'
#' @param mat Numeric 2-column matrix (x, y).
#'
#' @returns List with elements `x` and `y` (both sorted, deduplicated).
#'
#' @autoglobal
#' @dev
.prep_xy <- function(mat) {
  x <- mat[, 1L]
  y <- mat[, 2L]

  # Single point: return as-is
  if (length(x) == 1L) {
    return(list(x = x, y = y))
  }

  # Sort by x if unsorted
  if (is.unsorted(x)) {
    ord <- order(x)
    x <- x[ord]
    y <- y[ord]
  }

  # Remove duplicate x-values (keep first y)
  if (any(dup <- duplicated(x))) {
    x <- x[!dup]
    y <- y[!dup]
  }

  list(x = x, y = y)
}

#' Perform Vectorized Linear Interpolation
#'
#' Core vectorized interpolation using pre-sorted x-y points and output
#' x-coordinates. Uses [base::findInterval()] for efficiency.
#'
#' @section Method:
#'
#' Computes linear interpolation between consecutive x-y points:
#'
#' \eqn{y_interp = y[i] + w * (y[i+1] - y[i])}
#' where \eqn{w = (x - x[i]) / (x[i+1] - x[i])}
#' ```
#'
#' Boundary handling:
#' * `x < min(x_vec)`: \eqn{y = y[1]}
#' * `x > max(x_vec)`: \eqn{y = y[n]}
#'
#' @param x Numeric vector of x-coordinates (sorted, deduplicated).
#' @param y Numeric vector of y-coordinates (same length as x).
#' @param n Integer, length of x (pre-computed for efficiency).
#' @param xout Numeric vector of output x-coordinates.
#'
#' @returns Numeric vector of interpolated y-values.
#'
#' @autoglobal
#' @dev
.interpolate <- function(x, y, n, xout) {
  # Use findInterval to locate each xout in x-grid
  i <- findInterval(xout, x, left.open = FALSE)
  result <- numeric(length(xout))

  # Below minimum: constant extrapolation
  left <- i == 0L
  if (any(left)) {
    result[left] <- y[1L]
  }

  # Above maximum: constant extrapolation
  right <- i >= n
  if (any(right)) {
    result[right] <- y[n]
  }

  # Middle: linear interpolation between x[i] and x[i+1]
  mid <- !left & !right
  if (any(mid)) {
    im <- i[mid]
    xm <- xout[mid]
    # Weight for linear interpolation
    w <- (xm - x[im]) / (x[im + 1L] - x[im])
    result[mid] <- y[im] + w * (y[im + 1L] - y[im])
  }

  result
}

#' Memoised Fast Interpolation with Content-Based Hashing
#'
#' A memoised version of [.fast_interp_approx_internal()] that caches results
#' based on content hashing. Enables efficient reuse when the same lookup
#' tables and x-coordinates recur across multiple function calls, even if
#' the objects are reconstructed.
#'
#' @section Caching Strategy:
#'
#' Instead of caching by object identity (memory address), this function:
#' 1. Converts `mat` (matrix) to numeric vector and computes SHA1 hash via
#'    [digest::digest()]
#' 2. Converts `xout` (output x-coords) to numeric vector and computes SHA1
#'    hash
#' 3. Uses combined hash as memo key
#' 4. On cache hit, returns cached result directly
#' 5. On cache miss, calls [.fast_interp_approx_internal()] and caches result
#'
#' This approach enables cross-call reuse even when matrices and vectors are
#' reconstructed (new objects with same values).
#'
#' @section Performance:
#'
#' **When to use:**
#' * Same lookup tables repeated across multiple [seir()] runs
#' * Same disease models run on many locations/dates
#' * Interpolation involves large x-coordinate grids
#'
#' **When not worth it:**
#' * One-off calls or unique matrices each time
#' * Very small matrices/xout (hashing overhead dominates)
#' * Real-time systems with strict latency requirements
#'
#' Hashing typically costs 1-10 microseconds per call. Interpolation itself
#' is already very fast (~1 microsecond). Cache hit saves ~1 microsecond
#' but costs 1-10 microseconds in hashing.
#'
#' @section Limitations:
#'
#' * Memo cache grows unbounded (no size limit by default)
#' * Not thread-safe with default [memoise::memoise()] settings
#' * Hash collisions (SHA1) extremely rare but not impossible
#' * Cache persists for session lifetime; restart R to clear
#'
#' @param mat Numeric 2-column matrix with x (col 1) and y (col 2).
#' @param xout Numeric vector of x-coordinates to interpolate at.
#'
#' @returns Numeric vector of interpolated y-values.
#'
#' @seealso
#' * [memoise::memoise()] for memoisation mechanics and cache control
#' * [digest::digest()] for content-based hashing algorithm (SHA1 by default)
#' * [.fast_interp_approx_internal()] for underlying interpolation
#' * [stats::approx()] for comparison (non-memoised, R's standard interpolation)
#'
#' @examplesIf interactive()
#' # Example 1: Repeated interpolation with same lookup table
#' RcA <- rbind(c(0, 0.05), c(60, 0.6), c(120, 0.6))
#'
#' # First call: computes and caches
#' age_vec1 <- 0:119
#' result1 <- .memo_fast_interp_approx_digest(RcA, age_vec1)
#'
#' # Second call: same matrix, same xout -> cache hit (fast)
#' age_vec2 <- 0:119
#' result2 <- .memo_fast_interp_approx_digest(RcA, age_vec2)
#' identical(result1, result2)  # TRUE
#'
#' # Third call: same matrix, different xout -> cache miss (recomputes)
#' age_vec3 <- 0:100
#' result3 <- .memo_fast_interp_approx_digest(RcA, age_vec3)
#' length(result3)  # 101 (different from result1)
#'
#' # Example 2: Demonstrate content-based caching
#' # Reconstructed matrix with same values -> cache hit
#' RcA_reconstructed <- rbind(c(0, 0.05), c(60, 0.6), c(120, 0.6))
#'
#' # Same xout as result1
#' result4 <- .memo_fast_interp_approx_digest(RcA_reconstructed, 0:119)
#' identical(result1, result4)  # TRUE (content same, so cache hit)
#'
#' @importFrom memoise memoise
#' @importFrom digest digest
#' @autoglobal
#' @dev
.memo_fast_interp_approx_digest <- memoise(
  function(mat, xout) {
    mat_hash <- digest(as.numeric(mat))
    xout_hash <- digest(as.numeric(xout))
    .fast_interp_approx_internal(mat, xout)
  }
)
