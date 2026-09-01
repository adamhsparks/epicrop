#' Validate Weather Data
#' @autoglobal
#' @dev
.validate_weather <- function(wth) {
  if (!rlang::inherits_any(wth, "epicrop.wth")) {
    cli::cli_abort(
      c(
        "{.var wth} must be an object of class {.cls epicrop.wth}.",
        i = "Use {.fn get_wth} or {.fn format_wth} to create one."
      ),
      call = rlang::caller_env()
    )
  }

  required <- c("YYYYMMDD", "TEMP", "RAIN")
  missing <- setdiff(required, names(wth))

  if (length(missing) > 0L) {
    cli::cli_abort(
      c(
        "Missing required columns in {.var wth}:",
        x = "{.val {missing}}"
      ),
      call = rlang::caller_env()
    )
  }

  invisible(TRUE)
}

#' Validate scalar parameters
#' @autoglobal
#' @dev
.validate_scalars <- function(
  onset,
  duration,
  rhlim,
  rainlim,
  H0,
  RcOpt,
  Sx,
  a,
  steps_per_day
) {
  .validate_param_range(
    onset,
    "onset",
    min = 0,
    max = Inf,
    exclusive_min = FALSE
  )
  .validate_param_range(
    duration,
    "duration",
    min = 1,
    max = Inf,
    exclusive_min = FALSE
  )
  .validate_param_range(rhlim, "rhlim", min = 0, max = 100)
  .validate_param_range(
    rainlim,
    "rainlim",
    min = 0,
    max = Inf,
    exclusive_min = FALSE
  )
  .validate_param_range(H0, "H0", min = 0, max = Inf, exclusive_min = TRUE)
  .validate_param_range(
    RcOpt,
    "RcOpt",
    min = 0,
    max = Inf,
    exclusive_min = FALSE
  )
  .validate_param_range(Sx, "Sx", min = 0, max = Inf, exclusive_min = TRUE)
  .validate_param_range(
    steps_per_day,
    "steps_per_day",
    min = 1,
    max = Inf,
    exclusive_min = FALSE
  )

  # Validate 'a' separately since it can be scalar, vector, matrix, or function
  if (is.matrix(a)) {
    # For matrices (lookup tables), only validate the second column (values)
    if (ncol(a) >= 2 && any(a[, 2] < 1)) {
      cli::cli_abort(
        "{.var a} values must be >= 1, got minimum value {min(a[, 2])}"
      )
    }
  } else if (is.numeric(a)) {
    # For scalars/vectors, validate all values
    if (any(a < 1)) {
      cli::cli_abort(
        "{.var a} must be >= 1, got minimum value {min(a)}"
      )
    }
  } else if (!is.function(a)) {
    cli::cli_abort(
      "{.var a} must be numeric, matrix, or function, got {.cls {class(a)}}"
    )
  }

  invisible(TRUE)
}

#' Validate Parameter Range
#' @param value Parameter value
#' @param name Parameter name
#' @param min Minimum value (inclusive unless exclusive_min = TRUE)
#' @param max Maximum value (inclusive unless exclusive_max = TRUE)
#' @param exclusive_min If TRUE, value must be > min (not >=)
#' @param exclusive_max If TRUE, value must be < max (not <=)
#' @autoglobal
#' @dev
.validate_param_range <- function(
  value,
  name,
  min = NULL,
  max = NULL,
  exclusive_min = FALSE,
  exclusive_max = FALSE
) {
  if (!is.null(min)) {
    if (exclusive_min && value <= min) {
      cli::cli_abort(
        "{.var {name}} must be > {min}, got {.val {value}}",
        call = rlang::caller_env(2)
      )
    } else if (!exclusive_min && value < min) {
      cli::cli_abort(
        "{.var {name}} must be >= {min}, got {.val {value}}",
        call = rlang::caller_env(2)
      )
    }
  }

  if (!is.null(max)) {
    if (exclusive_max && value >= max) {
      cli::cli_abort(
        "{.var {name}} must be < {max}, got {.val {value}}",
        call = rlang::caller_env(2)
      )
    } else if (!exclusive_max && value > max) {
      cli::cli_abort(
        "{.var {name}} must be <= {max}, got {.val {value}}",
        call = rlang::caller_env(2)
      )
    }
  }

  invisible(TRUE)
}

#' Validate Wetness Parameters
#' @autoglobal
#' @dev
.validate_wetness_params <- function(simple_wetness, wth, RcW) {
  if (isFALSE(simple_wetness)) {
    missing_temp <- c("TMIN", "TMAX")[!c("TMIN", "TMAX") %in% names(wth)]

    if (length(missing_temp) > 0L) {
      cli::cli_abort(
        c(
          "{.var simple_wetness} is {.code FALSE} but weather data is missing
            {.val {missing_temp}}.",
          i = "Set {.var simple_wetness} to {.code TRUE} or add
            {.val {missing_temp}} to {.var wth}."
        ),
        call = rlang::caller_env()
      )
    }

    if (!is.null(RcW)) {
      .validate_RcW_matrix(RcW)
    }
  }

  return(invisible(TRUE))
}

#' Validate RcW matrix
#' @autoglobal
#' @dev
.validate_RcW_matrix <- function(RcW) {
  if (!is.numeric(RcW)) {
    cli::cli_abort(
      "{.var RcW} must be numeric.",
      call = rlang::caller_env()
    )
  }

  if (!is.matrix(RcW) || ncol(RcW) != 2L) {
    cli::cli_abort(
      "{.var RcW} must be a two-column matrix (hours: 0-24, modifiers: 0-1).",
      call = rlang::caller_env()
    )
  }

  .validate_param_range(max(RcW[, 1]), "RcW hours (column 1)", max = 24)
  .validate_param_range(min(RcW[, 1]), "RcW hours (column 1)", min = 0)
  .validate_param_range(max(RcW[, 2]), "RcW modifiers (column 2)", max = 1)
  .validate_param_range(min(RcW[, 2]), "RcW modifiers (column 2)", min = 0)

  return(invisible(TRUE))
}

#' Check if Value is a Single Boolean
#'
#' @param x Value to check
#' @returns TRUE if x is exactly TRUE or FALSE, FALSE otherwise (including NA)
#' @autoglobal
#' @dev
.is_boolean <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

#' Validates Dates, Normally the Emergence Date
#' @param x a date input (character, Date, IDate, POSIXct).
#' @return A validated IDate object.
#' @dev
#' @autoglobal

.validate_emergence <- function(x) {
  if (rlang::inherits_any(x, c("Date", "IDate", "POSIXt"))) {
    return(data.table::as.IDate(x))
  }

  if (is.character(x)) {
    if (.is_ambiguous_numeric_date(x)) {
      cli::cli_abort(
        c(
          x = "{.var emergence} is an ambiguous numeric date.",
          i = "Use an unambiguous format such as {.val 2000-07-01}
            {.val 31/01/2000}, or {.val 07/31/2000}."
        ),
        call = rlang::caller_env()
      )
    }

    formats <- c(
      "%Y-%m-%d",
      "%Y/%m/%d",
      "%d-%m-%Y",
      "%d/%m/%Y",
      "%m-%d-%Y",
      "%m/%d/%Y",
      "%d %b %Y",
      "%d %B %Y",
      "%b %d %Y",
      "%B %d %Y"
    )

    for (fmt in formats) {
      d <- suppressWarnings(data.table::as.IDate(x, format = fmt))

      if (
        length(d) == 1L &&
          !is.na(d) &&
          identical(format(d, format = fmt), x)
      ) {
        return(d)
      }
    }

    cli::cli_abort(
      "{.var emergence} is not in a valid date format.",
      call = rlang::caller_env()
    )
  }

  cli::cli_abort(
    paste(
      "{.var emergence} must be a Date, IDate, POSIXt, or character,",
      "not {.cls {class(x)}}."
    ),
    call = rlang::caller_env()
  )
}

#' Identify ambiguous numeric date strings
#'
#' Detects character date values that can be interpreted as either
#' day-month-year or month-day-year because both the first and second
#' numeric components are less than or equal to 12.
#'
#' Ambiguous dates should not be parsed automatically because their
#' intended meaning cannot be determined reliably. For example,
#' `"07/01/2000"` could represent either 7 January 2000 or
#' 1 July 2000.
#'
#' @param x A character scalar containing a potential date value.
#'
#' @return A single logical value. Returns `TRUE` if `x` is an ambiguous
#'   numeric date of the form `dd/mm/yy`, `mm/dd/yy`, `dd-mm-yy`, or
#'   `mm-dd-yy` where both the first and second components are less than
#'   or equal to 12. Returns `FALSE` otherwise.
#'
#' @examples
#' .is_ambiguous_numeric_date("07/01/2000")
#' # TRUE
#'
#' .is_ambiguous_numeric_date("01-07-2000")
#' # TRUE
#'
#' .is_ambiguous_numeric_date("31/01/2000")
#' # FALSE
#'
#' .is_ambiguous_numeric_date("07/31/2000")
#' # FALSE
#'
#' .is_ambiguous_numeric_date("2000-07-01")
#' # FALSE
#'
#' @keywords internal
#' @dev
.is_ambiguous_numeric_date <- function(x) {
  parts <- strsplit(x, "[-/]")[[1L]]

  if (length(parts) != 3L) {
    return(FALSE)
  }

  if (nchar(parts[[3L]]) != 4L) {
    return(FALSE)
  }

  first <- suppressWarnings(as.integer(parts[[1L]]))
  second <- suppressWarnings(as.integer(parts[[2L]]))

  !is.na(first) &&
    !is.na(second) &&
    first <= 12L &&
    second <= 12L
}
