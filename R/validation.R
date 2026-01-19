#' Validate All Simulation Inputs
#'
#' Central validation function for epicrop_sim() parameters
#' @autoglobal
#' @noRd
.validate_all_inputs <- function(
  wth,
  emergence,
  onset,
  duration,
  rhlim,
  rainlim,
  H0,
  I0,
  RcOpt,
  Sx,
  a,
  steps_per_day,
  simple_wetness,
  age_driver,
  RcW
) {
  # Validate weather data
  .validate_weather(wth)

  # Validate dates
  .validate_emergence(emergence)

  # Validate scalar parameters
  .validate_scalars(
    onset = onset,
    duration = duration,
    rhlim = rhlim,
    rainlim = rainlim,
    H0 = H0,
    RcOpt = RcOpt,
    Sx = Sx,
    a = a,
    steps_per_day = steps_per_day
  )

  # Validate boolean parameters
  if (!.is_boolean(simple_wetness)) {
    cli::cli_abort(
      "{.var simple_wetness} must be a Boolean ({.code TRUE/FALSE}) value.",
      call = rlang::caller_env()
    )
  }

  # Validate age_driver
  if (!age_driver %in% c("day", "dvs")) {
    cli::cli_abort(
      "{.var age_driver} must be 'day' or 'dvs', got {.val {age_driver}}",
      call = rlang::caller_env()
    )
  }

  # Validate wetness parameters
  .validate_wetness_params(simple_wetness, wth, RcW)

  invisible(TRUE)
}

#' Validate weather data
#' @autoglobal
#' @noRd
.validate_weather <- function(wth) {
  if (!inherits(wth, "epicrop.wth")) {
    cli::cli_abort(
      c(
        "{.var wth} must be an object of {.class epicrop.wth}.",
        "You can create your {.var wth} object using either ",
        "{.fn get_wth} to fetch weather data from the NASA POWER API ",
        "or format your own with {.fn format_wth}."
      ),
      call = rlang::caller_env()
    )
  }

  # Check required columns
  required <- c("YYYYMMDD", "TEMP", "RAIN")
  missing <- setdiff(required, names(wth))

  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Missing required columns in {.var wth}:",
      "x" = "{.val {missing}}"
    ))
  }

  invisible(TRUE)
}

#' Validate emergence date
#' @autoglobal
#' @noRd
.validate_emergence <- function(emergence) {
  .check_date(emergence)
  invisible(TRUE)
}

#' Validate scalar parameters
#' @autoglobal
#' @noRd
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
  # Check numeric
  params <- list(
    onset = onset,
    duration = duration,
    rhlim = rhlim,
    rainlim = rainlim,
    H0 = H0,
    RcOpt = RcOpt,
    Sx = Sx,
    a = a,
    steps_per_day = steps_per_day
  )

  non_numeric <- params[!sapply(params, is.numeric)]
  if (length(non_numeric) > 0) {
    cli::cli_abort(
      "{.var {names(non_numeric)}} must be numeric values.",
      call = rlang::caller_env()
    )
  }

  # Check non-negative
  if (H0 < 0) {
    cli::cli_abort("{.var H0} must be >= 0, got {.val {H0}}")
  }
  if (RcOpt < 0) {
    cli::cli_abort("{.var RcOpt} must be >= 0, got {.val {RcOpt}}")
  }
  if (rhlim < 0 || rhlim > 100) {
    cli::cli_abort("{.var rhlim} must be between 0 and 100, got {.val {rhlim}}")
  }
  if (rainlim < 0) {
    cli::cli_abort("{.var rainlim} must be >= 0, got {.val {rainlim}}")
  }

  # Check positive
  if (Sx <= 0) {
    cli::cli_abort("{.var Sx} must be > 0, got {.val {Sx}}")
  }
  if (duration <= 0) {
    cli::cli_abort("{.var duration} must be > 0, got {.val {duration}}")
  }
  if (onset < 1) {
    cli::cli_abort("{.var onset} must be >= 1, got {.val {onset}}")
  }
  if (steps_per_day < 1) {
    cli::cli_abort(
      "{.var steps_per_day} must be >= 1, got {.val {steps_per_day}}"
    )
  }

  # Check aggregation coefficient
  if (a < 1) {
    cli::cli_abort(
      c(
        "{.var a} cannot be set to less than 1.",
        "Valid aggregation values for {.var a} are from 1 to > 1."
      ),
      call = rlang::caller_env()
    )
  }

  # Warn about duration
  if (duration < 50) {
    cli::cli_warn("{.var duration} is < 50 days, is this intended?")
  }

  # Warn about steps_per_day
  if ((steps_per_day & (steps_per_day - 1)) != 0) {
    cli::cli_warn(c(
      "!" = "{.var steps_per_day} should be power of 2 for best accuracy",
      "i" = "Got {.val {steps_per_day}}, consider {.val 1}, {.val 2}, {.val 4}, or {.val 8}"
    ))
  }

  invisible(TRUE)
}

#' Validate wetness parameters
#' @autoglobal
#' @noRd
.validate_wetness_params <- function(simple_wetness, wth, RcW) {
  # Check TMIN/TMAX required for complex wetness
  if (isFALSE(simple_wetness) && any(c("TMIN", "TMAX") %notin% names(wth))) {
    cli::cli_abort(
      c(
        "You have set {.var simple_wetness} to {.code FALSE} but ",
        "you do not have 'TMIN' and 'TMAX' columns in your weather ",
        "data. Please set {.var simple_wetness} to {.code TRUE} or ",
        "provide 'TMIN' and 'TMAX' columns in your {.var wth} data."
      ),
      call = rlang::caller_env()
    )
  }

  # Validate RcW when complex wetness is used
  if (isFALSE(simple_wetness) && !is.null(RcW)) {
    .validate_RcW_matrix(RcW)
  }

  invisible(TRUE)
}

#' Validate RcW matrix
#' @autoglobal
#' @noRd
.validate_RcW_matrix <- function(RcW) {
  # Check that RcW is numeric
  if (!is.numeric(RcW)) {
    cli::cli_abort("{.var RcW} must contain numeric values.")
  }

  # Check that RcW is a matrix with 2 columns
  if (!is.matrix(RcW) || ncol(RcW) != 2) {
    cli::cli_abort(c(
      "{.var RcW} must be a two-column matrix with the first ",
      "column being hours (0 to 24) and the second column being ",
      "modifier values (0 to 1)."
    ))
  }

  # Check hour values are in valid range
  if ((max(RcW[, 1]) > 24) || (min(RcW[, 1]) < 0)) {
    cli::cli_abort(c(
      "The hourly values for {.var RcW} should be between 0 ",
      "and 24. Please provide a matrix with hour values ",
      "between 0 and 24, inclusive."
    ))
  }

  # Check modifier values are in valid range
  if ((max(RcW[, 2]) > 1) || (min(RcW[, 2]) < 0)) {
    cli::cli_abort(c(
      "The modifier values for {.var RcW} should be >= 0 and ",
      "<= 1. Please provide a matrix of modifier values that ",
      "are between 0 and 1."
    ))
  }

  invisible(TRUE)
}

#' Check if value is a single boolean
#'
#' @param x Value to check
#' @returns TRUE if x is exactly TRUE or FALSE, FALSE otherwise (including NA)
#'
#' @details
#' This function checks that a value is:
#' \itemize{
#'   \item Logical type
#'   \item Length 1 (scalar)
#'   \item Not NA
#' }
#'
#' @examples
#' \dontrun{
#' . is_boolean(TRUE)           # TRUE
#' .is_boolean(FALSE)          # TRUE
#' .is_boolean(NA)             # FALSE
#' .is_boolean(1)              # FALSE
#' .is_boolean(c(TRUE, FALSE)) # FALSE
#' }
#'
#' @autoglobal
#' @keywords internal
#' @noRd
.is_boolean <- function(x) {
  is.logical(x) && length(x) == 1L && !is.na(x)
}

#' Check and parse date
#'
#' @param x a date input (character, Date, POSIXct).
#'
#' @examples
#' # passes
#' .check_date("2023-10-15")
#'
#' # fails
#' \dontrun{
#'  .check_date("120-100-2023")
#' }
#' @return A validated POSIXct date object.
#' @dev
#' @autoglobal
.check_date <- function(x) {
  if (lubridate::is.POSIXct(x) || lubridate::is.Date(x)) {
    tz <- lubridate::tz(x)
  } else {
    tz <- Sys.timezone()
  }

  tryCatch(
    x <- lubridate::parse_date_time(
      x,
      c("Ymd", "dmY", "BdY", "Bdy"),
      tz = tz
    ),
    warning = function(c) {
      cli::cli_abort(
        "{x} is not in a valid date format. Please enter a valid date format."
      )
    }
  )
  return(x)
}
