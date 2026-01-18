#' Utility Function to Check User Inputs for epicrop_sim()
#'
#' Sanity checks for user inputs to [epicrop_sim()] to ensure that they will work.
#' @param seir_passed_args a `list` of values passed to [epicrop_sim()]
#'
#' @returns Nothing, called for its side-effects to check user input values.
#'
#' @dev
#'
.check_inputs <- function(seir_passed_args) {
  # Extract and evaluate arguments
  wth <- seir_passed_args$wth
  emergence <- seir_passed_args$emergence
  RcA <- eval(seir_passed_args$RcA)
  RcT <- eval(seir_passed_args$RcT)
  RRG <- .eval_RRG(seir_passed_args$RRG)
  RcW <- .eval_RcW(seir_passed_args$RcW)
  simple_wetness <- seir_passed_args$simple_wetness

  # Check dates
  .check_date(x = emergence)
  .check_emergence_date(x = emergence)

  # Check scalar numeric parameters
  .check_scalar_params(seir_passed_args, RRG)

  # Check matrix parameters
  .check_matrix_params(RcA, RcT, RRG)

  # Check boolean parameters
  .check_boolean_params(simple_wetness)

  # Check weather data
  .check_weather_data(wth)

  # Check specific parameter constraints
  .check_parameter_constraints(seir_passed_args, RcA)

  # Check wetness-related parameters
  .check_wetness_params(simple_wetness, wth, RcW, seir_passed_args$RcW)

  invisible(TRUE)
}


#' Evaluate RRG Parameter
#'
#' @param RRG_arg RRG argument from seir_passed_args
#' @returns Evaluated RRG value (scalar or matrix)
#' @dev
.eval_RRG <- function(RRG_arg) {
  if (length(RRG_arg) == 1) {
    return(RRG_arg)
  } else {
    return(eval(RRG_arg))
  }
}


#' Evaluate RcW Parameter
#'
#' @param RcW_arg RcW argument from seir_passed_args
#' @returns Evaluated RcW value or NULL
#' @dev
.eval_RcW <- function(RcW_arg) {
  if (!is.null(RcW_arg)) {
    return(eval(RcW_arg))
  } else {
    return(NULL)
  }
}


#' Check Scalar Numeric Parameters
#'
#' @param seir_passed_args List of arguments passed to epicrop_sim()
#' @param RRG Evaluated RRG parameter
#' @returns Nothing, called for side effects
#' @dev
.check_scalar_params <- function(seir_passed_args, RRG) {
  # Extract scalar numeric parameters
  scalar_params <- list(
    onset = seir_passed_args$onset,
    duration = seir_passed_args$duration,
    rhlim = seir_passed_args$rhlim,
    rainlim = seir_passed_args$rainlim,
    H0 = seir_passed_args$H0,
    I0 = seir_passed_args$I0,
    RcOpt = seir_passed_args$RcOpt,
    p = seir_passed_args$p,
    i = seir_passed_args$i,
    Sx = seir_passed_args$Sx,
    a = seir_passed_args$a,
    RRS = seir_passed_args$RRS,
    RRG = if (length(RRG) == 1) RRG else NULL
  )

  # Remove NULL values
  scalar_params <- scalar_params[!sapply(scalar_params, is.null)]

  # Check that all are numeric
  non_numeric <- scalar_params[!sapply(scalar_params, is.numeric)]

  if (length(non_numeric) > 0) {
    cli::cli_abort(
      c(
        "{.var {names(non_numeric)}} has a value of ",
        "{.code {non_numeric}} and must be a numeric value."
      ),
      call = rlang::caller_env(n = 2)
    )
  }

  # Check for negative values
  .check_non_negative_params(scalar_params)

  invisible(TRUE)
}


#' Check Parameters Must Be Non-Negative
#'
#' @param scalar_params List of scalar parameters
#' @returns Nothing, called for side effects
#' @dev
.check_non_negative_params <- function(scalar_params) {
  params_must_be_non_negative <- c(
    "H0",
    "I0",
    "RcOpt",
    "p",
    "i",
    "Sx",
    "a",
    "RRS",
    "rhlim",
    "rainlim"
  )

  for (param_name in params_must_be_non_negative) {
    if (param_name %in% names(scalar_params)) {
      param_value <- scalar_params[[param_name]]
      if (param_value < 0) {
        cli::cli_abort(
          c(
            "{.var {param_name}} has a value of {.code {param_value}} ",
            "and must be >= 0."
          ),
          call = rlang::caller_env(n = 3)
        )
      }
    }
  }

  invisible(TRUE)
}


#' Check Matrix Parameters
#'
#' @param RcA Age modifier matrix
#' @param RcT Temperature modifier matrix
#' @param RRG Growth rate (scalar or matrix)
#' @returns Nothing, called for side effects
#' @dev
.check_matrix_params <- function(RcA, RcT, RRG) {
  if (!is.numeric(RcA)) {
    cli::cli_abort(
      "{.var RcA} must contain numeric values.",
      call = rlang::caller_env(n = 2)
    )
  }

  if (!is.numeric(RcT)) {
    cli::cli_abort(
      "{.var RcT} must contain numeric values.",
      call = rlang::caller_env(n = 2)
    )
  }

  if (length(RRG) > 1 && !is.numeric(RRG)) {
    cli::cli_abort(
      "{.var RRG} must contain numeric values.",
      call = rlang::caller_env(n = 2)
    )
  }

  invisible(TRUE)
}


#' Check Boolean Parameters
#'
#' @param simple_wetness Boolean parameter for wetness calculation
#' @returns Nothing, called for side effects
#' @dev
.check_boolean_params <- function(simple_wetness) {
  if (isFALSE(.is_boolean(simple_wetness))) {
    cli::cli_abort(
      c(
        "{.var simple_wetness} must be a Boolean ",
        "({.code TRUE/FALSE}) value."
      ),
      call = rlang::caller_env(n = 2)
    )
  }

  invisible(TRUE)
}


#' Check Weather Data
#'
#' @param wth Weather data object
#' @returns Nothing, called for side effects
#' @dev
.check_weather_data <- function(wth) {
  if (!inherits(wth, "epicrop.wth")) {
    cli::cli_abort(
      c(
        "{.var wth} must be an object of {.class epicrop.wth}.   ",
        "You can create your {.var wth} object using either ",
        "{.fn get_wth} to fetch weather data from the NASA POWER API ",
        "or format your own with {.fn format_wth}."
      ),
      call = rlang::caller_env(n = 2)
    )
  }

  invisible(TRUE)
}


#' Check Parameter Constraints
#'
#' @param seir_passed_args List of arguments passed to epicrop_sim()
#' @param RcA Age modifier matrix
#' @returns Nothing, called for side effects
#' @dev
.check_parameter_constraints <- function(seir_passed_args, RcA) {
  # Check aggregation coefficient
  if (seir_passed_args$a < 1) {
    cli::cli_abort(
      c(
        "{.var a} cannot be set to less than 1. ",
        "Valid aggregation values for {.var a} are from 1 to > 1."
      ),
      call = rlang::caller_env(n = 2)
    )
  }

  # Check duration
  if (seir_passed_args$duration < 50) {
    cli::cli_abort(
      "{.var duration} is < 50 days, is this intended? ",
      call = rlang::caller_env(n = 2)
    )
  }

  # Check relative humidity limit
  if (seir_passed_args$rhlim > 100) {
    cli::cli_abort(
      c(
        "The value for {.var rhlim}, {seir_passed_args$rhlim}, ",
        "is invalid. Valid values are any value between or equal ",
        "to 0 to 100."
      ),
      call = rlang::caller_env(n = 2)
    )
  }

  # Check maximum sites
  if (seir_passed_args$Sx <= 0) {
    cli::cli_abort(
      "{.var Sx} must be > 0.",
      call = rlang::caller_env(n = 2)
    )
  }

  # Check onset
  if (seir_passed_args$onset < 1) {
    cli::cli_abort(
      "{.var onset} must be >= 1.",
      call = rlang::caller_env(n = 2)
    )
  }

  # Check duration is positive
  if (seir_passed_args$duration <= 0) {
    cli::cli_abort(
      "{.var duration} must be > 0.",
      call = rlang::caller_env(n = 2)
    )
  }

  # Check RcA duration alignment
  .check_RcA_duration(RcA, seir_passed_args$duration)

  invisible(TRUE)
}


#' Check RcA Duration Alignment
#'
#' @param RcA Age modifier matrix
#' @param duration Simulation duration
#' @returns Nothing, called for side effects
#' @dev
.check_RcA_duration <- function(RcA, duration) {
  max_rca_day <- max(RcA[, 1])

  if (max_rca_day > duration) {
    cli::cli_warn(
      c(
        "{.var RcA} has a duration, {max_rca_day}, that is greater ",
        "than {.var duration}, {duration} days. ",
        "A truncated matrix of values will be used that stops at ",
        "{.var duration} days ({duration})."
      )
    )
  }

  if (max_rca_day < duration) {
    cli::cli_abort(
      c(
        "{.var RcA} has a duration, {max_rca_day}, that is less ",
        "than {.var duration}, {duration} days. ",
        "Please provide a matrix of values that aligns with ",
        "{.var duration}."
      ),
      call = rlang::caller_env(n = 3)
    )
  }

  invisible(TRUE)
}


#' Check Wetness Parameters
#'
#' @param simple_wetness Boolean for simple wetness calculation
#' @param wth Weather data object
#' @param RcW Evaluated RcW parameter (can be NULL)
#' @param RcW_arg Original RcW argument (to check if user provided it)
#' @returns Nothing, called for side effects
#' @dev
.check_wetness_params <- function(simple_wetness, wth, RcW, RcW_arg) {
  # Check TMIN/TMAX required for complex wetness
  if (
    isFALSE(simple_wetness) &&
      (any(c("TMIN", "TMAX") %notin% names(wth)))
  ) {
    cli::cli_abort(
      c(
        "You have set {.var simple_wetness} to {.code FALSE} but ",
        "you do not have 'TMIN' and 'TMAX' columns in your weather ",
        "data.  Please set {.var simple_wetness} to {.code TRUE} or ",
        "provide 'TMIN' and 'TMAX' columns in your {.var wth} data."
      ),
      call = rlang::caller_env(n = 2)
    )
  }

  # Validate RcW when complex wetness is used
  if (isFALSE(simple_wetness)) {
    .check_RcW_matrix(RcW, RcW_arg)
  }

  invisible(TRUE)
}


#' Check RcW Matrix Validity
#'
#' @param RcW Evaluated RcW parameter
#' @param RcW_arg Original RcW argument
#' @returns Nothing, called for side effects
#' @dev
.check_RcW_matrix <- function(RcW, RcW_arg) {
  # Check that RcW is provided
  if (is.null(RcW)) {
    cli::cli_abort(
      c(
        "You have set {.var simple_wetness} to {.code FALSE} but ",
        "you have not provided a matrix of weighted values for ",
        "{.var RcW}."
      ),
      call = rlang::caller_env(n = 3)
    )
  }

  # Check that RcW is not a scalar
  if (length(RcW) == 1) {
    cli::cli_abort(
      c(
        "You have set {.var simple_wetness} to {.code FALSE} but ",
        "{.var RcW} is a scalar value. Please provide a ",
        "two-column matrix of weighted values for {.var RcW}."
      ),
      call = rlang::caller_env(n = 3)
    )
  }

  # Check that RcW is numeric
  if (!is.numeric(RcW)) {
    cli::cli_abort(
      "{.var RcW} must contain numeric values.",
      call = rlang::caller_env(n = 3)
    )
  }

  # Check that RcW is a matrix with 2 columns
  if (!is.matrix(RcW) || ncol(RcW) != 2) {
    cli::cli_abort(
      c(
        "{.var RcW} must be a two-column matrix with the first ",
        "column being hours (0 to 24) and the second column being ",
        "modifier values (0 to 1)."
      ),
      call = rlang::caller_env(n = 3)
    )
  }

  # Check hour values are in valid range
  .check_RcW_hours(RcW)

  # Check modifier values are in valid range
  .check_RcW_modifiers(RcW)

  invisible(TRUE)
}


#' Check RcW Hour Values
#'
#' @param RcW RcW matrix
#' @returns Nothing, called for side effects
#' @dev
.check_RcW_hours <- function(RcW) {
  if ((max(RcW[, 1]) > 24) || (min(RcW[, 1]) < 0)) {
    cli::cli_abort(
      c(
        "The hourly values for {.var RcW} should be between 0 ",
        "and 24. Please provide a matrix with hour values ",
        "between 0 and 24, inclusive."
      ),
      call = rlang::caller_env(n = 4)
    )
  }

  invisible(TRUE)
}


#' Check RcW Modifier Values
#'
#' @param RcW RcW matrix
#' @returns Nothing, called for side effects
#' @dev
.check_RcW_modifiers <- function(RcW) {
  if ((max(RcW[, 2]) > 1) || (min(RcW[, 2]) < 0)) {
    cli::cli_abort(
      c(
        "The modifier values for {.var RcW} should be >= 0 and ",
        "<= 1. Please provide a matrix of modifier values that ",
        "are between 0 and 1."
      ),
      call = rlang::caller_env(n = 4)
    )
  }

  invisible(TRUE)
}

#' Boolean
#'
#' Checks if provided object is a Boolean, _i.e._ a length-one logical vector.
#' @param x an object to check.
#' @returns a logical value indicating whether provided object is a `Boolean`.
#' @examples
#' is_boolean(TRUE) # [1] TRUE
#' # the following will work on most systems, unless you have tweaked .Rprofile
#' .is_boolean(T) # [1] TRUE
#' .is_boolean(1) # [1] FALSE
#' @source
#'  <https://github.com/Rapporter/rapportools/blob/master/R/utils.R>
#'
#' @dev
.is_boolean <- function(x) {
  is.logical(x) && length(x) == 1L
}

#' Check User Input Dates for Validity
#'
#' @param x User entered date value
#' @returns Validated date string as a `POSIXct` object.
#' @note This was taken from \CRANpkg{nasapower}.
#' @examples
#' .check_date("2024-01-01")
#' @author Adam H. Sparks \email{adamhsparks@@curtin.edu.au}
#' @autoglobal
#' @dev
.check_date <- function(x) {
  if (lubridate::is.POSIXct(x) || lubridate::is.Date(x)) {
    tz <- lubridate::tz(x)
  } else {
    tz <- Sys.timezone()
  }

  tryCatch(
    x <- lubridate::parse_date_time(
      x,
      c(
        "Ymd",
        "dmY",
        "BdY",
        "Bdy"
      ),
      tz = tz
    ),
    warning = function(c) {
      cli::cli_abort(
        "{ x } is not in a valid date format. Please enter a valid date format."
      )
    }
  )
  return(x)
}

#' Ensure that the establishment date is a single value
#'
#' @param x a date object
#'
#' @returns Nothing, called for its side-effects of checking the user-input
#' date.
#'
#' @examples
#' .check_establishment_date(x = "2000-01-01")
#' @dev

.check_emergence_date <- function(x) {
  if (length(x) > 1L) {
    cli::cli_alert_warning(
      "Only one day is used for emergence. Only the first value will be used."
    )
    x <- x[[1L]]
  }
}
