#' Utility Function to Check User Inputs for seir()
#'
#' Sanity checks for user inputs to [seir()] to ensure that they will work.
#' @param seir_passed_args a `list` of values passed to [seir()]
#'
#' @returns Nothing, called for its side-effects to check user input values.
#'
#' @dev
#'
.check_inputs <-
  function(
    seir_passed_args
  ) {
    wth <- seir_passed_args$wth
    emergence <- seir_passed_args$emergence
    RcA <- eval(seir_passed_args$RcA)
    RcT <- eval(seir_passed_args$RcT)

    # if RRG is a matrix, we need to evaluate it first
    if (length(seir_passed_args$RRG) == 1.0) {
      RRG <- seir_passed_args$RRG
    } else {
      RRG <- eval(seir_passed_args$RRG)
    }

    # if RcW exists take that value, otherwise, set it to a numeric value such
    # that it won't trigger an error and we can check all other values
    if (!is.null(seir_passed_args$RcW)) {
      RcW <- eval(seir_passed_args$RcW)
    } else {
      RcW <- 1L
    }
    simple_wetness <- seir_passed_args$simple_wetness

    .check_date(x = emergence)
    .check_emergence_date(x = emergence)

    numeric_args <- c(
      within(
        seir_passed_args,
        rm("wth", "emergence", "RcA", "RcT", "RRG", "RcW", "simple_wetness")
      ),
      RcA,
      RcT,
      RRG,
      RcW
    )

    non_numeric <-
      numeric_args[which(
        !grepl('^-?(0|[1-9][0-9]*)(\\.[0-9]+)?$', numeric_args)
      )]

    if (length(non_numeric) > 0L) {
      cli::cli_abort(
        "{.var { names(non_numeric) }} has a value of {.code { non_numeric }}
            and must be a numeric value.",
        call = rlang::caller_env()
      )
    }

    less_than_zero <- numeric_args[which(numeric_args < 0L)]
    if (length(less_than_zero) > 0L) {
      cli::cli_abort(
        message = "{.var { names(less_than_zero) }} has a value of
         {.code less_than_zero} and must be a positive value.",
        call = rlang::caller_env()
      )
    }

    if (isFALSE(.is_boolean(simple_wetness))) {
      cli::cli_abort(
        message = "{.var simple_wetness} must be a Boolean ({.code TRUE/FALSE})
           value.",
        call = rlang::caller_env()
      )
    }

    if (!inherits(wth, "epicrop.wth")) {
      cli::cli_abort(
        "{.var wth} must be an object of {.class epicrop.wth}.  You can create
           your {.var wth} object using either, {.fn get_wth} to fetch weather
           data from the NASA POWER API or format your own with
           {.fn format_wth}.",
        call = rlang::caller_env()
      )
    }

    if (seir_passed_args$a < 1L) {
      cli::cli_abort(
        "{.var a} cannot be set to less than 1. Valid aggregation values for
                {.var a} are from 1 to > 1.",
        call = rlang::caller_env()
      )
    }

    if (seir_passed_args$duration < 50.0) {
      cli::cli_abort(
        "{.var duration} is < 50 days, is this intended?",
        call = rlang::caller_env()
      )
    }

    if (seir_passed_args$rhlim > 100.0) {
      cli::cli_abort(
        "The value for {.var rhlim}, {{rhlim}}, is invalid. Valid values are any
               value between or equal to 0 to 100.",
        call = rlang::caller_env()
      )
    }

    max_rca_day <- max(RcA[, 1L])

    if (max_rca_day > seir_passed_args$duration) {
      cli::cli_warn(
        "{.var RcA} has a duration, {max_rca_day}, that is greater than
         {.var duration}, {seir_passed_args$duration} days.  A truncated
         matrix of values will be used that stops at {.var duration} days
         ({seir_passed_args$duration})."
      )
    }

    if (max_rca_day < seir_passed_args$duration) {
      cli::cli_abort(
        "{.var RcA} has a duration, {max_rca_day}, that is less than
        {.var duration}, {seir_passed_args$duration} days. Please provide a
        matrix of values that aligns with {.var duration}.",
        call = rlang::caller_env()
      )
    }

    if (
      isFALSE(simple_wetness) && (any(c("TMIN", "TMAX") %notin% names(wth)))
    ) {
      cli::cli_abort(
        "You have set {.var simple_wetness} to {.code TRUE} but you do not have
           'TMIN' and 'TMAX' columns in your weather data. Please set
           {.var simple_wetness} to {.code FALSE} or provide 'TMIN' and 'TMAX'
           columns in your {.var wth} data.",
        call = rlang::caller_env()
      )
    }

    if (isFALSE(simple_wetness) && (length(RcW) == 1L)) {
      cli::cli_abort(
        "You have set {.var simple_wetness} to {.code TRUE} but you have not
        provided a matrix of weighted values for {.var RcW}",
        call = rlang::caller_env()
      )
    }

    if (length(RcW) > 1L) {
      if ((max(RcW[, 1L]) > 24L) || min(RcW[, 1L] < 1)) {
        cli::cli_abort(
          "The hourly values for {.var RcW} should be between 1 and
                       24. Please provide a matrix with hour values between 1
                       and 24, inclusive."
        )
      }
      if ((max(RcW[, 2L]) > 1L) || min(RcW[, 2L] < 0L)) {
        cli::cli_abort(
          "The value for {.var RcW} should be >= 0 and <= 1. Please provide a
        matrix of modifier values that are between 0 and 1.",
          call = rlang::caller_env()
        )
      }
    }
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
