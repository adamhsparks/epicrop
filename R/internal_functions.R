#' Utility Function to Check User Inputs for seir()
#'
#' Sanity checks for user inputs to [seir] to ensure that they will work.

#' @param seir_passed_args a `list` of values passed to [seir]
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
    simple_wetness <- seir_passed_args$simple_wetness
    RcA <- eval(seir_passed_args$RcA)
    RcT <- eval(seir_passed_args$RcT)
    if (!is.null(seir_passed_args$RcW)) {
      RcW <- eval(seir_passed_args$RcW)
    }

    if (!is.Date(emergence)) {
      cli::cli_abort(
        "{.var { Date } has a value of {.code { Date }}
            and must be a date object.",
        call = rlang::caller_env()
      )
    }

    numeric_args <- c(
      within(
        seir_passed_args,
        rm("wth", "emergence", "RcA", "RcT", "RcW", "simple_wetness")
      ),
      RcA,
      RcT,
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

    if (
      exists(simple_wetness) &&
        (isFALSE(.is_boolean(simple_wetness)))
    ) {
      cli::cli_abort(
        message = "{.var simple_wetness} must be a Boolean ({.code TRUE/FALSE})
           value.",
        call = rlang::caller_env()
      )
    }

    if (!inherits("epicrop.wth", wth)) {
      cli::cli_abort(
        "{.var wth} must be an object of {.class epicrop.wth}.  You can create
           your {.var wth} object using either, {.fn get_wth} to fetch weather
           data from the NASA POWER API or format your own with {.fn format_wth}.",
        call = rlang::caller_env()
      )
    }

    if (a < 1L) {
      cli::cli_abort(
        "{.var a} cannot be set to less than 1. Valid aggregation values for
                {.var a} are from 1 to > 1.",
        call = rlang::caller_env()
      )
    }

    if (duration < 80L) {
      cli::cli_alert_danger(
        "{.var duration} is < 80 days, is this intended?",
        call = rlang::caller_env()
      )
    }

    if (rhlim > 100L) {
      cli::cli_abort(
        "The value for {.var rhlim}, {{rhlim}}, is invalid. Valid values are any
               value between or equal to 0 to 100.",
        call = rlang::caller_env()
      )
    }

    max_rca_day <- max(RcA[, 1L])
    max_rrg_day <- max(RRG[, 1L])

    if (max_rca_day > duration) {
      cli::cli_alert_info(
        "{.var RcA} has a duration, {{max_rca_day}}, that is greater than
           {. var duration}, {{duration}} days.  A truncated matrix of values
           will be used that stops at {.var duration} days ({{duration}}).",
        call = rlang::caller_env()
      )
    }

    if (max_rca_day < duration) {
      cli::cli_abort(
        "{.var RcA} has a duration, {{max_rca_day}} that is less than
           {. var duration}, {{duration}} days. Please provide a matrix of values
           that aligns with {.var duration}.",
        call = rlang::caller_env()
      )
    }

    if (max_rrg_day > duration) {
      cli::cli_alert_info(
        "{.var RRG} has a duration, {{max_rrg_day}}, that is greater than
           {. var duration}, {{duration}} days.  A truncated matrix of values
           will be used that stops at {.var duration} days ({{duration}}).",
        call = rlang::caller_env()
      )
    }

    if (max_rrg_day < duration) {
      cli::cli_abort(
        "{.var RRG} has a duration, {{max_rrg_day}} that is less than
           {. var duration}, {{duration}} days. Please provide a matrix of value
           that aligns with {.var duration}.",
        call = rlang::caller_env()
      )
    }

    if (
      exists(simple_wetness) &&
        (isFALSE(simple_wetness) && (c("TMIN", "TMAX") %notin% names(wth)))
    ) {
      cli::cli_abort(
        "You have set {.var simple_wetness} to {.code TRUE} but you do not have
           'TMIN' and 'TMAX' columns in your weather data. Please set
           {.var simple_wetness} to {.code FALSE} or provide 'TMIN' and 'TMAX'
           columns in your {.var wth} data.",
        call = rlang::caller_env()
      )
    }
  }


#' Boolean
#'
#' Checks if provided object is a Boolean i.e. a length-one logical vector.
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
  if (length(x) > 1L) {
    cli::cli_alert_warning(
      "Only one day is used for emergence. Only the first value will be used."
    )
    x <- x[[1L]]
  }

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
