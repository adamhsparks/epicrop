#' Utility Function to Check User Inputs for seir()
#'
#' Sanity checks for user inputs to [seir] to ensure that they will work.

#' @param wth a `data.frame` of class `epicrop.wth` from either [get_wth] or
#'  [format_wth] containing weather on a daily time-step containing data
#' with the following field names.
#'
#'   **Field Name** | **Value**
#'   --------------:|:----------
#'   _YYYYMMDD_ | Date as Year Month Day (ISO8601)
#'   _DOY_ | Consecutive day of year, commonly called "Julian date"
#'   _TEMP_ | Mean daily temperature (°C)
#'   _RHUM_ | Mean daily relative humidity (%)
#'   _RAIN_ | Mean daily rainfall (mm)
#'   _TMIN_ | **Optional** Minimum daily temperature (°C), see TMIN/TMAX Details
#'   _TMAX_ | **Optional** Maximum daily temperature (°C), see TMIN/TMAX Details
#'   _LAT_ | **Optional** latitude of weather observation, see LAT/LON Details
#'   _LON_ | **Optional** longitude of weather observation, see LAT/LON Details
#'
#' @param emergence character
#' @param onset numeric, must be >0.
#' @param emergence integer, must be >0.
#' @param duration integer ideally >=80 and <150 but may vary.
#' @param rhlim numeric bounded by 0 and 100.
#' @param rainlim numeric must be >0.
#' @param simple_wetness `Boolean` value
#' @param H0 numeric, must be >0.
#' @param I0 numeric, must be >0.
#' @param RcA numeric matrix with the first column being the crop age in days
#'  (1 to `duration`) and the second being the  modifier (bounded by 0 and 1).
#' @param RcT numeric matrix  with the first column being the temperature
#'  and the second column being the modfier (bounded by 0 and 1).
#' @param RcW numeric matrix with the first column being the hours of free
#'  moisture on the leaf surface and the second being the modifier (bounded by
#'  0 and 1).
#' @param RcOpt numeric, must be >0.
#' @param i integer, must be >0.
#' @param p integer, must be >0.
#' @param Sx integer, must be >0.
#' @param a numeric, 1 to >1
#' @param RRS numeric, must be >0.
#' @param RRG numeric, must be >0.
#'
#' @returns Nothing, called for its side-effects to check user input values.
#'
#' @dev
#'
.check_inputs <- function(
    wth,
    emergence,
    onset,
    duration,
    rhlim,
    rainlim,
    simple_wetness = TRUE,
    H0,
    I0,
    RcA,
    RcT,
    RcW,
    RcOpt,
    p,
    i,
    Sx,
    a,
    RRS,
    RRG) {
  passed_args <- as.list(match.call())[-c(1:2, 8)]
  passed_args$RcA <- eval(passed_args$RcA)
  passed_args$RcT <- eval(passed_args$RcT)
  if ("RcW" %in% names(passed_args)) {
    passed_args$RcW <- eval(passed_args$RcW)
  }
  non_numeric <- passed_args[which(
    !grepl("^-?(0|[1-9][0-9]*)(\\.[0-9]+)?$", passed_args)
  )]
  if (length(non_numeric) > 0) {
    cli::cli_abort(
      c(
        "{.var { names(non_numeric) }} has a value of {.code { non_numeric }}
         and must be a numeric value."
      )
    )
  }

  less_than_zero <- passed_args[passed_args < 0]
  if (length(less_than_zero > 0)) {
    cli::cli_abort(
      message = "{.var { names(less_than_zero) }} has a value of
       {.code less_than_zero} and must be a positive value.",
      call = rlang::caller_env()
    )
  }

  if (isFALSE(.is_boolean(passed_args$simple_wetness))) {
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
    cli::cli_message(
      "{.var duration} is < 80 days, is this intended?",
      call = rlang::caller_env()
    )
  }

  if (rhlim > 100) {
    cli::cli_abort(
      "The value for {.var rhlim}, {{rhlim}}, is invalid. Valid values are any
           value between or equal to 0 to 100.",
      call = rlang::caller_env()
    )
  }

  max_rca_day <- max(RcA[, 1])
  max_rrg_day <- max(RRG[, 1])

  if (max_rca_day > duration) {
    cli::cli_warning(
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
    cli::cli_warning(
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

  if (isFALSE(simple_wetness) && (c("TMIN", "TMAX") %notin% names(wth))) {
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
  is.logical(x) && length(x) == 1
}
