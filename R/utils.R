#' Utility Function to Check User Inputs for seir()
#'
#' Sanity checks for user inputs to [seir] to ensure that they will work.

#' @param wth a `data.frame` of class `epicrop.wth` from either [get_wth()] or
#'  [format_wth()] containing weather on a daily time-step containing data
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
#' @param onset numeric
#' @param emergence integer
#' @param duration integer
#' @param rhlim numeric
#' @param rainlim numeric
#' @param simple_wetness `Boolean` value
#' @param H0 numeric
#' @param I0 numeric
#' @param RcA numeric matrix
#' @param RcT numeric matrix
#' @param RcW numeric matrix
#' @param RcOpt numeric
#' @param i integer
#' @param p integer
#' @param Sx integer
#' @param a numeric
#' @param RRS numeric
#' @param RRG numeric
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
  # remove fn, `wth` and `simple_wetness` args from the passed_args, only want
  # the args that are numeric here.
  passed_args <- as.character(match.call())[-c(1:2, 8)]

  non_numeric <- passed_args[which(is.na(as.numeric(as.character(
    passed_args
  ))))]

  if (length(non_numeric > 0)) {
    cli::cli_abort(
      c(x = "{.var {{non_numeric}}} must be a numeric value.")
    )
  }

  if (!inherits("epicrop.wth", wth)) {
    cli::cli_abort(
      message = "{.var wth} must be an object of {.class epicrop.wth}.  You can
      create your {.var wth} object using either, {.fn get_wth} to fetch weather
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

  if (H0 < 0L) {
    cli::cli_abort(
      "{.var H0} cannot be < 0, check your initial number of healthy sites.",
      call = rlang::caller_env()
    )
  }

  if (I0 < 0L) {
    cli::cli_abort(
      "{.var I0} cannot be < 0, check your initial number of infective sites.",
      call = rlang::caller_env()
    )
  }

  if (duration < 80L) {
    cli::cli_message(
      "{.var duration} is < 80 days, is this intended?",
      call = rlang::caller_env()
    )
  }

  if (rainlim <= 0) {
    cli::cli_abort(
      c(
        "The value for {.var rainlim}, {{rainlim}}, is invalid. Valid values are
        any value greater than 0."
      ),
      call = rlang::caller_env()
    )
  }

  if ((rhlim < 0) || (rhlim > 100)) {
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
