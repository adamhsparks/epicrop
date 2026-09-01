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
