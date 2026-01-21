#' Build emergence dates from year × month_day combinations
#'
#' Construct a vector of emergence dates by combining a set of years with a set
#' of month-day strings (e.g., "-06-01"), returning a sorted `IDate` vector
#' tagged with class `epicrop.emerge` for downstream use with
#' [run_epicrop_model()].
#'
#' @param years Integer vector of years (e.g., `2001:2003`).
#' @param month_day Character vector of month-day strings in the form `"-MM-DD"`
#'   (e.g., `c("-06-01", "-06-14")`). If a string is supplied without a leading
#'   dash (e.g., `"06-01"`), callers should normalize it in advance; this helper
#'   expects the leading `-` as part of the format `"YYYY-MM-DD"`.
#'
#' @return A sorted vector of [data.table::IDate] emergence dates with class
#'   `epicrop.emerge` (i.e., `inherits(x, "epicrop.emerge")` will be `TRUE`).
#'
#' @seealso [run_epicrop_model()], [fetch_epicrop_weather_list()]
#'
#' @examples
#' # Basic usage: combine years with multiple month-day patterns
#' years <- 2001:2002
#' month_day <- c("-06-01", "-06-14", "-06-30")
#'
#' em <- build_epicrop_emergence(years, month_day)
#' em
#'
#' # Check class and structure
#' inherits(em, "epicrop.emerge")   # TRUE
#' head(em)                         # First few dates (IDate)
#' length(em)                       # 6 (2 years × 3 month-days)
#'
#' # Typical downstream: pass to run_epicrop_model(...)
#' # run_epicrop_model(bacterial_blight, em, wth_list, output = "audpc")
#'
#' @export
build_epicrop_emergence <- function(years, month_day) {
  g <- data.table::CJ(years = years, month_day = month_day, sorted = FALSE)
  g <- sort(
    data.table::as.IDate(
      sprintf("%s%s", g$years, g$month_day),
      format = "%Y-%m-%d"
    )
  )
  class(g) <- union("epicrop.emerge", class(g))
  g
}
