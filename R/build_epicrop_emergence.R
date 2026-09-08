#' Build Emergence Dates from Year and Month-Day Combinations
#'
#' Constructs a vector of emergence (planting) dates by combining years with
#' month-day patterns. Returns a sorted [data.table::IDate] vector tagged with
#' class `epicrop.emerge` for use with [run_epicrop_model()] and related
#' batch modelling functions.
#'
#' @section Purpose:
#'
#' Simplifies creation of emergence date vectors for epidemiological modelling
#' scenarios where you want to:
#' * Test multiple planting dates within a season (*e.g.*, June 1, 15, 30)
#' * Replicate across multiple years (*e.g.*, 2000-2005)
#' * Generate all combinations automatically
#'
#' Eliminates manual date construction: instead of typing out 18 dates
#' (6 month-days × 3 years), write:
#' ```r
#' build_epicrop_emergence(2000:2002, c("-06-01", "-06-15", "-07-01"))
#' ```
#'
#' @section Date Format:
#'
#' Month-day strings can be provided in two equivalent formats:
#' * **With leading dash** (recommended): `"-06-01"`, `"-12-31"`
#'   - Matches ISO 8601 suffix for `"YYYY-MM-DD"`
#'   - Function uses these directly
#' * **Without leading dash**: `"06-01"`, `"12-31"`
#'   - Function prepends `-` automatically
#'   - Less error-prone if mixed formats appear
#'
#' Both `c("-06-01", "-06-15")` and `c("06-01", "06-15")` produce identical
#' output.
#'
#' Validation catches:
#' * Invalid months (not 01-12)
#' * Invalid days (not 01-31 for month; leap years handled by R's Date class)
#' * Malformed strings (typos, missing separators)
#'
#' @section Output Class:
#'
#' The returned vector has class `epicrop.emerge` (subclass of
#' [data.table::IDate] and [base::Date]). This tag enables:
#' * Input validation in [run_epicrop_model()] (checks class, not just type)
#' * Clear intent: a vector of planting dates, not arbitrary dates
#' * Potential future S3 methods (*e.g.*, `plot.epicrop.emerge()`)
#'
#' @param years Integer vector of years. Examples:
#'   * `2000` – single year
#'   * `c(2000, 2002, 2005)` – specific years
#'   * `2000:2005` – range (recommended for clarity)
#'
#'   Dates are constructed as `paste0(year, month_day)`, so years can be
#'   any 4-digit integer. No validation against real calendar (*e.g.*, allows
#'   year 9999).
#'
#' @param month_day Character vector of month-day patterns. Each string
#'   specifies a fixed planting date recurring each year. Format (either):
#'   * `"-MM-DD"` with leading dash (*e.g.*, `"-06-01"`, `"-12-25"`)
#'   * `"MM-DD"` without dash (*e.g.*, `"06-01"`, `"12-25"`)
#'
#'   Validation:
#'   * Month: 01-12 (leading zero required)
#'   * Day: 01-31 (leading zero required)
#'   * Separator: dash (`-`), not `.` or `/`
#'
#'   Examples of valid input:
#'   * `c("-06-01", "-07-15", "-08-31")`
#'   * `c("01-15", "06-01", "12-25")` (no leading dash)
#'
#'   Invalid examples (will abort):
#'   * `"6-1"` (no leading zeros)
#'   * `"-06/01"` (wrong separator)
#'   * `"-13-01"` (month > 12)
#'   * `"-02-30"` (day > 29 for Feb; caught during date parsing)
#'
#' @returns A sorted vector of [data.table::IDate] emergence dates with class
#'   `epicrop.emerge`. Inherits from [base::Date] and [data.table::IDate],
#'   so standard date operations (subsetting, arithmetic, formatting) work.
#'   Length = `length(years) × length(month_day)`.
#'
#' @examplesIf interactive()
#' # Example 1: Basic usage - two years, three planting dates
#' years <- 2000:2001
#' month_day <- c("-06-01", "-06-15", "-07-01")
#'
#' em <- build_epicrop_emergence(years, month_day)
#' em
#'
#' # Check class
#' class(em)  # "epicrop.emerge" "IDate" "Date"
#' length(em) # 6 (2 years × 3 month-days)
#'
#' # Example 2: Month-day without leading dash (auto-normalized)
#' em2 <- build_epicrop_emergence(2000, c("06-01", "06-15", "07-01"))
#' identical(em2, build_epicrop_emergence(2000, c("-06-01", "-06-15", "-07-01")))
#'
#' # Example 3: Single year, multiple planting windows
#' # Early, mid, late June + early July
#' single_year <- build_epicrop_emergence(
#'   years = 2000,
#'   month_day = c("-06-01", "-06-10", "-06-20", "-07-01")
#' )
#' single_year
#' plot(single_year, rep(1, length(single_year)),
#'      main = "Planting Date Options for 2000",
#'      xlab = "Date", ylab = "")
#'
#' # Example 4: Long-term study across 10 years
#' long_term <- build_epicrop_emergence(
#'   years = 1990:1999,
#'   month_day = c("-06-15", "-07-01")  # Two standard planting dates
#' )
#' length(long_term)  # 20 (10 years × 2 dates)
#'
#' # Example 5: Use in batch modelling workflow
#' years <- 2000:2002
#' month_day <- c("-06-30", "-07-15")
#' emergence <- build_epicrop_emergence(years, month_day)
#'
#' # Fetch weather data (one year per list element)
#' wth_list <- fetch_epicrop_weather_list(
#'   lonlat = c(121.255669, 14.167425),
#'   start_date = month_day,
#'   duration = 120L,
#'   years = years
#' )
#'
#' # Run disease model across all emergence dates
#' results <- run_epicrop_model(
#'   model_fun = leaf_blast,
#'   emergence_dates = emergence,
#'   wth_list = wth_list,
#'   output = "audpc"
#' )
#' results  # AUDPC for each emergence date
#'
#' # Example 6: Subset and filter emergence dates
#' em <- build_epicrop_emergence(2000:2002, c("-06-01", "-07-01"))
#'
#' # Keep only June plantings
#' june_only <- em[month(em) == 6]
#' june_only
#'
#' # Keep only 2001 plantings
#' y2001 <- em[year(em) == 2001]
#' y2001
#'
#' # Example 7: Error handling
#' \dontrun{
#' # Invalid month (13)
#' build_epicrop_emergence(2000, "-13-01")
#' # Error: Invalid month_day value(s): -13-01
#'
#' # Invalid day (32)
#' build_epicrop_emergence(2000, "-02-30")
#' # Error: Failed to construct emergence dates
#'
#' # Typo in format (no dash, two-digit year in string)
#' build_epicrop_emergence(2000, "6-1")
#' # Error: Invalid month_day value(s): 6-1
#' }
#'
#' @details
#'
#' ## Internal Algorithm
#'
#' 1. Normalize `month_day` strings:
#'    - If matches `"^-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$"`: use as-is
#'    - If matches `"^(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$"`: prepend `-`
#'    - Otherwise: abort with error message
#' 2. Create cross-product: every year × every normalized month-day
#' 3. Construct ISO date strings: `sprintf("%s%s", year, month_day)`
#' 4. Parse to [data.table::IDate] with format `"%Y-%m-%d"`
#' 5. Sort by date and tag with `epicrop.emerge` class
#'
#' ## Leap Year Handling
#'
#' R's Date class handles leap years automatically:
#' * `"2000-02-29"` is valid (2000 is leap year)
#' * `"2001-02-29"` fails to parse (2001 is not leap year)
#'
#' Invalid dates (*e.g.*, Feb 30 for any year) are caught during parsing and
#' trigger an abort.
#'
#' ## Performance
#'
#' Fast even for large year/month-day combinations:
#' * Cross-product via [data.table::CJ()] (optimized)
#' * Vectorised date construction via [sprintf()]
#' * Sorting via base [sort()] on IDate (fast integer representation)
#'
#' Example: 20 years × 10 month-days = 200 dates constructed in < 1 ms.
#'
#' @seealso
#' * [run_epicrop_model()] – primary consumer of `epicrop.emerge` vectors
#' * [fetch_epicrop_weather_list()] – companion for building weather lists
#' * [base::Date], [data.table::IDate] – underlying date classes
#' * [base::seq.Date()] – alternative for building date sequences
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @export
build_epicrop_emergence <- function(years, month_day) {
  # Normalize month-day format: accept both "-MM-DD" and "MM-DD"
  md <- data.table::fcase(
    !is.na(month_day) & grepl("^-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$", month_day) ,
    month_day                                                                           ,
    !is.na(month_day) & grepl("^(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$", month_day)  ,
    paste0("-", month_day)                                                              ,
    default = NA_character_
  )

  # Check for invalid month-day values
  if (anyNA(md)) {
    bad <- unique(month_day[is.na(md)])
    cli::cli_abort(
      "Invalid {.arg month_day} value(s): {.val {bad}}. Use \"MM-DD\" or \"-MM-DD\" (01-12 month, 01-31 day)."
    )
  }

  # Create cross-product: all combinations of years × normalized month-days
  # CJ() = Cartesian join (cross-product of unique values)
  g <- data.table::CJ(years = years, month_day = md, sorted = FALSE)

  # Construct ISO date strings and parse to IDate
  # sprintf() is vectorised; constructs "YYYY-MM-DD" for each row
  dates <- data.table::as.IDate(
    sprintf("%s%s", g$years, g$month_day),
    format = "%Y-%m-%d"
  )

  # Check for parse failures (invalid dates like Feb 30)
  if (anyNA(dates)) {
    cli::cli_abort(c(
      "Failed to construct emergence dates from {.arg years} and {.arg month_day}.",
      i = "Check for invalid dates (e.g., Feb 30, month 13)"
    ))
  }

  # Sort dates and add class attribute
  out <- sort(dates)
  class(out) <- union("epicrop.emerge", class(out))

  out
}
