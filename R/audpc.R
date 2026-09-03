#' Calculate Area Under the Disease Progress Curve (AUDPC)
#'
#' Computes the Area Under the Disease Progress Curve (AUDPC) from daily
#' disease intensity values using the trapezoidal rule. This function is
#' designed to work with [seir()] output where observations are equally spaced
#' (one day apart).
#'
#' @section AUDPC Details:
#'
#' AUDPC is a standard epidemiological metric summarizing cumulative disease
#' intensity over time. It:
#' * Ranges from 0 (no disease) to duration (maximum disease throughout)
#' * Accounts for both disease severity and duration
#' * Enables comparison across different growing seasons, locations, or
#'    cultivars
#' * Is commonly used in cultivar resistance screening and management trials
#'
#' The trapezoidal rule approximates the area under the curve:
#'
#' \eqn{AUDPC = sum((intensity[i] + intensity[i+1]) / 2)}
#'  for all consecutive days
#'
#' @section When to Use:
#'
#' * **Direct use (rare):** Only when you have a vector of daily intensity
#'   values from [seir()] and want the AUDPC
#' * **Typical use:** Access via [get_audpc()] or [audpc_dt()] on model output
#'   (AUDPC is automatically calculated and stored as an attribute)
#'
#' @param intensity Numeric vector of daily disease intensity values
#'   (typically from [seir()] output). Values should be bounded \[0, 1\] where
#'   0 = no disease, 1 = complete infection. Length >= 2 required.
#'
#' @returns Numeric value (double) representing the area under the disease
#'   progress curve. Returns 0 if `intensity` has fewer than 2 observations.
#'
#' @examplesIf interactive()
#' # Example 1: Fetch weather and run disease model
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = "2000-06-30",
#'   duration = 120L
#' )
#'
#' # Run bacterial blight model
#' bb_sim <- bacterial_blight(wth, emergence = "2000-06-30")
#'
#' # Calculate AUDPC from intensity vector (already in object attribute)
#' audpc_value <- .calculate_audpc(intensity = bb_sim$intensity)
#' print(audpc_value)
#'
#' # Compare with automatic calculation via get_audpc()
#' audpc_attr <- get_audpc(bb_sim)
#' all.equal(audpc_value, audpc_attr)  # Should be TRUE
#'
#' # Example 2: Effect of disease severity on AUDPC
#' # Mild disease (slow progression)
#' mild <- seq(0, 0.3, length.out = 120)
#' audpc_mild <- .calculate_audpc(mild)
#'
#' # Severe disease (rapid progression)
#' severe <- seq(0, 1.0, length.out = 120)
#' audpc_severe <- .calculate_audpc(severe)
#'
#' cbind(Scenario = c("Mild", "Severe"),
#'       AUDPC = c(audpc_mild, audpc_severe))
#'
#' # Example 3: Compare resistant vs susceptible cultivars
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = "2000-06-30",
#'   duration = 120L
#' )
#'
#' # Susceptible cultivar (default parameters)
#' susceptible <- leaf_blast(wth, emergence = "2000-06-30")
#' audpc_sus <- get_audpc(susceptible)
#'
#' # Resistant cultivar (lower infection rate)
#' resistant <- leaf_blast(wth, emergence = "2000-06-30", RcOpt = 0.8)
#' audpc_res <- get_audpc(resistant)
#'
#' # Calculate resistance benefit
#' reduction <- (audpc_sus - audpc_res) / audpc_sus * 100
#' sprintf("Resistant cultivar reduces AUDPC by %.1f%%", reduction)
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#'
#' @references
#' Shaner, G., and R. E. Finney. 1977. The effect of nitrogen fertilization on
#' the expression of slow-mildewing resistance in Knox wheat.
#' *Phytopathology*, 67: 1051-1056.
#'
#' Sparks, A. H., P. D. Esker, M. Bates, W. Dall'Acqua, Z. Guo, V. Segovia,
#' S. D. Silwal, S. Tolos, and K. A. Garrett. 2008. Ecology and Epidemiology
#' in R: Disease Progress over Time. *The Plant Health Instructor*.
#' \doi{10.1094/PHI-A-2008-0129-02}.
#'
#' Madden, L. V., G. Hughes, and F. van den Bosch. 2007. *The Study of Plant
#' Disease Epidemics*. American Phytopathological Society, St. Paul, MN.
#' \doi{10.1094/9780890545058}.
#'
#' @autoglobal
#' @dev
.calculate_audpc <- function(intensity) {
  n <- length(intensity)

  if (n < 2L) {
    return(0)
  }

  # Trapezoidal rule: AUDPC = sum((y[i] + y[i+1]) / 2) for all intervals
  # Uses vector subsetting to avoid explicit loops
  sum((intensity[-1L] + intensity[-n]) / 2)
}

#' Extract AUDPC Value from Disease Model Output
#'
#' Retrieves the pre-calculated Area Under the Disease Progress Curve (AUDPC)
#' from an `epicrop.sim` object. AUDPC is automatically computed by [seir()]
#' and stored as an object attribute.
#'
#' @section Purpose:
#'
#' This function provides a safe, documented way to access AUDPC without
#' directly querying attributes. It includes validation to ensure the object
#' is a valid model result with a calculated AUDPC.
#'
#' @section AUDPC Interpretation:
#'
#' AUDPC values:
#' * Near 0: Minimal disease throughout season (resistant or unfavorable conditions)
#' * Mid-range: Moderate disease development
#' * Near duration: Severe, sustained disease pressure
#'
#' Typical ranges depend on disease and duration:
#' * 120-day rice season: 0-120 (perfect health to constant epidemic)
#' * 240-day wheat season: 0-240
#'
#' @param x An `epicrop.sim` object returned by [seir()], [bacterial_blight()],
#'   [leaf_blast()], or other disease model functions.
#'
#' @returns Numeric AUDPC value (double).
#'
#' @examplesIf interactive()
#' # Example 1: Basic usage - extract AUDPC from model output
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = "2000-06-30",
#'   duration = 120L
#' )
#'
#' sim <- bacterial_blight(wth, emergence = "2000-06-30")
#' audpc <- get_audpc(sim)
#' print(paste("Bacterial blight AUDPC:", round(audpc, 2)))
#'
#' # Example 2: Compare AUDPC across multiple diseases
#' diseases <- list(
#'   bb = bacterial_blight(wth, emergence = "2000-06-30"),
#'   bs = brown_spot(wth, emergence = "2000-06-30"),
#'   lb = leaf_blast(wth, emergence = "2000-06-30"),
#'   sb = sheath_blight(wth, emergence = "2000-06-30")
#' )
#'
#' audpc_values <- data.frame(
#'   disease = names(diseases),
#'   AUDPC = sapply(diseases, get_audpc)
#' )
#' print(audpc_values)
#' # Identify most damaging disease
#' worst <- audpc_values[which.max(audpc_values$AUDPC), ]
#' print(paste("Most severe:", worst$disease, "AUDPC =", round(worst$AUDPC, 1)))
#'
#' # Example 3: Use in analysis pipeline
#' locations <- list(
#'   IRRI = c(121.25562, 14.6774),
#'   Manila = c(120.985, 14.6042)
#' )
#'
#' audpc_by_location <- sapply(locations, function(lonlat) {
#'   wth <- get_wth(lonlat = lonlat, dates = "2000-06-30", duration = 120L)
#'   sim <- leaf_blast(wth, emergence = "2000-06-30")
#'   get_audpc(sim)
#' })
#'
#' print(audpc_by_location)
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @export
get_audpc <- function(x) {
  if (!inherits(x, "epicrop.sim")) {
    cli::cli_abort(c(
      "{.arg x} must be an {.cls epicrop.sim} object",
      i = "Created by {.fun seir} or disease-specific helper functions like
      {.fun bacterial_blight}, {.fun leaf_blast}, etc."
    ))
  }

  return(attr(x, "AUDPC", exact = TRUE))
}

#' Extract AUDPC Summary as data.table
#'
#' Extracts the Area Under Disease Progress Curve (AUDPC) from an epicrop
#' simulation output along with spatial (lat/lon) and temporal (emergence)
#'  metadata.
#' Returns a compact summary table suitable for analysis, mapping, or reporting.
#'
#' @section Output Structure:
#'
#' The returned [data.table::data.table()] includes:
#' * **lat**: Latitude (if available in the model object)
#' * **lon**: Longitude (if available in the model object)
#' * **AUDPC**: Area under disease progress curve (numeric)
#' * **emergence**: Emergence date as character (YYYY-MM-DD)
#'
#' Columns are present only if the corresponding data exist in the input object.
#' The table is keyed on `emergence` for efficient sorting and joining.
#'
#' @section Use Cases:
#'
#' * **Batch processing:** Quickly summarize results from [run_epicrop_model()]
#' * **Mapping:** Combine with spatial data for disease risk mapping
#' * **Reporting:** Generate summary tables for publications or extension
#' * **Analysis:** Join with cultivar, management, or climate data
#'
#' @param x An `epicrop.sim` object from [seir()], disease model functions
#'   ([bacterial_blight()], [leaf_blast()], etc.), or [run_epicrop_model()]
#'   with `output = "full"`.
#'
#' @returns A [data.table::data.table()] with one row containing:
#'   * `lat`: Latitude (double; NA if not available)
#'   * `lon`: Longitude (double; NA if not available)
#'   * `AUDPC`: Area under disease progress curve (double)
#'   * `emergence`: Emergence date (character, "YYYY-MM-DD")
#'
#'   Table is keyed on `emergence` for efficient operations.
#'
#' @examplesIf interactive()
#' # Example 1: Extract AUDPC summary from single model run
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = "2000-06-30",
#'   duration = 120L
#' )
#'
#' sim <- bacterial_blight(wth, emergence = "2000-06-30")
#' summary_table <- audpc_dt(sim)
#' print(summary_table)
#' # Output includes lat, lon, AUDPC, emergence
#'
#' # Example 2: Batch summarize multiple emergence dates
#' emergence_dates <- seq(as.Date("2000-06-15"), as.Date("2000-07-15"), by = 7)
#' summaries <- lapply(emergence_dates, function(ed) {
#'   sim <- bacterial_blight(wth, emergence = as.character(ed))
#'   audpc_dt(sim)
#' })
#' summary_all <- data.table::rbindlist(summaries)
#' print(summary_all)
#'
#' # Example 3: Compare multiple locations and emergence dates
#' locations <- data.frame(
#'   name = c("IRRI", "Manila"),
#'   lon = c(121.25562, 120.985),
#'   lat = c(14.6774, 14.6042)
#' )
#'
#' emergence_dates <- c("2000-06-30", "2000-07-15")
#'
#' results <- expand.grid(
#'   location = locations$name,
#'   emergence = emergence_dates,
#'   stringsAsFactors = FALSE
#' )
#'
#' results$AUDPC <- mapply(function(loc_name, em_date) {
#'   loc <- locations[locations$name == loc_name, ]
#'   wth <- get_wth(
#'     lonlat = c(loc$lon, loc$lat),
#'     dates = em_date,
#'     duration = 120L
#'   )
#'   sim <- leaf_blast(wth, emergence = em_date)
#'   get_audpc(sim)
#' }, results$location, results$emergence)
#'
#' print(results)
#' # Identify highest risk scenario
#' worst <- results[which.max(results$AUDPC), ]
#' sprintf("Highest risk: %s, emergence %s, AUDPC = %.1f",
#'         worst$location, worst$emergence, worst$AUDPC)
#'
#' # Example 4: Export for mapping or further analysis
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = "2000-06-30",
#'   duration = 120L
#' )
#' sim <- brown_spot(wth, emergence = "2000-06-30")
#' summary_dt <- audpc_dt(sim)
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @export
audpc_dt <- function(x) {
  # Validate input class
  if (!inherits(x, "epicrop.sim")) {
    cli::cli_abort(c(
      "{.arg x} must be an {.cls epicrop.sim} object",
      i = "Created by {.fun seir} or disease-specific helper functions like
      {.fun bacterial_blight}, {.fun leaf_blast}, etc."
    ))
  }

  # Get AUDPC from attribute
  audpc_val <- attr(x, "AUDPC", exact = TRUE)
  if (is.null(audpc_val)) {
    cli::cli_abort(c(
      "{.arg x} must contain AUDPC value as attribute",
      i = "This should be automatically added by {.fun seir}"
    ))
  }

  # Get emergence date (check both 'dates' and 'emergence' columns)
  emergence_val <- if (rlang::has_name(x, "dates")) {
    as.character(x$dates[1L])
  } else if (rlang::has_name(x, "emergence")) {
    as.character(x$emergence[1L])
  } else {
    cli::cli_abort(c(
      "{.arg x} must contain {.field dates} or {.field emergence} column",
      i = "This should be automatically added by {.fun seir}"
    ))
  }

  # Build column list - only add columns that exist
  cols <- list()

  if (rlang::has_name(x, "lat")) {
    cols$lat <- x$lat[1L]
  }

  if (rlang::has_name(x, "lon")) {
    cols$lon <- x$lon[1L]
  }

  cols$AUDPC <- audpc_val
  cols$emergence <- emergence_val

  # Create data.table from list
  result <- data.table::as.data.table(cols)

  # Set column order: spatial cols first, then AUDPC, then emergence
  desired_order <- c("lat", "lon", "AUDPC", "emergence")
  actual_order <- intersect(desired_order, names(result))
  data.table::setcolorder(result, actual_order)

  # Set key on emergence for efficient sorting/indexing
  data.table::setkey(result, emergence)

  result[]
}
