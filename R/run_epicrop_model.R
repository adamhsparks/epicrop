#' Run an epicrop Disease Model Across Emergence Dates and Locations
#'
#' A unified interface to execute any \pkg{epicrop} disease model
#' ([bacterial_blight()], [brown_spot()], [leaf_blast()], etc.) over multiple
#' emergence dates and/or locations. Handles cross-year and within-year weather
#' windows automatically without global weather binding.
#'
#' @section Workflow:
#'
#' This function bridges [fetch_epicrop_weather_list()] (weather prep) and
#' [build_epicrop_emergence()] (emergence date generation) with disease models.
#' For each emergence date, it:
#' 1. Slices weather data spanning emergence to emergence + `window_days - 1`
#' 2. Handles year boundaries (*e.g.*, Dec 15 – Jan 15)
#' 3. Executes the model with the sliced weather
#' 4. Returns either a compact AUDPC summary or full daily progression
#'
#' Supports weather bundles (multiple locations) by processing each location
#' independently and combining results with a `location` column.
#'
#' @param model_fun Function. An \pkg{epicrop} model function such as:
#'   [bacterial_blight()], [brown_spot()], [leaf_blast()], [sheath_blight()],
#'   [tungro()], [modified_kim_leaf_blast()], [modified_kim_sheath_blight()],
#'   [leaf_rust()], or [s_tritici_blotch()].
#' @param emergence_dates Vector of emergence dates (character `"YYYY-MM-DD"`,
#'   [Date], or [data.table::IDate]; must be of class `epicrop.emerge` from
#'   [build_epicrop_emergence()]).
#' @param wth_list Named list of weather [data.table::data.table]s, one per
#'  year.
#'   * **Names** must be year strings (*e.g.*, `"2001"`, `"2002"`)
#'   * Each table must have a `YYYYMMDD` column (Date, IDate, or ISO string)
#'   * Typically created by [fetch_epicrop_weather_list()]
#'   * Can also be an `epicrop.wth.bundle` (multiple locations)
#' @param window_days Integer. Days after emergence to simulate (default 120L).
#'   The model will run for exactly `window_days` rows when sufficient weather
#'   is available. If `emergence_dates` is at year-end, spans into next year.
#' @param output Character. Output format:
#'   * `"audpc"` (default): Compact summary with emergence date and AUDPC
#'   * `"full"`: All daily model columns plus `emergence` and `AUDPC`
#' @param ... Additional arguments forwarded to `model_fun` (*e.g.*, `RcOpt`,
#'   `simple_wetness`, custom modifier tables). Overrides model defaults.
#'
#' @returns A [data.table::data.table]:
#' * If `output = "audpc"`:
#'   - Columns: `emergence` (IDate), `AUDPC` (numeric)
#'   - One row per emergence date
#'   - `AUDPC = NA_real_` if weather data are insufficient
#' * If `output = "full"`:
#'   - All columns from the disease model (sites, latent, infectious, etc.)
#'   - Plus `emergence` (IDate) and `AUDPC` (numeric)
#'   - Multiple rows per emergence date (one per simulated day)
#'   - Class set to `epicrop.sim` for S3 methods
#' * If `wth_list` is a bundle (multiple locations):
#'   - Additional `location` column identifying source bundle member
#'
#' @section Cross-Year Windows:
#'
#' When an emergence date + `window_days` spans two calendar years (*e.g.*,
#' Dec 15 – Feb 15), the function automatically:
#' * Fetches weather from both years
#' * Stitches them in chronological order
#' * Returns a contiguous window
#'
#' Missing data (no weather for a required year) produces a warning and
#' `AUDPC = NA_real_`.
#'
#' @section Bundle Processing:
#'
#' If `wth_list` is an `epicrop.wth.bundle` (from [fetch_epicrop_weather_list()]
#' with multiple locations or location-date pairs):
#' * Each bundled weather object is processed independently
#' * Results are combined with a `location` column
#' * Location labels extracted from bundle names (text before `"__"`)
#'
#' @examplesIf interactive()
#' # --- Example 1: Single location, multiple emergence dates ---
#'
#' # Build emergence dates (every 15 days, June–July, years 2001–2003)
#' years <- 2001:2003
#' month_days <- c("-06-01", "-06-15", "-07-01")
#' emergence <- build_epicrop_emergence(years, month_days)
#'
#' # Fetch weather (one year per element)
#' lonlat <- c(121.255669, 14.16742)
#' wth_list <- fetch_epicrop_weather_list(
#'   lonlat = lonlat,
#'   start_date = month_days,
#'   duration = 120L,
#'   years = years
#' )
#'
#' # Run leaf blast model across all emergence dates
#' results <- run_epicrop_model(
#'   model_fun = leaf_blast,
#'   emergence_dates = emergence,
#'   wth_list = wth_list,
#'   window_days = 120L,
#'   output = "audpc"
#' )
#' head(results)
#'
#' # --- Example 2: Full progression output for detailed analysis ---
#'
#' full_results <- run_epicrop_model(
#'   model_fun = leaf_blast,
#'   emergence_dates = emergence[1:5],  # First 5 dates
#'   wth_list = wth_list,
#'   window_days = 120L,
#'   output = "full"
#' )
#'
#' # Plot disease progression for first emergence date
#' first_date <- emergence[1]
#' sub <- full_results[emergence == first_date]
#' plot(sub$simday, sub$intensity, type = "l",
#'      main = paste("Leaf Blast:", first_date),
#'      xlab = "Days After Emergence", ylab = "Disease Intensity")
#'
#' # --- Example 3: Sensitivity analysis (modify RcOpt) ---
#'
#' # Susceptible cultivar
#' susceptible <- run_epicrop_model(
#'   model_fun = leaf_blast,
#'   emergence_dates = emergence,
#'   wth_list = wth_list,
#'   window_days = 120L,
#'   output = "audpc",
#'   RcOpt = 1.14  # Default
#' )
#'
#' # Resistant cultivar (lower infection rate)
#' resistant <- run_epicrop_model(
#'   model_fun = leaf_blast,
#'   emergence_dates = emergence,
#'   wth_list = wth_list,
#'   window_days = 120L,
#'   output = "audpc",
#'   RcOpt = 0.8  # Lower
#' )
#'
#' comparison <- data.table(
#'   emergence = susceptible$emergence,
#'   susceptible_AUDPC = susceptible$AUDPC,
#'   resistant_AUDPC = resistant$AUDPC
#' )
#' comparison[, reduction := 1 - (resistant_AUDPC / susceptible_AUDPC)]
#' print(comparison)
#'
#' # --- Example 4: Bundle processing (multiple locations) ---
#'
#' locations <- rbind(
#'   "IRRI" = c(121.255669, 14.16742),
#'   "Laguna" = c(121.2, 14.1)
#' )
#'
#' wth_bundle <- fetch_epicrop_weather_list(
#'   lonlat = locations,
#'   start_date = month_days,
#'   duration = 120L,
#'   years = years,
#'   mode = "cross"  # All location × date combinations
#' )
#'
#' bundle_results <- run_epicrop_model(
#'   model_fun = leaf_blast,
#'   emergence_dates = emergence,
#'   wth_list = wth_bundle,
#'   window_days = 120L,
#'   output = "audpc"
#' )
#'
#' # Results now have a 'location' column
#' bundle_results[, .N, by = location]
#'
#' @seealso
#' * [build_epicrop_emergence()] – Construct emergence date vectors
#' * [fetch_epicrop_weather_list()] – Prepare weather data
#' * [get_audpc()] – Extract/compute AUDPC from results
#' * [seir()] – Core SEIR model
#' * [leaf_blast()], [sheath_blight()], etc. – Individual disease models
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
#' @export
run_epicrop_model <- function(
  model_fun,
  emergence_dates,
  wth_list,
  window_days = 120L,
  output = "audpc",
  ...
) {
  output <- rlang::arg_match(output, c("audpc", "full"))
  .validate_run_inputs(emergence_dates, wth_list)

  if (.is_wth_bundle(wth_list)) {
    return(.run_bundle(
      model_fun,
      emergence_dates,
      wth_list,
      window_days,
      output,
      ...
    ))
  }

  dots <- rlang::dots_list(..., .named = TRUE)

  # Auto-fill duration from window_days if the model exposes it as a formal
  if (.model_has_duration_formal(model_fun) && !("duration" %in% names(dots))) {
    dots$duration <- window_days
  }

  pieces <- lapply(emergence_dates, function(ed) {
    w_sub <- .slice_weather(ed, wth_list, window_days)

    if (is.null(w_sub)) {
      return(
        if (output == "audpc") {
          data.table(
            emergence = ed,
            AUDPC = NA_real_
          )
        } else {
          NULL
        }
      )
    }

    result <- rlang::exec(
      model_fun,
      wth = data.table::copy(w_sub),
      emergence = ed,
      !!!dots
    )

    audpc <- attr(result, "AUDPC", exact = TRUE)

    if (output == "audpc") {
      return(data.table(
        emergence = ed,
        AUDPC = audpc
      ))
    }

    result <- data.table::copy(result)
    result[, `:=`(
      emergence = ed,
      AUDPC = audpc
    )]

    result[]
  })

  .assemble(pieces, output)
}

#' Validate Inputs for run_epicrop_model
#'
#' Checks that `emergence_dates` and `wth_list` are the correct class and
#' structure for model execution.
#'
#' @param emergence_dates Vector of emergence dates (must be class
#'  `epicrop.emerge`).
#' @param wth_list Named list of weather tables (must be `epicrop.wth.list` or
#'   `epicrop.wth.bundle`).
#'
#' @returns NULL (invisibly). Called for side effects (validation/abort).
#'
#' @details
#' Aborts with helpful messages if:
#' * `emergence_dates` is not of class `epicrop.emerge`
#' * `wth_list` is neither `epicrop.wth.list` nor `epicrop.wth.bundle`
#'
#' @autoglobal
#' @dev
.validate_run_inputs <- function(emergence_dates, wth_list) {
  if (!inherits(emergence_dates, "epicrop.emerge")) {
    cli::cli_abort(c(
      "{.arg emergence_dates} is not of class {.cls epicrop.emerge}.",
      i = "Construct it via {.fn build_epicrop_emergence}."
    ))
  }
  if (
    !rlang::inherits_any(wth_list, c("epicrop.wth.list", "epicrop.wth.bundle"))
  ) {
    cli::cli_abort(c(
      "{.arg wth_list} must be {.cls epicrop.wth.list} or
        {.cls epicrop.wth.bundle}.",
      i = "Use {.fn fetch_epicrop_weather_list}."
    ))
  }
}

#' Detect Weather Bundle vs. Simple List
#'
#' Determines if `wth_list` is a bundle (multiple locations/scenarios) or a
#' simple year-indexed list.
#'
#' @param wth_list A weather list (checked for class and name structure).
#'
#' @returns Logical. TRUE if bundle, FALSE if year-indexed list.
#'
#' @details
#' A bundle has class `epicrop.wth.bundle` OR names that contain non-year
#' strings.  Year-indexed lists have names that are all 4-digit year strings.
#'
#' @autoglobal
#' @dev
.is_wth_bundle <- function(wth_list) {
  inherits(wth_list, "epicrop.wth.bundle") ||
    !all(grepl("^\\d{4}$", rlang::names2(wth_list)))
}

#' Run epicrop Model Across a Weather Bundle
#'
#' Internal helper that processes a weather bundle (multiple locations or
#' location-date combinations) by executing [run_epicrop_model()] separately
#' for each bundled weather list and combining results with location labels.
#'
#' @section Algorithm:
#'
#' 1. Extract bundle member names; generate labels for unnamed entries
#' 2. For each bundle member (location or location-date):
#'    - Call [run_epicrop_model()] with the sub-list
#'    - Add a `location` column to the result
#' 3. Combine all results via [data.table::rbindlist()]
#'
#' @param model_fun Function. An \pkg{epicrop} model function.
#' @param emergence_dates Vector of emergence dates (class `epicrop.emerge`).
#' @param wth_list An `epicrop.wth.bundle`; a named list of `epicrop.wth.list`
#'   objects.
#' @param window_days Integer. Days after emergence to include (default 120L).
#' @param output Character. `"audpc"` or `"full"`.
#' @param ... Additional arguments forwarded to `model_fun`.
#'
#' @returns A [data.table::data.table] with results from all bundled locations
#'   combined row-wise, with an additional `location` column identifying the
#'   source bundle member.
#'
#' @details
#' The function extracts location labels from the bundle's names by taking the
#' text before the `"__"` delimiter (used by [fetch_epicrop_weather_list()]).
#' For unnamed or empty-named entries, generates default labels (`location01`,
#' `location02`, etc.).
#'
#' Each bundled weather list is processed independently via [run_epicrop_model()],
#' then results are combined using [data.table::rbindlist()] with automatic
#' column alignment.
#'
#' @examplesIf interactive()
#' # Typically called indirectly via [run_epicrop_model()] when wth_list is
#' # an epicrop.wth.bundle, but can be called directly:
#'
#' locations <- rbind(
#'   "Site1" = c(121.255669, 14.16742),
#'   "Site2" = c(121.3, 14.2)
#' )
#' emergence <- build_epicrop_emergence(2001:2003, "-06-30")
#' wth_bundle <- fetch_epicrop_weather_list(
#'   lonlat = locations,
#'   start_date = "-06-30",
#'   duration = 120L,
#'   years = 2001:2003,
#'   mode = "cross"
#' )
#'
#' results <- .run_bundle(
#'   leaf_blast,
#'   emergence,
#'   wth_bundle,
#'   window_days = 120L,
#'   output = "audpc"
#' )
#' # Results now include a 'location' column
#'
#' @seealso [run_epicrop_model()], [fetch_epicrop_weather_list()]
#' @autoglobal
#' @dev
.run_bundle <- function(
  model_fun,
  emergence_dates,
  wth_list,
  window_days,
  output,
  ...
) {
  nms <- rlang::names2(wth_list)
  labels <- sub("__.*$", "", nms)
  labels[!nzchar(labels)] <- sprintf("location%02d", seq_along(wth_list))

  pieces <- Map(
    function(w, nm) {
      out <- run_epicrop_model(
        model_fun,
        emergence_dates,
        w,
        window_days,
        output,
        ...
      )
      out[, location := nm]
    },
    wth_list,
    labels
  )

  data.table::rbindlist(pieces, use.names = TRUE, fill = TRUE)
}

#' Check if Model Function Has Duration Formal Parameter
#'
#' Inspects a model function to determine if `duration` is an explicit
#' formal parameter, allowing automatic forwarding of `window_days`.
#'
#' @param model_fun Function. An \pkg{epicrop} model function.
#'
#' @returns Logical. TRUE if `duration` is in the function's formal parameters.
#'
#' @details
#' Used by [run_epicrop_model()] to auto-fill the `duration` argument with
#' `window_days` when the model exposes it. This enables flexible model
#' invocation without requiring the user to pass `duration` explicitly.
#'
#' @examplesIf interactive()
#' .model_has_duration_formal(leaf_blast)  # TRUE
#' .model_has_duration_formal(seir)        # TRUE (base model)
#'
#' @autoglobal
#' @dev
.model_has_duration_formal <- function(model_fun) {
  # Check if 'duration' is an explicit formal parameter
  fmls <- rlang::fn_fmls_names(model_fun) %||% character()
  "duration" %in% fmls
}

#' Slice Weather Data Across a Date Window
#'
#' Extracts weather for a single emergence date across one or more calendar
#' years. Handles year boundaries (*e.g.*, Dec 15 → Jan 15).
#'
#' @section Algorithm:
#'
#' 1. Compute start = `ed`, end = `ed + window_days - 1`
#' 2. For each year in range:
#'    - Fetch the year's weather table
#'    - Filter rows within \[max(start, year-01-01), min(end, year-12-31)\]
#'    - Warn if no data found for that year
#' 3. Combine segments and return contiguous window
#'
#' @param ed Emergence date (IDate).
#' @param wth_list Named list of weather tables (names = year strings).
#' @param window_days Integer. Days to fetch from emergence.
#'
#' @returns A [data.table::data.table] with rows covering `window_days`,
#'   or NULL if weather is completely unavailable.
#'
#' @details
#' Missing years or empty segments within a year produce warnings but do not
#' stop execution. If the final window spans fewer days than requested (due to
#' missing data), the shorter window is returned.
#'
#' @examplesIf interactive()
#' # Simulated weather lists (one year per element)
#' w_2000 <- data.table::data.table(
#'   YYYYMMDD = seq(
#'     as.Date("2000-06-01"),
#'     as.Date("2000-12-31"),
#'     by = "day"
#'   ),
#'   TEMP = 25
#' )
#' w_2001 <- data.table::data.table(
#'   YYYYMMDD = seq(
#'     as.Date("2001-01-01"),
#'     as.Date("2001-12-31"),
#'     by = "day"
#'   ),
#'   TEMP = 24
#' )
#' wth_list <- list("2000" = w_2000, "2001" = w_2001)
#'
#' # Extract window spanning Dec 2000 – Jan 2001
#' ed <- as.Date("2000-12-15")
#' sliced <- .slice_weather(ed, wth_list, window_days = 35L)
#' nrow(sliced)  # Should be close to 35 if data available
#'
#' @autoglobal
#' @dev
.slice_weather <- function(ed, wth_list, window_days) {
  start <- ed
  end <- ed + (window_days - 1L)
  years <- seq.int(data.table::year(start), data.table::year(end))

  segs <- Filter(
    Negate(is.null),
    lapply(years, function(y) {
      tab <- wth_list[[as.character(y)]]
      if (is.null(tab)) {
        cli::cli_warn("Skipping {ed}: no weather for year {y}.")
        return(NULL)
      }
      seg <- tab[
        YYYYMMDD %between%
          c(
            max(start, data.table::as.IDate(sprintf("%d-01-01", y))),
            min(end, data.table::as.IDate(sprintf("%d-12-31", y)))
          )
      ]
      if (nrow(seg) == 0L) {
        cli::cli_warn("Skipping {ed}: no rows in year {y}.")
        return(NULL)
      }
      seg
    })
  )

  if (!length(segs)) {
    return(NULL)
  }
  if (length(segs) == 1L) {
    segs[[1L]]
  } else {
    data.table::rbindlist(segs, use.names = TRUE)
  }
}

#' Assemble Results from Multiple Runs
#'
#' Combines results from multiple model runs across all emergence dates into a
#' single output table. Handles `NULL` entries, deduplication (AUDPC mode), and
#' class assignment.
#'
#' @section Logic:
#'
#' **AUDPC mode:**
#' * Combines rows from each emergence date
#' * Deduplicates (in case of replication)
#' * Returns compact table (emergence, AUDPC)
#'
#' **Full mode:**
#' * Combines all daily rows across all emergence dates and model runs
#' * Filters out NULL entries (failed runs due to missing weather)
#' * Sets class to `epicrop.sim` for S3 methods
#'
#' @param pieces List of [data.table::data.table]s.
#' @param output Character. `"audpc"` or `"full"`.
#'
#' @returns A [data.table::data.table] (or empty template if all pieces are
#'  `NULL`).
#'
#' @autoglobal
#' @dev
.assemble <- function(pieces, output) {
  if (output != "audpc") {
    out <- data.table::rbindlist(
      Filter(Negate(is.null), pieces),
      use.names = TRUE,
      fill = TRUE
    )
    return(data.table::setattr(
      out,
      "class",
      c("epicrop.sim", "data.table", "data.frame")
    ))
  }

  empty <- data.table::data.table(
    emergence = data.table::as.IDate(NA),
    AUDPC = NA_real_
  )
  if (!length(Filter(Negate(is.null), pieces))) {
    return(data.table::setkeyv(empty, c("emergence", "AUDPC")))
  }

  res <- data.table::rbindlist(
    lapply(pieces, function(x) x %||% empty),
    use.names = TRUE
  )
  unique(data.table::setkeyv(res, c("emergence", "AUDPC")))
}
