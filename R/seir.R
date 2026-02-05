#' Susceptible-Exposed-Infectious-Removed (SEIR) Model Framework
#'
#' This function is originally used by specific disease models in
#'  \sQuote{EPIRICE} to model disease intensity of several rice diseases.  Given
#'  proper values it can be used with other pathosystems as well. Optionally
#'  includes lesion expansion dynamics implemented in \sQuote{EPIWHEAT}
#'  following Savary et al. 2015.
#'
#' @param wth a `data.frame` of class `epicrop.wth` from either [get_wth()] or
#'  [format_wth()] containing weather on a daily time-step with the following
#'  field names:
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
#' @param emergence expected date of plant emergence (or transplanting for rice)
#'  entered in `YYYY-MM-DD` format (character).  Described in Table 1 of Savary
#'  _et al._ 2012.
#' @param onset expected number of days until the onset of disease after
#'  emergence date (day, integer).  Described in Table 1 of Savary _et al._
#'  2012.
#' @param duration simulation duration _i.e._, growing season length (day,
#'  integer).  Described in Table 1 of Savary _et al._ 2012.
#' @param rhlim relative humidity value threshold to decide whether leaves are
#'  wet or not (numeric).  Described in Table 1 of Savary _et al._ 2012.  Savary
#'  _et al._ 2012 used `90`.
#' @param rainlim rainfall amount (mm) threshold to decide whether leaves are
#'  wet or not (numeric).  Described in Table 1 of Savary _et al._ 2012.  Savary
#'  _et al._ 2012 used `5`.
#' @param H0 initial number of plant's healthy sites (integer).  Described in
#'  Table 1 of Savary _et al._ 2012.
#' @param I0 initial number of infective sites (integer).  Described in Table 1
#'  of Savary _et al._ 2012.
#' @param RcA modifier for _Rc_ (the basic infection rate corrected for
#'  removals) for crop age as a numeric two-column matrix with the first column
#'  being the crop age in days (1 to `duration`) and the second being the
#'  modifier (bounded by 0 and 1).  Described in Tables 1 and 2 of Savary
#'  _et al._ 2012.
#' @param RcT modifier for _Rc_ (the basic infection rate corrected for
#'  removals) for temperature as a numeric two-column matrix with the first
#'  column being temperature in ˚C and the second the modifier (bounded by 0 and
#'  1).  Described in Tables 1 and 2 of Savary _et al._ 2012.
#' @param RcOpt potential basic infection rate corrected for removals (numeric).
#'  Described in Table 1 of Savary _et al._ 2012.  This value can be modified
#'  to reflect crop cultivar resistance ratings as well, see Kim _et al._ 2015
#'  for more details.
#' @param i duration of infectious period (day, integer or numeric).  Described
#'  in Table 1 of Savary _et al._ 2012. Can be a scalar, a two-column matrix
#'  for temperature-dependent duration, or a numeric vector of length
#'  `duration`.
#' @param p duration of latent period (day, integer or numeric).  Described in
#'  Table 1 of Savary _et al._ 2012. Can be a scalar, a two-column matrix
#'  for temperature-dependent duration, or a numeric vector of length
#'  `duration`.
#' @param Sx maximum number of sites (integer).  Described in Table 1 of Savary
#'  _et al._ 2012.
#' @param a aggregation coefficient, values are from 1 to >1 (numeric).
#'  Described in Table 1 of Savary _et al._ 2012 and Table 1 of Savary _et al._
#'  2015.  See further details in **_a_ - Aggregation** section.
#' @param RRS relative rate of physiological senescence (numeric).  Described in
#'  Table 1 of Savary _et al._ 2012.
#' @param RRG relative rate of growth (numeric).  Described in Table 1 of Savary
#'  _et al._ 2012.
#' @param simple_wetness `Boolean` value, if `TRUE` the function will use a
#'  simple calculation of leaf wetness based on the daily `rhlim` and `rainlim`
#'  values. If `FALSE` the function will use a more complex calculation of leaf
#'  wetness based on the `wth` data and will calculate the leaf wetness value
#'  for each hour of the day and use `RcW` to determine the effect of leaf
#'  wetness on the infection rate.
#' @param RcW Optional modifier for _Rc_ to calculate leaf wetness effect
#'  from 1 if wet to 0 if dry.  This is a numeric two-column matrix with the
#'  first column being hours (1 to 24) and the second the modifier (bounded by 0
#'  and 1).  Used when `simple_wetness == FALSE` (hourly/derived wetness)., if
#'   it is provided and `simple_wetness == TRUE`, these values will be ignored.
#' @param lesion_expansion Logical. If `TRUE`, enables lesion expansion dynamics
#'  following Savary et al. 2015. Default is `FALSE` for backward compatibility.
#' @param RRLEX Relative rate of lesion expansion (numeric). Only used when
#'  `lesion_expansion = TRUE`. Described in Table 1 of Savary _et al._ 2015.
#'  Default is `NULL`.
#' @param infection_window Optional list defining DVS-based infection window:
#'   * `dvs_start`: DVS when susceptibility begins (e.g., 1.0 for flowering)
#'   * `dvs_end`: DVS when susceptibility ends (e.g., 1.3 for early grain fill)
#'   * `base_temp`: Base temperature for GDD (default 0)
#'   * `gdd_to_maturity`: Total GDD to reach DVS=2 (default 1100)
#'   If NULL, infection possible at all growth stages (default behavior).
#'
#' @references
#' Sparks, A.H., P.D. Esker, M. Bates, W. Dall' Acqua, Z. Guo, V. Segovia, S.D.
#' Silwal, S. Tolos, and K.A. Garrett, 2008. Ecology and Epidemiology in R:
#' Disease Progress over Time. *The Plant Health Instructor*.
#' \doi{10.1094/PHI-A-2008-0129-02}.
#'
#' Madden, L. V., G. Hughes, and F. van den Bosch. 2007. The Study of Plant
#' Disease Epidemics. American Phytopathological Society, St. Paul, MN.
#' \doi{10.1094/9780890545058}.
#'
#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario,
#' J. Modeling and mapping potential epidemics of rice diseases globally. _Crop
#' Protection_, Volume 34, 2012, Pages 6-17, ISSN 0261-2194
#' \doi{10.1016/j.cropro.2011.11.009}.
#'
#' Savary, S., Willocquet, L., Pethybridge, S.J., Esker, P., McRoberts, N., and
#' Nelson, A. 2015. The global burden of pathogens and pests on major food crops.
#' _European Journal of Plant Pathology_ 143:369-398.
#' \doi{10.1007/s10658-015-0650-7}.
#'
#' @section _a_ -- Aggregation Details:
#'
#' When _a_ is set to `1` the assumption is that that there is no disease
#' aggregation with new infections occurring at random among the healthy sites.
#' When _a_ is greater than `1` there is aggregation in the disease occurrence,
#' the pathogen is unable to access the entire population of healthy sites,
#' which results in disease aggregation. Refer to Savary _et al._ (2012) for
#' greater detail.
#'
#' @section Lesion Expansion Details:
#'
#' When `lesion_expansion = TRUE`, the model incorporates the mechanism where
#' healthy sites are colonized by expanding lesions at rate RRLEX (Savary et al.
#' 2015, Equation 1). This is implemented as:
#'
#' \deqn{RLEX = RRLEX \times INF \times COFR}
#'
#' Where RLEX is the rate of lesion expansion converting healthy sites to
#' infectious sites, and COFR is the correction factor for removals.
#'
#' This differs from simple lesion area tracking by directly incorporating
#' expansion into site dynamics, making it suitable for diseases like wheat
#' rusts and Septoria where lesion growth significantly impacts epidemic
#' development.
#'
#' @section TMIN/TMAX Details:
#'
#' If `simple_wetness` is set to `FALSE`, the function will use the `TMIN` and
#'  TMAX` columns in the `wth` object to calculate the leaf wetness value for
#'  24 hours of the day using the `rhlim` and `rainlim` values and then uses
#'  `RcW` to calculate a value between 0 and 1 for the whole day.  When
#'  `simple_wetness` is set to `TRUE`, the function only sets the leaf wetness
#'  to 0 or 1 for the day based on the `rhlim` and `rainlim` values.
#'
#' @section LAT/LON Details:
#'
#' If the `wth` object provides `LAT` and `LON` columns, these will be included
#' in the output for mapping purposes. Both values must be present. These
#' columns are provided by default when using [get_wth()].
#'
#' @seealso
#' `seir()` is called by the following specific disease modelling functions:
#' * [bacterial_blight()],
#' * [brown_spot()],
#' * [leaf_blast()],
#' * [sheath_blight()],
#' * [tungro()],
#' * [modified_kim_leaf_blast()],
#' * [modified_kim_sheath_blight()],
#' * [leaf_rust()], and
#' * [s_tritici_blotch()].
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com} from original by
#'   Robert J. Hijmans, Rene Pangga and Jorrel Aunario.
#'
#' @returns A [data.table::data.table()] object containing the following
#' columns:
#'
#' \describe{
#'   \item{simday}{Zero indexed day of simulation that was run}
#'   \item{dates}{Date of simulation}
#'   \item{sites}{Total number of sites present on day "x"}
#'   \item{latent}{Number of latent sites present on day "x"}
#'   \item{infectious}{Number of infectious sites present on day "x"}
#'   \item{removed}{Number of removed sites present on day "x"}
#'   \item{senesced}{Number of senesced sites present on day "x"}
#'   \item{rateinf}{Rate of infection}
#'   \item{rtransfer}{Rate of transfer from latent to infectious sites}
#'   \item{rgrowth}{Rate of growth of healthy sites}
#'   \item{rsenesced}{Rate of senescence of healthy sites}
#'   \item{rlex}{Rate of lesion expansion (only when `lesion_expansion = TRUE`)}
#'   \item{diseased}{Number of diseased (latent + infectious + removed) sites on day "x"}
#'   \item{intensity}{Proportion of diseased (latent + infectious + removed) sites per total sites not including removed sites on day "x"}
#'   \item{AUDPC}{Area under the disease progress curve \acronym{AUDPC} for the simulation}
#'   \item{lat}{Latitude value if provided by the `wth` object}
#'   \item{lon}{Longitude value if provided by the `wth` object}
#' }
#'
#' @autoglobal
#' @export

seir <- function(
  wth,
  emergence,
  onset,
  duration,
  rhlim,
  rainlim,
  H0,
  I0,
  RcA,
  RcT,
  RcOpt,
  p,
  i,
  Sx,
  a,
  RRS,
  RRG,
  RcW = NULL,
  RRLEX = 0,
  simple_wetness = TRUE,
  age_driver = c("day", "dvs"),
  thermal_time = NULL,
  steps_per_day = 1L,
  infection_window = NULL
) {
  age_driver <- rlang::arg_match(age_driver)

  # Validate DVS requirements
  if (age_driver == "dvs" && is.null(thermal_time)) {
    cli::cli_abort(
      "{.arg age_driver} = {.val dvs} requires {.arg thermal_time}",
      class = "epicrop_missing_thermal_time"
    )
  }

  if (!is.null(infection_window)) {
    if (!rlang::is_list(infection_window)) {
      cli::cli_abort("{.arg infection_window} must be a list")
    }

    if (
      !rlang::has_name(infection_window, "dvs_start") ||
        !rlang::has_name(infection_window, "dvs_end")
    ) {
      cli::cli_abort(
        "{.arg infection_window} must contain {.field dvs_start} and
        {.field dvs_end}"
      )
    }

    # Use thermal_time or extract from infection_window
    thermal_time <- thermal_time %||%
      list(
        base_temp = infection_window$base_temp %||%
          cli::cli_abort("{.arg infection_window} needs {.field base_temp}"),
        gdd_to_maturity = infection_window$gdd_to_maturity %||%
          cli::cli_abort(
            "{.arg infection_window} needs {.field gdd_to_maturity}"
          )
      )
  }

  # Prepare weather
  w <- .prep_weather(wth)

  # Parse emergence
  emergence_date <- as.Date(emergence)
  emergence_day <- which(w$dates == emergence_date)

  if (length(emergence_day) == 0) {
    cli::cli_abort(
      "Emergence {.val {emergence_date}} not in weather data range
      {.val {min(w$dates)}} to {.val {max(w$dates)}}"
    )
  }

  if (emergence_day + duration > nrow(w)) {
    cli::cli_abort(
      "Duration ({duration}) exceeds weather data ({nrow(w) - emergence_day}
      days available)"
    )
  }

  # Subset weather
  w <- w[emergence_day:(emergence_day + duration - 1)]

  # Calculate age variable
  if (age_driver == "dvs") {
    age_values <- .calculate_dvs(w, thermal_time)
  } else {
    age_values <- seq(0, duration - 1, by = 1)
  }

  # Calculate modifiers
  Rc_age <- .interp(RcA, age_values)
  Rc_temp <- .interp(RcT, w$TEMP)

  # Wetness
  if (simple_wetness) {
    Rc_wet <- as.numeric((w$RHUM >= rhlim) | (w$RAIN >= rainlim))
  } else {
    wetness_hours <- .calculate_leaf_wetness(
      w$RHUM,
      w$RAIN,
      w$TMIN,
      w$TMAX,
      w$LAT,
      w$DOY,
      rhlim,
      rainlim
    )
    Rc_wet <- if (is.null(RcW)) {
      pmin(1, wetness_hours / 24)
    } else {
      .interp(RcW, wetness_hours)
    }
  }

  # Combined infection rate
  Rc <- RcOpt * Rc_age * Rc_temp * Rc_wet

  # Apply infection window
  if (!is.null(infection_window)) {
    window_dvs <- if (age_driver == "dvs") {
      age_values
    } else {
      .calculate_dvs(w, thermal_time)
    }
    susceptible <- (window_dvs >= infection_window$dvs_start) &
      (window_dvs <= infection_window$dvs_end)
    Rc <- Rc * as.numeric(susceptible)
  }

  # Other parameters
  p_days <- if (is.function(p)) p(w$TEMP) else .interp(p, age_values)
  i_days <- if (is.function(i)) i(w$TEMP) else .interp(i, age_values)
  RRG_vals <- .interp(RRG, age_values)
  RRS_vals <- if (is.function(RRS)) {
    RRS(age_values)
  } else {
    .interp(RRS, age_values)
  }
  RRLEX_vals <- .interp(RRLEX, age_values)
  a_vals <- .interp(a, age_values)

  # Inoculum
  inoculum <- numeric(duration)
  inoculum[onset] <- I0

  # Initialize
  sites <- latent <- infectious <- removed <- senesced <- numeric(duration)
  rateinf <- rlex <- rtransfer <- rremoved <- rgrowth <- rsenesced <- numeric(
    duration
  )

  sites[1] <- H0
  dt <- 1.0 / steps_per_day

  # Main loop
  for (day in 2:duration) {
    S <- sites[day - 1]
    E <- latent[day - 1]
    I <- infectious[day - 1]
    R <- removed[day - 1]
    Sen <- senesced[day - 1]

    daily_inf <- daily_lex <- daily_trans <- daily_rem <- daily_grw <- daily_sen <- 0

    for (step in 1:steps_per_day) {
      total <- S + E + I

      # Rates
      inf_rate <- if (total > 0 && I > 0) {
        COFR <- 1 - (1 / (1 + a_vals[day] * I / total))
        Rc[day] * COFR * (I / total) * S
      } else {
        0
      }

      inf_rate <- inf_rate + inoculum[day] * dt
      lex_rate <- RRLEX_vals[day] * S
      trans_rate <- E / max(p_days[day], 1)
      rem_rate <- I / max(i_days[day], 1)
      grw_rate <- RRG_vals[day] * S * (1 - total / Sx)
      sen_rate <- RRS_vals[day] * S

      # Updates
      S <- max(0, S + (grw_rate - sen_rate - inf_rate - lex_rate) * dt)
      E <- max(0, E + (inf_rate - trans_rate) * dt)
      I <- max(0, I + (trans_rate + lex_rate - rem_rate) * dt)
      R <- max(0, R + rem_rate * dt)
      Sen <- max(0, Sen + sen_rate * dt)

      # Accumulate
      daily_inf <- daily_inf + inf_rate * dt
      daily_lex <- daily_lex + lex_rate * dt
      daily_trans <- daily_trans + trans_rate * dt
      daily_rem <- daily_rem + rem_rate * dt
      daily_grw <- daily_grw + grw_rate * dt
      daily_sen <- daily_sen + sen_rate * dt
    }

    sites[day] <- S
    latent[day] <- E
    infectious[day] <- I
    removed[day] <- R
    senesced[day] <- Sen

    rateinf[day] <- daily_inf
    rlex[day] <- daily_lex
    rtransfer[day] <- daily_trans
    rremoved[day] <- daily_rem
    rgrowth[day] <- daily_grw
    rsenesced[day] <- daily_sen
  }

  # Output
  living <- sites + latent + infectious
  intensity <- ifelse(living > 0, (latent + infectious) / living, 0)

  result <- data.table::data.table(
    simday = 1:duration,
    dates = w$dates,
    sites = sites,
    latent = latent,
    infectious = infectious,
    removed = removed,
    senesced = senesced,
    rateinf = rateinf,
    rlex = rlex,
    rtransfer = rtransfer,
    rremoved = rremoved,
    rgrowth = rgrowth,
    rsenesced = rsenesced,
    diseased = latent + infectious + removed,
    intensity = intensity
  )

  if (rlang::has_name(w, "LAT")) {
    result[, lat := w$LAT[1]]
  }
  if (rlang::has_name(w, "LON")) {
    result[, lon := w$LON[1]]
  }

  data.table::setattr(result, "AUDPC", .calculate_audpc(intensity))
  return(result[])
}


#' Calculate Development Stage (DVS)
#'
#' @param w Weather data with TEMP column.
#' @param thermal_time List with base_temp and gdd_to_maturity.
#' @returns Numeric vector of DVS (0-2).
#' @autoglobal
#' @dev
.calculate_dvs <- function(w, thermal_time) {
  if (!rlang::has_name(w, "TEMP")) {
    cli::cli_abort("{.arg w} must have {.field TEMP} column")
  }

  if (
    !rlang::has_name(thermal_time, "base_temp") ||
      !rlang::has_name(thermal_time, "gdd_to_maturity")
  ) {
    cli::cli_abort(
      "{.arg thermal_time} must have {.field base_temp} and
      {.field gdd_to_maturity}"
    )
  }

  if (anyNA(w$TEMP)) {
    cli::cli_abort("{.field TEMP} contains {sum(is.na(w$TEMP))} NA value{?s}")
  }

  if (thermal_time$gdd_to_maturity <= 0) {
    cli::cli_abort("{.field gdd_to_maturity} must be positive")
  }

  gdd <- cumsum(pmax(w$TEMP - thermal_time$base_temp, 0))
  pmin(2.0, 2.0 * gdd / thermal_time$gdd_to_maturity)
}

#' Calculate Development Stage (DVS)
#'
#' @param w Weather data with TEMP column.
#' @param thermal_time List with base_temp and gdd_to_maturity.
#' @returns Numeric vector of DVS (0-2).
#' @autoglobal
#' @dev
.calculate_dvs <- function(w, thermal_time) {
  if (!rlang::has_name(w, "TEMP")) {
    cli::cli_abort("{.arg w} must have {.field TEMP} column")
  }

  if (
    !rlang::has_name(thermal_time, "base_temp") ||
      !rlang::has_name(thermal_time, "gdd_to_maturity")
  ) {
    cli::cli_abort(
      "{.arg thermal_time} must have {.field base_temp} and
      {.field gdd_to_maturity}"
    )
  }

  if (anyNA(w$TEMP)) {
    cli::cli_abort("{.field TEMP} contains {sum(is.na(w$TEMP))} NA value{?s}")
  }

  if (thermal_time$gdd_to_maturity <= 0) {
    cli::cli_abort("{.field gdd_to_maturity} must be positive")
  }

  gdd <- cumsum(pmax(w$TEMP - thermal_time$base_temp, 0))
  pmin(2.0, 2.0 * gdd / thermal_time$gdd_to_maturity)
}

#' Fast Linear Interpolation (Vectorized)
#'
#' Replaces stats::approx() with faster implementation using findInterval().
#' Handles unsorted and duplicate x values automatically.
#'
#' @param .Rc Two-column matrix \[x, y\]
#' @param .xout Vector of x values to interpolate at
#' @returns Vector of interpolated y values
#' @autoglobal
#' @dev
.fn_Rc <- function(.Rc, .xout) {
  if (length(.xout) == 0L) {
    return(numeric(0))
  }

  x <- .Rc[, 1L]
  y <- .Rc[, 2L]
  n <- length(x)

  # Edge case: single row
  if (n == 1L) {
    return(rep(y[1L], length(.xout)))
  }

  # Sort if needed (check is cheap)
  if (is.unsorted(x)) {
    ord <- order(x)
    x <- x[ord]
    y <- y[ord]
  }

  # Remove duplicates (keep first occurrence)
  dup <- duplicated(x)
  if (any(dup)) {
    x <- x[!dup]
    y <- y[!dup]
    n <- length(x)
  }

  # Re-check after deduplication
  if (n == 1L) {
    return(rep(y[1L], length(.xout)))
  }

  # Find intervals
  i <- findInterval(.xout, x, left.open = FALSE)

  # Clamp to valid range [1, n-1]
  i <- pmax(1L, pmin(i, n - 1L))

  # Calculate interpolation fraction
  dx <- x[i + 1L] - x[i]
  frac <- (.xout - x[i]) / dx

  # Linear interpolation
  result <- y[i] + frac * (y[i + 1L] - y[i])

  # Handle extrapolation (use edge values)
  result[.xout <= x[1L]] <- y[1L]
  result[.xout >= x[n]] <- y[n]

  return(result)
}

#' Interpolate Parameter Values (Memoised)
#'
#' Handles scalars, vectors, matrices, data.frames, and functions.
#' Validates and cleans user-provided tables automatically.
#'
#' @param def Parameter definition
#' @param x Numeric vector of x-values
#' @returns Numeric vector of interpolated values
#'
#' @details
#' For tables (matrix/data.frame):
#' - Automatically sorts by x column if unsorted
#' - Removes duplicate x values (keeps first)
#' - Validates 2-column structure
#'
#' @autoglobal
#' @dev
.interp_impl <- function(def, x) {
  # Handle functions
  if (is.function(def)) {
    result <- tryCatch(
      def(x),
      error = function(e) {
        cli::cli_abort(c(
          "Function evaluation failed:",
          "x" = conditionMessage(e)
        ))
      }
    )
    return(pmax(0, as.numeric(result)))
  }

  # Handle scalars
  if (is.numeric(def) && length(def) == 1L) {
    return(rep(def, length(x)))
  }

  # Handle vectors (exact match required)
  if (is.numeric(def) && length(def) == length(x)) {
    return(as.numeric(def))
  }

  # Handle tables
  if (is.matrix(def) || is.data.frame(def)) {
    mat <- as.matrix(def)

    if (ncol(mat) != 2L) {
      cli::cli_abort(c(
        "Table must have 2 columns: [x, value]",
        "x" = "Got {ncol(mat)} column{?s}"
      ))
    }

    if (nrow(mat) == 0L) {
      cli::cli_abort("Table must have at least 1 row")
    }

    # Check for NA values
    if (anyNA(mat)) {
      cli::cli_warn(c(
        "!" = "Table contains NA values",
        "i" = "Removing {sum(is.na(mat[,1]))} row{?s} with NA in x column"
      ))
      mat <- mat[!is.na(mat[, 1L]), , drop = FALSE]

      if (nrow(mat) == 0L) {
        cli::cli_abort("No valid rows after removing NAs")
      }
    }

    return(.fn_Rc(mat, x))
  }

  # Handle NULL
  if (is.null(def)) {
    cli::cli_abort("Parameter definition is NULL")
  }

  # Unknown type
  cli::cli_abort(c(
    "Unsupported parameter type: {.cls {class(def)}}",
    "i" = "Expected: scalar, vector, matrix, data.frame, or function"
  ))
}

# Memoise the interpolation
.interp <- memoise::memoise(.interp_impl)
