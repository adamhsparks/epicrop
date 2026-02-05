# compare_seir_ignore_rrlex.R
# Compare modern seir() and legacy SEIR() while ignoring RRLEX (set to NULL).
# Usage:
# 1) Provide a daily weather data.frame `w` with at least:
#      - date (Date class) OR YYYY-MM-DD strings
#      - tavg  OR TEMP  (daily mean temperature °C)
#      - rhmax OR RHUM  (daily mean or max relative humidity %)
#      - prec  OR RAIN  (daily rainfall mm)
#    Optional: TMIN, TMAX, LAT, LON
#
# 2) Source this file or copy into your R session after loading the package
#    (or sourcing the R files) so modern `seir()` and legacy `SEIR()` are available.
#
# 3) Call run_compare(w, params...) with mapped parameters (examples below).
#
# Note: legacy SEIR() code expects an S4 object with a slot `@w` (data.frame),
# so we create a minimal temporary S4 class "tmp_wth" with slot w to host the data.frame.

library(data.table)
library(stats) # for approx if needed
# (Assumes modern seir() and legacy SEIR() are in scope)

# --- Helpers to normalize weather and create model inputs -------------------

# Accepts a data.frame 'w' and returns a list with modern and legacy representations.
# Input 'w' must have a date column or a YYYY/MM/DD character column.
# tolerant normalize_weather: accepts YYYYMMDD as "20000630" or "2000-06-30" or 'dates'/'date' Date
normalize_weather <- function(w) {
  df <- as.data.frame(w)

  # Build/parse a date column from several possible inputs
  if ("date" %in% names(df)) {
    df$date <- as.Date(df$date)
  } else if ("dates" %in% names(df)) {
    df$date <- as.Date(df$dates)
  } else if ("YYYYMMDD" %in% names(df)) {
    col <- df$YYYYMMDD
    # If already Date class
    if (inherits(col, "Date")) {
      df$date <- col
    } else {
      col_chr <- as.character(col)
      # Try ISO 'YYYY-MM-DD'
      parsed <- suppressWarnings(as.Date(col_chr, format = "%Y-%m-%d"))
      if (all(is.na(parsed))) {
        # Try compact 'YYYYMMDD'
        parsed <- suppressWarnings(as.Date(col_chr, format = "%Y%m%d"))
      }
      if (all(is.na(parsed))) {
        stop(
          "Could not parse YYYYMMDD column as dates. Expected formats 'YYYY-MM-DD' or 'YYYYMMDD'."
        )
      }
      df$date <- parsed
    }
  } else {
    stop(
      "Weather data.frame must include a 'date' or 'YYYYMMDD'/'dates' column"
    )
  }

  # standard column mapping
  if ("tavg" %in% names(df)) {
    tavg <- df$tavg
  } else if ("TEMP" %in% names(df)) {
    tavg <- df$TEMP
  } else {
    stop("Weather must include 'tavg' or 'TEMP' column")
  }

  if ("rhmax" %in% names(df)) {
    rh <- df$rhmax
  } else if ("RHUM" %in% names(df)) {
    rh <- df$RHUM
  } else if ("RH" %in% names(df)) {
    rh <- df$RH
  } else {
    stop("Weather must include 'rhmax' or 'RHUM' (relative humidity)")
  }

  if ("prec" %in% names(df)) {
    prec <- df$prec
  } else if ("RAIN" %in% names(df)) {
    prec <- df$RAIN
  } else if ("rain" %in% names(df)) {
    prec <- df$rain
  } else {
    stop("Weather must include 'prec' or 'RAIN' (precipitation)")
  }

  Tmin <- if ("TMIN" %in% names(df)) df$TMIN else if ("tmin" %in% names(df))
    df$tmin else rep(NA, nrow(df))
  Tmax <- if ("TMAX" %in% names(df)) df$TMAX else if ("tmax" %in% names(df))
    df$tmax else rep(NA, nrow(df))
  LAT <- if ("LAT" %in% names(df)) df$LAT else if ("lat" %in% names(df))
    df$lat else NA
  LON <- if ("LON" %in% names(df)) df$LON else if ("lon" %in% names(df))
    df$lon else NA

  dt_modern <- data.table::data.table(
    YYYYMMDD = as.Date(df$date), # use Date to match seir() emergence type
    dates = as.Date(df$date),
    DOY = as.integer(format(df$date, "%j")),
    TEMP = as.numeric(tavg),
    RHUM = as.numeric(rh),
    RAIN = as.numeric(prec),
    TMIN = as.numeric(Tmin),
    TMAX = as.numeric(Tmax)
  )

  if (!is.na(LAT[1]) && !is.na(LON[1])) {
    dt_modern[, `:=`(LAT = LAT[1], LON = LON[1])]
  }
  class(dt_modern) <- c("epicrop.wth", class(dt_modern))

  legacy_wdf <- data.frame(
    date = df$date,
    tavg = as.numeric(tavg),
    rhmax = as.numeric(rh),
    prec = as.numeric(prec),
    stringsAsFactors = FALSE
  )
  if (!isClass("tmp_wth")) {
    setClass("tmp_wth", slots = c(w = "data.frame"))
  }
  legacy_wth <- new("tmp_wth", w = legacy_wdf)

  list(modern = dt_modern, legacy = legacy_wth)
}
# --- Output conversion & alignment -----------------------------------------
# Robust convert_modern_to_legacy_df: builds a day-0 row matching modern_out columns
convert_modern_to_legacy_df <- function(modern_out, H0, I0 = 0) {
  dt <- as.data.frame(modern_out, stringsAsFactors = FALSE)
  if (nrow(dt) == 0L) stop("modern_out has zero rows")
  # ensure dates column exists and is Date
  if (!"dates" %in% names(dt)) stop("modern_out missing 'dates' column")
  first_date <- as.Date(dt$dates[1])

  cols <- names(dt)
  # build a named list for day0 with same column names
  day0 <- setNames(vector("list", length(cols)), cols)

  for (cn in cols) {
    col_vals <- dt[[cn]]
    # Date-like
    if (inherits(col_vals, "Date")) {
      if (cn == "dates") {
        day0[[cn]] <- first_date - 1L
      } else {
        day0[[cn]] <- as.Date(NA)
      }
    } else if (is.numeric(col_vals)) {
      # sensible initial values for common state/flow names
      if (cn %in% c("sites")) {
        day0[[cn]] <- as.numeric(H0)
      } else if (cn %in% c("rateinf") && !is.null(I0) && I0 > 0) {
        day0[[cn]] <- as.numeric(I0)
      } else {
        day0[[cn]] <- 0
      }
    } else if (is.integer(col_vals)) {
      if (cn == "simday") day0[[cn]] <- 0L else day0[[cn]] <- NA_integer_
    } else {
      # character or other: set NA_character_
      day0[[cn]] <- NA_character_
    }
  }

  day0df <- as.data.frame(day0, stringsAsFactors = FALSE)

  # rbind safely now that columns match
  full <- rbind(day0df, dt)

  # compute severity from intensity if present
  if ("intensity" %in% names(full)) {
    full$severity <- as.numeric(full$intensity) * 100
  } else {
    full$severity <- NA_real_
  }

  # Build legacy-like data.frame with required columns (create missing ones as NA)
  get_col <- function(nm, fallback = NA_real_) {
    if (nm %in% names(full)) return(full[[nm]])
    # return vector of NA with correct length
    rep(fallback, nrow(full))
  }

  legacy_like <- data.frame(
    date = as.Date(get_col("dates", NA)),
    simday = as.integer(get_col("simday", seq(0, nrow(full) - 1))),
    sites = as.numeric(get_col("sites")),
    latent = as.numeric(get_col("latent")),
    infectious = as.numeric(get_col("infectious")),
    removed = as.numeric(get_col("removed")),
    senesced = as.numeric(get_col("senesced")),
    rateinf = as.numeric(get_col("rateinf")),
    rtransfer = as.numeric(get_col("rtransfer")),
    rgrowth = as.numeric(get_col("rgrowth")),
    rsenesced = as.numeric(get_col("rsenesced")),
    diseased = as.numeric(get_col("diseased")),
    severity = as.numeric(get_col("severity")),
    stringsAsFactors = FALSE
  )

  return(legacy_like)
}


# Extract legacy data.frame from SEIR S4 object (slot 'd' or list$d)
extract_legacy_df <- function(legacy_obj) {
  if (is.list(legacy_obj) && !is.null(legacy_obj$d))
    return(as.data.frame(legacy_obj$d))
  if (!is.null(slotNames(legacy_obj)) && "d" %in% slotNames(legacy_obj))
    return(as.data.frame(slot(legacy_obj, "d")))
  # Try common fields if direct extraction fails
  stop(
    "Could not extract legacy result data.frame (expected slot 'd' or list$d)."
  )
}

# --- Main run & compare function -------------------------------------------

# params: list containing param mapping to both functions
#   required fields (example names):
#     emergence (as Date or string), onset (int), duration (int),
#     initSites, initInfection,
#     ageRc, tmpRc, rhRc, baseRc, latrans (p), inftrans (i), siteMax, AGGR, RRPhysiolSenesc, RRG
#
# modern params mapping:
#   H0 = initSites, I0 = initInfection, RcA = ageRc, RcT = tmpRc, RcOpt = baseRc,
#   p = latrans, i = inftrans, Sx = siteMax, a = AGGR, RRS = RRPhysiolSenesc, RRG = RRG
run_compare <- function(w_df, params, plot = TRUE) {
  # normalize
  nws <- normalize_weather(w_df)
  w_modern <- nws$modern
  w_legacy <- nws$legacy

  # modern seir call (RRLEX NULL to ignore lesion expansion)
  modern_args <- list(
    wth = w_modern,
    emergence = params$emergence,
    onset = params$onset,
    duration = params$duration,
    H0 = params$initSites,
    I0 = params$initInfection,
    RcA = params$ageRc,
    RcT = params$tmpRc,
    RcOpt = params$baseRc,
    p = params$latrans,
    i = params$inftrans,
    Sx = params$siteMax,
    a = params$AGGR,
    RRS = params$RRPhysiolSenesc,
    RRG = params$RRG,
    RcW = params$rhRc, # modern RcW corresponds to rhRc when simple_wetness = TRUE ignored
    simple_wetness = TRUE,
    RRLEX = NULL
  )
  # Run modern; allow errors to bubble up so you can inspect call
  modern_out <- do.call(seir, modern_args)

  # legacy SEIR call (note parameter names differ)
  legacy_args <- list(
    wth = w_legacy,
    emergence = params$emergence,
    onset = params$onset,
    duration = params$duration,
    initSites = params$initSites,
    initInfection = params$initInfection,
    ageRc = params$ageRc,
    tmpRc = params$tmpRc,
    rhRc = params$rhRc,
    baseRc = params$baseRc,
    latrans = params$latrans,
    inftrans = params$inftrans,
    siteMax = params$siteMax,
    AGGR = params$AGGR,
    RRPhysiolSenesc = params$RRPhysiolSenesc,
    RRG = params$RRG
  )
  legacy_out <- do.call(SEIR, legacy_args)

  # Align/convert modern output to legacy-like
  modern_legacy_df <- convert_modern_to_legacy_df(
    modern_out,
    H0 = params$initSites,
    I0 = params$initInfection
  )
  legacy_df <- extract_legacy_df(legacy_out)

  # Trim to common length
  n <- min(nrow(modern_legacy_df), nrow(legacy_df))
  modern_legacy_df <- modern_legacy_df[1:n, ]
  legacy_df <- legacy_df[1:n, ]

  # Compare infectious and other states
  cmp <- data.frame(
    simday = modern_legacy_df$simday,
    modern_infectious = modern_legacy_df$infectious,
    legacy_infectious = legacy_df$infectious,
    diff_infectious = modern_legacy_df$infectious - legacy_df$infectious,
    modern_latent = modern_legacy_df$latent,
    legacy_latent = legacy_df$latent,
    diff_latent = modern_legacy_df$latent - legacy_df$latent,
    modern_sites = modern_legacy_df$sites,
    legacy_sites = legacy_df$sites,
    diff_sites = modern_legacy_df$sites - legacy_df$sites,
    stringsAsFactors = FALSE
  )

  # Diagnostics
  cat("Comparison diagnostics:\n")
  cat(sprintf(" rows compared: %d\n", n))
  cat(sprintf(
    " infectious: max abs diff = %g, mean abs diff = %g\n",
    max(abs(cmp$diff_infectious), na.rm = TRUE),
    mean(abs(cmp$diff_infectious), na.rm = TRUE)
  ))
  cat(sprintf(
    " latent:      max abs diff = %g, mean abs diff = %g\n",
    max(abs(cmp$diff_latent), na.rm = TRUE),
    mean(abs(cmp$diff_latent), na.rm = TRUE)
  ))
  cat(sprintf(
    " sites:       max abs diff = %g, mean abs diff = %g\n",
    max(abs(cmp$diff_sites), na.rm = TRUE),
    mean(abs(cmp$diff_sites), na.rm = TRUE)
  ))

  if (plot) {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar), add = TRUE)
    par(mfrow = c(2, 1), mar = c(4, 4, 2, 1))
    plot(
      cmp$simday,
      cmp$modern_infectious,
      type = "l",
      col = "blue",
      lwd = 2,
      xlab = "simday",
      ylab = "infectious",
      main = "Infectious: modern (blue) vs legacy (red)"
    )
    lines(cmp$simday, cmp$legacy_infectious, col = "red", lwd = 2, lty = 2)
    legend(
      "topright",
      legend = c("modern", "legacy"),
      col = c("blue", "red"),
      lwd = 2,
      bty = "n"
    )

    plot(
      cmp$simday,
      cmp$diff_infectious,
      type = "h",
      col = "purple",
      xlab = "simday",
      ylab = "modern - legacy",
      main = "Difference in infectious (modern - legacy)"
    )
    abline(h = 0, col = "gray")
  }

  list(
    modern_out = modern_out,
    legacy_out = legacy_out,
    modern_like = modern_legacy_df,
    legacy_df = legacy_df,
    cmp = cmp
  )
}

# ---------------- Example / template call ----------------------------------
# Example parameter mapping (fill with your values)
params <- list(
  emergence = "2000-06-30",
  onset = 15,
  duration = 120,
  initSites = 600,
  initInfection = 1,
  ageRc = .RcA_bacterial_blight, # matrix cbind(age, modifier)
  tmpRc = .RcT_bacterial_blight, # matrix cbind(temp, modifier)
  rhRc = 90, # matrix or scalar (legacy AFGen uses these)
  baseRc = 1.14,
  latrans = 5,
  inftrans = 20,
  siteMax = 30000,
  AGGR = 1,
  RRPhysiolSenesc = 0.01,
  RRG = 0.1
)

# Then run:
result <- run_compare(w, params)
head(result$cmp)
#
# The function returns a list with the raw modern and legacy outputs plus aligned data.frames and a per-day comparison table.
