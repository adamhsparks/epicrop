# compare_seir_ignore_rrlex.R
# Compare modern seir() and legacy SEIR() while ignoring RRLEX (set to NULL).
# Updated to handle modern seir() 1-indexed onset and ensure legacy SEIR gets
# the required pre-emergence row (legacy expects weather starting at emergence-1).
#
# Usage:
#  - Provide a weather data.frame `w` and a params list (see example at bottom).
#  - Source this file or copy into your R session after loading the package
#    (or sourcing the R files) so modern `seir()` and legacy `SEIR()` are available.
#
# Changes in this version:
#  - modern_onset is passed as-is to modern seir() (1..duration)
#  - legacy_onset = modern_onset - 1 is passed to legacy SEIR() (0..duration)
#  - ensure legacy weather includes the day emergence-1 and at least duration+1 rows

library(data.table)
library(stats) # for approx if needed

# --- Helpers to normalize weather and create model inputs -------------------

normalize_weather <- function(w) {
  df <- as.data.frame(w)

  # Build/parse a date column from several possible inputs
  if ("date" %in% names(df)) {
    df$date <- as.Date(df$date)
  } else if ("dates" %in% names(df)) {
    df$date <- as.Date(df$dates)
  } else if ("YYYYMMDD" %in% names(df)) {
    col <- df$YYYYMMDD
    if (inherits(col, "Date")) {
      df$date <- col
    } else {
      col_chr <- as.character(col)
      parsed <- suppressWarnings(as.Date(col_chr, format = "%Y-%m-%d"))
      if (all(is.na(parsed))) {
        parsed <- suppressWarnings(as.Date(col_chr, format = "%Y%m%d"))
      }
      if (all(is.na(parsed))) {
        stop(
          "Could not parse YYYYMMDD column as dates. Expected 'YYYY-MM-DD' or 'YYYYMMDD'."
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

  Tmin <- if ("TMIN" %in% names(df)) {
    df$TMIN
  } else if ("tmin" %in% names(df)) {
    df$tmin
  } else {
    rep(NA, nrow(df))
  }
  Tmax <- if ("TMAX" %in% names(df)) {
    df$TMAX
  } else if ("tmax" %in% names(df)) {
    df$tmax
  } else {
    rep(NA, nrow(df))
  }
  LAT <- if ("LAT" %in% names(df)) {
    df$LAT
  } else if ("lat" %in% names(df)) {
    df$lat
  } else {
    NA
  }
  LON <- if ("LON" %in% names(df)) {
    df$LON
  } else if ("lon" %in% names(df)) {
    df$lon
  } else {
    NA
  }

  dt_modern <- data.table::data.table(
    YYYYMMDD = as.Date(df$date),
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

# Ensure legacy weather contains emergence-1 and at least duration+1 rows
.ensure_legacy_preemergence <- function(legacy_wth, emergence, duration) {
  em <- as.Date(emergence)
  need_date <- em - 1L
  wdf <- legacy_wth@w

  # If the exact need_date exists, fine. If not and the first available date is after need_date,
  # prepend a copy of the first row but with date = need_date.
  if (!any(wdf$date == need_date)) {
    if (min(wdf$date) > need_date) {
      first_row <- wdf[1, , drop = FALSE]
      first_row$date <- need_date
      wdf <- rbind(first_row, wdf)
    } else {
      # if need_date is between dates, try to insert a row (copy nearest prior).
      # Find the row just before need_date and copy it (conservative).
      prior_idx <- max(which(wdf$date < need_date), na.rm = TRUE)
      if (length(prior_idx) && !is.infinite(prior_idx)) {
        new_row <- wdf[prior_idx, , drop = FALSE]
        new_row$date <- need_date
        wdf <- rbind(
          wdf[1:prior_idx, , drop = FALSE],
          new_row,
          wdf[(prior_idx + 1):nrow(wdf), , drop = FALSE]
        )
      } else {
        # fallback: copy first row
        first_row <- wdf[1, , drop = FALSE]
        first_row$date <- need_date
        wdf <- rbind(first_row, wdf)
      }
    }
  }

  # Now check we have at least duration+1 rows starting at need_date
  start_idx <- which(wdf$date == need_date)[1]
  if (is.na(start_idx) || (nrow(wdf) - start_idx + 1) < (duration + 1)) {
    stop(
      "Insufficient legacy weather rows after ensuring pre-emergence day; legacy SEIR requires at least duration+1 rows starting at emergence-1"
    )
  }

  legacy_wth@w <- wdf
  legacy_wth
}

# --- Output conversion & alignment -----------------------------------------
convert_modern_to_legacy_df <- function(modern_out, H0, I0 = 0) {
  dt <- as.data.frame(modern_out, stringsAsFactors = FALSE)
  if (nrow(dt) == 0L) {
    stop("modern_out has zero rows")
  }
  if (!"dates" %in% names(dt)) {
    stop("modern_out missing 'dates' column")
  }
  first_date <- as.Date(dt$dates[1])

  cols <- names(dt)
  day0 <- setNames(vector("list", length(cols)), cols)

  for (cn in cols) {
    col_vals <- dt[[cn]]
    if (inherits(col_vals, "Date")) {
      if (cn == "dates") {
        day0[[cn]] <- first_date - 1L
      } else {
        day0[[cn]] <- as.Date(NA)
      }
    } else if (is.numeric(col_vals)) {
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
      day0[[cn]] <- NA_character_
    }
  }

  day0df <- as.data.frame(day0, stringsAsFactors = FALSE)
  full <- rbind(day0df, dt)

  if ("intensity" %in% names(full)) {
    full$severity <- as.numeric(full$intensity) * 100
  } else {
    full$severity <- NA_real_
  }

  get_col <- function(nm, fallback = NA_real_) {
    if (nm %in% names(full)) {
      return(full[[nm]])
    }
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
  if (is.list(legacy_obj) && !is.null(legacy_obj$d)) {
    return(as.data.frame(legacy_obj$d))
  }
  if (!is.null(slotNames(legacy_obj)) && "d" %in% slotNames(legacy_obj)) {
    return(as.data.frame(slot(legacy_obj, "d")))
  }
  stop(
    "Could not extract legacy result data.frame (expected slot 'd' or list$d)."
  )
}

# --- Main run & compare function -------------------------------------------

# params: list containing param mapping to both functions (see example below)
run_compare <- function(w_df, params, plot = TRUE) {
  # normalize
  nws <- normalize_weather(w_df)
  w_modern <- nws$modern
  w_legacy <- nws$legacy

  # Ensure legacy weather includes emergence-1 and enough rows
  w_legacy <- .ensure_legacy_preemergence(
    w_legacy,
    params$emergence,
    params$duration
  )

  # Onset mapping:
  # modern seir() uses 1..duration (1-indexed). Legacy SEIR uses 0..duration (0-indexed).
  modern_onset <- as.integer(params$onset) # 1-indexed expected by modern seir()
  legacy_onset <- as.integer(params$onset) - 1L # legacy expects 0-index

  if (legacy_onset < 0L) {
    stop(
      "params$onset must be >= 1 for modern-onset to map to legacy (legacy_onset would be < 0)"
    )
  }

  # modern seir call (RRLEX NULL to ignore lesion expansion)
  modern_args <- list(
    wth = w_modern,
    emergence = params$emergence,
    onset = modern_onset,
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
    RcW = params$rhRc,
    simple_wetness = TRUE,
    RRLEX = NULL
  )
  cat("Calling modern seir() with onset (1-index) =", modern_onset, "\n")
  modern_out <- do.call(seir, modern_args)

  # legacy SEIR call: translate onset and pass legacy weather (S4 tmp_wth)
  legacy_args <- list(
    wth = w_legacy,
    emergence = params$emergence,
    onset = legacy_onset,
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
  cat("Calling legacy SEIR() with onset (0-index) =", legacy_onset, "\n")
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
    date = modern_legacy_df$date,
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

  # Build drivers diagnostics if available on both
  drivers <- list()
  drivers$modern_Rc <- if ("Rc" %in% names(modern_out)) modern_out$Rc else NA
  drivers$legacy_Rc <- NA
  # if legacy exposes Rc in its output, user can add extraction here

  # Diagnostics summary
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
    cmp = cmp,
    drivers = drivers
  )
}

# ---------------- Example / template call ----------------------------------
# Example parameter mapping (fill with your values)
# NOTE: onset below is 1-indexed for modern seir() (1 = emergence day).
params <- list(
  emergence = "2000-06-30",
  onset = 15, # 1-indexed (modern). legacy will get onset-1 (0-index).
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

# Then run (example):
# result <- run_compare(w, params)
# head(result$cmp)
