# Diagnostics for modern seir() vs legacy SEIR()
# Inputs: result from run_compare(w, params) stored in 'result'
# Run: diagnostics <- seir_driver_diagnostics(result); head(diagnostics$drivers); diagnostics$summary

seir_driver_diagnostics <- function(result) {
  # result must include modern_out (data.table from seir) and legacy_out (S4 object with slot d)
  modern <- result$modern_out
  legacy <- result$legacy_df

  # Ensure alignment lengths
  n <- min(nrow(as.data.frame(modern)), nrow(legacy))
  modern_df <- as.data.frame(modern)[1:n, , drop = FALSE]
  legacy_df <- legacy[1:n, , drop = FALSE]

  # Helper to safely extract columns (return NA if missing)
  getcol <- function(df, nm)
    if (nm %in% names(df)) df[[nm]] else rep(NA_real_, nrow(df))

  # Compute COFR for modern (matches modern core: COFR = 1 - (diseased / (sites + diseased)))
  modern_sites <- getcol(modern_df, "sites")
  modern_diseased <- getcol(modern_df, "diseased")
  modern_cofr <- ifelse(
    (modern_sites + modern_diseased) > 0,
    1 - modern_diseased / (modern_sites + modern_diseased),
    NA_real_
  )

  # For legacy compute COFR from the legacy_df fields (legacy already had COFR computed but not exported)
  legacy_sites <- getcol(legacy_df, "sites")
  legacy_diseased <- getcol(legacy_df, "diseased")
  legacy_cofr <- ifelse(
    (legacy_sites + legacy_diseased) > 0,
    1 - legacy_diseased / (legacy_sites + legacy_diseased),
    NA_real_
  )

  # Extract Rc (modern stored as Rc? not always exported) - attempt to recompute if missing:
  # Modern: Rc vector used before rateinf = now_infectious * Rc * (COFR^a)
  modern_Rc <- getcol(modern_df, "Rc")
  if (all(is.na(modern_Rc))) {
    # try to reconstruct from rateinf and infectious/COFR/a if possible:
    now_inf_mod <- getcol(modern_df, "infectious")
    a_mod <- if ("a" %in% names(modern_df)) getcol(modern_df, "a") else NA_real_
    rateinf_mod <- getcol(modern_df, "rateinf")
    # Where possible: Rc = rateinf / (now_inf * COFR^a)
    modern_Rc <- rep(NA_real_, n)
    mask <- !is.na(now_inf_mod) & now_inf_mod > 0 & !is.na(modern_cofr)
    # If a is unknown, we will try using a=1 assumption later; we keep NAs if we can't compute.
    modern_Rc[mask] <- rateinf_mod[mask] /
      (now_inf_mod[mask] * (modern_cofr[mask]^1))
  }

  # Legacy Rc: reconstruct similarly if not present
  legacy_Rc <- getcol(legacy_df, "Rc")
  if (all(is.na(legacy_Rc))) {
    now_inf_leg <- getcol(legacy_df, "infectious")
    rateinf_leg <- getcol(legacy_df, "rateinf")
    legacy_Rc <- rep(NA_real_, n)
    mask2 <- !is.na(now_inf_leg) & now_inf_leg > 0 & !is.na(legacy_cofr)
    legacy_Rc[mask2] <- rateinf_leg[mask2] /
      (now_inf_leg[mask2] * (legacy_cofr[mask2]^1))
  }

  # Gather rtransfer, rgrowth, rsenesced available directly (or NA)
  modern_rtransfer <- getcol(modern_df, "rtransfer")
  modern_rgrowth <- getcol(modern_df, "rgrowth")
  modern_rsenesced <- getcol(modern_df, "rsenesced")
  modern_rateinf <- getcol(modern_df, "rateinf")

  legacy_rtransfer <- getcol(legacy_df, "rtransfer")
  legacy_rgrowth <- getcol(legacy_df, "rgrowth")
  legacy_rsenesced <- getcol(legacy_df, "rsenesced")
  legacy_rateinf <- getcol(legacy_df, "rateinf")

  drivers <- data.frame(
    simday = getcol(modern_df, "simday"),
    date = as.Date(getcol(modern_df, "dates")),
    modern_infectious = getcol(modern_df, "infectious"),
    legacy_infectious = getcol(legacy_df, "infectious"),
    diff_infectious = getcol(modern_df, "infectious") -
      getcol(legacy_df, "infectious"),
    modern_latent = getcol(modern_df, "latent"),
    legacy_latent = getcol(legacy_df, "latent"),
    diff_latent = getcol(modern_df, "latent") - getcol(legacy_df, "latent"),
    modern_sites = modern_sites,
    legacy_sites = legacy_sites,
    diff_sites = modern_sites - legacy_sites,
    modern_Rc = modern_Rc,
    legacy_Rc = legacy_Rc,
    diff_Rc = modern_Rc - legacy_Rc,
    modern_COFR = modern_cofr,
    legacy_COFR = legacy_cofr,
    diff_COFR = modern_cofr - legacy_cofr,
    modern_rateinf = modern_rateinf,
    legacy_rateinf = legacy_rateinf,
    diff_rateinf = modern_rateinf - legacy_rateinf,
    modern_rtransfer = modern_rtransfer,
    legacy_rtransfer = legacy_rtransfer,
    diff_rtransfer = modern_rtransfer - legacy_rtransfer,
    modern_rgrowth = modern_rgrowth,
    legacy_rgrowth = legacy_rgrowth,
    diff_rgrowth = modern_rgrowth - legacy_rgrowth,
    modern_rsenesced = modern_rsenesced,
    legacy_rsenesced = legacy_rsenesced,
    diff_rsenesced = modern_rsenesced - legacy_rsenesced,
    stringsAsFactors = FALSE
  )

  # summary diagnostics
  summary_list <- list(
    n_days = n,
    max_abs_diff_infectious = max(abs(drivers$diff_infectious), na.rm = TRUE),
    mean_abs_diff_infectious = mean(abs(drivers$diff_infectious), na.rm = TRUE),
    max_abs_diff_Rc = max(abs(drivers$diff_Rc), na.rm = TRUE),
    max_abs_diff_COFR = max(abs(drivers$diff_COFR), na.rm = TRUE),
    top_days_infectious_diff = drivers[order(-abs(drivers$diff_infectious)), ][
      1:10,
      c(
        "simday",
        "date",
        "modern_infectious",
        "legacy_infectious",
        "diff_infectious"
      )
    ],
    top_days_Rc_diff = drivers[order(-abs(drivers$diff_Rc)), ][
      1:10,
      c("simday", "date", "modern_Rc", "legacy_Rc", "diff_Rc")
    ],
    top_days_COFR_diff = drivers[order(-abs(drivers$diff_COFR)), ][
      1:10,
      c("simday", "date", "modern_COFR", "legacy_COFR", "diff_COFR")
    ],
    aggregate_sums = c(
      total_rlex = if ("rlex" %in% names(modern_df))
        sum(getcol(modern_df, "rlex"), na.rm = TRUE) else 0,
      total_rateinf_modern = sum(modern_rateinf, na.rm = TRUE),
      total_rateinf_legacy = sum(legacy_rateinf, na.rm = TRUE)
    )
  )

  # Basic plots (if interactive)
  diagnostics_plot <- function() {
    op <- par(no.readonly = TRUE)
    on.exit(par(op), add = TRUE)
    par(mfrow = c(3, 1), mar = c(3.5, 4, 2, 1))
    # Infectious
    plot(
      drivers$simday,
      drivers$modern_infectious,
      type = "l",
      col = "blue",
      lwd = 2,
      ylab = "infectious",
      xlab = "simday",
      main = "infectious: modern (blue) vs legacy (red)"
    )
    lines(
      drivers$simday,
      drivers$legacy_infectious,
      col = "red",
      lwd = 2,
      lty = 2
    )
    abline(h = 0, col = "grey")
    # Rc
    plot(
      drivers$simday,
      drivers$modern_Rc,
      type = "l",
      col = "blue",
      lwd = 2,
      ylab = "Rc (approx)",
      xlab = "simday",
      main = "Rc (reconstructed) modern vs legacy"
    )
    lines(drivers$simday, drivers$legacy_Rc, col = "red", lwd = 2, lty = 2)
    abline(h = 0, col = "grey")
    # COFR
    plot(
      drivers$simday,
      drivers$modern_COFR,
      type = "l",
      col = "blue",
      lwd = 2,
      ylab = "COFR",
      xlab = "simday",
      main = "COFR modern vs legacy"
    )
    lines(drivers$simday, drivers$legacy_COFR, col = "red", lwd = 2, lty = 2)
    abline(h = 0, col = "grey")
  }

  return(list(
    drivers = drivers,
    summary = summary_list,
    plot = diagnostics_plot
  ))
}
