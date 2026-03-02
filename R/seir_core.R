# R wrappers that dispatch to Rcpp cores (daily or step-resolution) and return data.table
# Expects Rcpp::compileAttributes() has been run so seir_core_*_cpp functions are available.

#' Wrapper that dispatches to no-lex core (daily or steps) and returns data.table
#' @param steps_per_day integer >= 1
#' @param audpc optional externally computed AUDPC (numeric scalar)
#' @param simday,dates,lat,lon optional metadata to attach
#' @dev
.seir_core_no_lex_dt <- function(
  duration,
  steps_per_day = 1L,
  onset_idx,
  H0,
  Iprim,
  Rc,
  p_days,
  i_days,
  RRG_vals,
  RRS_vals,
  a_vals,
  Sx,
  audpc = NULL,
  simday = NULL,
  dates = NULL,
  lat = NULL,
  lon = NULL
) {
  if (as.integer(steps_per_day) == 1L) {
    out_list <- seir_core_no_lex_cpp(
      duration = as.integer(duration),
      onset_idx = as.integer(onset_idx),
      H0 = as.double(H0),
      Iprim = as.double(Iprim),
      Rc = as.double(Rc),
      p_days = as.double(p_days),
      i_days = as.double(i_days),
      RRG_vals = as.double(RRG_vals),
      RRS_vals = as.double(RRS_vals),
      a_vals = as.double(a_vals),
      Sx = as.double(Sx)
    )
  } else {
    out_list <- seir_core_no_lex_steps_cpp(
      duration = as.integer(duration),
      steps_per_day = as.integer(steps_per_day),
      onset_idx = as.integer(onset_idx),
      H0 = as.double(H0),
      Iprim = as.double(Iprim),
      Rc = as.double(Rc),
      p_days = as.double(p_days),
      i_days = as.double(i_days),
      RRG_vals = as.double(RRG_vals),
      RRS_vals = as.double(RRS_vals),
      a_vals = as.double(a_vals),
      Sx = as.double(Sx)
    )
  }

  dt <- data.table::as.data.table(out_list)

  cols_order <- c(
    "sites",
    "latent",
    "infectious",
    "removed",
    "senesced",
    "rateinf",
    "rlex",
    "rtransfer",
    "rremoved",
    "rgrowth",
    "rsenesced",
    "diseased",
    "intensity"
  )
  data.table::setcolorder(dt, intersect(cols_order, names(dt)))

  # attach optional metadata
  if (!is.null(simday)) {
    dt[, simday := simday]
  }
  if (!is.null(dates)) {
    dt[, dates := dates]
  }
  if (!is.null(lat)) {
    dt[, lat := lat]
  }
  if (!is.null(lon)) {
    dt[, lon := lon]
  }

  # move simday/dates to front if present
  front <- intersect(c("simday", "dates"), names(dt))
  if (length(front)) {
    remaining <- setdiff(names(dt), front)
    data.table::setcolorder(dt, c(front, remaining))
  }

  if (!is.null(audpc)) {
    data.table::setattr(dt, "AUDPC", audpc)
  } else {
    data.table::setattr(dt, "AUDPC", .calculate_audpc(dt$intensity))
  }

  data.table::setattr(dt, "class", unique(c("epicrop.sim", class(dt))))
  dt
}

#' Wrapper that dispatches to with-lex core and returns data.table
#' @dev
.seir_core_with_lex_dt <- function(
  duration,
  steps_per_day = 1L,
  onset_idx,
  H0,
  Iprim,
  Rc,
  p_days,
  i_days,
  RRG_vals,
  RRS_vals,
  RRLEX_vals,
  a_vals,
  Sx,
  audpc = NULL,
  simday = NULL,
  dates = NULL,
  lat = NULL,
  lon = NULL
) {
  if (as.integer(steps_per_day) == 1L) {
    out_list <- seir_core_with_lex_cpp(
      duration = as.integer(duration),
      onset_idx = as.integer(onset_idx),
      H0 = as.double(H0),
      Iprim = as.double(Iprim),
      Rc = as.double(Rc),
      p_days = as.double(p_days),
      i_days = as.double(i_days),
      RRG_vals = as.double(RRG_vals),
      RRS_vals = as.double(RRS_vals),
      RRLEX_vals = as.double(RRLEX_vals),
      a_vals = as.double(a_vals),
      Sx = as.double(Sx)
    )
  } else {
    out_list <- seir_core_with_lex_steps_cpp(
      duration = as.integer(duration),
      steps_per_day = as.integer(steps_per_day),
      onset_idx = as.integer(onset_idx),
      H0 = as.double(H0),
      Iprim = as.double(Iprim),
      Rc = as.double(Rc),
      p_days = as.double(p_days),
      i_days = as.double(i_days),
      RRG_vals = as.double(RRG_vals),
      RRS_vals = as.double(RRS_vals),
      RRLEX_vals = as.double(RRLEX_vals),
      a_vals = as.double(a_vals),
      Sx = as.double(Sx)
    )
  }

  dt <- data.table::as.data.table(out_list)

  cols_order <- c(
    "sites",
    "latent",
    "infectious",
    "removed",
    "senesced",
    "rateinf",
    "rlex",
    "rtransfer",
    "rremoved",
    "rgrowth",
    "rsenesced",
    "diseased",
    "intensity"
  )
  data.table::setcolorder(dt, intersect(cols_order, names(dt)))

  if (!is.null(simday)) {
    dt[, simday := simday]
  }
  if (!is.null(dates)) {
    dt[, dates := dates]
  }
  if (!is.null(lat)) {
    dt[, lat := lat]
  }
  if (!is.null(lon)) {
    dt[, lon := lon]
  }

  front <- intersect(c("simday", "dates"), names(dt))
  if (length(front)) {
    remaining <- setdiff(names(dt), front)
    data.table::setcolorder(dt, c(front, remaining))
  }

  if (!is.null(audpc)) {
    data.table::setattr(dt, "AUDPC", audpc)
  } else {
    data.table::setattr(dt, "AUDPC", .calculate_audpc(dt$intensity))
  }

  data.table::setattr(dt, "class", unique(c("epicrop.sim", class(dt))))
  dt
}
