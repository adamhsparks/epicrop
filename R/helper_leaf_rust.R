#' Predict Wheat Leaf (Brown) Rust Intensity (EPIWHEAT defaults)
#'
#' A dynamic mechanistic simulation of leaf/brown rust (*Puccinia triticina*)
#' using the unified EPICROP core (SEIR + optional lesion expansion) with
#' the EPIWHEAT parameterization of Savary et al. (2015).
#'
#' This wrapper configures:
#'   - DVS-driven crop age modifier (slight decrease with age),
#'   - Temperature modifier peaking ≈15 °C,
#'   - Moisture-neutral infection (RcW = 1; no wetness effect),
#'   - Lesion expansion disabled (RRLEX = 0),
#'   - Fixed latent/infectious periods (p = 7 d, i = 31 d),
#'   - Inoculum impulse at day 20 from emergence; 120-day run,
#'   - Logistic host growth and age-increasing physiological senescence.
#'
#' @param wth A `data.frame` or `data.table` with at least:
#'   `YYYYMMDD` (Date), `TEMP` (°C), `RAIN` (mm). Optional: `RHUM` (%), `LAT`,
#'   `LON`.
#' @param emergence Character date `"YYYY-MM-DD"` for expected crop emergence.
#' @param ... Optional advanced arguments forwarded to [epicrop_sim()],
#'   e.g., `steps_per_day = 2`, or to override defaults (see Details).
#'
#' @details
#' **Defaults (Savary et al., 2015)**:
#' \itemize{
#'   \item \strong{Timing}: onset = 20 d after emergence, duration = 120 d
#'   \item \strong{Epidemiology}: RcOpt = 1.472; p = 7; i = 31; a = 1; RRLEX = 0
#'   \item \strong{Host}: SMax ≈ 750000; RRG = 0.10; physiological RRS increases
#'   with age
#'   \item \strong{Moisture}: RcW preset = "const1" (wetness does not affect
#'   infection)
#' }
#'
#' **Modifiers used here**:
#' \itemize{
#'   \item RcA(DVS): slightly decreasing with development stage (DVS ∈ \[0, 2\])
#'   \item RcT(T): optimum ≈15 °C, tapering on both sides
#' }
#'
#' You can override any default by supplying named arguments in `...` that are
#' recognized by [epicrop_sim()]—for example different `RcA`/`RcT` tables,
#' a custom senescence function `RRS`, or `start = list(mode="heat_sum", ...)`.
#'
#' @return A `data.table` containing daily states, flows, diagnostics, and
#'         `AUDPC_trapz` (attribute). Key columns: `sites`, `latent`, `infectious`,
#'         `removed`, `senesced`, `rateinf`, `rlex`, `rtransfer`, `rremoved`,
#'         `rgrowth`, `rsenesced`, `diseased`, `intensity`, plus diagnostics if enabled.
#'
#' @references
#' Savary, S., Stetkiewicz, S., Brun, F., & Willocquet, L. (2015).
#' Modelling and mapping potential epidemics of wheat diseases—examples on leaf
#' rust and Septoria tritici blotch using EPIWHEAT. \emph{Eur. J.
#' Plant Pathol.}, 142:771–790. https://doi.org/10.1007/s10658-015-0650-7
#'
#' @examplesIf interactive()
#' # Example (emergence-mode start)
#' # wth <- get_wth(lonlat = c(121.8919, -33.8611),
#' #                dates = c("2021-06-01","2021-12-31"))
#' # lr <- predict_leaf_rust(wth, emergence = "2021-07-01")
#' # plot(lr$dates, lr$intensity, type = "l",
#' #      ylab = "Disease intensity", xlab = "Date")
#'
#' # Example (heat-sum start, EPIWHEAT-style initialization)
#' # lr_hs <- predict_leaf_rust(
#' #   wth, emergence = NULL,
#' #   start = list(mode = "heat_sum", base_temp = 0, heat_sum_threshold = 200)
#' # )
#'
#' @export
leaf_rust <- function(wth, emergence, ...) {
  # --- Age modifier RcA as a function of development stage (DVS ∈ [0,2]) ---
  # Slight decrease with age (matches EPIWHEAT narrative for BR)
  RcA_BR_DVS <- cbind(
    c(0.00, 0.30, 0.60, 1.00, 1.40, 1.70, 2.00),
    c(1.00, 0.96, 0.92, 0.88, 0.84, 0.80, 0.76)
  )

  # --- Temperature modifier RcT(T) with optimum ≈15 °C ---
  RcT_BR_15 <- cbind(
    c(5, 10, 12, 15, 18, 20, 25, 30),
    c(0.00, 0.45, 0.75, 1.00, 0.70, 0.55, 0.20, 0.00)
  )

  # --- Physiological senescence increasing with age (gentle slope for BR) ---
  RRS_fun <- function(dvs) {
    # Returns a vector same length as dvs; tune if you have crop-specific curves
    seq(0.003, 0.015, length.out = length(dvs))
  }

  # --- Call the unified simulator with EPIWHEAT defaults for BR ---
  epicrop_sim(
    wth = wth,
    start = list(mode = "emergence", emergence = emergence),
    run_days = 120L,
    inoculum = list(mode = "impulse", I0 = 10, start_offset = 20L),

    # EPIWHEAT epidemiological parameters for BR
    RcOpt = 1.472,
    RcA = RcA_BR_DVS, # DVS-driven (age_driver="dvs" below)
    RcT = RcT_BR_15,
    RcW = list(preset = "const1"), # wetness has no effect on infection (BR)
    RRLEX = 0.0, # negligible lesion expansion for BR
    p = 7, # latent period (days)
    i = 31, # infectious period (days)
    SMax = 750000, # carrying capacity for BR
    a = 1, # aggregation exponent
    RRG = 0.10, # host growth
    RRS = RRS_fun, # physiological senescence increases with age
    age_driver = "dvs",

    # Users may pass extra args (e.g., steps_per_day=2, return_diagnostics=FALSE, etc.)
    ...
  )
}

#' @rdname leaf_rust
#' @examplesIf interactive()
#' # use shorthand function
#' br <- brown_rust(wth, emergence = "2000-07-01")
#' plot(x = br$dates, y = br$intensity, type = "l")
#' @export
brown_rust <- leaf_rust # alias


#' @rdname leaf_rust
#' @examplesIf interactive()
#' # use shorthand function
#' br <- lr(wth, emergence = "2000-07-01")
#' plot(x = lr$dates, y = lr$intensity, type = "l")
#' @export
lr <- leaf_rust # alias
