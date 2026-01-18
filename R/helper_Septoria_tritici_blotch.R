#' Predict Wheat Septoria tritici blotch (STB) Intensity (EPIWHEAT defaults)
#'
#' A dynamic mechanistic simulation of Septoria tritici blotch intensity in wheat
#' built on the unified EPICROP core (SEIR + optional lesion expansion), using
#' the EPIWHEAT parameterization of Savary et al. (2015).
#'
#' This wrapper configures:
#'   - DVS-driven crop age modifier (slight decrease with age),
#'   - Temperature modifier peaking ~20–24 °C,
#'   - Moisture via consecutive rainy days over the last 4 days,
#'   - Lesion expansion enabled (RRLEX = 0.09),
#'   - Fixed latent/infectious periods (p = 11 d, i = 18 d),
#'   - Inoculum impulse at day 20 from emergence, 120-day run,
#'   - Logistic host growth and age-increasing physiological senescence.
#'
#' @param wth A `data.frame` or `data.table` with at least columns:
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
#'   \item \strong{Epidemiology}: RcOpt = 1.17; p = 11; i = 18; a = 1;
#'   RRLEX = 0.09
#'   \item \strong{Host}: SMax ≈ 174000; RRG = 0.10; physiological RRS increases
#'   with age
#'   \item \strong{Moisture}: RcW preset = "consec_rain" using the 0–4 day
#'   mapping {0→0.23, 1→0.36, 2→0.68, 3→0.93, 4→1.00}
#' }
#'
#' **Modifiers used here** (tabulated and linearly interpolated):
#' \itemize{
#'   \item RcA(DVS): slightly decreasing with development stage (DVS ∈ \[0, 2\])
#'   \item RcT(T): optimum 20–24 °C with tapering on both sides
#' }
#'
#' You may override any default by supplying named arguments in `...` that are
#' recognized by [epicrop_sim()]—for example a different `RcA`/`RcT` table,
#' a custom senescence function `RRS`, or `start = list(mode="heat_sum", ...)`.
#'
#' @return A `data.table` containing daily states, flows, diagnostics, and
#'         `AUDPC_trapz` as an attribute. Key columns: `sites`, `latent`,
#'         `infectious`, `removed`, `senesced`, `rateinf`, `rlex`, `rtransfer`,
#'         `rremoved`, `rgrowth`, `rsenesced`, `diseased`, `intensity`,
#'         plus diagnostics if enabled.
#'
#' @references
#' Savary, S., Stetkiewicz, S., Brun, F., & Willocquet, L. (2015).
#' Modelling and mapping potential epidemics of wheat diseases—examples on leaf
#' rust and Septoria tritici blotch using EPIWHEAT.
#' \emph{Eur. J. Plant Pathol.}, 142:771–790.
#' https://doi.org/10.1007/s10658-015-0650-7
#'
#' @examplesIf interactive()
#' # Example (emergence-mode start)
#' wth <- get_wth(lonlat = c(121.8919, -33.8611),
#'                dates = c("2021-06-01","2021-12-31"))
#' stb <- predict_s_tritici_blotch(wth, emergence = "2021-07-01")
#' plot(stb$dates,
#'      stb$intensity,
#'      type = "l",
#'      ylab = "Disease intensity",
#'      xlab = "Date")
#'
#' @export
#' @autoglobal
s_tritici_blotch <- function(wth, emergence, ...) {
  # --- Age modifier RcA as a function of development stage (DVS ∈ [0,2]) ---
  # Slight decrease with age (Wainshilbaum & Lipps, 1991; used in EPIWHEAT)
  RcA_STB_DVS <- cbind(
    c(0.00, 0.30, 0.60, 1.00, 1.40, 1.70, 2.00),
    c(1.00, 0.97, 0.94, 0.90, 0.87, 0.84, 0.80)
  )

  # --- Temperature modifier RcT(T) with optimum ~20–24 °C (Magboul et al., 1992) ---
  RcT_STB_22 <- cbind(
    c(10, 15, 18, 20, 22, 24, 26, 28, 30, 32),
    c(0.00, 0.20, 0.70, 0.90, 1.00, 0.95, 0.80, 0.55, 0.25, 0.00)
  )

  # --- Physiological senescence increasing with age (simple linear example) ---
  RRS_fun <- function(dvs) {
    # Low early, higher late; adjust if you have crop-specific senescence curves
    # Returns a vector same length as dvs
    seq(0.003, 0.020, length.out = length(dvs))
  }

  # --- Call the unified simulator with EPIWHEAT defaults for STB ---
  epicrop_sim(
    wth = wth,
    start = list(mode = "emergence", emergence = emergence),
    run_days = 120L,
    inoculum = list(mode = "impulse", I0 = 15, start_offset = 20L),

    # EPIWHEAT epidemiological parameters for STB
    RcOpt = 1.17,
    RcA = RcA_STB_DVS, # DVS-driven (age_driver="dvs" is default here)
    RcT = RcT_STB_22,
    RcW = list(preset = "consec_rain"), # consecutive rainy-days mapping
    RRLEX = 0.09, # lesion expansion enabled
    p = 11, # latent period (days)
    i = 18, # infectious period (days)
    SMax = 174000, # carrying capacity for STB
    a = 1, # aggregation exponent
    RRG = 0.10, # host growth
    RRS = RRS_fun, # physiological senescence increases with age
    age_driver = "dvs",

    # Users may pass extra args (e.g., steps_per_day=2, return_diagnostics=FALSE, etc.)
    ...
  )
}


#' @rdname s_tritici_blotch
#' @examplesIf interactive()
#' # use shorthand function
#' stb <- stb(wth, emergence = "2000-07-01")
#' plot(x = stb$dates, y = stb$intensity, type = "l")
#' @export
stb <- s_tritici_blotch
