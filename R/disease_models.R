#' Rice and Wheat Disease Models
#'
#' Convenience wrappers around [seir()] with pre-configured parameter tables
#' for well-characterised crop diseases. Each function encapsulates parameters
#' from published epidemiological studies, allowing one-line disease
#' simulations.
#'
#' All functions support parameter override via `...` arguments, enabling
#' sensitivity analysis and custom cultivar/environment modelling.
#'
#' @section Available Models:
#'
#' **Rice Diseases (Savary *et al.* 2012):**
#' * [bacterial_blight()] – Bacterial blight (*Xanthomonas oryzae* pv. *oryzae*)
#' * [brown_spot()] – Brown spot (*Bipolaris oryzae*)
#' * [leaf_blast()] – Leaf blast (*Magnaporthe oryzae*)
#' * [sheath_blight()] – Sheath blight (*Rhizoctonia solani*)
#' * [tungro()] – Tungro (viral, vector-borne)
#'
#' **Rice Diseases (Kim *et al.* 2015 modifications):**
#' * [modified_kim_leaf_blast()] – Cultivar-adjusted leaf blast
#' * [modified_kim_sheath_blight()] – Cultivar-adjusted sheath blight
#'
#' **Wheat Diseases (Savary *et al.* 2015):**
#' * [leaf_rust()] – Leaf rust (*Puccinia triticina*)
#' * [s_tritici_blotch()] – Septoria tritici blotch (*Zymoseptoria tritici*)
#'
#' @name disease_models
#' @inheritParams seir
#' @param RcA Optional custom age modifier table (2-column numeric matrix).
#'   Overrides the published default for this disease.
#' @param RcT Optional custom temperature modifier table (2-column numeric
#'   matrix).
#'   Overrides the published default for this disease.
#' @param RRG Optional custom relative growth rate (scalar numeric, vector, or
#'   function). Overrides the published default.
#' @param RRS Optional custom relative senescence rate (scalar numeric, vector,
#'   function, or matrix). Overrides the published default. Applies to rice
#'   disease models only.
#' @param RRPSN Optional custom relative photoperiod-adjusted senescence rate
#'   (scalar numeric, vector, function, or matrix). Overrides the published
#'   default. Applies to wheat disease models only.
#' @param rhlim Optional override for relative humidity threshold (%) for leaf
#'   wetness. Defaults to disease-specific value (typically 90).
#' @param rainlim Optional override for rainfall threshold (mm) for leaf
#'   wetness. Defaults to disease-specific value (typically 5).
#' @param ... Additional parameters passed to [seir()], allowing full
#'   customisation of any parameter (onset, duration, H0, I0, p, i, Sx, a,
#'   simple_wetness, etc.).
#'
#' @returns A [data.table::data.table()] of class `epicrop.sim` with disease
#'   progression over time. See [seir()] for output details.
#'
#' @section Parameter Sources:
#'
#' **Rice models** use baseline parameters from:
#' * Savary *et al.* (2012) – Original parameterisation from literature
#' * Kim *et al.* (2015) – Versions calibrated for Korean rice cultivars
#'
#' **Wheat models** use:
#' * Savary *et al.* (2015) – European calibration
#'
#' @inheritSection seir TMIN/TMAX Details
#' @inheritSection seir LAT/LON Details
#'
#' @references
#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J. 2012.
#' Modeling and mapping potential epidemics of rice diseases globally.
#' *Crop Protection*, 34: 6–17.
#' \doi{10.1016/j.cropro.2011.11.009}
#'
#' Kim, K. H., Cho, J., Lee, Y. H., and Lee, W. S. 2015.
#' Predicting potential epidemics of rice leaf blast and sheath blight in South
#' Korea. *Agricultural and Forest Meteorology*, 203: 191–207.
#' \doi{10.1016/j.agrformet.2015.01.011}
#'
#' Savary, S., Stetkiewicz, S., Brun, F., and Willocquet, L. 2015.
#' Modelling and mapping potential epidemics of wheat diseases.
#' *European Journal of Plant Pathology*, 142: 771–790.
#' \doi{10.1007/s10658-015-0650-7}
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
NULL

#' @rdname disease_models
#' @export
#'
#' @description
#' `bacterial_blight()` models rice bacterial blight (*Xanthomonas oryzae*
#' pv. *oryzae*). The disease requires high humidity and warm temperatures
#' (optimum 25–30\ifelse{latex}{\out{$^\circ$}}{°}.RC)
#' and is most severe in the Philippines and Asia-Pacific
#' region.
#'
#' @examplesIf interactive()
#' # Fetch weather data for a location in the Philippines
#' wth <- get_wth(
#'   lonlat = c(121.255669, 14.16742),
#'   dates = "2000-06-30",
#'   duration = 120L
#' )
#'
#' # Run bacterial blight model with default parameters (Savary *et al.* 2012)
#' bb_sim <- bacterial_blight(
#'   wth = wth,
#'   emergence = "2000-06-30"
#' )
#'
#' # Inspect results
#' head(bb_sim)
#' plot(bb_sim$simday, bb_sim$intensity, type = "l",
#'      main = "Rice Bacterial Blight Progression",
#'      xlab = "Days", ylab = "Disease Intensity")
#'
#'
bacterial_blight <- function(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
) {
  params <- .epi_get_params("rice", "bacterial_blight")
  seir(
    wth = wth,
    emergence = emergence,
    RcA = RcA %||% params$RcA,
    RcT = RcT %||% params$RcT,
    RRG = RRG %||% params$RRG,
    RRS = RRS %||% params$RRS,
    onset = 20L,
    duration = 120L,
    rhlim = rhlim %||% 90L,
    rainlim = rainlim %||% 5L,
    H0 = 100L,
    I0 = 1L,
    RcOpt = 0.87,
    p = 5L,
    i = 30L,
    Sx = 3200L,
    a = 4L,
    simple_wetness = TRUE,
    age_driver = "day",
    ...
  )
}

#' @rdname disease_models
#' @export
#'
#' @description
#' `brown_spot()` models rice brown spot (*Bipolaris oryzae*). A seed-borne
#' fungal disease prevalent in tropical and subtropical regions, particularly
#' damaging to older rice varieties.
#'
#' @examplesIf interactive()
#' # Simulate brown spot for a standard rice season
#' wth <- get_wth(
#'   lonlat = c(121.255669, 14.16742),
#'   dates = "2000-06-30",
#'   duration = 120L
#' )
#'
#' bs_sim <- brown_spot(
#'   wth = wth,
#'   emergence = "2000-06-30"
#' )
#'
#' # Brown spot develops slowly compared to leaf blast
#' plot(bs_sim$simday, bs_sim$intensity, type = "l",
#'      main = "Rice Brown Spot Progression",
#'      xlab = "Days After Emergence", ylab = "Disease Intensity")
#'
brown_spot <- function(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
) {
  params <- .epi_get_params("rice", "brown_spot")
  seir(
    wth = wth,
    emergence = emergence,
    RcA = RcA %||% params$RcA,
    RcT = RcT %||% params$RcT,
    RRG = RRG %||% params$RRG,
    RRS = RRS %||% params$RRS,
    onset = 20L,
    duration = 120L,
    rhlim = rhlim %||% 90L,
    rainlim = rainlim %||% 5L,
    H0 = 600L,
    I0 = 1L,
    RcOpt = 0.61,
    p = 6L,
    i = 19L,
    a = 1L,
    Sx = 100000L,
    simple_wetness = TRUE,
    age_driver = "day",
    ...
  )
}

#' @rdname disease_models
#' @export
#'
#' @description
#' `leaf_blast()` models rice leaf blast (*Magnaporthe oryzae*). The most
#' economically important rice disease worldwide, favoured by cool
#' (25\ifelse{latex}{\out{$^\circ$}}{°}.RC), wet conditions. Rapid disease
#' progression can cause total crop loss.
#'
#' @examplesIf interactive()
#' # Simulate leaf blast under favourable conditions
#' wth <- get_wth(
#'   lonlat = c(121.255669, 14.16742),
#'   dates = "2000-06-30",
#'   duration = 120L
#' )
#'
#' lb_sim <- leaf_blast(
#'   wth = wth,
#'   emergence = "2000-06-30"
#' )
#'
#' # Leaf blast shows rapid increase in intensity after onset
#' plot(lb_sim$simday, lb_sim$intensity, type = "l", ylim = c(0, 1),
#'      main = "Rice Leaf Blast Progression",
#'      xlab = "Days After Emergence", ylab = "Disease Intensity")
#'
leaf_blast <- function(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
) {
  params <- .epi_get_params("rice", "leaf_blast")
  seir(
    wth = wth,
    emergence = emergence,
    RcA = RcA %||% params$RcA,
    RcT = RcT %||% params$RcT,
    RRG = RRG %||% params$RRG,
    RRS = RRS %||% params$RRS,
    onset = 15L,
    duration = 120L,
    rhlim = rhlim %||% 90L,
    rainlim = rainlim %||% 5L,
    H0 = 600L,
    I0 = 1L,
    RcOpt = 1.14,
    p = 5L,
    i = 20L,
    a = 1L,
    Sx = 30000L,
    simple_wetness = TRUE,
    age_driver = "day",
    ...
  )
}

#' @rdname disease_models
#' @export
#'
#' @description
#' `sheath_blight()` models rice sheath blight (*Rhizoctonia solani*).
#' A soil-borne disease that infects the leaf sheath; can cause catastrophic
#' damage in high-N fertilised crops.
#'
#' @examplesIf interactive()
#' wth <- get_wth(
#'   lonlat = c(121.255669, 14.16742),
#'   dates = "2000-06-30",
#'   duration = 120L
#' )
#'
#' sb_sim <- sheath_blight(
#'   wth = wth,
#'   emergence = "2000-06-30"
#' )
#'
#' # Sheath blight has long infectious period and delayed onset
#' plot(sb_sim$simday, sb_sim$intensity, type = "l", ylim = c(0, 1),
#'      main = "Rice Sheath Blight Progression",
#'      xlab = "Days After Emergence", ylab = "Disease Intensity")
#'
sheath_blight <- function(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
) {
  params <- .epi_get_params("rice", "sheath_blight")
  seir(
    wth = wth,
    emergence = emergence,
    RcA = RcA %||% params$RcA,
    RcT = RcT %||% params$RcT,
    RRG = RRG %||% params$RRG,
    RRS = RRS %||% params$RRS,
    onset = 30L,
    duration = 120L,
    rhlim = rhlim %||% 90L,
    rainlim = rainlim %||% 5L,
    H0 = 25L,
    I0 = 1L,
    RcOpt = 0.46,
    p = 3L,
    i = 120L,
    Sx = 800L,
    a = 2.8,
    simple_wetness = TRUE,
    age_driver = "day",
    ...
  )
}

#' @rdname disease_models
#' @export
#'
#' @description
#' `tungro()` models rice tungro, a viral disease complex (*Tungro bacilliform*
#' and *Tungro spherical viruses*) transmitted by the insect vector
#' *Nephotettix* spp. Vector populations increase with temperature and
#' humidity; disease pressure peaks mid-season.
#'
#' @examplesIf interactive()
#' # Simulate tungro (vector-borne viral disease)
#' wth <- get_wth(
#'   lonlat = c(121.255669, 14.16742),
#'   dates = "2000-06-30",
#'   duration = 120L
#' )
#'
#' tg_sim <- tungro(
#'   wth = wth,
#'   emergence = "2000-06-30"
#' )
#'
#' # Tungro shows plateau phase (low Sx due to limited vector population)
#' plot(tg_sim$simday, tg_sim$intensity, type = "l",
#'      main = "Rice Tungro Progression",
#'      xlab = "Days After Emergence", ylab = "Disease Intensity")
#'
tungro <- function(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
) {
  params <- .epi_get_params("rice", "tungro")
  seir(
    wth = wth,
    emergence = emergence,
    RcA = RcA %||% params$RcA,
    RcT = RcT %||% params$RcT,
    RRG = RRG %||% params$RRG,
    RRS = RRS %||% params$RRS,
    onset = 25L,
    duration = 120L,
    rhlim = rhlim %||% 90L,
    rainlim = rainlim %||% 5L,
    H0 = 100L,
    I0 = 1L,
    RcOpt = 0.18,
    p = 6L,
    i = 120L,
    Sx = 100L,
    a = 1L,
    simple_wetness = TRUE,
    age_driver = "day",
    ...
  )
}

#' @rdname disease_models
#' @export
#'
#' @description
#' `modified_kim_leaf_blast()` models rice leaf blast using parameters from
#' Kim *et al.* (2015), calibrated for South Korean rice cultivars and growing
#' conditions.
#'
#' @examplesIf interactive()
#' # Simulate using Kim *et al.* (2015) cultivar-adjusted parameters
#' wth <- get_wth(
#'   lonlat = c(126.9780, 37.5665),  # Seoul, South Korea
#'   dates = "2000-06-01",
#'   duration = 77L
#' )
#'
#' kim_lb <- modified_kim_leaf_blast(
#'   wth = wth,
#'   emergence = "2000-06-01"
#' )
#'
#' # Kim version has shorter duration and higher Sx (larger canopy model)
#' plot(kim_lb$simday, kim_lb$intensity, type = "l",
#'      main = "Rice Leaf Blast (Kim *et al.* 2015)",
#'      xlab = "Days", ylab = "Intensity")
#'
modified_kim_leaf_blast <- function(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
) {
  params <- .epi_get_params("rice_modified_kim", "leaf_blast")
  seir(
    wth = wth,
    emergence = emergence,
    RcA = RcA %||% params$RcA,
    RcT = RcT %||% params$RcT,
    RRG = RRG %||% params$RRG,
    RRS = RRS %||% params$RRS,
    onset = 15L,
    duration = 77L,
    rhlim = rhlim %||% 90L,
    rainlim = rainlim %||% 5L,
    H0 = 600L,
    I0 = 1L,
    RcOpt = 0.86,
    p = 4L,
    i = 20L,
    a = 1L,
    Sx = 90000L,
    simple_wetness = TRUE,
    age_driver = "day",
    ...
  )
}

#' @rdname disease_models
#' @export
#'
#' @description
#' `modified_kim_sheath_blight()` models rice sheath blight using parameters
#' from Kim *et al.* (2015), calibrated for South Korean rice cultivars. Uses
#' slightly higher humidity threshold (95%) reflecting Korean monsoon climate.
#'
#' @examplesIf interactive()
#' # Simulate sheath blight with Kim *et al.* (2015) parameters
#' wth <- get_wth(
#'   lonlat = c(126.9780, 37.5665),
#'   dates = "2000-06-01",
#'   duration = 88L
#' )
#'
#' kim_sb <- modified_kim_sheath_blight(
#'   wth = wth,
#'   emergence = "2000-06-01"
#' )
#'
#' plot(kim_sb$simday, kim_sb$intensity, type = "l",
#'      main = "Rice Sheath Blight (Kim *et al.* 2015)",
#'      xlab = "Days", ylab = "Intensity")
#'
modified_kim_sheath_blight <- function(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRS = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
) {
  params <- .epi_get_params("rice_modified_kim", "sheath_blight")
  seir(
    wth = wth,
    emergence = emergence,
    RcA = RcA %||% params$RcA,
    RcT = RcT %||% params$RcT,
    RRG = RRG %||% params$RRG,
    RRS = RRS %||% params$RRS,
    onset = 30L,
    duration = 88L,
    rhlim = rhlim %||% 95L,
    rainlim = rainlim %||% 5L,
    H0 = 90L,
    I0 = 1L,
    RcOpt = 0.58,
    p = 4L,
    i = 65L,
    Sx = 800L,
    a = 2.8,
    simple_wetness = TRUE,
    age_driver = "day",
    ...
  )
}

#' @rdname disease_models
#' @export
#'
#' @description
#' `leaf_rust()` models wheat leaf rust (*Puccinia triticina*). A major wheat
#' pathogen globally, favoured by moderate temperatures
#' (15–25\ifelse{latex}{\out{$^\circ$}}{°}.RC) and high humidity. #' DVS-based
#' modelling accounts for developmental susceptibility.

#' @examplesIf interactive()
#' # Simulate wheat leaf rust with DVS (development stage) modelling
#' wth <- get_wth(
#'   lonlat = c(8.5, 50.1),  # Example: France
#'   dates = "2000-10-15",   # Winter wheat sowing
#'   duration = 240L         # Full growing season
#' )
#'
#' lr_sim <- leaf_rust(
#'   wth = wth,
#'   emergence = "2000-10-15"
#' )
#'
#' # Leaf rust shows gradual progression across growing season
#' plot(lr_sim$simday, lr_sim$intensity, type = "l",
#'      main = "Wheat Leaf Rust Progression (DVS-based)",
#'      xlab = "Days After Sowing", ylab = "Disease Intensity")
#'
leaf_rust <- function(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRPSN = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
) {
  params <- .epi_get_params("wheat", "leaf_rust")

  # Resolve RRPSN once, then pass as RRS via do.call to avoid duplication
  dots <- list(...)
  # Allow RRS in ... as an alias for RRPSN (backward compat with tests/users)
  if (is.null(RRPSN) && !is.null(dots[["RRS"]])) {
    RRPSN <- dots[["RRS"]]
  }
  dots[["RRS"]] <- NULL # always strip RRS from dots before passing through

  do.call(
    seir,
    c(
      list(
        wth = wth,
        emergence = emergence,
        RcA = RcA %||% params$RcA,
        RcT = RcT %||% params$RcT,
        RRG = RRG %||% params$RRG,
        RRS = RRPSN %||% params$RRPSN,
        onset = 20L,
        duration = 120L,
        rhlim = rhlim %||% 90L,
        rainlim = rainlim %||% 5L,
        H0 = 250L,
        I0 = 10L,
        RcOpt = 1.472,
        RRLEX = 0.0,
        p = 7L,
        i = 31L,
        Sx = 750000L,
        a = 1L,
        age_driver = "dvs",
        thermal_time = list(base_temp = 0, gdd_to_maturity = 1500),
        simple_wetness = TRUE
      ),
      dots # RRS has been removed; remaining ... args pass through cleanly
    )
  )
}

#' @rdname disease_models
#' @export
#'
#' @description
#' `s_tritici_blotch()` models wheat Septoria tritici blotch (*Zymoseptoria
#' tritici*), a major wheat pathogen in temperate regions. Features lesion
#' expansion (direct H→I pathway) allowing rapid canopy senescence.
#' DVS-based modelling accounts for developmental susceptibility.
#'
#' @examplesIf interactive()
#' # Example: Septoria tritici blotch - compare lesion expansion effect
#'
#' # Fetch weather for a wheat-growing region (France, Oct-May season)
#' wth <- get_wth(
#'   lonlat = c(8.5, 50.1),
#'   dates = "2000-10-15",
#'   duration = 240L
#' )
#'
#' stb <- s_tritici_blotch(
#'   wth = wth,
#'   emergence = "2000-10-15"
#' )
#'
#' # Create single plot with both lines for comparison
#' plot(stb$simday, stb$intensity,
#'      type = "l", col = "black", lty = 1,
#'      main = "Wheat Septoria tritici Blotch",
#'      xlab = "Days After Sowing",
#'      ylab = "Disease Intensity")
#'
s_tritici_blotch <- function(
  wth,
  emergence,
  RcA = NULL,
  RcT = NULL,
  RRPSN = NULL,
  RRG = NULL,
  rhlim = NULL,
  rainlim = NULL,
  ...
) {
  params <- .epi_get_params("wheat", "s_tritici_blotch")
  # Resolve RRPSN once, then pass as RRS via do.call to avoid duplication
  dots <- list(...)
  # Allow RRS in ... as an alias for RRPSN (backward compat with tests/users)
  if (is.null(RRPSN) && !is.null(dots[["RRS"]])) {
    RRPSN <- dots[["RRS"]]
  }
  dots[["RRS"]] <- NULL # always strip RRS from dots before passing through

  do.call(
    seir,
    c(
      list(
        wth = wth,
        emergence = emergence,
        RcA = RcA %||% params$RcA,
        RcT = RcT %||% params$RcT,
        RRG = RRG %||% params$RRG,
        RRS = RRPSN %||% params$RRPSN,
        RRLEX = params$RRLEX,
        onset = 20L,
        duration = 120L,
        rhlim = rhlim %||% 90L,
        rainlim = rainlim %||% 5L,
        H0 = 250L,
        I0 = 15L,
        RcOpt = 1.17,
        p = 11L,
        i = 18L,
        Sx = 174000L,
        a = 1L,
        age_driver = "dvs",
        thermal_time = list(base_temp = 0, gdd_to_maturity = 1500),
        simple_wetness = TRUE
      ),
      dots
    )
  )
}

# --- Shorter aliases for convenience ---

#' @rdname disease_models
#' @export
#' @description `bb()` is an alias for [bacterial_blight()].
bb <- bacterial_blight

#' @rdname disease_models
#' @export
#' @description `bs()` is an alias for [brown_spot()].
bs <- brown_spot

#' @rdname disease_models
#' @export
#' @description `lb()` is an alias for [leaf_blast()].
lb <- leaf_blast

#' @rdname disease_models
#' @export
#' @description `sb()` is an alias for [sheath_blight()].
sb <- sheath_blight

#' @rdname disease_models
#' @export
#' @description `tg()` is an alias for [tungro()].
tg <- tungro

#' @rdname disease_models
#' @export
#' @description `mk_lb()` is an alias for [modified_kim_leaf_blast()].
mk_lb <- modified_kim_leaf_blast

#' @rdname disease_models
#' @export
#' @description `mk_sb()` is an alias for [modified_kim_sheath_blight()].
mk_sb <- modified_kim_sheath_blight

#' @rdname disease_models
#' @export
#' @description `lr()` is an alias for [leaf_rust()].
lr <- leaf_rust

#' @rdname disease_models
#' @export
#' @description `brown_rust()` is an alias for [leaf_rust()].
brown_rust <- leaf_rust

#' @rdname disease_models
#' @export
#' @description `stb()` is an alias for [s_tritici_blotch()].
stb <- s_tritici_blotch
