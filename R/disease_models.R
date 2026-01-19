#' Rice and Wheat Disease Models
#'
#' Helper functions that provide sensible defaults for disease-specific
#' simulations using [epicrop_sim()].
#'
#' @name disease_models
#' @param wth Weather data with a daily time-step, normally \acronym{NASA}
#' \acronym{POWER} data from [get_wth()], but any [base::data.frame()] object
#' that has been formatted using [format_wth()] will work.
#' @param emergence Expected date of crop emergence in `YYYY-MM-DD` format.
#' @param ... Additional arguments passed to [epicrop_sim()].
#'
#' @returns A [data.table::data.table()] of disease intensity and infection
#' sites. See [epicrop_sim()] for a full description of the column values.
#'
#' @section Available Models:
#'
#' **Rice Diseases (Savary et al. 2012)**:
#' * [bacterial_blight()] / [bb()] - Bacterial blight (*Xanthomonas oryzae* pv.  *oryzae*)
#' * [brown_spot()] / [bs()] - Brown spot (*Cochliobolus miyabeanus*)
#' * [leaf_blast()] / [lb()] - Leaf blast (*Magnaporthe oryzae*)
#' * [sheath_blight()] / [sb()] - Sheath blight (*Rhizoctonia solani*)
#' * [tungro()] / [tg()] - Tungro (viral disease)
#'
#' **Rice Diseases (Kim et al. 2015)**:
#' * [modified_kim_leaf_blast()] / [mk_lb()] - Modified leaf blast model
#' * [modified_kim_sheath_blight()] / [mk_sb()] - Modified sheath blight model
#'
#' **Wheat Diseases (Savary et al. 2015)**:
#' * [leaf_rust()] / [brown_rust()] / [lr()] - Leaf/brown rust (*Puccinia triticina*)
#' * [s_tritici_blotch()] / [stb()] - Septoria tritici blotch
#'
#' @references
#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J. (2012).
#' Modeling and mapping potential epidemics of rice diseases globally.
#' *Crop Protection*, 34: 6-17. \doi{10.1016/j.cropro.2011.11.009}
#'
#' Kim, K.  H., Cho, J., Lee, Y. H., & Lee, W. S. (2015).
#' Predicting potential epidemics of rice leaf blast and sheath blight in South Korea.
#' *Agricultural and Forest Meteorology*, 203: 191-207.
#' \doi{10.1016/j.agrformet. 2015.01.011}
#'
#' Savary, S., Stetkiewicz, S., Brun, F., & Willocquet, L. (2015).
#' Modelling and mapping potential epidemics of wheat diseases.
#' *European Journal of Plant Pathology*, 142:771-790.
#' \doi{10.1007/s10658-015-0650-7}
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
NULL

# ==============================================================================
# RICE DISEASES (Savary et al. 2012)
# ==============================================================================

#' @rdname disease_models
#' @export
bacterial_blight <- function(wth, emergence, ...) {
  epicrop_sim(
    wth = wth,
    emergence = emergence,
    onset = 20L,
    duration = 120L,
    rhlim = 90L,
    rainlim = 5L,
    H0 = 100L,
    I0 = 1L,
    RcA = cbind(
      c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L, 100L, 110L, 120L),
      c(
        1.0,
        1.0,
        1.0,
        0.9,
        0.62,
        0.43,
        0.42,
        0.42,
        0.41,
        0.41,
        0.41,
        0.41,
        0.41
      )
    ),
    RcT = cbind(
      c(16L, 19L, 22L, 25L, 28L, 31L, 34L, 37L, 40L),
      c(0.0, 0.29, 0.44, 0.90, 0.90, 1.0, 0.88, 0.01, 0.0)
    ),
    RcOpt = 0.87,
    p = 5L,
    i = 30L,
    Sx = 3200L,
    a = 4L,
    RRS = 0.01,
    RRG = 0.1,
    simple_wetness = TRUE,
    age_driver = "day",
    ...
  )
}

#' @rdname disease_models
#' @export
bb <- bacterial_blight

#' @rdname disease_models
#' @export
brown_spot <- function(wth, emergence, ...) {
  epicrop_sim(
    wth = wth,
    emergence = emergence,
    onset = 20L,
    duration = 120L,
    rhlim = 90L,
    rainlim = 5L,
    H0 = 600L,
    I0 = 1L,
    RcA = cbind(
      c(0L, 20L, 40L, 60L, 80L, 100L, 120L),
      c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0)
    ),
    RcT = cbind(
      c(15L, 20L, 25L, 30L, 35L, 40L),
      c(0.0, 0.06, 1.0, 0.85, 0.16, 0.0)
    ),
    RcOpt = 0.61,
    p = 6L,
    i = 19L,
    a = 1L,
    Sx = 100000L,
    RRS = 0.01,
    RRG = 0.1,
    simple_wetness = TRUE,
    age_driver = "day",
    ...
  )
}

#' @rdname disease_models
#' @export
bs <- brown_spot

#' @rdname disease_models
#' @export
leaf_blast <- function(wth, emergence, ...) {
  epicrop_sim(
    wth = wth,
    emergence = emergence,
    onset = 15L,
    duration = 120L,
    rhlim = 90L,
    rainlim = 5L,
    H0 = 600L,
    I0 = 1L,
    RcA = cbind(
      c(
        0L,
        5L,
        10L,
        15L,
        20L,
        25L,
        30L,
        35L,
        40L,
        45L,
        50L,
        55L,
        60L,
        65L,
        70L,
        75L,
        80L,
        85L,
        90L,
        95L,
        100L,
        105L,
        110L,
        115L,
        120L
      ),
      c(
        1.0,
        1.0,
        1.0,
        0.9,
        0.8,
        0.7,
        0.64,
        0.59,
        0.53,
        0.43,
        0.32,
        0.22,
        0.16,
        0.09,
        0.03,
        0.02,
        0.02,
        0.02,
        0.01,
        0.01,
        0.01,
        0.01,
        0.01,
        0.01,
        0.01
      )
    ),
    RcT = cbind(
      c(10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L),
      c(0.0, 0.5, 1.0, 0.6, 0.2, 0.05, 0.01, 0.0)
    ),
    RcOpt = 1.14,
    p = 5L,
    i = 20L,
    a = 1L,
    Sx = 30000L,
    RRS = 0.01,
    RRG = 0.1,
    simple_wetness = TRUE,
    age_driver = "day",
    ...
  )
}

#' @rdname disease_models
#' @export
lb <- leaf_blast

#' @rdname disease_models
#' @export
sheath_blight <- function(wth, emergence, ...) {
  epicrop_sim(
    wth = wth,
    emergence = emergence,
    onset = 30L,
    duration = 120L,
    rhlim = 90L,
    rainlim = 5L,
    H0 = 25L,
    I0 = 1L,
    RcA = cbind(
      c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L, 100L, 110L, 120L),
      c(
        0.84,
        0.84,
        0.84,
        0.84,
        0.84,
        0.84,
        0.84,
        0.88,
        0.88,
        1.0,
        1.0,
        1.0,
        1.0
      )
    ),
    RcT = cbind(
      c(12L, 16L, 20L, 24L, 28L, 32L, 36L, 40L),
      c(0.0, 0.42, 0.94, 0.94, 1.0, 0.85, 0.64, 0.0)
    ),
    RcOpt = 0.46,
    p = 3L,
    i = 120L,
    Sx = 800L,
    a = 2.8,
    RRS = 0.005,
    RRG = 0.2,
    simple_wetness = TRUE,
    age_driver = "day",
    ...
  )
}

#' @rdname disease_models
#' @export
sb <- sheath_blight

#' @rdname disease_models
#' @export
tungro <- function(wth, emergence, ...) {
  epicrop_sim(
    wth = wth,
    emergence = emergence,
    onset = 25L,
    duration = 120L,
    rhlim = 90L,
    rainlim = 5L,
    H0 = 100L,
    I0 = 1L,
    RcA = cbind(
      c(0L, 15L, 30L, 45L, 60L, 75L, 90L, 105L, 120L),
      c(1.0, 1.0, 0.98, 0.73, 0.51, 0.34, 0.0, 0.0, 0.0)
    ),
    RcT = cbind(
      c(
        9.0,
        10.0,
        13.1111,
        16.2222,
        19.3333,
        22.4444,
        25.5555,
        28.6666,
        31.7777,
        34.8888,
        37.9999,
        40.0
      ),
      c(0.0, 0.13, 0.65, 0.75, 0.83, 0.89, 0.93, 0.97, 1.0, 0.96, 0.93, 0.0)
    ),
    RcOpt = 0.18,
    p = 6L,
    i = 120L,
    Sx = 100L,
    a = 1L,
    RRS = 0.01,
    RRG = 0.1,
    simple_wetness = TRUE,
    age_driver = "day",
    ...
  )
}

#' @rdname disease_models
#' @export
tg <- tungro

# ==============================================================================
# RICE DISEASES (Kim et al. 2015)
# ==============================================================================

#' @rdname disease_models
#' @export
modified_kim_leaf_blast <- function(wth, emergence, ...) {
  epicrop_sim(
    wth = wth,
    emergence = emergence,
    onset = 15L,
    duration = 77L,
    rhlim = 90L,
    rainlim = 5L,
    H0 = 600L,
    I0 = 1L,
    RcA = cbind(
      c(
        0L,
        5L,
        10L,
        15L,
        20L,
        25L,
        30L,
        35L,
        40L,
        45L,
        50L,
        55L,
        60L,
        65L,
        70L,
        77L
      ),
      c(
        1.0,
        1.0,
        1.0,
        1.0,
        1.0,
        1.0,
        1.0,
        1.0,
        1.0,
        0.4,
        0.3,
        0.1,
        0.1,
        0.05,
        0.03,
        0.03
      )
    ),
    RcT = cbind(
      c(9L, 12L, 15L, 18L, 21L, 24L, 27L, 30L, 33L, 36L, 39L),
      c(0.0, 0.4, 0.72, 0.88, 1.0, 0.9, 0.45, 0.2, 0.05, 0.01, 0.0)
    ),
    RcOpt = 0.86,
    p = 4L,
    i = 20L,
    a = 1L,
    Sx = 90000L,
    RRS = 0.005,
    RRG = cbind(
      c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 77L),
      c(0.12, 0.12, 0.11, 0.11, 0.09, 0.09, 0.09, 0.09, 0.09)
    ),
    simple_wetness = TRUE,
    age_driver = "day",
    ...
  )
}

#' @rdname disease_models
#' @export
mk_lb <- modified_kim_leaf_blast

#' @rdname disease_models
#' @export
modified_kim_sheath_blight <- function(wth, emergence, ...) {
  epicrop_sim(
    wth = wth,
    emergence = emergence,
    onset = 30L,
    duration = 88L,
    rhlim = 95L,
    rainlim = 5L,
    H0 = 90L,
    I0 = 1L,
    RcA = cbind(
      c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 88L),
      c(0.43, 0.5, 0.73, 0.81, 0.94, 1.0, 1.0, 1.0, 1.0, 1.0)
    ),
    RcT = cbind(
      c(12L, 15L, 18L, 21L, 24L, 27L, 30L, 33L, 36L, 39L),
      c(0.0, 0.1, 0.8, 1.0, 1.0, 1.0, 0.2, 0.01, 0.01, 0.0)
    ),
    RcOpt = 0.58,
    p = 4L,
    i = 65L,
    Sx = 800L,
    a = 2.8,
    RRS = 0.005,
    RRG = cbind(
      c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 88L),
      c(
        0.15,
        0.21,
        0.11,
        0.0001,
        0.0001,
        0.0001,
        0.0001,
        0.0001,
        0.0001,
        0.0001
      )
    ),
    simple_wetness = TRUE,
    age_driver = "day",
    ...
  )
}

#' @rdname disease_models
#' @export
mk_sb <- modified_kim_sheath_blight

# ==============================================================================
# WHEAT DISEASES (Savary et al. 2015)
# ==============================================================================

#' @rdname disease_models
#' @export
leaf_rust <- function(wth, emergence, ...) {
  # DVS-driven age modifier for leaf rust
  RcA_LR_DVS <- cbind(
    c(0.00, 0.30, 0.60, 1.00, 1.40, 1.70, 2.00),
    c(1.00, 0.96, 0.92, 0.88, 0.84, 0.80, 0.76)
  )

  # Temperature modifier with optimum ~15°C
  RcT_LR_15 <- cbind(
    c(5, 10, 12, 15, 18, 20, 25, 30),
    c(0.00, 0.45, 0.75, 1.00, 0.70, 0.55, 0.20, 0.00)
  )

  # Physiological senescence increasing with age
  RRS_fun <- function(dvs) {
    seq(0.003, 0.015, length.out = length(dvs))
  }

  epicrop_sim(
    wth = wth,
    emergence = emergence,
    onset = 20L,
    duration = 120L,
    rhlim = 90L,
    rainlim = 5L,
    H0 = 250L,
    I0 = 10L,
    RcOpt = 1.472,
    RcA = RcA_LR_DVS,
    RcT = RcT_LR_15,
    RcW = NULL, # Wetness has no effect for leaf rust
    RRLEX = 0.0,
    p = 7L,
    i = 31L,
    Sx = 750000L,
    a = 1L,
    RRG = 0.10,
    RRS = RRS_fun,
    simple_wetness = TRUE,
    age_driver = "dvs",
    thermal_time = list(base_temp = 0, gdd_to_maturity = 1100),
    ...
  )
}

#' @rdname disease_models
#' @export
brown_rust <- leaf_rust

#' @rdname disease_models
#' @export
lr <- leaf_rust

#' @rdname disease_models
#' @export
s_tritici_blotch <- function(wth, emergence, ...) {
  # DVS-driven age modifier for STB
  RcA_STB_DVS <- cbind(
    c(0.00, 0.30, 0.60, 1.00, 1.40, 1.70, 2.00),
    c(1.00, 0.97, 0.94, 0.90, 0.87, 0.84, 0.80)
  )

  # Temperature modifier with optimum ~20-24°C
  RcT_STB_22 <- cbind(
    c(10, 15, 18, 20, 22, 24, 26, 28, 30, 32),
    c(0.00, 0.20, 0.70, 0.90, 1.00, 0.95, 0.80, 0.55, 0.25, 0.00)
  )

  # Physiological senescence increasing with age
  RRS_fun <- function(dvs) {
    seq(0.003, 0.020, length.out = length(dvs))
  }

  epicrop_sim(
    wth = wth,
    emergence = emergence,
    onset = 20L,
    duration = 120L,
    rhlim = 90L,
    rainlim = 5L,
    H0 = 250L,
    I0 = 15L,
    RcOpt = 1.17,
    RcA = RcA_STB_DVS,
    RcT = RcT_STB_22,
    RcW = NULL, # Could implement consecutive rain days here
    RRLEX = 0.09,
    p = 11L,
    i = 18L,
    Sx = 174000L,
    a = 1L,
    RRG = 0.10,
    RRS = RRS_fun,
    simple_wetness = TRUE,
    age_driver = "dvs",
    thermal_time = list(base_temp = 0, gdd_to_maturity = 1100),
    ...
  )
}

#' @rdname disease_models
#' @export
stb <- s_tritici_blotch
