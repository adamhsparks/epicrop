#' Rice and Wheat Disease Models
#'
#' Helper functions with pre-loaded parameter tables for optimal performance.
#' Custom parameter tables can be provided to override defaults.
#'
#' @name disease_models
#' @inheritParams seir
#' @param RcA Optional custom age modifier table (2-column matrix).
#' @param RcT Optional custom temperature modifier table (2-column matrix).
#' @param RRG Optional custom growth rate (scalar, vector, or table).
#' @param RRS Optional custom senescence rate (scalar, vector, or function).
#' @param ... Other parameters passed to `seir()` allowing the user to override
#'   the default values from published papers.
#'
#' @references
#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J. (2012).
#' Modeling and mapping potential epidemics of rice diseases globally.
#' *Crop Protection*, 34: 6-17. \doi{10.1016/j.cropro.2011.11.009}
#'
#' Kim, K. H., Cho, J., Lee, Y. H., & Lee, W. S. (2015).
#' Predicting potential epidemics of rice leaf blast and sheath blight in South
#' Korea. *Agricultural and Forest Meteorology*, 203: 191-207.
#' \doi{10.1016/j.agrformet.2015.01.011}
#'
#' Savary, S., Stetkiewicz, S., Brun, F., & Willocquet, L. (2015).
#' Modelling and mapping potential epidemics of wheat diseases.
#' *European Journal of Plant Pathology*, 142: 771-790.
#' \doi{10.1007/s10658-015-0650-7}
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @autoglobal
NULL

#' @rdname disease_models
#' @export
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
  params <- epi_get_params("rice", "bacterial_blight")
  seir(
    wth = wth,
    emergence = emergence,
    RcA = RcA %||% params$RcA,
    RcT = RcT %||% params$RcT,
    RRG = RRG %||% params$RRG, # 0.1  (Savary et al. 2012, Table 2)
    RRS = RRS %||% params$RRS, # 0.01 (Savary et al. 2012, Table 2)
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
  params <- epi_get_params("rice", "brown_spot")
  seir(
    wth = wth,
    emergence = emergence,
    RcA = RcA %||% params$RcA,
    RcT = RcT %||% params$RcT,
    RRG = RRG %||% params$RRG, # 0.1  (Savary et al. 2012, Table 2)
    RRS = RRS %||% params$RRS, # 0.01 (Savary et al. 2012, Table 2)
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
  params <- epi_get_params("rice", "leaf_blast")
  seir(
    wth = wth,
    emergence = emergence,
    RcA = RcA %||% params$RcA,
    RcT = RcT %||% params$RcT,
    RRG = RRG %||% params$RRG, # 0.1  (Savary et al. 2012, Table 2)
    RRS = RRS %||% params$RRS, # 0.01 (Savary et al. 2012, Table 2)
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
  params <- epi_get_params("rice", "sheath_blight")
  seir(
    wth = wth,
    emergence = emergence,
    RcA = RcA %||% params$RcA,
    RcT = RcT %||% params$RcT,
    RRG = RRG %||% params$RRG, # 0.2   (Savary et al. 2012, Table 2)
    RRS = RRS %||% params$RRS, # 0.005 (Savary et al. 2012, Table 2)
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
  params <- epi_get_params("rice", "tungro")
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
  params <- epi_get_params("rice_modified_kim", "leaf_blast")
  seir(
    wth = wth,
    emergence = emergence,
    RcA = RcA %||% params$RcA,
    RcT = RcT %||% params$RcT,
    RRG = RRG %||% params$RRG,
    RRS = RRS %||% params$RRS, # 0.01 (unchanged from original leaf blast)
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
  params <- epi_get_params("rice_modified_kim", "sheath_blight")
  seir(
    wth = wth,
    emergence = emergence,
    RcA = RcA %||% params$RcA,
    RcT = RcT %||% params$RcT,
    RRG = RRG %||% params$RRG,
    RRS = RRS %||% params$RRS, # 0.005 (unchanged from original sheath blight)
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
leaf_rust <- function(
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
  params <- epi_get_params("wheat", "leaf_rust")
  seir(
    wth = wth,
    emergence = emergence,
    RcA = RcA %||% params$RcA,
    RcT = RcT %||% params$RcT,
    RRG = RRG %||% params$RRG,
    RRS = RRS %||% params$RRPSN,
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
    simple_wetness = TRUE,
    ...
  )
}

#' @rdname disease_models
#' @export
s_tritici_blotch <- function(
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
  params <- epi_get_params("wheat", "s_tritici_blotch")
  seir(
    wth = wth,
    emergence = emergence,
    RcA = RcA %||% params$RcA,
    RcT = RcT %||% params$RcT,
    RRG = RRG %||% params$RRG,
    RRS = RRS %||% params$RRPSN,
    onset = 20L,
    duration = 120L,
    rhlim = rhlim %||% 90L,
    rainlim = rainlim %||% 5L,
    H0 = 250L,
    I0 = 15L,
    RcOpt = 1.17,
    RRLEX = 0.09,
    p = 11L,
    i = 18L,
    Sx = 174000L,
    a = 1L,
    age_driver = "dvs",
    thermal_time = list(base_temp = 0, gdd_to_maturity = 1500),
    simple_wetness = TRUE,
    ...
  )
}

#' @rdname disease_models
#' @export
bb <- bacterial_blight

#' @rdname disease_models
#' @export
bs <- brown_spot

#' @rdname disease_models
#' @export
lb <- leaf_blast

#' @rdname disease_models
#' @export
sb <- sheath_blight

#' @rdname disease_models
#' @export
tg <- tungro

#' @rdname disease_models
#' @export
mk_lb <- modified_kim_leaf_blast

#' @rdname disease_models
#' @export
mk_sb <- modified_kim_sheath_blight

#' @rdname disease_models
#' @export
lr <- leaf_rust

#' @rdname disease_models
#' @export
brown_rust <- leaf_rust

#' @rdname disease_models
#' @export
stb <- s_tritici_blotch
