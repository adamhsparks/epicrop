#' Predict Rice Sheath Blight Intensity Using a Modified Kim Model
#'
#' A helper function to provide values used by Kim _et al. 2015 for a dynamic
#' mechanistic simulation of sheath blight disease of rice, causal agent
#' _Rhizoctonia solani_ AG1-1A Kühn. The model is driven by daily weather data,
#' which can easily be accessed using [get_wth] to download weather data from
#' \acronym{NASA} \acronym{POWER}.
#'
#'
#' @details
#' The model represents site size as 1 rice plant's tiller.
#'
#' This model is a modified version of the Kim *et al.* (2015) model, which
#' starts with a high growth rate followed by a rapid decline after the max
#' tillering stage, based on observed tiller numbers in Korean rice varieties,
#' Kim (personal communication). The age effect increases until max tillering
#' and decreases after heading stage. The optimum temperature is from 23--27 ˚C.
#'
#' @note Adapted from Kim *et al.* by Adam H. Sparks, Curtin University,
#' Bentley, WA, AU, with assistance from Kwang-Hyung Kim, APEC Climate Center,
#' 12 Centum7-ro, Haeundae-gu, Busan 612-020, South Korea.
#' Original model development: Serge Savary & Rene Pangga (IRRI). #' Original \R
#' implementation by Robert J. Hijmans, Rene Pangga, & Jorrel Aunario (IRRI).
#'
#' @note
#' If the `wth` object provides _LAT_ and _LON_ columns, these will be included
#' in the output for mapping purposes. Both values must be present. These
#' columns are provided by default when using [get_wth].
#'
#' @inherit bacterial_blight
#' @inheritSection seir TMIN/TMAX Details
#' @inheritSection seir LAT/LON Details
#'
#' @returns A [data.table::data.table] of disease intensity and infection
#' sites. See [seir] for a full description of the column values.
#'
#' @examplesIf interactive()
#' # get weather for IRRI Zeigler Experiment Station in wet season 2000
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#' mk_sb <- modified_kim_sheath_blight(wth, emergence = "2000-07-01")
#' plot(x = mk_sb$dates, y = mk_sb$intensity, type = "l")
#'
#' @author Kwang-Hyung Kim, Serge Savary, Ireneo Pangga, Robert Hijmans,
#' Jorrel Khalil Aunario, and Adam H. Sparks
#'
#' @references
#'
#' Kim, K. H., Cho, J., Lee, Y. H., & Lee, W. S. (2015). Predicting potential
#' epidemics of rice leaf blast and sheath blight in South Korea under the RCP
#' 4.5 and RCP 8.5 climate change scenarios using a rice disease epidemiology
#' model, EPIRICE. Agricultural and forest meteorology, 203, 191-207.
#' \doi{10.1016/j.agrformet.2015.01.011}
#'
#' Castilla, N.P., Leano, R.M., Elazegui, F.A., Teng, P.S., Savary, S., 1996.
#' Effects of plant contacts, inoculation pattern, leaf wetness regime, and
#' nitrogen supply on inoculum efficiency in rice sheath blight.
#' Journal of Phytopathology 144:187-192.
#'
#' Gross, M.K., Santini, J.B., Tikhonova, I. and Latin, R. 1998. The influence
#' of temperature and leaf wetness duration on infection of perennial ryegrass
#' by _Rhizoctonia solani_. Plant Disease 82:1012-1016. DOI:
#' \doi{10.1094/PDIS.1998.82.9.1012}.
#'
#' Hashiba, T. and Ijiri, T., 1989. Estimation of yield loss and computerized
#' forecasting system (BLIGHTAS) for rice sheath blight disease. International
#' Symposium on Tropical Agricultural Research: Crop losses due to disease
#' outbreaks in the tropics and countermeasures. Tropical Agricultural
#' Research Series (Japan) No. 22 pp. 163-171.
#'
#' Savary, S., Willocquet, L., Teng, P.S., 1997. Modelling sheath blight
#' epidemics on rice tillers. Agricultural Systems 55:359-384. DOI:
#' \doi{10.1016/S0308-521X(97)00014-0}.
#'
#' Savary, S., Castilla, N.P., Willocquet, L. 2001. Analysis of the spatio-
#' temporal structure of rice sheath blight epidemics in a farmer's field.
#' Plant Pathology 50:53-68. DOI: \doi{10.1046/j.1365-3059.2001.00531.x}.
#'
#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J. Modeling
#' and mapping potential epidemics of rice diseases globally. Crop Protection,
#' Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
#' \doi{10.1016/j.cropro.2011.11.009}.
#'
#' Sharma, N.R., Teng, P.S., Olivares, F.M., 1990. Effect of rice growth stage
#' on sheath blight (ShB) development and yield loss. International Rice
#' Research Newsletter 15:19-20.
#'
#' Tu, C.C., Chang, Y.C., Wang, C.W., 1979. Studies on the ecology of
#' _Rhizoctonia solani_, the causal organism of rice sheath blight. National
#' Science Council Monthly, ROC 7:1208-1219.
#'
#' @family helper functions
#'
#' @seealso
#' [seir]
#'
#' @export
modified_kim_sheath_blight <- function(wth, emergence, ...) {
  seir(
    wth = wth,
    emergence = emergence,
    onset = 30L,
    duration = 100L,
    rhlim = 95L,
    rainlim = 5L,
    H0 = 90L,
    I0 = 1L,
    RcA = cbind(
      c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L, 100L, 110L, 120L),
      c(0.43, 0.5, 0.73, 0.81, 0.94, 1, 1, 1, 1, 1, 0.8, 0.7, 0.5)
    ),
    RcT = cbind(
      c(12L, 15L, 18L, 21L, 24L, 27L, 30L, 33L, 36L, 39L),
      c(0, 0.1, 0.8, 1, 1, 1, 0.2, .01, 0.01, 0)
    ),
    RcW = cbind(c(0:10L), c(0, 0.24, 0.41, 0.68, 0.94, 0.97, 1, 1, 1, 1, 1)),
    RcOpt = 0.58,
    p = 4L,
    i = 65L,
    Sx = 800L,
    a = 2.8,
    RRS = 0.005,
    RRG = cbind(
      c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L, 100L, 110L, 120L),
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
        0.0001,
        0.0001,
        0.0001,
        0.0001
      )
    )
  )
}

#' @rdname modified_kim_sheath_blight
#' @examplesIf interactive()
#' # use shorthand function
#' mk_sb <- mk_sb(wth, emergence = "2000-07-01")
#' plot(x = mk_sb$dates, y = mk_sb$intensity, type = "l")
#' @export
mk_sb <- modified_kim_sheath_blight
