
#' Predict rice leaf blast intensity
#'
#' A dynamic mechanistic simulation of leaf blast disease of rice, causal agent
#' _Magnaporthe oryzae_. The model is driven by daily weather data, which can
#' easily be accessed using[get_wth()] to download weather data from
#' \acronym{NASA} \acronym{POWER} using \CRANpkg{nasapower}.
#'
#' @details
#' The model represents site size as 45
#'  \ifelse{html}{\out{mm<sup>2</sup>}}{\eqn{mm^2}} of a rice plant's leaf.
#'
#' Default values for this disease model are derived from Table 2 (Savary
#' _et al._ 2012).
#'
#' [predict_lb()] is a shorthand alias for [predict_leaf_blast()].
#'
#' @note Adapted from \pkg{cropsim} package version 0.2.0-5 by Adam H. Sparks,
#' Department of Primary Industries and Regional Development, WA, AU.
#' Original model development: Serge Savary & Rene Pangga (IRRI).
#' Original \R implementation by Robert J. Hijmans, Rene Pangga, & Jorrel
#' Aunario (IRRI).
#'
#' If the `wth` object provides _LAT_ and _LON_ columns, these will be included
#' in the output for mapping purposes. Both values must be present. These
#' columns are provided by default when using [get_wth()].
#'
#' The optimum temperature for leaf blast as presented in Table 2 of
#' Savary _et al._ 2012 has a typo. The optimal value should be 20 °C, not
#' 25 °C as shown. The correct value, 20 °C, is used in this implementation.
#'
#' @inherit predict_bacterial_blight
#'
#' @return A [data.table::data.table()] of disease intensity and infection
#' sites. See [SEIR()] for a full description of the column values.
#'
#' @examplesIf interactive()
#'
#' # get weather for IRRI Zeigler Experiment Station in wet season 2000
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#' lb <- predict_leaf_blast(wth, emergence = "2000-07-01")
#' plot(x = lb$dates, y = lb$intensity, type = "l")
#'
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario,
#' and Adam H. Sparks
#'
#' @references El Refaei, M.I., 1977. Epidemiology of rice blast disease in the
#' tropics with special reference to the leaf wetness in relation to disease
#' development. PhD Thesis, Indian Agricultural Research Institute, New Delhi,
#' 195 p.
#'
#' Hemmi, T., Abe, T., Ikaya, J., and Inoue, Y. 1936. Studies on the rice blast
#' disease. IV. Relation of the environment to the development of blast disease
#' and physiologic specialization in the rice blast fungus. Materials for Rural
#' Improvement, Department of Agriculture and Forestry, Japan No. 105, 145p.
#'
#' Hwang, B.K., Koh, Y.J., Chung, H.S., 1987. Effects of adult-plant resistance
#' on blast severity and yield of rice. Plant Disease 71:1035-1038. DOI:
#' \doi{10.1094/PD-71-1035}.
#'
#' Kato, H. and Kozaka, T., 1974. Effect of temperature on lesion enlargement
#' and sporulation of _Pyricularia oryzae_ in rice leaves. Phytopathology
#' 64:828-830. DOI: \doi{10.1094/Phyto-64-828}.
#'
#' Wei-Hong, L. 1996. Simulation and measurement of leaf wetness formation in
#' paddy rice crops. PhD Thesis, Wageningen Agricultural University, 87 p.
#'
#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J.
#' Modeling and mapping potential epidemics of rice diseases globally. Crop
#' Protection, Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
#' \doi{10.1016/j.cropro.2011.11.009}.
#'
#' Torres, C.Q., 1986. Effect of plant age on the expression of resistance to
#' _Pyricularia oryzae_ Cav. in upland rice varieties. PhD Thesis, University of
#' the Philippines at Los Baños, 82 p.
#'
#' @family predict functions
#'
#' @seealso
#' [SEIR()]
#'
#' @export
predict_leaf_blast <- function(wth, emergence) {
  age_coef_rc <-
    cbind(
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
        1,
        1,
        1,
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
    )
  temp_coef_rc <-
    cbind(c(10L, 15L, 20L, 25L, 30L, 35L, 40L, 45L),
          c(0, 0.5, 1, 0.6, 0.2, 0.05, 0.01, 0))
  return(
    SEIR(
      wth = wth,
      emergence = emergence,
      onset = 15L,
      duration = 120L,
      rhlim = 90L,
      rainlim = 5L,
      H0 = 600L,
      I0 = 1L,
      RcA = age_coef_rc,
      RcT = temp_coef_rc,
      RcOpt = 1.14,
      p = 5L,
      i = 20L,
      a = 1L,
      Sx = 30000L,
      RRS = 0.01,
      RRG = 0.1
    )
  )
}

#' @rdname predict_leaf_blast
#' @examplesIf interactive()
#' # use shorthand function
#' lb <- predict_lb(wth, emergence = "2000-07-01")
#' plot(x = lb$dates, y = lb$intensity, type = "l")
#' @export
predict_lb <- predict_leaf_blast
