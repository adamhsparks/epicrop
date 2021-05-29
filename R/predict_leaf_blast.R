
#' Predict rice leaf blast severity
#'
#' A dynamic mechanistic simulation of leaf blast disease of rice, causal agent
#' _Magnaporthe oryzae_. The model is driven by daily weather data, which can
#' easily be accessed using[get_wth()] to download weather data from
#' \acronym{NASA} \acronym{POWER} using \CRANpkg{nasapower}.
#'
#' Default values for this disease model are derived from Table 2 (Savary
#' _et al._ 2012).
#'
#' @note Adapted from \pkg{cropsim} package version 0.2.0-5 by Adam H. Sparks,
#' Department of Primary Industries and Regional Development, WA, AU.
#' Original model development: Serge Savary & Rene Pangga (IRRI).
#' Original \R implementation by Robert J. Hijmans, Rene Pangga, & Jorrel
#' Aunario (IRRI).
#'
#' @note The optimum temperature for leaf blast as presented in Table 2 of
#' Savary _et al._ 2012 has a typo. The optimal value should be 20 °C, not
#' 25 °C as shown. The correct value, 20 °C, is used in this implementation.
#'
#' @param wth Weather data with a daily time-step, normally NASA POWER from
#'[get_wth()], but any[base::data.frame()] object that has
#' the following properly named columns in them will work.
#'
#'   **Field Name** | **Value**
#'   --------------:|:----------
#'   _YYYYMMDD_ | Date as Year Month Day (ISO8601)
#'   _DOY_ | Consecutive day of year, commonly called "Julian date"
#'   _TEMP_ | Mean daily temperature (°C)
#'   _RHUM_ | Mean daily temperature (°C)
#'   _RAIN_ | Mean daily rainfall (mm)
#'
#' @param emergence Expected date of crop emergence
#' @param ... Additional arguments, see [SEIR()]
#'
#' @return A [data.table::data.table()] of disease severity and infection sites.
#' See [SEIR()] for a full description of the column values.
#'
#' @examplesIf interactive()
#'
#' # get weather for IRRI Zeigler Experiment Station in wet season 2000
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#' lb <- predict_leaf_blast(wth, emergence = "2000-07-01")
#' plot(x = lb$dates, y = lb$severity, type = "l")
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
#' Hwang B.K., Koh, Y.J., Chung, H.S., 1987. Effects of adult-plant resistance
#' on blast severity and yield of rice. Plant Disease 71:1035-1038. DOI:
#' <https://doi.org/10.1094/PD-71-1035>.
#'
#' Kato, H and Kozaka, T., 1974. Effect of temperature on lesion enlargement and
#' sporulation of _Pyricularia oryzae_ in rice leaves. Phytopathology
#' 64:828-830. DOI: <https://doi.org/10.1094/Phyto-64-828>.
#'
#' Luo Wei-Hong, 1996. Simulation and measurement of leaf wetness formation in
#' paddy rice crops. PhD Thesis, Wageningen Agricultural University, 87 p.
#'
#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J.
#' Modeling and mapping potential epidemics of rice diseases globally. Crop
#' Protection, Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
#' <https://doi.org/10.1016/j.cropro.2011.11.009>.
#'
#' Torres, C.Q., 1986. Effect of plant age on the expression of resistance to
#' _Pyricularia oryzae_ Cav. in upland rice varieties. PhD Thesis, University of
#' the Philippines at Los Baños, 82 p.
#'
#' @seealso
#' * [SEIR()],
#' * [predict_bacterial_blight()],
#' * [predict_brown_spot()],
#' * [predict_sheath_blight()],
#' * [predict_tungro()]
#'
#' @export
predict_leaf_blast <- function(wth, emergence, ...) {
  age_coef_rc <-
    cbind(
      0:24 * 5,
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
    cbind(2:9 * 5, c(0, 0.5, 1, 0.6, 0.2, 0.05, 0.01, 0))
  return(
    SEIR(
      wth = wth,
      emergence = emergence,
      onset = 15,
      duration = 120,
      rhlim = 90,
      rainlim = 5,
      I0 = 1,
      RcA = age_coef_rc,
      RcT = temp_coef_rc,
      RcOpt = 1.14,
      p =  5,
      i = 20,
      H0 = 600,
      a = 1,
      Sx = 30000,
      RRS = 0.01,
      RRG = 0.1,
      ...
    )
  )
}
