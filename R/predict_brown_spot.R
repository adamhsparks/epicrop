
#' Predict rice brown spot severity
#'
#' A dynamic mechanistic simulation of rice brown spot, causal agent
#' *Cochliobolus miyabeanus*. The model is driven by daily weather data, which
#' can easily be accessed using[get_wth()] to download weather data from
#' \acronym{NASA} \acronym{POWER} using \CRANpkg{nasapower}.
#'
#' Default values for this disease model are derived from Table 2 (Savary
#' *et al.* 2012).
#'
#' @note Adapted from \pkg{cropsim} package version 0.2.0-5 by Adam H. Sparks,
#' University of Southern Queensland Centre for Crop Health.
#' Original model development: Serge Savary & Rene Pangga (IRRI).
#' Original \R implementation by Robert J. Hijmans, Rene Pangga, & Jorrel
#' Aunario (IRRI).
#'
#' @note The optimum temperature for brown spot as presented in Table 2 of
#' Savary *et al.* 2012 has a typo. The optimal value should be 25 °C, not
#' 20 °C as shown. The correct value, 25 °C, is used in this implementation.
#'
#' @param wth Weather data with a daily time-step, normally NASA POWER from
#'[get_wth()], but any[base::data.frame()] object that has
#' the following properly named columns in them will work.
#'
#'   **Field Name** | **Value**
#'   --------------:|:----------
#'   *YYYYMMDD* | Date as Year Month Day (ISO8601)
#'   *DOY* | Consecutive day of year, commonly called "Julian date"
#'   *TEMP* | Mean daily temperature (°C)
#'   *RHUM* | Mean daily temperature (°C)
#'   *RAIN* | Mean daily rainfall (mm)
#'
#' @param emergence Expected date of crop emergence
#' @param ... Additional arguments, see [SEIR()]
#'
#' @return A [data.table::data.table()] of disease severity and
#'  infection sites. See [SEIR()] for a full description of the
#'  column values.
#'
#' @examplesIf interactive()
#' # get weather for IRRI Zeigler Experiment Station in wet season 2000
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#' bs <- predict_brown_spot(wth, emergence = "2000-07-01")
#' plot(x = bs$dates, y = bs$severity, type = "l")
#'
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario
#'
#' @references Klomp, A.O., 1977. Early senescence of rice and *Drechslera
#' oryzae* in the Wageningen polder, Surinam. PhD thesis, 97p.
#'
#' Levy, Y. and Cohen, Y., 1980. Sporulation of *Helminthosporium turcicum* on
#' sweet corn: Effects of temperature and dew period. Canadian Journal of Plant
#' Pathology 2:65-69. DOI: <https://doi.org/10.1080/07060668009501440>.
#'
#' Luo Wei-Hong, 1996. Simulation and measurement of leaf wetness formation in
#' paddy rice crops. PhD, Wageningen Agricultural University, 87 p.
#'
#' Padmanabhan, S.Y. and Ganguly, D. 1954. Relation between the age of rice
#' plant and its susceptibility to *Helminthosporium* and blast disease.
#' Proceedings of the Indian Academy of Sciences B 29:44-50.
#'
#' Sarkar, A.K. and Sen Gupta, P.K., 1977. Effect of temperature and humidity on
#' disease development and sporulation of *Helminthosporium oryzae* on rice.
#' Indian Phytopathology 30:258-259.
#'
#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J. Modeling
#' and mapping potential epidemics of rice diseases globally. Crop Protection,
#' Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
#' <https://doi.org/10.1016/j.cropro.2011.11.009>.
#'
#' Waggoner. P.E., Horsfall, J.G., and Lukens, R.J. 1972. EPIMAY. A Simulator of
#' Southern Corn Leaf Blight. Bulletin of the Connecticut Experiment Station,
#' New Haven, 85 p.
#'
#' @seealso
#' * [SEIR()],
#' * [predict_bacterial_blight()],
#' * [predict_leaf_blast()],
#' * [predict_sheath_blight()],
#' * [predict_tungro()]
#'
#' @export
predict_brown_spot <- function(wth, emergence, ...) {
  age_coef_rc <-
    cbind(0:6 * 20, c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0))
  temp_coef_rc <-
    cbind(15 + (0:5) * 5, c(0, 0.06, 1.0, 0.85, 0.16, 0))
  return(
    SEIR(
      wth = wth,
      emergence = emergence,
      onset = 20,
      duration = 120,
      RcA = age_coef_rc,
      RcT = temp_coef_rc,
      RcOpt = 0.61,
      p =  6,
      i = 19,
      H0 = 600,
      a = 1,
      Sx = 100000,
      RRS = 0.01,
      RRG = 0.1,
      ...
    )
  )
}
