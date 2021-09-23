
#' Predict rice brown spot intensity
#'
#' A dynamic mechanistic simulation of rice brown spot, causal agent
#' _Cochliobolus miyabeanus_. The model is driven by daily weather data, which
#' can easily be accessed using[get_wth()] to download weather data from
#' \acronym{NASA} \acronym{POWER} using \CRANpkg{nasapower} or \CRANpkg{chirps}
#' for data from \acronym{CHIRPS} and \acronym{CHIRTS}.
#'
#'
#' @details
#' #' The model represents site size as 10
#'  \ifelse{html}{\out{mm<sup>2</sup>}}{\eqn{mm^2}} of a rice plant's leaf.
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
#' @note
#' If the `wth` object provides _LAT_ and _LON_ columns, these will be included
#' in the output for mapping purposes. Both values must be present. These
#' columns are provided by default when using [get_wth()].
#'
#' @note The optimum temperature for brown spot as presented in Table 2 of
#' Savary _et al._ 2012 has a typo. The optimal value should be 25 °C, not
#' 20 °C as shown. The correct value, 25 °C, is used in this implementation.
#'
#' @inherit predict_bacterial_blight
#'
#' @return A [data.table::data.table()] of disease intensity and infection
#' sites. See [SEIR()] for a full description of the column values.
#'
#' @examplesIf interactive()
#' # get weather for IRRI Zeigler Experiment Station in wet season 2000
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#' bs <- predict_brown_spot(wth, emergence = "2000-07-01")
#' plot(x = bs$dates, y = bs$intensity, type = "l")
#'
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario
#'
#' @references Klomp, A.O., 1977. Early senescence of rice and _Drechslera
#' oryzae_ in the Wageningen polder, Surinam. PhD Thesis, 97p.
#'
#' Levy, Y. and Cohen, Y., 1980. Sporulation of _Helminthosporium turcicum_ on
#' sweet corn: Effects of temperature and dew period. Canadian Journal of Plant
#' Pathology 2:65-69. DOI: <https://doi.org/10.1080/07060668009501440>.
#'
#' Luo Wei-Hong, 1996. Simulation and measurement of leaf wetness formation in
#' paddy rice crops. PhD, Wageningen Agricultural University, 87 p.
#'
#' Padmanabhan, S.Y. and Ganguly, D. 1954. Relation between the age of rice
#' plant and its susceptibility to _Helminthosporium_ and blast disease.
#' Proceedings of the Indian Academy of Sciences B 29:44-50.
#'
#' Sarkar, A.K. and Sen Gupta, P.K., 1977. Effect of temperature and humidity on
#' disease development and sporulation of _Helminthosporium oryzae_ on rice.
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
#' @family predict functions
#'
#' @seealso
#' [SEIR()]
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
      rhlim = 90,
      rainlim = 5,
      H0 = 600,
      I0 = 1,
      RcA = age_coef_rc,
      RcT = temp_coef_rc,
      RcOpt = 0.61,
      p =  6,
      i = 19,
      a = 1,
      Sx = 100000,
      RRS = 0.01,
      RRG = 0.1
    )
  )
}
