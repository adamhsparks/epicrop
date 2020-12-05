
#' Predict rice leaf severity
#'
#' A dynamic mechanistic simulation of leaf blast disease of rice, causal agent
#' \emph{Magnaporthe oryzae}. The model is driven by daily weather data,
#' which can easily be accessed using \code{\link{get_wth}} to download weather
#' data from \acronym{NASA} \acronym{POWER} using \CRANpkg{nasapower}.
#'
#' Default values for this disease model are derived from Table 2 (Savary
#' \emph{et al.} 2012).
#'
#' @note Adapted from \pkg{cropsim} package version 0.2.0-5 by Adam H. Sparks,
#' University of Southern Queensland Centre for Crop Health.
#' Original model development: Serge Savary & Rene Pangga (IRRI).
#' Original \R implementation by Robert J. Hijmans, Rene Pangga, & Jorrel
#' Aunario (IRRI).
#'
#' @note The optimum temperature for leaf blast as presented in Table 2 of
#' Savary \emph{et al.} 2012 has a typo. The optimal value should be 20 °C, not
#' 25 °C as shown. The correct value, 20 °C, is used in this implementation.
#'
#' @param wth Weather data with a daily time-step, normally NASA POWER from
#' \code{\link{get_wth}}, but any \code{\link[base]{data.frame}} object that has
#' the following properly named columns in them will work.
#'   \tabular{rl}{
#'   **yyyymmdd**:\tab Date as Year Month Day (ISO8601).\cr
#'   **doy**:\tab  Consecutive day of year, commonly called "Julian date".\cr
#'   **tmp**:\tab Mean daily temperature (°C).\cr
#'   **tmn**:\tab Minimum daily temperature (°C).\cr
#'   **tmx**:\tab Maximum daily temperature (°C).\cr
#'   **tdew**:\tab Mean daily dew point temperature (°C).\cr
#'   **rh**:\tab Mean daily relative humidity (%).\cr
#'   **rain**:\tab Mean daily rainfall (mm).\cr
#'   }
#'
#' @param emergence Expected date of crop emergence
#' @param ... Additional arguments, see \code{\link{SEIR}}
#'
#' @return A \code{\link[data.table]{data.table}} of disease severity and
#'  infection sites. See \code{\link{SEIR}} for a full description of the
#'  column values.
#'
#' @examples
#' \donttest{
#' # get weather for IRRI Zeigler Experiment Station in wet season 2000
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#' lb <- predict_leaf_blast(wth, emergence = "2000-07-01")
#' plot(x = lb$dates, y = lb$severity, type = "l")
#' }
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario,
#' and Adam H. Sparks
#'

#' @references El Refaei, M.I., 1977. Epidemiology of rice blast disease in the
#' tropics with special reference to the leaf wetness in relation to disease
#' development. PhD Thesis, Indian Agricultural Research Institute, New Delhi,
#' 195 p.
#' @references Hemmi, T., Abe, T., Ikaya, J., and Inoue, Y. 1936. Studies on the
#' rice blast disease. IV. Relation of the environment to the development of
#' blast disease and physiologic specialization in the rice blast fungus.
#' Materials for Rural Improvement, Department of Agriculture and Forestry,
#' Japan No. 105, 145p.
#' @references Hwang B.K., Koh, Y.J., Chung, H.S., 1987. Effects of adult-plant
#' resistance on blast severity and yield of rice. Plant Disease 71:1035-1038.
#' @references Kato, H and Kozaka, T., 1974. Effect of temperature on lesion
#' enlargement and sporulation of \emph{Pyricularia oryzae} in rice leaves.
#' Phytopathology 64:828-830.
#' @references Luo Wei-Hong, 1996. Simulation and measurement of leaf wetness
#' formation in paddy rice crops. PhD Thesis, Wageningen Agricultural
#' University, 87 p.
#' @references Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario,
#' J. Modeling and mapping potential epidemics of rice diseases globally. Crop
#' Protection, Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
#' <http://dx.doi.org/10.1016/j.cropro.2011.11.009>.
#' @references Torres, C.Q., 1986. Effect of plant age on the expression of
#' resistance to \emph{Pyricularia oryzae} Cav. in upland rice varieties. PhD
#' Thesis, University of the Philippines at Los Banos, 82 p.
#'
#' @seealso
#' * \code{\link{SEIR}},
#' * \code{\link{predict_bacterial_blight}},
#' * \code{\link{predict_brown_spot}},
#' * \code{\link{predict_sheath_blight}}
#' * \code{\link{predict_tungro}}
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
  rh_coef_rc <- cbind(4 + (0:10) * 2,
                      c(0, 0.02, 0.09, 0.19, 0.29, 0.43, 0.54, 0.63, 0.77,
                        0.88, 1.0))
  return(
    SEIR(
      wth = wth,
      emergence = emergence,
      onset = 15,
      duration = 120,
      RcA = age_coef_rc,
      RcT = temp_coef_rc,
      RcW = rh_coef_rc,
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
