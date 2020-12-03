
#' Predict rice brown spot severity
#'
#' A dynamic mechanistic simulation of rice brown spot, causal agent
#' \emph{Cochliobolus miyabeanus}. The model is driven by daily weather data,
#' which can easily be accessed using \code{\link{get_wth}} to download weather
#' data from \acronym{NASA} \acronym{POWER} using \CRANpkg{nasapower}.
#'
#' Default values for this disease model are derived from Table 2 (Savary
#' \emph{et al.} 2012).
#'
#' @note
#' Adapted from \pkg{cropsim} package version 0.2.0-5 by Adam H. Sparks - USQ
#' CCH.
#' Original model development: Serge Savary & Rene Pangga (IRRI).
#' Original \R implementation by Robert J. Hijmans, Rene Pangga, & Jorrel
#' Aunario (IRRI).
#'
#' @param wth Weather data with a daily time-step, normally NASA-POWER from
#' \code{\link{get_wth}}, but any \code{\link[base]{data.frame}} object that has
#' the following properly named columns in them will work.
#'   \tabular{rl}{
#'   **YYYYMMDD**:\tab Date as Year Month Day (ISO8601).\cr
#'   **DOY**:\tab  Consecutive day of year, commonly called "Julian date".\cr
#'   **TM**:\tab Mean daily temperature (°C).\cr
#'   **TN**:\tab Minimum daily temperature (°C).\cr
#'   **TX**:\tab Maximum daily temperature (°C).\cr
#'   **TDEW**:\tab Mean daily dew point temperature (°C).\cr
#'   **RH**:\tab Mean daily relative humidity (%).\cr
#'   **RAIN**:\tab Mean daily rainfall (mm).\cr
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
#' bs <- predict_brown_spot(wth, emergence = "2000-07-01")
#' plot(x = bs$dates, y = bs$severity, type = "l")
#' }
#'
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario
#'
#' @references Klomp, A.O., 1977. Early senescence of rice and \emph{Dreschslera
#' oryzae} in the Wageningen polder, Surinam. PhD thesis, 97p.
#' @references Levy, Y. and Cohen, Y., 1980. Sporulation of
#' \emph{Helminthosporium turcicum} on sweet corn: Effects of temperature and
#' dew period. Canadian Journal of Plant Pathology 2:65-69.
#' @references Padmanabhan, S.Y. and Ganguly, D. 1954. Relation between the age
#' of rice plant and its susceptibility to \emph{Helminthosporium} and blast
#' disease. Proceedings of the Indian Academy of Sciences B 29:44-50.
#' @references Sarkar, A.K. and Sen Gupta, P.K., 1977. Effect of temperature and
#' humidity on disease development and sporulation of \emph{Helminthosporium
#' oryzae} on rice. Indian Phytopathology 30:258-259.
#' @references Luo Wei-Hong, 1996. Simulation and measurement of leaf wetness
#' formation in paddy rice crops. PhD, Wageningen Agricultural University, 87 p.
#' @references Waggoner. P.E., Horsfall, J.G., and Lukens, R.J. 1972. EPIMAY. A
#' Simulator of Southern Corn Leaf Blight. Bulletin of the Connecticut
#' Experiment Station, New Haven, 85 p.
#'
#' @seealso
#' * \code{\link{SEIR}},
#' * \code{\link{predict_bacterial_blight}},
#' * \code{\link{predict_leaf_blast}},
#' * \code{\link{predict_sheath_blight}}
#' * \code{\link{predict_tungro}}
#'
#' @export
predict_brown_spot <- function(wth, emergence, ...) {
  age_coef_rc <-
    cbind(0:6 * 20, c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0))
  temp_coef_rc <-
    cbind(15 + (0:5) * 5, c(0, 0.06, 1.0, 0.85, 0.16, 0))
  rh_coef_rc <- cbind(0:8 * 3,
                      c(0, 0.12, 0.20, 0.38, 0.46, 0.60, 0.73, 0.87, 1.0))
  return(
    SEIR(
      wth = wth,
      emergence = emergence,
      onset = 20,
      duration = 120,
      RcA = age_coef_rc,
      RcT = temp_coef_rc,
      RcW = rh_coef_rc,
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
