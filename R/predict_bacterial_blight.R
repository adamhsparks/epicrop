
#' Predict rice bacterial blight severity
#'
#' A dynamic mechanistic simulation of bacterial blight disease of rice,
#' causal agent \emph{Xanthomonas oryzae} pv. \emph{oryzae}.
#' The model is driven by daily weather data, which can easily be accessed using
#' \code{\link{get_wth}} to download weather data from \acronym{NASA}
#' \acronym{POWER} using \CRANpkg{nasapower}.
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
#' bb <- predict_bacterial_blight(wth, emergence = "2000-07-01")
#' plot(x = bb$dates, y = bb$severity, type = "l")
#' }
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario,
#' and Adam H. Sparks
#'
#' @references Adhikari, T.B., 1991. Effects of rice genotype and environment on
#' bacterial blight progression. PhD thesis, University of the Philippines at
#' Los Baños, 143 p.
#' @references Baw A. and Mew, T.W., 1988. Scoring systems for evaluating rice
#' varietal resistance to bacterial blight (BB): lesion size by growth stage.
#' International Rice Research Newsletter 13:10-11.
#' @references Horino, O., Mew, T.W., Yamada, T., 1982. The effect of
#' temperature on the development of bacterial leaf blight on rice. Annals of
#' the Phytopathological Society of Japan 48: 72-75.
#' @references Luo Wei-Hong, 1996. Simulation and measurement of leaf wetness
#' formation in paddy rice crops. PhD, Wageningen Agricultural University, 87 p.
#' @references Medalla, E. 1992. Characterization of resistance of IR cultivars
#' to two races of \emph{Xanthomonas oryzae} pv. \emph{oryzae}. Unpublished M.S.
#' Thesis, University of the Philippines at Los Baños, 81 p.
#' @references Nayak, P., Suriya Rao, A.V., Chakrabarti, N.K., 1987. Components
#' of resistance to bacterial blight disease of rice. Journal of Phytopathology
#' 119:312-318.
#' @references Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario,
#' J. Modeling and mapping potential epidemics of rice diseases globally. Crop
#' Protection, Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
#' <http://dx.doi.org/10.1016/j.cropro.2011.11.009>.
#'
#' @seealso
#' * \code{\link{SEIR}},
#' * \code{\link{predict_brown_spot}},
#' * \code{\link{predict_leaf_blast}},
#' * \code{\link{predict_sheath_blight}}
#' * \code{\link{predict_tungro}}
#'
#' @export
predict_bacterial_blight <- function(wth, emergence, ...) {
  age_coef_rc <-
    cbind(0:12 * 10,
          c(1, 1, 1, 0.9, 0.62, 0.43, 0.41, 0.42, 0.41,
            0.41, 0.41, 0.41, 0.41))
  temp_coef_rc <-
    cbind(16 + (0:8 * 3),
          c(0, 0.29, 0.44, 0.90, 0.90, 1.0, 0.88,
                            0.01, 0))
  rh_coef_rc <-
    cbind(c(2, 1:8 * 3),
          c(0, 0.67, 0.81, 0.84, 0.87, 0.91, 0.94,
                           0.97, 1.0))
  return(
    SEIR(
      wth = wth,
      emergence = emergence,
      onset = 20,
      duration = 120,
      RcA = age_coef_rc,
      RcT = temp_coef_rc,
      RcW = rh_coef_rc,
      RcOpt = 0.87,
      p =  5,
      i = 30,
      Sx = 3200,
      a = 4,
      H0 = 100,
      RRS = 0.01,
      RRG = 0.1,
      ...
    )
  )
}
