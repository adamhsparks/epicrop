
#' Predict rice tungro severity
#'
#' A dynamic mechanistic simulation of tungro disease of rice, causal agents
#' \emph{Rice Tungro Spherical Virus} and \emph{Rice Tungro Baciliform Virus}.
#' The model is driven by daily weather data, which can easily be accessed using
#' \code{\link{get_wth}} to download weather data from \acronym{NASA}
#' \acronym{POWER} using \CRANpkg{nasapower}.
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
#' @param wth Weather data with a daily time-step, normally NASA POWER from
#' \code{\link{get_wth}}, but any \code{\link[base]{data.frame}} object that has
#' the following properly named columns in them will work.
#'   \tabular{rl}{
#'   **YYYYMMDD**:\tab Date as Year Month Day (ISO8601).\cr
#'   **DOY**:\tab  Consecutive day of year, commonly called "Julian date".\cr
#'   **TEMP**:\tab Mean daily temperature (°C).\cr
#'   **TMIN**:\tab Minimum daily temperature (°C).\cr
#'   **TMAX**:\tab Maximum daily temperature (°C).\cr
#'   **TDEW**:\tab Mean daily dew point temperature (°C).\cr
#'   **RHUM**:\tab Mean daily temperature (°C).\cr
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
#' tg <- predict_tungro(wth, emergence = "2000-07-01")
#' plot(x = tg$dates, y = tg$severity, type = "l")
#' }
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario
#' and Adam H. Sparks
#'
#' @references Ling, K.C., and Tiongco, E.R., 1976. Effect of temperature on the
#' transmission of rice tungro virus by \emph{Nephotettix virescens}.
#' Philippine Phytopathology 11:46-57.
#' @references Ling, K.C., Palomar, M.K., 1966. Studies on rice plants infected
#' with the tungro virus at different ages. Philippines Agriculturist
#' 50:165-177.
#' @references Rivera, C.T. and Ou, S.H., 1965. Leafhopper transmission of
#' tungro disease of rice. Plant Disease Reporter 49:127-131.
#' @references Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario,
#' J. Modeling and mapping potential epidemics of rice diseases globally. Crop
#' Protection, Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
#' <http://dx.doi.org/10.1016/j.cropro.2011.11.009>.
#' @references Tiongco, E.R., Cabunagan, R.C., Flores, Z.M., Hibino, H., and
#' Koganezawa, H., 1993. Serological monitoring of rice tungro disease
#' development in the field: its implication in disease management.
#' Plant Disease 77:877-882.
#'
#' @seealso
#' * \code{\link{SEIR}},
#' * \code{\link{predict_bacterial_blight}},
#' * \code{\link{predict_brown_spot}},
#' * \code{\link{predict_leaf_blast}}
#' * \code{\link{predict_sheath_blight}}
#'
#' @export
predict_tungro <- function(wth, emergence, ...) {
  age_coef_rc <-
    cbind(0:8 * 15, c(1.0, 1.0, 0.98, 0.73, 0.51, 0.34, 0, 0, 0))
  temp_coef_rc <- cbind(c(9, 10 + (0:9 * 3.1111), 40),
                        c(0, 0.13, 0.65, 0.75, 0.83, 0.89, 0.93, 0.97, 1.0,
                          0.96, 0.93, 0))
  rh_coef_rc <- 1
  return(
    SEIR(
      wth = wth,
      emergence = emergence,
      onset = 25,
      duration = 120,
      RcA = age_coef_rc,
      RcT = temp_coef_rc,
      RcW = rh_coef_rc,
      RcOpt = 0.18,
      p =  6,
      i = 120,
      Sx = 100,
      a = 1,
      H0 = 100,
      RRS = 0.01,
      RRG = 0.1,
      ...
    )
  )
}
