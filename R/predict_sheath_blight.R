
#' Predict rice sheath blight severity
#'
#' A dynamic mechanistic simulation of sheath blight disease of rice, causal
#' agent \emph{Rhizoctonia solani} AG1-1A Kühn. The model is driven by daily
#' weather data, which can easily be accessed using \code{\link{get_wth}} to
#' download weather data from \acronym{NASA} \acronym{POWER} using
#' \CRANpkg{nasapower}.
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
#' sb <- predict_sheath_blight(wth, emergence = "2000-07-01")
#' plot(x = sb$dates, y = sb$severity, type = "l")
#' }
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario,
#' and Adam H. Sparks
#'
#' @references Castilla, N.P., Leano, R.M., Elazegui, F.A., Teng, P.S., Savary,
#' S., 1996. Effects of plant contacts, inoculation pattern, leaf wetness
#' regime, and nitrogen supply on inoculum efficiency in rice sheath blight.
#' Journal of Phytopathology 144:187-192.
#' @references Gross, M.K., Santini, J.B., Tikhonova, I. and Latin, R. 1998. The
#' influence of temperature and leaf wetness duration on infection of perennial
#' ryegrass by \emph{Rhizoctonia solani}. Plant Disease 82:1012-1016.
#' @references Hashiba, T. and Ijiri, T., 1989. Estimation of yield loss and
#' computerized forecasting system (BLIGHTAS) for rice sheath blight disease.
#' International Symposium on Tropical Agricultural Research: Crop losses due to
#' disease outbreaks in the tropics and countermeasures. Tropical Agricultural
#' Research Series (Japan) No. 22 pp. 163-171.
#' @references Savary, S., Willocquet, L., Teng, P.S., 1997. Modelling sheath
#' blight epidemics on rice tillers. Agricultural Systems 55:359-384.
#' @references Savary, S., Castilla, N.P., Willocquet, L. 2001. Analysis of the
#' spatio-temporal structure of rice sheath blight epidemics in a farmer's
#' field. Plant Pathology 50:53-68.
#' @references Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario,
#' J. Modeling and mapping potential epidemics of rice diseases globally. Crop
#' Protection, Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
#' <http://dx.doi.org/10.1016/j.cropro.2011.11.009>.
#' @references Sharma, N.R., Teng, P.S., Olivares, F.M., 1990. Effect of rice
#' growth stage on sheath blight (ShB) development and yield loss. International
#' Rice Research Newsletter 15:19-20.
#' @references Tu, C.C., Chang, Y.C., Wang, C.W., 1979. Studies on the ecology
#' of \emph{Rhizoctonia solani}, the causal organism of rice sheath blight.
#' National Science Council Monthly, ROC 7:1208-1219.
#'
#' @seealso
#' * \code{\link{SEIR}},
#' * \code{\link{predict_bacterial_blight}},
#' * \code{\link{predict_brown_spot}},
#' * \code{\link{predict_leaf_blast}},
#' * \code{\link{predict_tungro}}
#'
#' @export
predict_sheath_blight <- function(wth, emergence, ...) {
  age_coef_rc <-
    cbind(0:12 * 10,
          c(
            0.84,
            0.84,
            0.84,
            0.84,
            0.84,
            0.84,
            0.83,
            0.88,
            0.88,
            1.0,
            1.0,
            1.0,
            1.0
          ))
  rh_coef_rc <-
    cbind(c(8, 3:8 * 3), c(0, 0.24, 0.41, 0.68, 0.94, 0.97, 1.0))
  temp_coef_rc <-
    cbind(3:10 * 4, c(0, 0.42, 0.94, 0.94, 1.0, 0.85, 0.64, 0))
  return(
    SEIR(
      wth = wth,
      emergence = emergence,
      onset = 30,
      duration = 120,
      RcA = age_coef_rc,
      RcT = temp_coef_rc,
      RcW = rh_coef_rc,
      RcOpt = 0.46,
      p =  3,
      i = 120,
      Sx = 800,
      a = 2.8,
      H0 = 25,
      RRS = 0.005,
      RRG = 0.2,
      ...
    )
  )
}
