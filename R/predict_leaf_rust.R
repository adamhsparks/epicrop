
#' Predict wheat leaf rust severity
#'
#' A dynamic mechanistic simulation of wheat leaf rust, causal agent,
#'  \emph{Puccinia tritici}. The model is driven by daily weather data,
#' which can easily be accessed using \code{\link{get_wth}} to download weather
#' data from \acronym{NASA} \acronym{POWER} using \CRANpkg{nasapower}.
#'
#' Default values for this disease model are derived from Table 2 (Savary
#' \emph{et al.} 2015).
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
#' # get weather for Manhattan, Kansas
#' wth <- get_wth(
#'   lonlat = c(-96.5717, 39.1836),
#'   dates = c("2000-08-01", "2001-06-30")
#' )
#' bs <- predict_leaf_rust(wth, emergence = "2000-08-15")
#' plot(x = bs$dates, y = bs$severity, type = "l")
#' }
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#'
#' @references
#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J. Modeling
#' and mapping potential epidemics of rice diseases globally. Crop Protection,
#' Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
#' <https://doi.org/10.1016/j.cropro.2011.11.009>.
#'
#' Savary, S., Stetkiewicz, S., Brun, F., and Willocquet, L. Modelling and
#' Mapping Potential Epidemics of Wheat Diseases—Examples on Leaf Rust and
#' Septoria Tritici Blotch Using EPIWHEAT. European Journal of Plant Pathology
#' 142, no. 4 (August 1, 2015): 771–90. DOI:
#' <https://doi.org/10.1007/s10658-015-0650-7>.
#'
#' @seealso
#' * \code{\link{SEIR}},
#' * \code{\link{predict_bacterial_blight}},
#' * \code{\link{predict_brown_spot}},
#' * \code{\link{predict_leaf_blast}},
#' * \code{\link{predict_sheath_blight}}
#' * \code{\link{predict_tungro}}
#'
#' @export
predict_leaf_rust <- function(wth, emergence, ...) {
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
      RcOpt = 1.472,
      p =  7,
      i = 31,
      H0 = 600,
      a = 1,
      Sx = 100000,
      RRS = 0.01,
      RRG = 0.1,
      RRLEX = 0,
      lesion_size = 4,
      ...
    )
  )
}
