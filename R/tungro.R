
#' Predict rice tungro area under the disease progress curve (AUDPC)
#'
#' A dynamic mechanistic simulation of tungro disease of rice, causal agents
#' \emph{Rice Tungro Spherical Virus} and \emph{Rice Tungro Baciliform Virus}.
#' The model is driven by daily weather data. Adapted from \pkg{cropsim} package
#' version 0.2.0-5 by Adam H. Sparks - USQ CCH.
#' Original model development: Serge Savary & Rene Pangga (IRRI).
#' Original \R implementation by Robert J. Hijmans, Rene Pangga, & Jorrel
#' Aunario (IRRI).
#'
#' @param wth Weather data with a daily time-step, normally NASA-POWER from
#' \code{\link{get_wth}}, but any data that has the properly named fields data
#' in them will work.
#'   * YYYYMMDD Date in YYYY-MM-DD format
#'   * DOY Numeric day of year, e.g. 1 - 365
#'   * T2M Mean daily temperature
#'   * T2MN Minimum daily temperature
#'   * T2MX Maximum daily temperature
#'   * RH2M Relative humidity
#'   * RAIN Precipitation
#'
#' @param emergence Expected date of crop emergence
#' @param ... Additional arguments, see \code{\link{SEIR}}
#'
#' @return An \pkg{epirice} \code{SEIR} object
#'
#' @examples
#' \donttest{
#' # get weather for IRRI Zeigler Experiment Station in dry season 2000
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-05-15", "2000-12-31")
#' )
#' tg <- predict_tungro(wth, emergence = "2000-05-18")
#' }
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario
#'
#' @references Tiongco, E.R., Cabunagan, R.C., Flores, Z.M., Hibino, H., and
#' Koganezawa, H., 1993. Serological monitoring of rice tungro disease
#' development in the field: its implication in disease management.
#' Plant Disease 77:877-882.
#' @references Rivera, C.T. and Ou, S.H., 1965. Leafhopper transmission of
#' tungro disease of rice. Plant Disease Reporter 49:127-131.
#' @references Ling, K.C., Palomar, M.K., 1966. Studies on rice plants infected
#' with the tungro virus at different ages. Philippines Agriculturist 50:165-177.
#' @references Ling, K.C., and Tiongco, E.R., Effect of temperature on the
#' transmission of rice tungro virus by \emph{Nephotettix virescens}.
#' Philippine Phytopathology 11:46-57.
#'
#' @seealso \code{\link{predict_leaf_blast}},
#' \code{\link{predict_bacterial_blight}}, \code{\link{predict_brown_spot}},
#' \code{\link{predict_sheath_blight}}
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
      age_rc = age_coef_rc,
      tmp_rc = temp_coef_rc,
      rh_rc = rh_coef_rc,
      base_rc = 0.18,
      latrans = 6,
      inftrans = 120,
      site_max = 100,
      aggr = 1,
      init_sites = 100,
      rr_physiol_senesc = 0.01,
      rrg = 0.1,
      ...
    )
  )
}
