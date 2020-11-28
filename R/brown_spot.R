
#' Predict rice brown spot area under the disease progress curve (AUDPC)
#'
#' A dynamic mechanistic simulation of rice brown spot, causal agent
#' \emph{Cochliobolus miyabeanus}.
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
#' wth <- get_wth(lonlat = c(-179.5, -89.5),
#'                start = "1985-01-15",
#'                end = "1983-05-31")
#' bs <- predict_brown_spot(wth, emergence = "2000-05-15")
#' plot(bs, type = 2)
#' }
#'
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario
#'
#' @references Klomp, A.O., 1977. Early senescence of rice and \emph{Dreschslera
#' oryzae} in the Wageningen polder, Surinam. PhD thesis, 97p.
#' @references Levy, Y. and Cohen, Y., 1980. Sporulation of \emph{
#' Helminthosporium turcicum} on sweet corn: Effects of temperature and dew
#' period. Canadian Journal of Plant Pathology 2:65-69.
#' @references Padmanabhan, S.Y. and Ganguly, D. 1954. Relation between the age
#' of rice plant and its susceptibility to \emph{Helminthosporium} and blast
#' disease. Proceedings of the Indian Academy of Sciences B 29:44-50.
#' @references Sarkar, A.K. and Sen Gupta, P.K., 1977. Effect of temperature and
#' humidity on disease development and sporulation of \emph{Helminthosporium
#' oryzae} on rice. Indian Phytopathology 30:258-259.
#' @references Luo Wei-Hong, 1996. Simulation  and measurement of leaf wetness
#' formation in paddy rice crops. PhD, Wageningen Agricultural University, 87 p.
#' @references Waggoner. P.E., Horsfall, J.G., and Lukens, R.J. 1972. EPIMAY. A
#' Simulator of Southern Corn Leaf Blight. Bulletin of the Connecticut
#' Experiment Station, New Haven, 85 p.
#'
#' @seealso \code{\link{predict_leaf_blast}},
#' \code{\link{predict_bacterial_blight}}, \code{\link{predict_brown_spot}},
#' \code{\link{predict_sheath_blight}}
#'
#' @export
predict_brown_spot <- function(wth, emergence = "2000-05-15", ...) {
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
      age_rc = age_coef_rc,
      tmp_rc = temp_coef_rc,
      rh_rc = rh_coef_rc,
      base_rc = 0.61,
      latrans = 6,
      inftrans = 19,
      init_sites = 600,
      aggr = 1,
      site_max = 100000,
      rr_physiol_senesc = 0.01,
      rrg = 0.1,
      ...
    )
  )
}
