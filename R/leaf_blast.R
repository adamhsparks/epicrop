
#' Predict rice leaf blast area under the disease progress curve (AUDPC)
#'
#' A dynamic mechanistic simulation of leaf blast disease of rice, causal agent
#' \emph{Magnaporthe oryzae}.
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
#'   dates = c("2000-01-15", "2000-05-31")
#' )
#' lb <- predict_leaf_blast(wth, emergence = "2000-01-15")
#' plot(lb, type = 2)
#' }
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario
#'
#' @references Hwang B.K., Koh, Y.J., Chung, H.S., 1987. Effects of adult-plant
#' resistance on blast severity and yield of rice. Plant Disease 71:1035-1038.
#' @references Hemmi, T., Abe, T., Ikaya, J., and Inoue, Y. 1936. Studies on the
#' rice blast disease. IV. Relation of the environment to the development of
#' blast disease and physiologic specialization in the rice blast fungus.
#' Materials for Rural Improvement, Department of Agriculture and Forestry,
#' Japan No. 105, 145p.
#' @references Kato, H and Kozaka, T., 1974. Effect of temperature on lesion
#' enlargement and sporulation of \emph{Pyricularia oryzae} in rice leaves.
#' Phytopathology 64:828-830.
#' @references Torres, C.Q., 1986. Effect of plant age on the expression of
#' resistance to \emph{Pyricularia oryzae} Cav. in upland rice varieties. PhD
#' Thesis, University of the Philippines at Los Banos, 82 p.
#' @references El Refaei, M.I., 1977. Epidemiology of rice blast disease in the
#' tropics with special reference to the leaf wetness in relation to disease
#' development. PhD Thesis, Indian Agricultural Research Institute, New Delhi,
#' 195 p.
#' @references Luo Wei-Hong, 1996. Simulation  and measurement of leaf wetness
#' formation in paddy rice crops. PhD Thesis, Wageningen Agricultural
#' University, 87 p.
#'
#' @seealso \code{\link{predict_leaf_blast}},
#' \code{\link{predict_bacterial_blight}}, \code{\link{predict_brown_spot}},
#' \code{\link{predict_sheath_blight}}
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
      age_rc = age_coef_rc,
      tmp_rc = temp_coef_rc,
      rh_rc = rh_coef_rc,
      base_rc = 1.14,
      latrans = 5,
      inftrans = 20,
      init_sites = 600,
      aggr = 1,
      site_max = 30000,
      rr_physiol_senesc = 0.01,
      rrg = 0.1,
      ...
    )
  )
}
