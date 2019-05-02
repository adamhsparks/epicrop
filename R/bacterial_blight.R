
#' Predict Bacterial Blight Area Under the Disease Progress Curve (AUDPC)
#'
#' A dynamic mechanistic simulation of bacterial blight disease of rice,
#' causal agent \emph{Xanthomonas oryzae} pv. \emph{oryzae}.
#' The model is driven by daily weather data. Adapted from \pkg{cropsim} package
#' version 0.2.0-5 by Adam H. Sparks - USQ CCH.
#' Original model development: Serge Savary & Rene Pangga (IRRI).
#' Original \R implementation by Robert J. Hijmans, Rene Pangga, & Jorrel
#' Aunario (IRRI).
#'
#' @param wth Weather data with a daily time-step, normally NASA-POWER, but any
#' data that has the following fields and proper data in them will work:
#' \describe{
#'   \item{YYYYMMDD}{Date in YYYY-MM-DD format}
#'   \item{DOY}{Numeric day of year, e.g. 1 - 365}
#'   \item{T2M}{Mean daily temperature}
#'   \item{T2MN}{Minimum daily temperature}
#'   \item{T2MX}{Maximum daily temperature}
#'   \item{RH2M}{Relative humidity}
#'   \item{RAIN}{Precipitation}
#' }
#' @param emergence Expected date of crop emergence
#' @param ... Additional arguments, see \code{\link{SEIR}}
#'
#' @return A raster of AUDPC values for bacterial blight based on given weather
#' data
#'
#' @examples
#' \dontrun{
#' library(nasapower)
#' wth <- get_cell(lonlat = c(-179.5, -89.5), endate = "1983-05-31")
#' bb <- predict_bacterial_blight(wth, emergence = "1983-01-15")
#' plot(bb, type = 2)
#' }
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario
#'
#' @references Adhikari, T.B., 1991. Effects of rice genotype and environment on
#' bacterial blight progression. PhD thesis, University of the Philippines at
#' Los Baños, 143 p.
#' @references Nayak, P., Suriya Rao, A.V., Chakrabarti, N.K., 1987. Components
#' of resistance to bacterial blight disease of rice. Journal of Phytopathology
#' 119:312-318.
#' @references Baw A. and Mew, T.W., 1988. Scoring systems for evaluating rice
#' varietal resistance to bacterial blight (BB): lesion size by growth stage.
#' International Rice Research Newsletter 13:10-11.
#' @references Luo Wei-Hong, 1996. Simulation and measurement of leaf wetness
#' formation in paddy rice crops. PhD, Wageningen Agricultural University, 87 p.
#' @references Horino, O., Mew, T.W., Yamada, T., 1982. The effect of
#' temperature on the development of bacterial leaf blight on rice. Annals of
#' the Phytopathological Society of Japan 48: 72-75
#' @references Medalla, E. 1992. Characterization of resistance of IR cultivars
#' to two races of \emph{Xanthomonas oryzae} pv. \emph{oryzae}. Unpublished M.S.
#' Thesis, University of the Philippines at Los Baños, 81 p.
#'
#' @seealso \code{\link{predict_leaf_blast}},
#' \code{\link{predict_bacterial_blight}}, \code{\link{predict_brown_spot}},
#' \code{\link{predict_sheath_blight}}
#'
#' @export
predict_bacterial_blight <- function(wth, emergence = "2000-05-15", ...) {
  age_coef_rc <-
    cbind(0:12 * 10,
          c(1, 1, 1, 0.9, 0.62, 0.43, 0.41, 0.42, 0.41,
            0.41, 0.41, 0.41, 0.41))
  temp_coef_rc <-
    cbind(16 + (0:8 * 3), c(0, 0.29, 0.44, 0.90, 0.90, 1.0, 0.88,
                            0.01, 0))
  rh_coef_rc <-
    cbind(c(2, 1:8 * 3), c(0, 0.67, 0.81, 0.84, 0.87, 0.91, 0.94,
                           0.97, 1.0))
  return(
    SEIR(
      wth = wth,
      emergence = emergence,
      age_rc = age_coef_rc,
      tmp_rc = temp_coef_rc,
      rh_rc = rh_coef_rc,
      base_rc = 0.87,
      latrans = 5,
      inftrans = 30,
      site_max = 3200,
      aggr = 4,
      init_sites = 100,
      rr_physiol_senesc = 0.01,
      rrg = 0.1,
      ...
    )
  )
}
