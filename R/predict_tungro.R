
#' Predict rice tungro disease severity
#'
#' A dynamic mechanistic simulation of tungro disease of rice, causal agents
#' _Rice Tungro Spherical Virus_ and _Rice Tungro Bacilliform Virus_.
#' The model is driven by daily weather data, which can easily be accessed using
#'[get_wth()] to download weather data from \acronym{NASA}
#' \acronym{POWER} using \CRANpkg{nasapower}.
#'
#' Default values for this disease model are derived from Table 2 (Savary *et
#' al.* 2012).
#'
#' @note Adapted from \pkg{cropsim} package version 0.2.0-5 by Adam H. Sparks,
#' Department of Primary Industries and Regional Development, WA, AU.
#' Original model development: Serge Savary & Rene Pangga (IRRI).
#' Original \R implementation by Robert J. Hijmans, Rene Pangga, & Jorrel
#' Aunario (IRRI).
#'
#' @param wth Weather data with a daily time-step, normally \acronym{NASA}
#' \acronym{POWER} from [get_wth()], but any[base::data.frame()] object that has
#' the following properly named columns in them will work.
#'
#'   **Field Name** | **Value**
#'   --------------:|:----------
#'   _YYYYMMDD_ | Date as Year Month Day (ISO8601)
#'   _DOY_ | Consecutive day of year, commonly called "Julian date"
#'   _TEMP_ | Mean daily temperature (°C)
#'   _RHUM_ | Mean daily temperature (°C)
#'   _RAIN_ | Mean daily rainfall (mm)
#'
#' @param emergence Expected date of crop emergence
#' @param ... Additional arguments, see [SEIR()]
#'
#' @return A [data.table::data.table()] of disease severity and infection sites.
#' See [SEIR()] for a full description of the column values.
#'
#' @examplesIf interactive()
#'
#' # get weather for IRRI Zeigler Experiment Station in wet season 2000
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#' tg <- predict_tungro(wth, emergence = "2000-07-01")
#' plot(x = tg$dates, y = tg$severity, type = "l")
#'
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario
#' and Adam H. Sparks
#'
#' @references Ling, K.C., and Tiongco, E.R., 1976. Effect of temperature on the
#' transmission of rice tungro virus by _Nephotettix virescens_.
#' Philippine Phytopathology 11:46-57.
#'
#' Ling, K.C., Palomar, M.K., 1966. Studies on rice plants infected with the
#' tungro virus at different ages. Philippines Agriculturist 50:165-177.
#'
#' Rivera, C.T. and Ou, S.H., 1965. Leafhopper transmission of tungro disease of
#' rice. Plant Disease Reporter 49:127-131.
#'
#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J.
#' Modeling and mapping potential epidemics of rice diseases globally. Crop
#' Protection, Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
#' <https://doi.org/10.1016/j.cropro.2011.11.009>.
#'
#' Tiongco, E.R., Cabunagan, R.C., Flores, Z.M., Hibino, H., and Koganezawa, H.,
#' 1993. Serological monitoring of rice tungro disease development in the field:
#' its implication in disease management. Plant Disease 77:877-882. DOI
#' <https://doi.org/10.1094/PD-77-0877>.
#'
#' @seealso
#' * [SEIR()],
#' * [predict_bacterial_blight()],
#' * [predict_brown_spot()],
#' * [predict_leaf_blast()],
#' * [predict_sheath_blight()]
#'
#' @export
predict_tungro <- function(wth, emergence, ...) {
  age_coef_rc <-
    cbind(0:8 * 15, c(1.0, 1.0, 0.98, 0.73, 0.51, 0.34, 0, 0, 0))
  temp_coef_rc <- cbind(c(9, 10 + (0:9 * 3.1111), 40),
                        c(0, 0.13, 0.65, 0.75, 0.83, 0.89, 0.93, 0.97, 1.0,
                          0.96, 0.93, 0))
  return(
    SEIR(
      wth = wth,
      emergence = emergence,
      onset = 25,
      duration = 120,
      rhlim = 90,
      rainlim = 5,
      I0 = 1,
      RcA = age_coef_rc,
      RcT = temp_coef_rc,
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
