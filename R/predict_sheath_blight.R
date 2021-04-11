
#' Predict rice sheath blight severity
#'
#' A dynamic mechanistic simulation of sheath blight disease of rice, causal
#' agent *Rhizoctonia solani* AG1-1A Kühn. The model is driven by daily weather
#' data, which can easily be accessed using[get_wth()] to download weather data
#' from \acronym{NASA} \acronym{POWER} using \CRANpkg{nasapower}.
#'
#' Default values for this disease model are derived from Table 2 (Savary *et
#' al.* 2012).
#'
#' @note Adapted from \pkg{cropsim} package version 0.2.0-5 by Adam H. Sparks,
#' University of Southern Queensland Centre for Crop Health.
#' Original model development: Serge Savary & Rene Pangga (IRRI).
#' Original \R implementation by Robert J. Hijmans, Rene Pangga, & Jorrel
#' Aunario (IRRI).
#'
#' @param wth Weather data with a daily time-step, normally NASA POWER from
#'[get_wth()], but any[base][data.frame()] object that has
#' the following properly named columns in them will work.
#'
#'   **Field Name** | **Value**
#'   --------------:|:----------
#'   *YYYYMMDD* | Date as Year Month Day (ISO8601)
#'   *DOY* | Consecutive day of year, commonly called "Julian date"
#'   *TEMP* | Mean daily temperature (°C)
#'   *RHUM* | Mean daily temperature (°C)
#'   *RAIN* | Mean daily rainfall (mm)
#'
#' @param emergence Expected date of crop emergence
#' @param ... Additional arguments, see [SEIR()]
#'
#' @return A [data.table][data.table()] of disease severity and
#'  infection sites. See [SEIR()] for a full description of the
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
#'
#' Gross, M.K., Santini, J.B., Tikhonova, I. and Latin, R. 1998. The influence
#' of temperature and leaf wetness duration on infection of perennial ryegrass
#' by *Rhizoctonia solani*. Plant Disease 82:1012-1016. DOI:
#' <https://doi.org/10.1094/PDIS.1998.82.9.1012>
#'
#' Hashiba, T. and Ijiri, T., 1989. Estimation of yield loss and computerized
#' forecasting system (BLIGHTAS) for rice sheath blight disease. International
#' Symposium on Tropical Agricultural Research: Crop losses due to disease
#' outbreaks in the tropics and countermeasures. Tropical Agricultural
#' Research Series (Japan) No. 22 pp. 163-171.
#'
#' Savary, S., Willocquet, L., Teng, P.S., 1997. Modelling sheath blight
#' epidemics on rice tillers. Agricultural Systems 55:359-384. DOI:
#' <https://doi.org/10.1016/S0308-521X(97)00014-0>.
#'
#' Savary, S., Castilla, N.P., Willocquet, L. 2001. Analysis of the spatio-
#' temporal structure of rice sheath blight epidemics in a farmer's field.
#' Plant Pathology 50:53-68. DOI:
#' <https://doi.org/10.1046/j.1365-3059.2001.00531.x>
#'
#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J. Modeling
#' and mapping potential epidemics of rice diseases globally. Crop Protection,
#' Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
#' <https://doi.org/10.1016/j.cropro.2011.11.009>.
#'
#' Sharma, N.R., Teng, P.S., Olivares, F.M., 1990. Effect of rice growth stage
#' on sheath blight (ShB) development and yield loss. International Rice
#' Research Newsletter 15:19-20.
#'
#' Tu, C.C., Chang, Y.C., Wang, C.W., 1979. Studies on the ecology of
#' *Rhizoctonia solani*, the causal organism of rice sheath blight. National
#' Science Council Monthly, ROC 7:1208-1219.
#'
#' @seealso
#' * [SEIR()],
#' * [predict_bacterial_blight()],
#' * [predict_brown_spot()],
#' * [predict_leaf_blast()],
#' * [predict_tungro()]
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
