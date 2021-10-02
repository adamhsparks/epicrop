
#' Predict rice sheath blight intensity
#'
#' A dynamic mechanistic simulation of sheath blight disease of rice, causal
#' agent _Rhizoctonia solani_ AG1-1A Kühn. The model is driven by daily weather
#' data, which can easily be accessed using[get_wth()] to download weather data
#' from \acronym{NASA} \acronym{POWER} using \CRANpkg{nasapower} or
#' \CRANpkg{chirps} for data from \acronym{CHIRPS} and \acronym{CHIRTS}.
#'
#'
#' @details
#' The model represents site size as 1 rice plant's tiller.
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
#' @note
#' If the `wth` object provides _LAT_ and _LON_ columns, these will be included
#' in the output for mapping purposes. Both values must be present. These
#' columns are provided by default when using [get_wth()].
#'
#' @inherit predict_bacterial_blight
#'
#' @return A [data.table::data.table()] of disease intensity and infection
#' sites. See [SEIR()] for a full description of the column values.
#'
#' @examplesIf interactive()
#' # get weather for IRRI Zeigler Experiment Station in wet season 2000
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#' sb <- predict_sheath_blight(wth, emergence = "2000-07-01")
#' plot(x = sb$dates, y = sb$intensity, type = "l")
#'
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
#' by _Rhizoctonia solani_. Plant Disease 82:1012-1016. DOI:
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
#' _Rhizoctonia solani_, the causal organism of rice sheath blight. National
#' Science Council Monthly, ROC 7:1208-1219.
#'
#' @family predict functions
#'
#' @seealso
#' [SEIR()]
#'
#' @export
predict_sheath_blight <- function(wth, emergence) {
  age_coef_rc <-
    cbind(c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L, 100L, 110L, 120L),
          c(
            0.84,
            0.84,
            0.84,
            0.84,
            0.84,
            0.84,
            0.84,
            0.88,
            0.88,
            1.0,
            1.0,
            1.0,
            1.0
          ))
  temp_coef_rc <-
    cbind(c(12L, 16L, 20L, 24L, 28L, 32L, 36L, 40L),
          c(0, 0.42, 0.94, 0.94, 1.0, 0.85, 0.64, 0))
  return(
    SEIR(
      wth = wth,
      emergence = emergence,
      onset = 30L,
      duration = 120L,
      rhlim = 90L,
      rainlim = 5L,
      H0 = 25L,
      I0 = 1L,
      RcA = age_coef_rc,
      RcT = temp_coef_rc,
      RcOpt = 0.46,
      p = 3L,
      i = 120L,
      Sx = 800L,
      a = 2.8,
      RRS = 0.005,
      RRG = 0.2
    )
  )
}
