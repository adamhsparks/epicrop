
#' Predict rice bacterial blight intensity
#'
#' A dynamic mechanistic simulation of bacterial blight disease of rice,
#' causal agent _Xanthomonas oryzae_ pv. _oryzae_.  The model is driven by daily
#' weather data, which can easily be accessed using [get_wth()] to download
#' weather data from \acronym{NASA} \acronym{POWER} using \CRANpkg{nasapower}.
#'
#' @details
#' The model represents site size as 1 rice plant's leaf.
#'
#' Default values for this disease model are derived from Table 2 (Savary
#' _et al._ 2012).
#'
#' [predict_bb()] is a shorthand alias for [predict_bacterial_blight()].
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
#' @param wth Weather data with a daily time-step, normally \acronym{NASA}
#' \acronym{POWER} data from [get_wth()], but any[base::data.frame()] object
#' that has the following properly named columns in them will work.
#'
#'   **Field Name** | **Value**
#'   --------------:|:----------
#'   _YYYYMMDD_ | Date as Year Month Day (ISO8601)
#'   _DOY_ | Consecutive day of year, commonly called "Julian date"
#'   _TEMP_ | Mean daily temperature (°C)
#'   _RHUM_ | Mean daily relative humidity (%)
#'   _RAIN_ | Mean daily rainfall (mm)
#'   _LAT_ | **Optional** latitude of weather observation. See LAT/LON Note.
#'   _LON_ | **Optional** longitude of weather observation. See LAT/LON Note.
#'
#' @param emergence Expected date of crop emergence
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
#' bb <- predict_bacterial_blight(wth, emergence = "2000-07-01")
#' plot(x = bb$dates, y = bb$intensity, type = "l")
#'
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario,
#' and Adam H. Sparks
#'
#' @references Adhikari, T.B., 1991. Effects of rice genotype and environment on
#' bacterial blight progression. PhD Thesis, University of the Philippines at
#' Los Baños, 143 p.
#'
#' Baw A. and Mew, T.W., 1988. Scoring systems for evaluating rice varietal
#' resistance to bacterial blight (BB): lesion size by growth stage.
#' International Rice Research Newsletter 13:10-11.
#'
#' Horino, O., Mew, T.W., Yamada, T., 1982. The effect of temperature on the
#' development of bacterial leaf blight on rice. Annals of the Phytopathological
#' Society of Japan 48: 72-75.
#'
#' Luo Wei-Hong, 1996. Simulation and measurement of leaf wetness formation in
#' paddy rice crops. PhD, Wageningen Agricultural University, 87 p.
#'
#' Medalla, E. 1992. Characterization of resistance of IR cultivars to two races
#' of _Xanthomonas oryzae_ pv. _oryzae_. Unpublished M.S. Thesis, University
#' of the Philippines at Los Baños, 81 p.
#'
#' Nayak, P., Suriya Rao, A.V., Chakrabarti, N.K., 1987. Components of
#' resistance to bacterial blight disease of rice. Journal of Phytopathology
#' 119:312-318. DOI: \doi{10.1111/j.1439-0434.1987.tb04402.x}.
#'
#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J. Modeling
#' and mapping potential epidemics of rice diseases globally. Crop Protection,
#' Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
#' \doi{10.1016/j.cropro.2011.11.009}.
#'
#' @family predict functions
#'
#' @seealso
#' [SEIR()]
#'
#' @export
predict_bacterial_blight <- function(wth, emergence) {
  age_coef_rc <-
    cbind(c(0L, 10L, 20L, 30L, 40L, 50L, 60L, 70L, 80L, 90L, 100L, 110L, 120L),
          c(1, 1, 1, 0.9, 0.62, 0.43, 0.41, 0.41, 0.41, 0.41, 0.41, 0.41, 0.41))
  temp_coef_rc <-
    cbind(c(16L, 19L, 22L, 25L, 28L, 31L, 34L, 37L, 40L),
          c(0, 0.29, 0.44, 0.90, 0.90, 1.0, 0.88, 0.01, 0))
  return(
    SEIR(
      wth = wth,
      emergence = emergence,
      onset = 20L,
      duration = 120L,
      rhlim = 90L,
      rainlim = 5L,
      H0 = 100L,
      I0 = 1L,
      RcA = age_coef_rc,
      RcT = temp_coef_rc,
      RcOpt = 0.87,
      p = 5L,
      i = 30L,
      Sx = 3200L,
      a = 4L,
      RRS = 0.01,
      RRG = 0.1
    )
  )
}

#' @rdname predict_bacterial_blight
#' @examplesIf interactive()
#' # use shorthand function
#' bb <- predict_bb(wth, emergence = "2000-07-01")
#' plot(x = bb$dates, y = bb$intensity, type = "l")
#' @export
predict_bb <- predict_bacterial_blight
