
#' Predict rice brown spot intensity
#'
#' A dynamic mechanistic simulation of rice brown spot, causal agent
#' _Cochliobolus miyabeanus_. The model is driven by daily weather data, which
#' can easily be accessed using[get_wth()] to download weather data from
#' \acronym{NASA} \acronym{POWER} using \CRANpkg{nasapower} or \CRANpkg{chirps}
#' for data from \acronym{CHIRPS} and \acronym{CHIRTS}.
#'
#' @details
#' The model represents site size as
#'  4.3\ifelse{html}{\out{mm<sup>2</sup>}}{\eqn{mm^2}} of a wheat plant's leaf.
#'
#' Default values for this disease model are derived from Table 2 (Savary
#' _et al._ 2015).
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
#' sb <- predict_septoria_tritici_blotch(wth, emergence = "2000-07-01")
#' plot(x = sb$dates, y = sb$intensity, type = "l")
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#'
#' @references
#'
#' @family predict functions
#'
#' @seealso
#' [SEIR()]
#'
#' @export
predict_Septoria_tritici_blotch <- function(wth, emergence) {
  age_coef_rc <-
    cbind(c(0L, 20L, 40L, 60L, 80L, 100L, 120L),
          c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0))
  temp_coef_rc <-
    cbind(c(15L, 20L, 25L, 30L, 35L, 40L),
          c(0, 0.06, 1.0, 0.85, 0.16, 0))
  return(
    SEIR(
      wth = wth,
      emergence = emergence,
      onset = 20L,
      duration = 120L,
      rhlim = 90L,
      rainlim = 5L,
      H0 = 250,
      I0 = 15L,
      RcA = age_coef_rc,
      RcT = temp_coef_rc,
      RcOpt = 1.17,
      RRLEX = 0.09,
      p = 11L,
      i = 18L,
      a = 1L,
      Sx = 174000L,
      RRS = 0.01,
      RRG = 0.1
    )
  )
}
