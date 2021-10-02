
#' Predict wheat leaf or brown rust intensity
#'
#' A dynamic mechanistic simulation of leaf or brown rust disease of wheat,
#' causal agent _Puccinia triticina_.  The model is driven by daily
#' weather data, which can easily be accessed using [get_wth()] to download
#' weather data from \acronym{NASA} \acronym{POWER} using \CRANpkg{nasapower} or
#' \CRANpkg{chirps} for data from \acronym{CHIRPS} and \acronym{CHIRTS}.
#'
#' @details
#' The model represents site size as
#'  4 \ifelse{html}{\out{mm<sup>2</sup>}}{\eqn{mm^2}} of a wheat plant's leaf.
#'
#' Default values for this disease model are derived from Table 2 (Savary
#' _et al._ 2015).
#'
#' @note
#' If the `wth` object provides _LAT_ and _LON_ columns, these will be included
#' in the output for mapping purposes. Both values must be present. These
#' columns are provided by default when using [get_wth()].
#'
#' @param wth Weather data with a daily time-step, normally \acronym{NASA}
#' \acronym{POWER} or \acronym{CHIRPS}/\acronym{CHIRTS} data from [get_wth()],
#' but any[base::data.frame()] object that has the following properly named
#' columns in them will work.
#'
#'   **Field Name** | **Value**
#'   --------------:|:----------
#'   _YYYYMMDD_ | Date as Year Month Day (ISO8601)
#'   _DOY_ | Consecutive day of year, commonly called "Julian date"
#'   _TEMP_ | Mean daily temperature (°C)
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
#' lr <- predict_leaf_rust(wth, emergence = "2000-07-01")
#' plot(x = lr$dates, y = lr$intensity, type = "l")
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
predict_leaf_rust <- function(wth, emergence) {
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
      H0 = 250L,
      I0 = 10L,
      RcA = age_coef_rc,
      RcT = temp_coef_rc,
      RRLEX = 0,
      RcOpt = 0.87,
      p = 7L,
      i = 31L,
      Sx = 750000L,
      a = 1L,
      RRS = 0.01,
      RRG = 0.1
    )
  )
}
