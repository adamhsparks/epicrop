#' Calculate the area under the disease progress curve (AUDPC)
#'
#' This function is used to return the AUDPC in the output of SEIR().  Not to be
#' used alone.
#'
#' @param intensity A `vector` of disease intensity from `SEIR()`.
#' @param simday A `vector` of simulation days from `SEIR()`.
#'
#' @return  A `numeric` value as `double`.
#'
#' @examplesIf interactive()
#' # get weather for IRRI Zeigler Experiment Station in wet season 2000
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#' bb <- predict_bacterial_blight(wth, emergence = "2000-07-01")
#' calculate_audpc(x = bb)
#'
#' @author Adam H. Sparks, \email{adamhsparks@@gmail.com}
#' @references
#' Sparks, A.H., P.D. Esker, M. Bates, W. Dall' Acqua, Z. Guo, V. Segovia, S.D.
#' Silwal, S. Tolos, and K.A. Garrett, 2008. Ecology and Epidemiology in R:
#' Disease Progress over Time. *The Plant Health Instructor*.
#' DOI:[10.1094/PHI-A-2008-0129-02]https://doi.org/10.1094/PHI-A-2008-0129-02).
#'
#' Madden, L. V., G. Hughes, and F. van den Bosch. 2007. The Study of Plant
#' Disease Epidemics. American Phytopathological Society, St. Paul, MN.
#' DOI:[10.1094/9780890545058](https://doi.org/10.1094/9780890545058).
#'
#' @keywords internal
#' @noRd

.calculate_audpc <- function(intensity, simday) {
  n <- sum(length(intensity), -1)

  meanvec <- intvec <- vector(mode = "double", length = n)

  for (i in 1:n) {
    j <- sum(i, 1)
    meanvec[i] <- mean(c(intensity[i], intensity[j]))
    intvec[i] <- sum(simday[j], -simday[i])
  }

  infprod <- meanvec * intvec

  return(sum(infprod))
}
