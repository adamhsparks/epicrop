#### Brown spot ----------------------------------------------------------------
#' Predict brown spot area under the disease progress curve (AUDPC)
#'
#' A dynamic mechanistic simulation of rice brown spot.
#' The model is driven by daily weather data. Adapted from cropsim package
#' version 0.2.0-5 by Adam H. Sparks - USQ CCH.
#' Original model development: Serge Savary & Rene Pangga (IRRI).
#' Original R implementation by Robert J. Hijmans, Rene Pangga, & Jorrel Aunario
#'(IRRI).
#'
#' @param wth - Weather data source
#' @param emergence - Expected date of crop emergence
#' @param ... Additional arguments - See \link[epiRice]{SEIR}
#'
#' @return A raster of AUDPC values for brown spot based on given weather data
#'
#' @examples
#' wth <- readr::read_table(system.file("extdata", "daily_weather_28368.nasa",
#' package = "epiRice"), skip = 13)
#' bs <- brown_spot(wth, onset = 20, duration = 120)
#' plot(bs, type = 2)
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
#' @seealso \code{\link[epiRice]{leaf_blast}},
#' \code{\link[epiRice]{tungro}}, \code{\link[epiRice]{sheath_blight}},
#' \code{\link[epiRice]{bacterial_blight}}
#'
#' @export
brown_spot <- function(wth, emergence = "2000-05-15", ...) {
  age_coef_rc <- cbind(0:6 * 20, c(0.35, 0.35, 0.35, 0.47, 0.59, 0.71, 1.0))
  temp_coef_rc <- cbind(15 + (0:5) * 5, c(0, 0.06, 1.0, 0.85, 0.16, 0))
  rh_coef_rc <- cbind(0:8 * 3,
                      c(0, 0.12, 0.20, 0.38, 0.46, 0.60, 0.73, 0.87, 1.0))
  return(SEIR(wth = wth, emergence = emergence, age_rc = age_coef_rc,
              tmp_rc = temp_coef_rc, rh_rc = rh_coef_rc, base_rc = 0.61,
              latrans = 6, inftrans = 19, init_sites = 600, aggr = 1,
              site_max = 100000, rr_physiol_senesc = 0.01, rrg = 0.1, ...))
}

### Leaf blast -----------------------------------------------------------------
#' Predict leaf blast area under the disease progress curve (AUDPC)
#'
#' A dynamic mechanistic simulation of leaf blast disease of rice.
#' The model is driven by daily weather data. Adapted from cropsim package
#' version 0.2.0-5 by Adam H. Sparks - USQ CCH.
#' Original model development: Serge Savary & Rene Pangga (IRRI).
#' Original R implementation by Robert J. Hijmans, Rene Pangga, & Jorrel Aunario
#'(IRRI).
#'
#' @param wth - Weather data source
#' @param emergence - Expected date of crop emergence
#' @param ... Additional arguments - See \link[epiRice]{SEIR}
#'
#' @return A raster of AUDPC values for leaf blast based on given weather data
#'
#' @examples
#' wth <- readr::read_table(system.file("extdata", "daily_weather_28368.nasa",
#' package = "epiRice"), skip = 13)
#' lb <- leaf_blast(wth, onset = 20, duration = 120)
#' plot(lb, type = 2)
#'
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
#' @seealso \code{\link[epiRice]{sheath_blight}},
#' \code{\link[epiRice]{bacterial_blight}}, \code{\link[epiRice]{brown_spot}},
#' \code{\link[epiRice]{tungro}}
#'
#' @export
leaf_blast <- function(wth, emergence = "2000-05-15", ...) {
  age_coef_rc <- cbind(0:24 * 5, c(1, 1, 1, 0.9, 0.8, 0.7, 0.64, 0.59, 0.53,
                                   0.43, 0.32, 0.22, 0.16, 0.09, 0.03, 0.02,
                                   0.02, 0.02, 0.01, 0.01, 0.01, 0.01, 0.01,
                                   0.01, 0.01))
  temp_coef_rc <- cbind(2:9 * 5, c(0, 0.5, 1, 0.6, 0.2, 0.05, 0.01, 0))
  rh_coef_rc <- cbind(4 + (0:10) * 2,
                      c(0, 0.02, 0.09, 0.19, 0.29, 0.43, 0.54, 0.63, 0.77,
                        0.88, 1.0))
  return(SEIR(wth = wth, emergence = emergence,
              age_rc = age_coef_rc, tmp_rc = temp_coef_rc, rh_rc = rh_coef_rc,
              base_rc = 1.14, latrans = 5, inftrans = 20, init_sites = 600,
              aggr = 1, site_max = 30000, rr_physiol_senesc = 0.01,
              rrg = 0.1, ...))
}

### Bacterial blight -----------------------------------------------------------
#' Predict bacterial blight area under the disease progress curve (AUDPC)
#'
#' A dynamic mechanistic simulation of bacterial blight disease of rice.
#' The model is driven by daily weather data. Adapted from cropsim package
#' version 0.2.0-5 by Adam H. Sparks - USQ CCH.
#' Original model development: Serge Savary & Rene Pangga (IRRI).
#' Original R implementation by Robert J. Hijmans, Rene Pangga, & Jorrel Aunario
#'(IRRI).
#'
#' @param wth - Weather data source
#' @param emergence - Expected date of crop emergence
#' @param ... Additional arguments - See \link[epiRice]{SEIR}
#'
#' @return A raster of AUDPC values for bacterial blight based on given weather
#' data
#'
#' @examples
#' wth <- readr::read_table(system.file("extdata", "daily_weather_28368.nasa",
#' package = "epiRice"), skip = 13)
#' bb <- bacterial_blight(wth, onset = 20, duration = 120)
#' plot(bb, type = 2)
#'
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
#' @seealso \code{\link[epiRice]{leaf_blast}},
#' \code{\link[epiRice]{sheath_blight}}, \code{\link[epiRice]{brown_spot}},
#' \code{\link[epiRice]{tungro}}
#'
#' @export
bacterial_blight <- function(wth, emergence = "2000-05-15", ...) {
  age_coef_rc <- cbind(0:12 * 10, c( 1, 1, 1, 0.9, 0.62, 0.43, 0.41, 0.42, 0.41,
                                    0.41, 0.41, 0.41, 0.41))
  temp_coef_rc <- cbind(16 + (0:8 * 3), c(0, 0.29, 0.44, 0.90, 0.90, 1.0, 0.88,
                                          0.01, 0))
  rh_coef_rc <- cbind(c(2, 1:8 * 3), c(0, 0.67, 0.81, 0.84, 0.87, 0.91, 0.94,
                                      0.97, 1.0))
  return(SEIR(wth = wth, emergence = emergence,
              age_rc = age_coef_rc, tmp_rc = temp_coef_rc, rh_rc = rh_coef_rc,
              base_rc = 0.87, latrans = 5, inftrans = 30,
              site_max = 3200, aggr = 4, init_sites = 100,
              rr_physiol_senesc = 0.01, rrg = 0.1, ...))
}

### Sheath blight -----------------------------------------------------------
#' Predict sheath blight area under the disease progress curve (AUDPC)
#'
#' A dynamic mechanistic simulation of sheath blight disease of rice.
#' The model is driven by daily weather data. Adapted from cropsim package
#' version 0.2.0-5 by Adam H. Sparks - USQ CCH.
#' Original model development: Serge Savary & Rene Pangga (IRRI).
#' Original R implementation by Robert J. Hijmans, Rene Pangga, & Jorrel Aunario
#'(IRRI).
#'
#' @param wth - Weather data source
#' @param emergence - Expected date of crop emergence
#' @param ... Additional arguments - See \link[epiRice]{SEIR}
#'
#' @return A raster of AUDPC values for sheath blight based on given weather
#' data
#'
#' @examples
#' wth <- readr::read_table(system.file("extdata", "daily_weather_28368.nasa",
#' package = "epiRice"), skip = 13)
#' sb <- sheath_blight(wth, onset = 20, duration = 120)
#' plot(bb, type = 2)
#'
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario
#'
#' @references Savary, S., Castilla, N.P., Willocquet, L. 2001. Analysis of the
#' spatio-temporal structure of rice sheath blight epidemics in a farmer's
#' field. Plant Pathology 50:53-68.
#' @references Savary, S., Willocquet, L., Teng, P.S., 1997. Modelling sheath
#' blight epidemics on rice tillers. Agricultural Systems 55:359-384.
#' @references Castilla, N.P., Leano, R.M., Elazegui, F.A., Teng, P.S., Savary,
#' S., 1996. Effects of plant contacts, inoculation pattern, leaf wetness
#' regime, and nitrogen supply on inoculum efficiency in rice sheath blight.
#' Journal of Phytopathology 144:187-192.
#' @references Sharma, N.R., Teng, P.S., Olivares, F.M., 1990. Effect of rice
#' growth stage on sheath blight (ShB) development and yield loss. International
#' Rice Research Newsletter 15:19-20.
#' @references Hashiba, T. and Ijiri, T., 1989. Estimation of yield loss and
#' computerized forecasting system (BLIGHTAS) for rice sheath blight disease.
#' International Symposium on Tropical Agricultural Research: Crop losses due to
#' disease outbreaks in the tropics and countermeasures. Tropical Agricultural
#' Research Series (Japan) No. 22 pp. 163-171.
#' @references Tu, C.C., Chang, Y.C., Wang, C.W., 1979. Studies on the ecology
#' of \emph{Rhizoctonia solani}, the causal organism of rice sheath blight.
#' National Science Council Monthly, ROC 7:1208-1219.
#' @references Gross, M.K., Santini, J.B., Tikhonova, I. and Latin, R. 1998. The
#' influence of temperature and leaf wetness duration on infection of perennial
#' ryegrass by \emph{Rhizoctonia solani}. Plant Disease 82:1012-1016.
#'
#' @seealso \code{\link[epiRice]{leaf_blast}},
#' \code{\link[epiRice]{bacterial_blight}}, \code{\link[epiRice]{brown_spot}},
#' \code{\link[epiRice]{tungro}}
#'
#' @export
sheath_blight <- function(wth, emergence = "2000-05-15", ...) {
  age_coef_rc <- cbind(0:12 * 10, c(0.84, 0.84, 0.84, 0.84, 0.84, 0.84, 0.83,
                                    0.88, 0.88, 1.0, 1.0, 1.0, 1.0))
  rh_coef_rc <- cbind(c(8, 3:8 * 3), c(0, 0.24, 0.41, 0.68, 0.94, 0.97, 1.0))
  temp_coef_rc <- cbind(3:10 * 4, c(0, 0.42, 0.94, 0.94, 1.0, 0.85, 0.64, 0))
  return(SEIR(wth = wth, emergence = emergence,
              age_rc = age_coef_rc, tmp_rc = temp_coef_rc, rh_rc = rh_coef_rc,
              base_rc = 0.46, latrans = 3, inftrans = 120,
              site_max = 800, aggr = 2.8, init_sites = 25,
              rr_physiol_senesc = 0.005, rrg = 0.2, ...))
}
### Tungro----------------------------------------------------------------------
#' Predict tungro area under the disease progress curve (AUDPC)
#'
#' A dynamic mechanistic simulation of tungro disease of rice.
#' The model is driven by daily weather data. Adapted from cropsim package
#' version 0.2.0-5 by Adam H. Sparks - USQ CCH.
#' Original model development: Serge Savary & Rene Pangga (IRRI).
#' Original R implementation by Robert J. Hijmans, Rene Pangga, & Jorrel Aunario
#'(IRRI).
#'
#' @param wth - Weather data source
#' @param emergence - Expected date of crop emergence
#' @param ... Additional arguments - See \link[epiRice]{SEIR}
#'
#' @return A raster of AUDPC values for tungro based on given weather data
#'
#' @examples
#' wth <- readr::read_table(system.file("extdata", "daily_weather_28368.nasa",
#' package = "epiRice"), skip = 13)
#' tg <- tungro(wth, onset = 20, duration = 120)
#' plot(tg, type = 2)
#'
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario
#'
#' @references Tiongco, E.R., Cabunagan, R.C., Flores, Z.M., Hibino, H., and
#' Koganezawa, H., 1993. Serological monitoring of rice tungro disease
#' development in the field: its implication in disease management.
#' Plant Disease 77:877-882.
#' @references Rivera, C.T. and Ou, S.H., 1965. Leafhopper transmission of
#' tungro disease of rice. Plant Disease Reporter 49:127-131.
#' @references Ling, K.C., Palomar, M.K., 1966. Studies on rice plants infected
#' with the tungro virus at different ages. Philippines Agriculturist 50:165-177.
#' @references Ling, K.C., and Tiongco, E.R., Effect of temperature on the
#' transmission of rice tungro virus by \emph{Nephotettix virescens}.
#' Philippine Phytopathology 11:46-57.
#'
#' @seealso \code{\link[epiRice]{leaf_blast}},
#' \code{\link[epiRice]{bacterial_blight}}, \code{\link[epiRice]{brown_spot}},
#' \code{\link[epiRice]{sheath_blight}}
#'
#' @export
tungro <- function(wth, emergence = "2000-05-15", ...) {
  age_coef_rc <- cbind(0:8 * 15, c(1.0, 1.0, 0.98, 0.73, 0.51, 0.34, 0, 0, 0))
  temp_coef_rc <- cbind(c(9, 10 + (0:9 * 3.1111), 40),
                        c(0, 0.13, 0.65, 0.75, 0.83, 0.89, 0.93, 0.97, 1.0,
                          0.96, 0.93, 0))
  rh_coef_rc <- 1
  return(SEIR(wth = wth, emergence = emergence,
              age_rc = age_coef_rc, tmp_rc = temp_coef_rc, rh_rc = rh_coef_rc,
              base_rc = 0.18, latrans = 6, inftrans = 120, site_max = 100,
              aggr = 1, init_sites = 100, rr_physiol_senesc = 0.01,
              rrg = 0.1, ...))
}

#### SEIR ----------------------------------------------------------------------
#' Susceptible-Exposed-Infectious-Removed (SEIR) Model
#'
#' This function is used by specific disease models in EPIRICE to model disease
#' severity.
#'
#' @param wth - daily weather data frame containing relative humidity (relh),
#' minimum temperature (tmin), and maximum temperature (tmax)
#' @param emergence - expected date of plant emergence
#' @param onset -expected number of days until the onset of disease after
#' emergence date
#' @param duration - simulation duration
#' @param rhlim -threshold to decide whether leaves are wet or not (usually
#' 90\%)
#' @param rainlim - threshold to decide whether leaves are wet or not
#' @param wetness -simulate RHmax or rain threshold (0) or leaf wetness duration
#' (1)
#' @param init_sites -
#' @param init_infection -
#' @param age_rc -
#' @param tmp_rc -
#' @param rh_rc -
#' @param base_rc - corrected basic infection rate
#' @param latrans - latent period
#' @param inftrans - infectious period
#' @param site_max -
#' @param aggr -
#' @param rr_physiol_senesc -
#' @param rrg -
#' @param senesc_type -
#'
#' @usage SEIR(wth, emergence, onset = 15, duration = 120, rhlim = 90,
#' rainlim = 5, wetness = 0, initSites, initInfection = 1, ageRc, tmpRc, rhRc,
#' baseRc, latrans, inftrans, siteMax, AGGR, RRPhysiolSenesc, RRG,
#' SenescType = 1)
#'
#' @author Serge Savary, Ireneo Pangga, Robert Hijmans, Jorrel Khalil Aunario
#'
#' @details SEIR is called by the following specific disease models:
#' \code{\link[epiRice]{leaf_blast}}, \code{\link[epiRice]{bacterial_blight}},
#' \code{\link[epiRice]{brown_spot}}, \code{\link[epiRice]{sheath_blight}},
#' \code{\link[epiRice]{tungro}}

SEIR <- function(wth, emergence, onset = 15, duration = 120, rhlim = 90,
                 rainlim = 5, wetness = 0, init_sites, init_infection = 1,
                 age_rc, tmp_rc, rh_rc, base_rc, latrans, inftrans, site_max,
                 aggr, rr_physiol_senesc, rrg, senesc_type = 1) {
  infday <- NULL
  leaf_wet <- NULL

  emergence <- as.Date(emergence)
  wth@w <- subset(wth@w, wth@w$date >= emergence - 1)
  if (dim(wth@w)[1] < duration) {
    stop("Incomplete weather data")
    }
  wth@w <- wth@w[1:(duration + 1), ]

  if (wetness  ==  1) {
    W <- leaf_wet(wth, simple = TRUE)
  }

  # outputvars
  cofr <- rc <- RHCoef <- latency <- infectious <- severity <- rsenesced <-
    rgrowth <- rtransfer <- infection <- diseased <- senesced <- removed <-
    now_infectious <- now_latent <- sites <- total_sites <-
    rep(0, times = duration + 1)

  for (day in 0:duration) {
    # State calculations
    if (day == 0) {
      # start crop growth
      sites[day + 1] <- init_sites
      rsenesced[day + 1] <- rr_physiol_senesc * sites[day + 1]
    } else {
      if (day > inftrans) {
        removed_today <- infectious[infday + 2]
      } else {
        removed_today <- 0
      }

      sites[day + 1] <- sites[day] + rgrowth[day] - infection[day] -
        rsenesced[day]
      rsenesced[day + 1] <- removed_today * senesc_type +
        rr_physiol_senesc * sites[day + 1]
      senesced[day + 1] <- senesced[day] + rsenesced[day]

      latency[day + 1] <- infection[day]
      latday <- day - latrans + 1
      latday <- max(0, latday)
      now_latent[day + 1] <- sum(latency[latday:day + 1])

      infectious[day + 1] <- rtransfer[day]
      infday <- day - inftrans + 1
      infday <- max(0, infday)
      now_infectious[day + 1] <- sum(infectious[infday:day + 1])
    }

    if (sites[day + 1] < 0 ) {
      sites[day + 1] <- 0
      break
    }

    if (wetness == 0) {
      if (wth@w$rhmax[day + 1] == rhlim | wth@w$prec[day + 1] >= rainlim) {
        RHCoef[day + 1] <- 1
      }
    } else {
      RHCoef[day + 1] <- afgen(rh_rc, W[day + 1])
    }

    rc[day + 1] <- base_rc * afgen(age_rc, day) *
      afgen(tmp_rc, wth@w$tavg[day + 1]) * RHCoef[day + 1]
    diseased[day + 1] <- sum(infectious) +
      now_latent[day + 1] + removed[day + 1]
    removed[day + 1] <- sum(infectious) - now_infectious[day + 1]

    cofr[day + 1] <- 1 - (diseased[day + 1] /
                            (sites[day + 1] + diseased[day + 1]))

    if (day == onset) {
      # initialization of the disease
      infection[day + 1] <- init_infection
    } else if (day > onset) {
      infection[day + 1] <- now_infectious[day + 1] *
        rc[day + 1] * (cofr[day + 1] ^ aggr)
    } else {
      infection[day + 1] <- 0
    }

    if (day >=  latrans) {
      rtransfer[day + 1] <- latency[latday + 1]
    } else {
      rtransfer[day + 1] <- 0
    }

    total_sites[day + 1] <- diseased[day + 1] + sites[day + 1]
    rgrowth[day + 1] <- rrg * sites[day + 1] *
      (1 - (total_sites[day + 1] / site_max))
    severity[day + 1] <- (diseased[day + 1] - removed[day + 1]) /
      (total_sites[day + 1] - removed[day + 1]) * 100
  }

  res <- cbind(0:duration, sites, now_latent, now_infectious, removed,
               senesced, infection, rtransfer, rgrowth, rsenesced, diseased,
               severity)
  res <- as.data.frame(res[1:(day + 1), ])

  dates <- seq(emergence - 1, emergence + duration, 1)
  res <- cbind(dates[1:(day + 1)], res)
  colnames(res) <- c("date", "simday", "sites", "latent", "infectious",
                     "removed", "senesced", "rateinf", "rtransfer", "rgrowth",
                     "rsenesced", "diseased", "severity")

  result <- new("SEIR")
  result@d <- res
  return(result)
}
