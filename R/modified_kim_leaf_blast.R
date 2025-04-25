#' Predict Rice Leaf Blast Intensity Using Modified Kim Model
#'
#' A dynamic mechanistic simulation of leaf blast disease of rice, causal agent
#' _Magnaporthe oryzae_. The model is driven by daily weather data, which can
#' easily be accessed using[get_wth] to download weather data from
#' \acronym{NASA} \acronym{POWER}.
#'
#' @details
#' The model represents site size as 44
#'  \ifelse{html}{\out{mm<sup>2</sup>}}{\eqn{mm^2}} of a rice plant's leaf.
#'
#' @note This model is a modified version of the Kim *et al.* (2015) model,
#' which
#'
#' If the `wth` object provides _LAT_ and _LON_ columns, these will be included
#' in the output for mapping purposes. Both values must be present. These
#' columns are provided by default when using [get_wth].
#'
#' The optimum temperature for leaf blast as presented in Table 2 of
#' Savary _et al._ 2012 has a typo. The optimal value should be 20 °C, not
#' 25 °C as shown. The correct value, 20 °C, is used in this implementation.
#'
#' @inherit bacterial_blight
#'
#' @returns A [data.table::data.table] of disease intensity and infection
#' sites. See [seir] for a full description of the column values.
#'
#' @examplesIf interactive()
#'
#' # get weather for IRRI Zeigler Experiment Station in wet season 2000
#' wth <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#' lb <- leaf_blast(wth, emergence = "2000-07-01")
#' plot(x = lb$dates, y = lb$intensity, type = "l")
#'
#' @author Kwang-Hyung Kim, Serge Savary, Ireneo Pangga, Robert Hijmans,
#' Jorrel Khalil Aunario, and Adam H. Sparks
#'
#' @references Kim, K. H., Cho, J., Lee, Y. H., & Lee, W. S. (2015). Predicting
#' potential epidemics of rice leaf blast and sheath blight in South Korea under
#' the RCP 4.5 and RCP 8.5 climate change scenarios using a rice disease
#' epidemiology  model, EPIRICE. Agricultural and forest meteorology, 203,
#' 191-207. ' \doi{10.1016/j.agrformet.2015.01.011}

#' El Refaei, M.I., 1977. Epidemiology of rice blast disease in the tropics with
#' special reference to the leaf wetness in relation to disease  development.
#' PhD Thesis, Indian Agricultural Research Institute, New Delhi, India. 195 p.
#'
#' Hemmi, T., Abe, T., Ikaya, J., and Inoue, Y. 1936. Studies on the rice blast
#' disease. IV. Relation of the environment to the development of blast disease
#' and physiologic specialization in the rice blast fungus. Materials for Rural
#' Improvement, Department of Agriculture and Forestry, Japan No. 105, 145p.
#'
#' Hwang, B.K., Koh, Y.J., Chung, H.S., 1987. Effects of adult-plant resistance
#' on blast severity and yield of rice. Plant Disease 71:1035-1038. DOI:
#' \doi{10.1094/PD-71-1035}.
#'
#' Kato, H. and Kozaka, T., 1974. Effect of temperature on lesion enlargement
#' and sporulation of _Pyricularia oryzae_ in rice leaves. Phytopathology
#' 64:828-830. DOI: \doi{10.1094/Phyto-64-828}.
#'
#' Wei-Hong, L. 1996. Simulation and measurement of leaf wetness formation in
#' paddy rice crops. PhD Thesis, Wageningen Agricultural University, 87 p.
#'
#' Savary, S., Nelson, A., Willocquet, L., Pangga, I., and Aunario, J.
#' Modeling and mapping potential epidemics of rice diseases globally. Crop
#' Protection, Volume 34, 2012, Pages 6-17, ISSN 0261-2194 DOI:
#' \doi{10.1016/j.cropro.2011.11.009}.
#'
#' Torres, C.Q., 1986. Effect of plant age on the expression of resistance to
#' _Pyricularia oryzae_ Cav. in upland rice varieties. PhD Thesis, University of
#' the Philippines at Los Baños, 82 p.
#'
#' @family predict functions
#'
#' @seealso
#' [seir]
#'
#' @export
modified_kim_leaf_blast <- function(wth, emergence) {
  seir(
    wth = wth,
    emergence = emergence,
    onset = 15L,
    duration = 120L,
    rhlim = 90L,
    rainlim = 5L,
    H0 = 600L,
    I0 = 1L,
    RcA = cbind(
      c(
        0L,
        5L,
        10L,
        15L,
        20L,
        25L,
        30L,
        35L,
        40L,
        45L,
        50L,
        55L,
        60L,
        65L,
        70L,
        75L,
        80L,
        85L,
        90L,
        95L,
        100L,
        105L,
        110L,
        115L,
        120L
      ),
      c(
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        1,
        0.4,
        0.3,
        0.1,
        0.1,
        0.05,
        0.03,
        0.03,
        0.02,
        0.02,
        0.02,
        0.01,
        0.01,
        0.01,
        0.01,
        0.01,
        0.01
      )
    ),
    RcT = cbind(
      c(9L, 12L, 15L, 18L, 21L, 24L, 27L, 30L, 33L, 36L, 39L),
      c(0, 0.4, 0.72, 0.88, 1, 0.9, 0.45, 0.2, 0.05, 0.01, 0)
    ),
    RcW = cbind(
      c(4L, 6L, 8L, 10L, 12L, 14L, 16L, 18L, 20L, 22L, 24L),
      c(0, 0.24, 0.41, 0.68, 0.94, 0.97, 1, 1, 1, 1, 1)
    ),
    RcOpt = 0.86,
    p = 4L,
    i = 20L,
    a = 1L,
    Sx = 90000L,
    RRS = 0.005,
    RRG = 0.1
  )
}
