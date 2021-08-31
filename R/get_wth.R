
#' Get weather data from NASA POWER API for use in epicrop modelling
#'
#' This function is a wrapper for the [nasapower::get_power()] or
#'  [chirps::get_chirps()]/[chirps::get_chirts()] functions with predefined
#'  parameters suitable for use in \pkg{epicrop}.
#'
#' @param lonlat A numeric vector of geographic coordinates for a cell or region
#'  entered as x, y coordinates.
#' @param dates A character vector of start and end dates in that order.
#' @param duration A numeric value indicating how many days a single
#'  growing season being simulated is to be.  If this is used, only the start
#'  date will be used and the end date will be ignored if supplied.  This must
#'  match the `duration` parameter value passed along to [SEIR()] or any of the
#'  `predict` family of functions.
#' @param source A character string value that denotes whether to use the
#'  \acronym{NASA} \acronym{POWER} data (`nasapower`), the default, or
#'  \acronym{CHIRPS}/\acronym{CHIRTS} data, `chirps`, for rainfall,
#'  temperature and relative humidity from the Climate Hazards Data center at
#'  UC Santa Barbara, \url{https://www.chc.ucsb.edu/data}.  See parameter
#'  details for more discussion.
#'
#' @return A [data.table::data.table()] of weather data, dates and
#'  geolocation information (LAT/LON values) suitable for use in \pkg{epicrop}
#'  with the following columns:
#'
#'   **Field Name** | **Value**
#'   --------------:|:----------
#'   _YYYYMMDD_ | Date as Year Month Day (ISO8601)
#'   _DOY_      | Consecutive day of year, commonly called "Julian date"
#'   _TEMP_     | Mean daily temperature (°C)
#'   _RHUM_     | Mean daily temperature (°C)
#'   _RAIN_     | Mean daily rainfall (mm)
#'   _LAT_      | Latitude of area of interest
#'   _LON_      | Longitude of area of interest
#'
#' @details # _source_ - Select your weather data source of choice
#' Access to three databases is provided by this function through the use of
#' two possible parameter values, `nasapower` or `chirps`.
#'
#' `nasapower`: the default database is the \acronym{NASA} \acronym{POWER}
#' database, \url{https://power.larc.nasa.gov}, which can supply all parameters
#' required for \pkg{epicrop} to run.  The global data are available from Jan.
#' 1, 1983 to near-present at a 0.5 x 0.625 arc-degree resolution.  Using this
#' will request and return all values necessary to use \pkg{epicrop}.
#'
#' `chirps`: \acronym{CHIRPS} is quasi-global (50°S – 50°N) high-resolution
#' (0.05 arc-degrees) rainfall data set, which incorporates satellite imagery
#' and in-situ station data to create gridded rainfall time series for trend
#' analysis and seasonal drought monitoring. \acronym{CHIRTS} is a quasi-global
#' (60°S – 70°N), high-resolution data set of daily maximum and minimum
#' temperatures.  For more details on \acronym{CHIRPS} and \acronym{CHIRTS} data
#' please visit the official homepage \url{https://chc.ucsb.edu/data}.
#'
#' By using `chirps`, the function will return the precipitation, temperature
#' and relative humidity from the \acronym{CHIRPS} and \acronym{CHIRTS}
#' data in a single [data.table] for you.
#'
#' The \acronym{POWER} database covers a longer period of time but the spatial
#' resolution is not as small.  However, it is faster to respond than the
#' \acronym{CHIRPS} and \acronym{CHIRTS} data.  Differences in the data
#' are likely to be minimal and have little effect on the \pkg{epicrop} models.
#'
#' @examplesIf interactive()
#'
#' # get weather for IRRI Zeigler Experiment Station in wet season 2000 from the
#' # default NASA POWER data.
#' power <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = c("2000-06-30", "2000-12-31")
#' )
#'
#' # get 120 days of weather for IRRI Zeigler Experiment Station in wet season
#' # 2000 by specifying the duration but not the end-date and specifying to use
#' # POWER data.
#' power <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = "2000-06-30",
#'   duration = 120,
#'   source = "nasapower"
#' )
#'
#' # get the same data from CHIRPS and CHIRTS data
#' chirps <- get_wth(
#'   lonlat = c(121.25562, 14.6774),
#'   dates = "2000-06-30",
#'   duration = 120,
#'   source = "chirps"
#' )
#'
#' @author Adam H. Sparks
#' @export get_wth
#' @importFrom data.table setDT
#' @importFrom data.table setkey
#' @importFrom data.table setnames
#' @importFrom data.table setcolorder
#' @importFrom data.table .I

get_wth <- function(lonlat,
                    dates,
                    duration,
                    source = "nasapower") {
  if (!missing(duration)) {
    dates[2] <-
      as.character(as.Date(as.Date(dates[1]) + (duration)))
  }
  if (source == "nasapower") {
    wth <- setDT(
      nasapower::get_power(
        lonlat = lonlat,
        dates = dates,
        community = "AG",
        pars = c("T2M",
                 "RH2M",
                 "PRECTOTCORR"),
        temporal_api = "DAILY"
      )
    )
    wth[, c("YEAR", "MM", "DD") := NULL][]
    setnames(
      wth,
      old = c("T2M",
              "PRECTOTCORR",
              "RH2M"),
      new = c("TEMP",
              "RAIN",
              "RHUM")
    )
    setcolorder(wth,
                c("YYYYMMDD",
                  "DOY",
                  "TEMP",
                  "RHUM",
                  "RAIN",
                  "LAT",
                  "LON"))
    .check_na(.wth = wth)
    return(wth)
  } else if (source == "chirps") {
    # CHIRPS ----
    names(lonlat) <- c("lon", "lat")
    lonlat <- data.frame(as.list(lonlat))
    chirps <-
      setDT(
        suppressMessages(chirps::get_chirps(object = lonlat,
                                            dates = dates,
                                            server = "ClimateSERV"))
        )
    chirps[, id := .I]

    # CHIRTS ----
    j <- vector(mode = "list", length = 3)
    names(j) <- c("Tmin", "Tmax", "RHum")

    for (i in names(j)) {
      j[[i]] <- setDT(chirps::get_chirts(
        object = lonlat,
        dates = dates,
        var = i
      ))
      setnames(j[[i]], "chirts", i)
    }

    chirts <- Reduce(function(...)
      merge(..., all = TRUE), j)
    chirts[, TEMP := round(rowMeans(chirts[, c("Tmin", "Tmax")]), 2)]
    chirts[, c("Tmin", "Tmax") := NULL]
    chirts[, id := .I]

    wth <- merge(chirps, chirts, by = c("id", "lon", "lat", "date"))
    wth$DOY <- format(wth$date, "%j")
    setnames(
      wth,
      old = c("lon",
              "lat",
              "date",
              "chirps",
              "RHum"),
      new = c("LON",
              "LAT",
              "YYYYMMDD",
              "RAIN",
              "RHUM")
    )
    wth[, id := NULL]
    setcolorder(wth,
                c("YYYYMMDD",
                  "DOY",
                  "TEMP",
                  "RHUM",
                  "RAIN",
                  "LAT",
                  "LON"))

    .check_na(.wth = wth)
    return(wth)
  }
}

#' Check POWER data for any missing values
#' @param .wth A data.frame object from [nasapower::get_power()] with weather
#'  data for checking
#' Checks the data returned from the POWER dataset and informs the user if
#' missing values are found. It does not do anything other than emitting am
#' message about missing data.
#' @return NULL
#' @example .check_na(.wth)
#' @noRd

.check_na <- function(.wth) {
  if (anyNA(.wth[, c("TEMP", "RHUM", "RAIN")])) {
    message(
      "The weather data have missing values in your request.\n",
      "This can cause errors when running the model.\n",
      "You should inspect the weather data carefully and either fill missing\n",
      "or try again to download to see if that provides a complete set of data."
    )
    return(NULL)
  }
}
