# Shared test fixtures used across test-run_epicrop_model.R,
# test-fetch_epicrop_weather_list.R, and any other file that needs a
# lightweight stand-in for a real epicrop disease model or a mocked
# get_wth(). testthat auto-sources every helper-*.R file before running
# tests, so these are available everywhere without an explicit source().

# A lightweight stand-in for a real epicrop disease model (*e.g.*
# bacterial_blight()). Mimics seir()'s real contract: errors if the weather
# slice it receives has fewer rows than `duration`, rather than silently
# truncating.
mock_model <- function(
  wth,
  emergence,
  duration = 30L,
  RcOpt = 1,
  fail = FALSE,
  ...
) {
  if (fail) {
    stop("mock model failure")
  }

  i0 <- which(wth$YYYYMMDD == data.table::as.IDate(emergence))
  if (!length(i0)) {
    cli::cli_abort("Emergence date not found in weather data")
  }
  if ((nrow(wth) - i0 + 1L) < duration) {
    cli::cli_abort(
      "Insufficient weather data to run {duration} days after emergence"
    )
  }

  w <- wth[i0:(i0 + duration - 1L)]
  out <- data.table::data.table(
    simday = seq_len(duration),
    dates = w$YYYYMMDD,
    intensity = seq(0, 0.5, length.out = duration),
    RcOpt = RcOpt
  )
  data.table::setattr(out, "AUDPC", sum(out$intensity))
  data.table::setattr(out, "class", c("epicrop.sim", class(out)))
  out[]
}

.mk_year_wth <- function(year, temp = 25) {
  dates <- seq(
    as.Date(sprintf("%d-01-01", year)),
    as.Date(sprintf("%d-12-31", year)),
    by = "1 day"
  )
  dt <- data.table::data.table(
    YYYYMMDD = data.table::as.IDate(dates),
    TEMP = temp,
    RAIN = 2,
    RHUM = 80,
    TMIN = temp - 5,
    TMAX = temp + 5
  )
  class(dt) <- c("epicrop.wth", "data.table", "data.frame")
  dt
}

.mk_wth_list <- function(years, ...) {
  wl <- stats::setNames(lapply(years, .mk_year_wth, ...), as.character(years))
  class(wl) <- c("epicrop.wth.list", class(wl))
  wl
}

.mk_emergence <- function(dates) {
  d <- data.table::as.IDate(dates)
  class(d) <- union("epicrop.emerge", class(d))
  d
}

# A fake get_wth() replacement for mocking network calls in
# fetch_epicrop_weather_list() tests. Returns exactly `duration` consecutive
# days of synthetic weather starting at `dates`.
.fake_get_wth <- function(lonlat, dates, duration = NULL, source = "power") {
  start <- data.table::as.IDate(dates)
  dt <- data.table::data.table(
    YYYYMMDD = start + seq_len(duration) - 1L,
    TEMP = 25,
    RAIN = 1,
    RHUM = 80,
    TMIN = 20,
    TMAX = 30
  )
  class(dt) <- c("epicrop.wth", class(dt))
  dt
}
