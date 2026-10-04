# Hourly ERA5 JSON for 2000-06-30 to 2000-07-02 (3 x 24 hourly values) in the
# shape returned by the Open-Meteo Archive API. NA values are written as JSON
# null.
.era5_hourly_json <- function(temp, rh, rain) {
  arr <- function(x) {
    paste0("[", paste(ifelse(is.na(x), "null", x), collapse = ","), "]")
  }

  times <- paste0(
    rep(c("2000-06-30", "2000-07-01", "2000-07-02"), each = 24),
    "T",
    sprintf("%02d", rep(0:23, 3)),
    ":00"
  )

  sprintf(
    '{"latitude":14.1875,"longitude":121.25,"hourly":{"time":%s,"temperature_2m":%s,"relative_humidity_2m":%s,"precipitation":%s}}',
    paste0("[\"", paste(times, collapse = "\",\""), "\"]"),
    arr(temp),
    arr(rh),
    arr(rain)
  )
}

.era5_json <- function(fixture) {
  switch(
    fixture,
    # day 1: temp 10:33 (mean 21.5, min 10, max 33), RH 80, rain total 10 mm
    # day 2: temp 20, RH 60, no rain
    # day 3: everything missing
    "era5-ok.json" = .era5_hourly_json(
      temp = c(10:33, rep(20, 24), rep(NA, 24)),
      rh = c(rep(80, 24), rep(60, 24), rep(NA, 24)),
      rain = c(rep(0, 20), 1, 2, 3, 4, rep(0, 24), rep(NA, 24))
    ),
    # a whole variable returned as all-null
    "era5-no-temp.json" = .era5_hourly_json(
      temp = rep(NA, 72),
      rh = rep(80, 72),
      rain = rep(0, 72)
    ),
    "era5-no-rh.json" = .era5_hourly_json(
      temp = rep(20, 72),
      rh = rep(NA, 72),
      rain = rep(0, 72)
    ),
    "era5-no-precip.json" = .era5_hourly_json(
      temp = rep(20, 72),
      rh = rep(80, 72),
      rain = rep(NA, 72)
    ),
    "era5-error.json" = '{"error":true,"reason":"Latitude must be in range of -90 to 90."}',
    "era5-no-hourly.json" = '{"latitude":14.1875,"longitude":121.25}',
    stop("Unknown ERA5 fixture: ", fixture, call. = FALSE)
  )
}

era5_response <- function(fixture, status = 200L) {
  httr2::response(
    status_code = status,
    headers = list("Content-Type" = "application/json"),
    body = charToRaw(.era5_json(fixture))
  )
}
