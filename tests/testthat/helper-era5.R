.era5_json <- function(fixture) {
  arr <- function(x) {
    paste0("[", paste(ifelse(is.na(x), "null", x), collapse = ","), "]")
  }

  switch(
    fixture,
    "era5-ok.json" = {
      times <- paste0(
        rep(c("2000-06-30", "2000-07-01", "2000-07-02"), each = 24),
        "T",
        sprintf("%02d", rep(0:23, 3)),
        ":00"
      )

      temp <- c(10:33, rep(20, 24), rep(NA, 24))
      dew <- c(10:33, rep(10, 24), rep(NA, 24))
      rain <- c(rep(0, 20), 1, 2, 3, 4, rep(0, 48))

      sprintf(
        '{"latitude":14.1875,"longitude":121.25,"hourly":{"time":%s,"temperature_2m":%s,"dewpoint_2m":%s,"precipitation":%s}}',
        paste0("[\"", paste(times, collapse = "\",\""), "\"]"),
        arr(temp),
        arr(dew),
        arr(rain)
      )
    },
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
