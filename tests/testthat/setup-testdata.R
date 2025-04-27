load(system.file(file = "wth_test_data.Rdata", package = "epicrop"))

wth_formatted <- format_wth(
  wth,
  date = "YYYYMMDD",
  temp = "T2M",
  rhum = "RH2M",
  rain = "PRECTOTCORR",
  tmin = "T2M_MIN",
  tmax = "T2M_MAX",
  lat = "LAT",
  lon = "LON"
)
