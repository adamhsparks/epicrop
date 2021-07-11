library("furrr")
library("dplyr")
library("lubridate")
plan(multisession, workers = 4)
seed = NULL

years <- 2017:2021
seasons <- paste0(years, "-01-01")

seasons_wth <-
  future_map(
    .x = seasons,
    .f = get_wth,
    lonlat = c(-46.0490, -19.3108),
    duration = 120
  )

lb <- future_map(
  .x = seasons_wth,
  .f = ~ predict_leaf_blast(emergence = .x$YYYYMMDD[1],
                            wth = .x),
  .options = furrr_options(seed = NULL)
) %>%
  mutate(
    season = case_when(
      year(dates) == 2017 ~ 2017,
      year(dates) == 2018 ~ 2018,
      year(dates) == 2019 ~ 2019,
      year(dates) == 2020 ~ 2020,
      year(dates) == 2021 ~ 2021
    )
  )
