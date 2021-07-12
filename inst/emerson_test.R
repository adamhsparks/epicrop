library("furrr")
library("purrr")
library("dplyr")
library("ggplot2")
library("lubridate")
library("epicrop")

plan(multisession, workers = 4)

years <- 2017:2021
month_day <- c("-01-01", "-01-14", "-01-31")
emergence_dates <-
  cross2(years, month_day) %>%
  map_chr(paste0, collapse = "") %>%
  sort()

# create a vector of dates for weather data.
wth_start_dates <- paste0(years, month_day[1])

seasons_wth <-
  future_map(
    .x = wth_start_dates,
    .f = get_wth,
    lonlat = c(-46.0490, -19.3108),
    duration = 180, # grab extra days
    .options = furrr_options(seed = NULL)
  )

future_map_dfr(.x = emergence_dates,
               ~ map2_dfr(.x,
                          .y = seasons_wth,
                          .f = ~ predict_leaf_blast(emergence = .x,
                                                    wth = .y)),
               .options = furrr_options(seed = NULL))

lb <- future_map2_dfr(
  .x = seasons_wth,
  .y = emergence_dates,
  .f = ~ predict_leaf_blast(emergence = .y,
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

ggplot(lb, aes(x = simday, y = diseased, group = 1)) +
  geom_line() +
  geom_line(aes(y = rhum * 10)) +
  ylab("Number of sites") +
  facet_wrap(. ~ season)
