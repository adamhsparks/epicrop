library("furrr")
library("purrr")
library("dplyr")
library("ggplot2")
library("lubridate")
library("epicrop")

plan(multisession, workers = 4)

years <- 2016:2021
month_day <- c("-01-01", "-01-14", "-01-31")
emergence_dates <-
  cross2(years, month_day) %>%
  map_chr(paste0, collapse = "") %>%
  sort() %>%
  as_date()

# create a vector of dates for weather data.
# Only take the earliest establishment date, then extend the duration out past
# 120 days from final establishment date.
# Here just taking 180 days, 6 months, works well.
wth_start_dates <- paste0(years, month_day[1])

seasons_wth <-
  future_map_dfr(
    .x = wth_start_dates,
    .f = get_wth,
    lonlat = c(-49.264, -16.6869),
    duration = 180,
    .options = furrr_options(seed = NULL)
  ) %>%
  mutate(YYYYMMDD = as_date(YYYYMMDD))

# Once we have all the weather, create a list of weather data.frames for each
# establishment date
# Create time intervals to subset the weather data, creating a list new
# data.frames for each establishment date.

wth <-
  future_map(.x = emergence_dates,
             .f = ~ subset(seasons_wth, YYYYMMDD >= .x &
                             YYYYMMDD <= .x + 120))

names(wth) <- emergence_dates

lb <- future_map2(
  .x = wth,
  .y = emergence_dates,
  .f = ~ predict_leaf_blast(emergence = .y,
                            wth = .x),
  .options = furrr_options(seed = NULL)
)

# check diseased sites
lb %>%
  bind_rows(.id = "emergence") %>%
  ggplot(aes(x = simday, y = diseased, group = 1)) +
  geom_line() +
  ylab("Diseased sites (n)") +
  xlab("Simulation day") +
  facet_wrap(. ~ emergence, ncol = 3) +
  theme_classic()

# check AUDPC
lb %>%
  bind_rows(.id = "emergence") %>%
  distinct(emergence, AUDPC) %>%
  ggplot(aes(x = as.factor(emergence), y = AUDPC)) +
  geom_col(width = 0.5,
           aes(fill = as.factor(year(emergence)))) +
  theme_classic() +
  coord_flip()
