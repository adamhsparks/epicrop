## ----setup, include = FALSE---------------------------------------------------
suggested_packages <-
  c(
    "ggplot2",
    "lubridate",
    "dplyr",
    "purrr",
    "furrr",
    "microbenchmark",
    "pander"
  )

knitr::opts_chunk$set(eval = all(
  vapply(
    suggested_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = FALSE
  )
))

knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 7
)


## ----seasons-list-------------------------------------------------------------
years <- 2000:2001
seasons <- paste0(years, "-06-30")


## ----get-seasons--------------------------------------------------------------
library(epicrop)
library(purrr)

seasons_wth <-
  map(
    .x = seasons,
    .f = get_wth,
    lonlat = c(121.255669, 14.16742),
    duration = 120
  )


## ----run-predict, message=FALSE-----------------------------------------------
library(dplyr)
library(lubridate)
library(pander)

seasons <- map_df(
  .x = seasons_wth,
  .f = ~ bacterial_blight(emergence = .x$YYYYMMDD[1],
                                  wth = .x)
) |>
  mutate(season = case_when(year(dates) == 2000 ~ 2000,
                            year(dates) == 2001 ~ 2001)) |>
  group_by(season) |>
  relocate(season, AUDPC) |>
  distinct(AUDPC, season)

pander(seasons)


## ----graph_seasons, fig.cap="Bacterial blight area under the disease progress curve (AUDPC) for the 2000 and 2001 wet seasons at the IRRI Zeigler Experiment Station in Calabarzon, Philippines."----
library(ggplot2)
ggplot(seasons,
       aes(y = as.factor(season),
           x = AUDPC)) +
  geom_col(width = 0.5,
           orientation = "y") +
  ylab("Season") +
  theme_classic()


## ----parallel-seasons---------------------------------------------------------
library(epicrop)
library(furrr)
library(dplyr)
library(lubridate)
library(pander)

# Respect the API server and use 1 core
plan(multisession, workers = 1)

years <- 2000:2001
seasons <- paste0(years, "-06-30")
seasons_wth <-
  future_map(
    .x = seasons,
    .f = get_wth,
    lonlat = c(121.255669, 14.16742),
    duration = 120,
    .options = furrr_options(seed = NULL)
  )

# for local processing use 2 cores, this will assign one to each year, if you
# have more years and more cores, e.g., an HPC, explore using more resources

plan(multisession, workers = 2)

# run bacterial_blight() over the data frames of weather
future_map_dfr(
  .x = seasons_wth,
  .f = ~ bacterial_blight(emergence = .x$YYYYMMDD[1],
                                  wth = .x),
  .options = furrr_options(seed = NULL)
) |>
  mutate(season = case_when(year(dates) == 2000 ~ 2000,
                            year(dates) == 2001 ~ 2001)) |>
  group_by(season) |>
  relocate(season, AUDPC) |>
  distinct(AUDPC, season) |>
  pander()


## ----multiple-establishment---------------------------------------------------
library(epicrop)
library(purrr)
library(dplyr)
library(lubridate)
library(pander)
library(furrr)
library(tidyr)

years <- as.character(2001:2010)
month_day <- c("-06-01", "-06-14", "-06-30")
emergence_dates <-
  expand_grid(years, month_day) |>
  unite("emergence_dates", years:month_day, sep = "") |>
  pull(emergence_dates) |>
  sort() |>
  as_date()

# Respect the API server and use 1 core
plan(multisession, workers = 1)

# create a vector of dates for weather data.
# Only take the earliest establishment date, then extend the duration out past
# 120 days from final establishment date.
# Here just taking 180 days, 6 months, works well.
wth_start_dates <- paste0(years, month_day[1])

seasons_wth <- map_dfr(
  .x = wth_start_dates,
  .f = get_wth,
  lonlat = c(121.255669, 14.16742),
  duration = 180
) |>
  mutate(YYYYMMDD = as_date(YYYYMMDD))

# Now use 4 cores for the local processing
plan(multisession, workers = 4)

# Once we have all the weather, create a list of weather data.frames for each
# establishment date
# Create time intervals to subset the weather data, creating a list new
# data.frames for each establishment date.

wth <-
  map(.x = emergence_dates,
      .f = ~ subset(seasons_wth, YYYYMMDD >= .x &
                      YYYYMMDD <= .x + 120)) |>
  setNames(emergence_dates)

multi_dates <- map2(
  .x = wth,
  .y = emergence_dates,
  .f = ~ bacterial_blight(emergence = .y,
                                  wth = .x)
) |>
  bind_rows(.id = "emergence") |>
  group_by(emergence) |>
  relocate(emergence, AUDPC) |>
  distinct(AUDPC, emergence)

pander(multi_dates)


## ----stem-plot, fig.cap="Rice bacterial blight area under the disease progress curve (AUDPC) values for three establishment dates over ten years at the IRRI Zeigler Experiment Station in Calabarzon, Philippines."----
library(ggplot2)
ggplot(data = multi_dates,
       aes(
         x = AUDPC,
         xend = 0,
         y = as.factor(emergence),
         yend = as.factor(emergence)
       )) +
  geom_segment() +
  geom_point() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(limits = rev) +
  ylab("Establishment Date") +
  theme_classic()


## ----parallel-establishment, fig.cap="Rice bacterial blight area under the disease progress curve (AUDPC) values for three establishment dates over ten years at the IRRI Zeigler Experiment Station in Calabarzon, Philippines."----
library(furrr)
library(purrr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(epicrop)

plan(multisession, workers = 1)

years <- 2001:2010
month_day <- c("06-01", "06-14", "06-30")
start_dates <- list(years, month_day, sep = "-")

emergence_dates <-
  tidyr::expand_grid(!!!start_dates) |>
  pmap_chr(paste) |>
  sort() |>
  as_date()


## ----get-weather-for-parallel-establishment-----------------------------------
wth_start_dates <- paste(years, month_day[1], sep = "-")

seasons_wth <-
  future_map_dfr(
    .x = wth_start_dates,
    .f = get_wth,
    lonlat = c(121.255669, 14.16742),
    duration = 190,
    .options = furrr_options(seed = NULL)
  ) |>
  mutate(YYYYMMDD = as_date(YYYYMMDD))


## ----create-data.frames-parallel----------------------------------------------
# Now use 4 cores for the local processing
plan(multisession, workers = 4)

wth <-
  future_map(.x = emergence_dates,
             .y = seasons_wth,
             .f = ~ subset(.y, YYYYMMDD >= .x &
                          YYYYMMDD <= .x + 120)) |>
  setNames(emergence_dates)

future_map2(
  .x = wth,
  .y = emergence_dates,
  .f = ~ bacterial_blight(emergence = .y,
                                  wth = .x),
  .options = furrr_options(seed = NULL)
) |>
  bind_rows(.id = "emergence") |>
  group_by(emergence) |>
  relocate(emergence, AUDPC) |>
  distinct(AUDPC, emergence) |>
  ggplot(aes(
    x = AUDPC,
    xend = 0,
    y = as.factor(emergence),
    yend = as.factor(emergence)
  )) +
  geom_segment() +
  geom_point() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(limits = rev) +
  ylab("Establishment Date") +
  theme_classic()


## ----locations-list-----------------------------------------------------------
locations <- list(
  "Mekong River" = c(105.5943, 10.0634),
  "Red River" = c(105.9700, 20.9034),
  "Prachin Buri" = c(101.6601, 14.0421),
  "Chennai" = c(80.2707, 13.0827),
  "Neuva Ecija" = c(121.1113, 15.5784)
)


## ----get-locations------------------------------------------------------------
library(epicrop)
library(purrr)

locations_wth <-
  map(
    .x = locations,
    .f = get_wth,
    dates = "2000-06-30",
    duration = 120
  )


## ----run-multi-loc, fig.cap="Rice bacterial blight area under the disease progress curve (AUDPC) values for an establishment date of June 30, 2000 at five locations, Mekong River Delta and Red River Delta in Vietnam; Prachin Buri, Thailand; Chennai, India and Nueva Ecija, in the Philippines."----
library(dplyr)
library(lubridate)
library(ggplot2)

map(
  .x = locations_wth,
  .f = ~ bacterial_blight(emergence = .x$YYYYMMDD[1],
                                  wth = .x)
) |>
  bind_rows(.id = "location") |>
  group_by(location) |>
  relocate(location, AUDPC) |>
  distinct(AUDPC, location) |>
  ggplot(aes(
    x = AUDPC,
    xend = 0,
    y = as.factor(location),
    yend = as.factor(location)
  )) +
  geom_segment() +
  geom_point() +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_y_discrete(limits = rev) +
  ylab("Location") +
  theme_classic()


## ----benchmark, message=FALSE, warning=FALSE, fig.cap="Microbenchmark results comparing sequential and parallel processing methods simulating three different establishment dates over ten years at the IRRI Zeigler Experiment Station in Calabarzon, Philippines"----

library(microbenchmark)

plan(multisession, workers = 4)

mbm <- microbenchmark(
  sequential = map2(
    .x = wth,
    .y = emergence_dates,
    .f = ~ bacterial_blight(emergence = .y,
                                    wth = .x)
  ) |>
    bind_rows(.id = "emergence"),
  parallel = future_map2(
    .x = wth,
    .y = emergence_dates,
    .f = ~ bacterial_blight(emergence = .y,
                                    wth = .x),
    .options = furrr_options(seed = NULL)
  ) |>
    bind_rows(.id = "emergence"),
  times = 100L
)

mbm

autoplot(mbm) +
  theme_classic()

