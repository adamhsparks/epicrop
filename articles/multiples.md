# Multiple Simulations

You may wish to compare the effects of different establishment dates,
different seasons or different locations on disease levels. This
vignette details how you can automate several runs of {epicrop} in R and
visualise them.

### Fetching NASA POWER data for multiple seasons

Start by creating a list of the dates representing the seasons that you
want to simulate. In this case, since we specify the duration of the
season, there is no need to explicitly state the end-date, that will
automatically be determined by
[`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md)
with the `duration` argument that we pass along in the
[`purrr::map()`](https://purrr.tidyverse.org/reference/map.html). You
can use as many start dates, representing as many seasons as you desire,
but for the purposes of speed in this vignette, only two will be used.

Use
[`build_epicrop_emergence()`](https://adamhsparks.codeberg.page/epicrop/reference/build_epicrop_emergence.md)
and
[`fetch_epicrop_weather_list()`](https://adamhsparks.codeberg.page/epicrop/reference/fetch_epicrop_weather_list.md)
to fetch the weather data from the [POWER
API](https://power.larc.nasa.gov). In this example we will use the IRRI
Zeigler Experiment Station coordinates as shown in the example for
[`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md).

``` r

library(epicrop)

years <- 2000:2001
emergence_date <- "06-30"
duration <- 120L

ymd <- build_epicrop_emergence(years = years, month_day = emergence_date)
```

A helper function,
[`fetch_epicrop_weather_list()`](https://adamhsparks.codeberg.page/epicrop/reference/fetch_epicrop_weather_list.md),
is provided to simplify fetching multiple years of weather data from
NASA POWER that uses very similar arguments to
[`get_wth()`](https://adamhsparks.codeberg.page/epicrop/reference/get_wth.md).
Note that since we only need the starting date as we have supplied
`duration`, that the argument is rather `start_date`, a single value,
not `dates`. The function uses the `duration` value to fetch the
necessary weather data for all years and start dates inclusive of the
end of the last season of the last start date.

``` r

wth_list <- fetch_epicrop_weather_list(
  years = years,
  lonlat = c(121.255669, 14.16742),
  start_date = emergence_date,
  duration = duration
)
#> Warning in `[.data.table`(tab, , `:=`(year, data.table::year(YYYYMMDD))): A
#> shallow copy of this data.table was taken so that := can add or remove 1
#> columns by reference. At an earlier point, this data.table was copied by R (or
#> was created manually using structure() or similar). Avoid names<- and attr<-
#> which in R currently (and oddly) may copy the whole data.table. Use set* syntax
#> instead to avoid copying: ?set, ?setnames and ?setattr. It's also not unusual
#> for data.table-agnostic packages to produce tables affected by this issue. If
#> this message doesn't help, please report your use case to the data.table issue
#> tracker so the root cause can be fixed or this message improved.
```

### Using a Helper Function to Model Several Seasons

Now that we have a list of weather data for two seasons we will use
[`run_epicrop_model()`](https://adamhsparks.codeberg.page/epicrop/reference/run_epicrop_model.md)
now to run
[`bacterial_blight()`](https://adamhsparks.codeberg.page/epicrop/reference/disease_models.md)
for two seasons and create a single data frame of the two seasons.

``` r

bb_2_seasons <- run_epicrop_model(model = bacterial_blight,
                                  emergence = ymd,
                                  wth_list = wth_list,
                                  window_days = duration)
#> Error in `seir()`:
#> ! formal argument "duration" matched by multiple actual arguments

bb_2_seasons
#> Error:
#> ! object 'bb_2_seasons' not found
```

### Visualising differences in AUDPC between seasons

A simple bar chart created using {ggplot2} is an effective way to
visualise the difference between the two seasons.

``` r

library(ggplot2)
ggplot(bb_2_seasons,
       aes(y = as.factor(emergence),
           x = AUDPC)) +
  geom_col(width = 0.5,
           orientation = "y") +
  ylab("Emergence (YYYY-MM-DD)") +
  xlab("AUDPC") +
  theme_classic()
#> Error:
#> ! object 'bb_2_seasons' not found
```

### Simulating multiple seasons and establishment dates

#### Single Site with Multiple Dates

A helper function,
[`run_epicrop_model()`](https://adamhsparks.codeberg.page/epicrop/reference/run_epicrop_model.md)
is provided to simplify running multiple establishment dates.

``` r

years <- 2001:2020
month_day <- c("-06-01", "-06-14", "-06-30")
lonlat <- c(121.255669, 14.16742)
duration <- 120L

emergence <- build_epicrop_emergence(years, month_day)

# select 200 days duration to allow for multiple seasonal runs on same data
wth_list <- fetch_epicrop_weather_list(years = years,
                                       lonlat = lonlat,
                                       start_date = month_day,
                                       duration = duration)
#> Warning in `[.data.table`(tab, , `:=`(year, data.table::year(YYYYMMDD))): A
#> shallow copy of this data.table was taken so that := can add or remove 1
#> columns by reference. At an earlier point, this data.table was copied by R (or
#> was created manually using structure() or similar). Avoid names<- and attr<-
#> which in R currently (and oddly) may copy the whole data.table. Use set* syntax
#> instead to avoid copying: ?set, ?setnames and ?setattr. It's also not unusual
#> for data.table-agnostic packages to produce tables affected by this issue. If
#> this message doesn't help, please report your use case to the data.table issue
#> tracker so the root cause can be fixed or this message improved.
```

``` r

 run_epicrop_model(bacterial_blight,
                   emergence,
                   wth_list,
                   window_days = duration,
                   output = "audpc")
#> Error in `seir()`:
#> ! formal argument "duration" matched by multiple actual arguments
```

#### Multiple Sites and Seasons

Multiple locations can also be simulated using
[`fetch_epicrop_weather_list()`](https://adamhsparks.codeberg.page/epicrop/reference/fetch_epicrop_weather_list.md)
and
[`run_epicrop_model()`](https://adamhsparks.codeberg.page/epicrop/reference/run_epicrop_model.md).
Here Septoria tritici blotch is simulated for three locations in Western
Australia and New South Wales over two seasons and two planting dates
with a 240 day growing season.

``` r

# set up the dates
years <- 2020:2021
start_date <- c("04-15", "05-01")

# using a list of multiple locations
locs <- list(
  "Merredin" = c(x = 118.28, y = -31.48),
  "Corrigin" = c(x = 117.87, y = -32.33),
  "Tamworth" = c(x = 150.84, y = -31.07)
)

wth_list <- fetch_epicrop_weather_list(
  lonlat = locs,
  start_date = start_date,
  duration = 240L,
  years = years,
  mode = "cross"
  )
#> Error in `data.table::rbindlist()`:
#> ! Item 1 of input is not a data.frame, data.table or list

emergence <- build_epicrop_emergence(years, start_date)
```

Examples of how to run the model sequentially and in parallel.

``` r

run_epicrop_model(s_tritici_blotch,
                  emergence,
                  wth_list,
                  window_days = 240L,
                  output = "audpc")
#> Error in `seir()`:
#> ! formal argument "duration" matched by multiple actual arguments
```
