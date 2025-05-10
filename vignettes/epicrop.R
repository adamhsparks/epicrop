## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(fig.width = 7L, fig.height = 7L)

suggested_packages <- c("ggplot2", "tidyr")

knitr::opts_chunk$set(eval = all(
  vapply(
    suggested_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = FALSE
  )
))


## ----epicrop------------------------------------------------------------------
library(epicrop)


## ----power--------------------------------------------------------------------
# Fetch weather for year 2000 season at the IRRI Zeigler Experiment Station
wth <- get_wth(
  lonlat = c(121.25562, 14.6774),
  dates = c("2000-01-01", "2000-12-31")
)

wth


## ----bb-----------------------------------------------------------------------
# Predict bacterial blight intensity for the year 2000 wet season at IRRI
bb_wet <- bacterial_blight(wth, emergence = "2000-07-01")


## ----bb_wet-------------------------------------------------------------------
summary(bb_wet)


## ----ggplot2------------------------------------------------------------------
library(ggplot2)
library(tidyr)


## ----wet_plot_sites, fig.cap="Site states over time for bacterial blight. Results for wet season year 2000 at IRRI Zeigler Experiment Station shown. Weather data used to run the model were obtained from the NASA Langley Research Center POWER Project funded through the NASA Earth Science Directorate Applied Science Program."----
dat <- pivot_longer(
  bb_wet,
  cols = c("diseased", "removed", "latent", "infectious"),
  names_to = "site",
  values_to = "value"
)

ggplot(data = dat,
       aes(
         x = dates,
         y = value,
         shape = site,
         linetype = site
       )) +
  labs(y = "Sites",
       x = "Date") +
  geom_line(aes(group = site, colour = site)) +
  geom_point(aes(colour = site)) +
  theme_classic()


## ----wet_plot_intensity, fig.cap="Wet season disease intensity over time for bacterial blight. Results for wet season year 2000 at IRRI Zeigler Experiment Station shown.  Weather data used to run the model were obtained from the NASA Langley Research Center POWER Project funded through the NASA Earth Science Directorate Applied Science Program."----
ggplot(data = bb_wet,
       aes(x = dates,
           y = intensity * 100)) +
  labs(y = "Intensity (%)",
       x = "Date") +
  geom_line() +
  geom_point() +
  theme_classic()


## ----dry_summary--------------------------------------------------------------
bb_dry <- bacterial_blight(wth = wth, emergence = "2000-01-05")
summary(bb_dry)


## ----dry_plot_intensity, fig.cap="Dry season site states over time for bacterial blight. Results for dry season year 2000 at IRRI Zeigler Experiment Station shown. Weather data used to run the model were obtained from the NASA Langley Research Center POWER Project funded through the NASA Earth Science Directorate Applied Science Program."----
ggplot(data = bb_dry,
       aes(x = dates,
           y = intensity * 100)) +
  labs(y = "Intensity (%)",
       x = "Date") +
  geom_line() +
  geom_point() +
  theme_classic()


## ----audpc--------------------------------------------------------------------
# Dry season
bb_dry$AUDPC[1]

# Wet season
bb_wet$AUDPC[1]

