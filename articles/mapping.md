# Mapping Simulations

You may wish to compare the effects of different establishment dates,
different seasons or different locations on disease levels. This
vignette details how you can automate several runs of {epicrop} in R and
visualise them on maps.

## Map of AUDPC at multiple locations

In this example we will fetch data for the Mekong River Delta and Red
River Delta in Vietnam; Prachin Buri, Thailand; Chennai, India and Nueva
Ecija, in the Philippines for the wet season (second half of the year)
in 2000. The map will show a point, with the size relative to the AUDPC
value for that location using the latitude and longitude values that
were supplied to fetch the weather data.

The [{ggspatial}](https://paleolimbot.github.io/ggspatial/) package is
used to provide a base layer of countries using [Open Street
Map](https://www.openstreetmap.org/) map baselayer data and labelling
capabilities.

\
[`library`](https://rdrr.io/r/base/library.html)`(`[`epicrop`](https://codeberg.org/adamhsparks/epicrop)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ggplot2`](https://ggplot2.tidyverse.org)`)`\
[`library`](https://rdrr.io/r/base/library.html)`(`[`ggspatial`](https://paleolimbot.github.io/ggspatial/)`)`\
\
`locations`` ``<-`` `[`list`](https://rdrr.io/r/base/list.html)`(`\
`  ``"Mekong River"`` ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``105.5943``, ``10.0634``)``,`\
`  ``"Red River"`` ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``105.9700``, ``20.9034``)``,`\
`  ``"Prachin Buri"`` ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``101.6601``, ``14.0421``)``,`\
`  ``"Chennai"`` ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``80.2707``, ``13.0827``)``,`\
`  ``"Neuva Ecija"`` ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``121.1113``, ``15.5784``)`\
`)`\
\
`year`` ``<-`` ``2000`\
`emergence_date`` ``<-`` ``"06-30"`\
`ymd`` ``<-`` `[`build_epicrop_emergence`](https://adamhsparks.codeberg.page/epicrop/reference/build_epicrop_emergence.md)`(``years ``=`` ``year``, month_day ``=`` ``emergence_date``)`\
\
`wth_list`` ``<-`` `[`fetch_epicrop_weather_list`](https://adamhsparks.codeberg.page/epicrop/reference/fetch_epicrop_weather_list.md)`(`\
`  lonlat ``=`` ``locations``,`\
`  start_date ``=`` ``"2000-06-30"``,`\
`  duration ``=`` ``120L``,`\
`  years ``=`` ``year``,`\
`  mode ``=`` ``"cross"`\
`)`\
\
`bb_sim`` ``<-`` `[`run_epicrop_model`](https://adamhsparks.codeberg.page/epicrop/reference/run_epicrop_model.md)`(`\
`  model_fun ``=`` ``bacterial_blight``,`\
`  emergence_dates ``=`` ``ymd``,`\
`  wth_list ``=`` ``wth_list``,`\
`  window_days ``=`` ``120L``,`\
`  output ``=`` ``"full"`\
`)`\
\
[`ggplot`](https://ggplot2.tidyverse.org/reference/ggplot.html)`(``bb_sim``, `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``x ``=`` ``lon``, y ``=`` ``lat``)``)`` ``+`\
`  `[`annotation_map_tile`](https://paleolimbot.github.io/ggspatial/reference/annotation_map_tile.html)`(``type ``=`` ``"cartolight"``, zoomin ``=`` ``0``)`` ``+`\
`  `[`geom_spatial_point`](https://paleolimbot.github.io/ggspatial/reference/stat_spatial_identity.html)`(`[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``size ``=`` ``AUDPC``)``)`` ``+`\
`  `[`geom_spatial_label_repel`](https://paleolimbot.github.io/ggspatial/reference/stat_spatial_identity.html)`(`\
`    `[`aes`](https://ggplot2.tidyverse.org/reference/aes.html)`(``label ``=`` `[`paste`](https://rdrr.io/r/base/paste.html)`(``location``, `[`round`](https://rdrr.io/r/base/Round.html)`(``AUDPC``, ``0``)``, sep ``=`` ``", "``)``)``,`\
`    box.padding ``=`` ``1``,`\
`    size ``=`` ``3`\
`  ``)`` ``+`\
`  `[`theme`](https://ggplot2.tidyverse.org/reference/theme.html)`(`\
`    legend.position ``=`` ``"inside"``,`\
`    legend.position.inside ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``0.025``, ``0.025``)``,`\
`    legend.justification ``=`` `[`c`](https://rdrr.io/r/base/c.html)`(``"left"``, ``"bottom"``)``,`\
`    legend.box.just ``=`` ``"right"``,`\
`    legend.margin ``=`` `[`margin`](https://ggplot2.tidyverse.org/reference/element.html)`(``3``, ``3``, ``3``, ``3``)``,`\
`    legend.direction ``=`` ``"horizontal"``,`\
`    legend.spacing.y ``=`` `[`unit`](https://rdrr.io/r/grid/unit.html)`(``0``, ``"mm"``)``,`\
`    panel.border ``=`` `[`element_rect`](https://ggplot2.tidyverse.org/reference/element.html)`(``colour ``=`` ``"black"``, fill ``=`` ``NA``)``,`\
`    axis.text ``=`` `[`element_text`](https://ggplot2.tidyverse.org/reference/element.html)`(``colour ``=`` ``1``, size ``=`` ``12``)``,`\
`    legend.background ``=`` `[`element_blank`](https://ggplot2.tidyverse.org/reference/element.html)`(``)``,`\
`    legend.key ``=`` `[`element_blank`](https://ggplot2.tidyverse.org/reference/element.html)`(``)``,`\
`    legend.box.background ``=`` `[`element_rect`](https://ggplot2.tidyverse.org/reference/element.html)`(``colour ``=`` ``"black"``)`\
`  ``)`` ``+`\
`  `[`labs`](https://ggplot2.tidyverse.org/reference/labs.html)`(`\
`    x ``=`` ``"Longitude"``,`\
`    y ``=`` ``"Latitude"`\
`  ``)`` ``+`\
`  `[`ylim`](https://ggplot2.tidyverse.org/reference/lims.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``0``, ``25``)``)`` ``+`\
`  `[`xlim`](https://ggplot2.tidyverse.org/reference/lims.html)`(`[`c`](https://rdrr.io/r/base/c.html)`(``75``, ``127``)``)`` ``+`\
`  `[`coord_sf`](https://ggplot2.tidyverse.org/reference/ggsf.html)`(``crs ``=`` ``4326``)`

![Rice bacterial blight area under the disease progress curve (AUDPC)
values for an establishment date of June 30, 2000 at five locations,
Mekong River Delta and Red River Delta in Vietnam; Prachin Buri,
Thailand; Chennai, India and Nueva Ecija, in the Philippines. These data
were obtained from the NASA Langley Research Center POWER Project funded
through the NASA Earth Science Directorate Applied Science Program. Map
base layer data is © OpenStreetMap contributors.](map-AUDPC-1.png)

Rice bacterial blight area under the disease progress curve (AUDPC)
values for an establishment date of June 30, 2000 at five locations,
Mekong River Delta and Red River Delta in Vietnam; Prachin Buri,
Thailand; Chennai, India and Nueva Ecija, in the Philippines. These data
were obtained from the NASA Langley Research Center POWER Project funded
through the NASA Earth Science Directorate Applied Science Program. Map
base layer data is © OpenStreetMap contributors.
