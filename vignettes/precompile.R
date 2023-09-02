# vignettes that depend on Internet access need to be precompiled
library(knitr)
library(here)

# epicrop vignette
knit(input = "vignettes/epicrop.Rmd.orig",
     output = "vignettes/epicrop.Rmd")
purl("vignettes/epicrop.Rmd.orig",
     output = "vignettes/epicrop.R")

# multiples vignette
knit(input = "vignettes/multiples.Rmd.orig",
     output = "vignettes/multiples.Rmd")
purl("vignettes/multiples.Rmd.orig",
     output = "vignettes/multiples.R")

# mapping vignette
knit(input = "vignettes/mapping.Rmd.orig",
     output = "vignettes/mapping.Rmd")
purl("vignettes/mapping.Rmd.orig",
     output = "vignettes/mapping.R")

# move image files
figs <-
  list.files(here("figure/"),
             pattern = ".png$",
             full.names = TRUE)
file.copy(from = figs,
          to = paste0(here("vignettes/"),
                        basename(figs)),
          overwrite = TRUE)
file.remove(figs)
file.remove(here("figure"))

# remove file path such that vignettes will build with figures
## epicrop vignette
epicrop_replace <- readLines("vignettes/epicrop.Rmd")
epicrop_replace <- gsub("\\(figure/", "\\(", epicrop_replace)

epicrop_file_con <- file("vignettes/epicrop.Rmd")
writeLines(epicrop_replace, epicrop_file_con)
close(epicrop_file_con)

## multiples vignette
multiples_replace <- readLines("vignettes/multiples.Rmd")
multiples_replace <- gsub("\\(figure/", "\\(", multiples_replace)

multiples_file_con <- file("vignettes/multiples.Rmd")
writeLines(multiples_replace, multiples_file_con)
close(multiples_file_con)

## mapping vignette
mapping_replace <- readLines("vignettes/mapping.Rmd")
mapping_replace <- gsub("\\(figure/", "\\(", mapping_replace)

mapping_file_con <- file("vignettes/mapping.Rmd")
writeLines(mapping_replace, mapping_file_con)
close(mapping_file_con)

# build vignettes
library("devtools")
build_vignettes()

# move resource files (images) to /doc
resources <-
  list.files(here("vignettes/"),
             pattern = ".png$",
             full.names = TRUE)
file.copy(from = resources,
          to = here("doc"),
          overwrite =  TRUE)
