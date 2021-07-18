# vignettes that depend on Internet access need to be precompiled
library("knitr")
library("here")
knit(input = "vignettes/epicrop.Rmd.orig", output = "vignettes/epicrop.Rmd")
purl("vignettes/epicrop.Rmd.orig", output = "vignettes/epicrop.R")

knit(input = "vignettes/multiples.Rmd.orig",
     output = "vignettes/multiples.Rmd")
purl("vignettes/multiples.Rmd.orig",
     output = "vignettes/multiples.R")

# move image files
figs <-
  list.files(here("figure/"), pattern = ".png$", full.names = TRUE)
file.copy(from = figs,
          to   = paste0(here("vignettes/"), basename(figs)),
          overwrite = TRUE)
file.remove(figs)
file.remove(here("figure"))

# remove file path such that vignettes will build with figures
replace <- readLines("vignettes/epicrop.Rmd")
replace <- gsub("\\(figure/", "\\(", replace)

fileConn <- file("vignettes/epicrop.Rmd")
writeLines(replace, fileConn)
close(fileConn)

replace <- readLines("vignettes/multiples.Rmd")
replace <- gsub("\\(figure/", "\\(", replace)

fileConn <- file("vignettes/multiples.Rmd")
writeLines(replace, fileConn)
close(fileConn)

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
