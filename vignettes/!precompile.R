# vignettes that depend on Internet access need to be precompiled
library(knitr)
library(devtools)
library(here)
library(fs)

install() # ensure we're using latest version of epicrop

# --- knit and purl vignettes (inputs/outputs unchanged) ----------------------

# epicrop vignette
knit(input = "vignettes/epicrop.Rmd.orig", output = "vignettes/epicrop.Rmd")
purl("vignettes/epicrop.Rmd.orig", output = "vignettes/epicrop.R")

# multiples vignette
knit(input = "vignettes/multiples.Rmd.orig", output = "vignettes/multiples.Rmd")
purl("vignettes/multiples.Rmd.orig", output = "vignettes/multiples.R")

# mapping vignette
knit(input = "vignettes/mapping.Rmd.orig", output = "vignettes/mapping.Rmd")
purl("vignettes/mapping.Rmd.orig", output = "vignettes/mapping.R")

# --- move figure image files into vignettes/ using fs ------------------------

fig_dir <- here("figure")
vig_dir <- here("vignettes")

# list .pngs in figure/
figs <- dir_ls(fig_dir, glob = "*.png")

# copy images to vignettes, then remove originals and the folder
if (length(figs)) {
  file_copy(
    path = figs,
    new_path = path(vig_dir, path_file(figs)),
    overwrite = TRUE
  )
  file_delete(figs)
}
if (dir_exists(fig_dir)) {
  dir_delete(fig_dir)
}

# --- strip '(figure/...' from image paths in vignettes -----------------------

# helper to replace in-place
replace_figure_path <- function(path) {
  x <- readLines(path, warn = FALSE)
  x <- gsub("\\(figure/", "(", x)
  writeLines(x, path)
  invisible(path)
}

replace_figure_path(path(vig_dir, "epicrop.Rmd"))
replace_figure_path(path(vig_dir, "multiples.Rmd"))
replace_figure_path(path(vig_dir, "mapping.Rmd"))

# --- build vignettes ---------------------------------------------------------

build_vignettes()

# --- move resource files (images) to /doc using fs ---------------------------

doc_dir <- here("doc")
pngs_in_vig <- dir_ls(vig_dir, glob = "*.png")
if (length(pngs_in_vig)) {
  file_copy(path = pngs_in_vig, new_path = doc_dir, overwrite = TRUE)
}
