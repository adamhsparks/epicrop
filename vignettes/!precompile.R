# vignettes that depend on Internet access need to be precompiled
library(knitr)
library(devtools)
library(here)
library(fs)
library(purrr)
library(tibble)

pak::pak() # ensure we have all dependencies
install() # ensure we're using latest version of epicrop

# --- knit and purl vignettes --------------------------------------------------

vig_dir <- here("vignettes")
fig_dir <- here("figure")

# Define vignette sources
vignettes <- tribble(
  ~input                         ,
  ~base_name                     ,
  "vignettes/epicrop.Rmd.orig"   ,
  "epicrop"                      ,
  "vignettes/multiples.Rmd.orig" ,
  "multiples"                    ,
  "vignettes/mapping.Rmd.orig"   ,
  "mapping"
)

# Process each vignette
for (i in seq_len(nrow(vignettes))) {
  input_file <- vignettes$input[i]
  base_name <- vignettes$base_name[i]

  # Check input exists
  if (!file_exists(input_file)) {
    warning("Input file not found: ", input_file)
    next
  }

  # Knit to .Rmd
  output_rmd <- path(vig_dir, paste0(base_name, ".Rmd"))
  knit(input = input_file, output = output_rmd)
  cat("✓ Knitted:", output_rmd, "\n")

  # Purl to .R
  output_r <- path(vig_dir, paste0(base_name, ".R"))
  purl(input_file, output = output_r)
  cat("✓ Purled:", output_r, "\n")
}

# --- move figure images after knitting is complete ----------------------------

figs <- dir_ls(fig_dir, glob = "*.png", recurse = FALSE)

if (length(figs) > 0) {
  file_copy(
    path = figs,
    new_path = path(vig_dir, path_file(figs)),
    overwrite = TRUE
  )
  cat("✓ Copied", length(figs), "figures to vignettes/\n")

  # Clean up figure directory
  file_delete(figs)
  if (dir_exists(fig_dir)) {
    dir_delete(fig_dir)
  }
}

# --- fix image paths in vignettes (CORRECTED) --------------------------------

replace_figure_path <- function(path) {
  x <- readLines(path, warn = FALSE)
  # Replace "figure/imagename.png" with "imagename.png"
  x <- gsub("figure/", "", x, fixed = TRUE)
  writeLines(x, path)
  invisible(path)
}

walk(
  dir_ls(vig_dir, glob = "*.Rmd", recurse = FALSE),
  replace_figure_path
)
cat("✓ Fixed image paths in .Rmd files\n")

# --- build vignettes ----------------------------------------------------------

build_vignettes()
cat("✓ Built vignettes\n")

# --- move resource files to /doc using fs ------------------------------------

doc_dir <- here("doc")
dir_create(doc_dir, recurse = TRUE)

# Images should be in inst/doc after build_vignettes()
inst_doc_dir <- here("inst", "doc")
if (dir_exists(inst_doc_dir)) {
  pngs_in_doc <- dir_ls(inst_doc_dir, glob = "*.png", recurse = FALSE)

  if (length(pngs_in_doc) > 0) {
    file_copy(path = pngs_in_doc, new_path = doc_dir, overwrite = TRUE)
    cat("✓ Copied", length(pngs_in_doc), "PNG files to doc/\n")
  }
}

cat("\n✓ All vignettes successfully precompiled!\n")
