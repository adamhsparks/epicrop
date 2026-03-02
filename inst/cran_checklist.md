# epicrop — CRAN submission checklist (compiled code + Rcpp + data.table)

This file is a focused pre-submission checklist and reference for submitting the `epicrop` package (or any R package that includes compiled code via Rcpp and uses data.table). Follow the steps in order and fix errors before submission.

---

## Quick start (commands to run locally)

```sh
# install or refresh tooling
R -e "install.packages(c('devtools','Rcpp','data.table','roxygen2','testthat','rhub'))"

# from package root in R
R -e "Rcpp::compileAttributes()"
R -e "devtools::document()"
R -e "devtools::check(pkg='.', cran=TRUE)"   # runs R CMD check --as-cran under the hood

# additional platform checks
R -e "rhub::check_for_cran()"
# Windows builder (web):
# upload built tar.gz to https://win-builder.r-project.org/
```

---

## Required metadata and package files

- `DESCRIPTION`
  - Provide: `Package`, `Version`, `Title`, `Description`.
  - `Authors@R` with roles (include creator/maintainer).
  - `License` and include LICENSE file.
  - Use `Imports:` (avoid heavy use of `Depends:`).
  - Include `LinkingTo: Rcpp` (required for Rcpp).
  - Example:
    ```
    LinkingTo: Rcpp
    Imports: Rcpp, data.table, rlang, cli
    Suggests: testthat, knitr, rmarkdown, rhub
    ```

- `NAMESPACE`
  - Must include:
    ```
    useDynLib(yourpkg, .registration = TRUE)
    importFrom(Rcpp, evalCpp)
    ```
  - Export public functions.

- `R/`, `src/`, `man/`, `tests/` directories as appropriate.

---

## Compiled code: registration & safety

- Run `Rcpp::compileAttributes()` to generate `RcppExports.*`.
- Keep `useDynLib(..., .registration = TRUE)` in `NAMESPACE`.
- Avoid `.C`/`.Fortran` for new code; prefer `.Call` (Rcpp uses `.Call`).
- Allocate output R vectors on the R side (or return SEXPs from C++) — do not return native-allocated pointers to R.
- Register native routines (Rcpp handles this when you run `compileAttributes()`).
- Add `R_useDynamicSymbols(dll, FALSE)` if you write your own init.

---

## Threading / OpenMP considerations

- If using OpenMP:
  - Add `SystemRequirements: OpenMP` to DESCRIPTION.
  - Provide fallbacks for platforms without OpenMP.
  - Do not call the R API from parallel threads.
  - Be aware macOS and Windows behave differently with OpenMP; test on those platforms.

---

## Portability & compilation

- Ensure `LinkingTo: Rcpp` is present so `Rcpp.h` is found at compile time.
- Avoid absolute include/library paths in `src/Makevars`.
- Test with the same R installation that will build the package (ensure Rcpp is installed there).
- Test on macOS, Linux, Windows:
  - `rhub::check_for_cran()` and win-builder recommended.

---

## R coding standards & CRAN policy

- Fix all ERRORs from `R CMD check --as-cran` first.
- Address WARNINGS where feasible.
- Avoid `no visible binding for global variable` notes (common with data.table):
  - Use `utils::globalVariables()` for used names, or use data.table-safe coding style.
- Avoid internet access in examples/tests.
- Long-running examples should use `\donttest{}` or `\dontshow{}`.
- Vignettes should build non-interactively.

---

## Testing, examples and vignettes

- Add `tests/testthat/` tests.
  - Unit tests comparing pure-R and C++ outputs on small examples.
  - Edge cases: zero/short durations, p_days/i_days extremes, RRLEX off/on.
- Keep examples short and fast.
- Include a vignette for usage but keep long computations optional.

---

## Documentation & man pages

- Use roxygen2 for exported functions and run `devtools::document()`.
- Provide short examples in man pages.
- Provide `NEWS` for important changes.

---

## Data and external files

- Place sample data in `inst/extdata` (small files only).
- Avoid including large binaries.

---

## Runtime & resource handling

- Support user interrupts in long native loops:
  - In C++: call `Rcpp::checkUserInterrupt()` periodically.
- Try to minimize copies between R and native code.

---

## Native code safety

- Avoid out-of-bounds indexing; check array bounds.
- If possible, run sanitizer checks (via rhub) to catch UB / memory errors.
- Do not expose raw pointers or free memory allocated by R.

---

## Common checks & fixes

- `'Rcpp.h' not found` → ensure `LinkingTo: Rcpp` and Rcpp installed.
- data.table notes: declare `utils::globalVariables()` or use data.table-friendly code to avoid NOTES.
- If issues arise during compilation, run a verbose install:
  ```
  R CMD INSTALL --preclean --no-multiarch --clean --install-tests --verbose /path/to/pkg
  ```
- Clean up generated files before rebuild:
  ```
  rm -f src/*.o src/*.so RcppExports.*
  Rcpp::compileAttributes()
  ```

---

## Minimal pre-submission checklist

- [ ] `Rcpp::compileAttributes()` run and `RcppExports.*` committed.
- [ ] `DESCRIPTION` contains `LinkingTo: Rcpp` and accurate `Imports:`.
- [ ] `NAMESPACE` uses `useDynLib(..., .registration = TRUE)`.
- [ ] `R CMD check --as-cran` passes locally (no ERRORs).
- [ ] `rhub::check_for_cran()` and Windows builder checks pass (or you have addressed failures).
- [ ] Unit tests implemented and passing.
- [ ] Examples and vignettes fast and non-interactive.
- [ ] No platform-specific include paths in `src/Makevars`.
- [ ] LICENSE file present and Authors@R correct.

---

## Useful commands & services

- Local build & check:
  ```sh
  R CMD build .
  R CMD check --as-cran epicrop_*.tar.gz
  ```
- devtools helpers:
  ```r
  devtools::document()
  devtools::check(cran = TRUE)
  ```
- R-hub:
  ```r
  rhub::check_for_cran()
  ```
- Windows builder: https://win-builder.r-project.org/

---

## Optional: pre-submission script

Create a small R script (`tools/precheck.R`) to run common steps:

```r
install.packages(c("Rcpp","data.table","devtools","rhub","testthat"))
Rcpp::compileAttributes()
devtools::document()
res <- devtools::check(cran = TRUE)
print(res)
rhub::check_for_cran()
```

---

If you want, I can:

- produce a rendered `RcppExports.*` pair for your package name,
- generate `utils::globalVariables()` declarations for the data.table column names used,
- create `testthat` tests comparing the pure-R and Rcpp outputs for several scenarios.

Which would you like next?
