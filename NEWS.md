# epicrop 0.0.0.9010-1

## Minor changes

* Fixes issue in vignettes with duplicate titles and plot theming.

# epicrop 0.0.0.9010

## Major changes

* **BREAKING CHANGE** `intensity` column, in `SEIR()` is now a proportion, _i.e._ values range between 0 - 1.
If you wish to have the same values as previously reported, `df$intensity*100` will give you these values.

* **BREAKING CHANGE** Removes column, `severity`, in `SEIR()` output.

* Adds a new vignette illustrating how to run multiple simulations and use parallel processing to reduce run time for these simulations.

# epicrop 0.0.0.9009

## Major changes

* **BREAKING CHANGE** Add a new column to `SEIR()` output, `severity`, not to be confused with the previous column called `severity` (now called `intensity`), which represents the cumulative proportion of diseased sites on day "x" expressed as, $\frac{diseased\ sites}{total\ sites}\times100$.

## Minor changes

* More improvements to function documentation, include `#' @family predict functions` in each of the `predict` functions rather than only using `'# @seealso`.

# epicrop 0.0.0.9008

## Major changes

* * **BREAKING CHANGE** The `severity` output column from `SEIR()` has been renamed as `intensity` to follow the nomenclature of Savary _et al._ 2012.
This column represents the proportion of actively diseased sites on day "x" expressed as, $\frac{(diseased\ sites-removed\ sites)}{(total\ sites-removed\ sites)}\times100$.

# epicrop 0.0.0.9007

## Minor changes

* Only include Lat/Lon values if the `wth` object provides them.

* Better documentation about the Lat/Lon values in the output of `SEIR()`

* Parameters for `predict_bacterial_blight()` are used as basis for all of the other `predict_*()` family of functions.

* Add DOI to CITATION and DESCRIPTION files.

* Add Lifecycle and DOI badges to README files.

# epicrop 0.0.0.9006

## Minor changes

* Optimise `SEIR()` internal functionality.

* Minor improvements to documentation.

* Polish NEWS.md.

# epicrop 0.0.0.9005

## Minor changes

* Only import _data.table_ functions as necessary, don't import whole package.

## Bug fixes

* Convert `wth` (weather input object) to a `data.table` type object internally if it is not already one.
Prior, if the object was not a `data.table`, `SEIR()` would fail with a message that the dates did not align.
This should fix that issue and any `data.frame` type object including a `tibble` can be provided now.
Thanks to Jean Fabrice Adanve for helping me find this bug.

# epicrop 0.0.0.9004

## Bug fixes

* Fixes bug where the relative humidity checks in `SEIR()` only checked if the daily RH value was equal to (`==`) not equal to or greater than (`>=`) the set parameter for `rhlim` (default is 90%).

* Example for `SEIR()` in _roxygen_ section now works properly when executed by the user.

## Major changes

* Any default parameter values are moved from `SEIR()` to the `predict_()` functions themselves, so any calls directly to `SEIR()` must specify all parameters.

* Deleted `inst/alt_versions/tungrov2.R` file.

* Deleted `inst/workflows/spatsim.R` file. 

## Minor changes

* Edit documentation for better clarity

* Better commenting in code for self and others' reference

* `SEIR()` is simplified and further optimised
  * In some cases in the `for()` loop, the value of `day + 1` was repeatedly calculated and assigned to a new object. This has been corrected with a single object, `d1` being created at the beginning of each loop instance.
  * In some cases where RH, TEMP or RAIN are repeatedly checked against, vectors of these three values are created outside the loop to save time when checking or extracting values.
  * Redundant code and other unused objects are cleaned up and removed.
  
* Standardise library loading calls to use standard evaluation in documentation.

* Standardise italics to use "_" rather than "*".

* Use `ggplot2::theme_classic()` for example figures in README and _epicrop_ vignette.

# epicrop 0.0.0.9003

* Added a `NEWS.md` file to track changes to the package.
