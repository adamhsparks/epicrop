# epicrop 0.0.0.9012

## Bug fixes

* Fixes bug in `SEIR()` where Rc value was calculated incorrectly that was introduced with commit https://github.com/r-forge/cropsim/commit/ba093b252deb222f1ffe54b3a7f0d14b6192f18c#diff-17fd120d02f173b87db7e09b563e2057b95391bef823eee29cbff30cb3d80287. See Lines 124:126 that were changed to Lines 118:120 where
```r 
Rc[day+1] <- baseRc * AFGen(ageRc, day) * AFGen(tmpRc, wthsub$tavg[day+1]) * RHCoef[day+1]
```
should have been
```r
Rc[day+1] <- baseRc * AFGen(ageRc, day+1) * AFGen(tmpRc, wthsub$tavg[day+1]) * RHCoef[day+1]
```
to calculate the basic infection rate, `Rc`, for `day + 1`. Commit: [248df5512b5897e950dd58a0a8b3fd57922add47](https://github.com/adamhsparks/epicrop/commit/248df5512b5897e950dd58a0a8b3fd57922add47).

* Fixes typos in `RcA` values for `predict_bacterial_blight()` and `predict_sheath_blight()`.
  * In `predict_bacterial_blight()` the eighth entry was improperly encoded as 0.42, it has been corrected to 0.41, commit [8b2c36b61d4f949385dcac4c03ff435b8400e335](https://github.com/adamhsparks/epicrop/commit/8b2c36b61d4f949385dcac4c03ff435b8400e335).
  * In `predict_sheath_blight()` the seventh entry was improperly encoded as 0.83, it has been corrected to 0.84, Commit: [69b89ce24dd41c16d4627225c028355f76a64b80](https://github.com/adamhsparks/epicrop/commit/69b89ce24dd41c16d4627225c028355f76a64b80).

## Major changes

* Implements a new function to calculate the correction values for crop age, `RcA`, and temperature, `RcT`, using `approx()` in a custom function that is faster than the `select_mod_value()` function.

* Implements new functionality for `get_wth()` to optionally get data from the CHIRPS/CHIRTS APIs in addition to the default NASA POWER API.

* Uses one-indexed `for()` loop, in place of the zero-indexed `for()` loop found in _cropsim_.

# epicrop 0.0.0.9011

## Minor changes

* Fix title in vignette entry.

* Remove `lazyData` field from DESCRIPTION.

* Remove redundant `maintainer` field from DESCRIPTION.

* Remove dates (date when knit) from vignettes.

* Fix/clean up note on parallel processing in "Multiples" vignette.

* Better, more consistent documentation.

* Update `get_wth()` to work with the new (unreleased) version of _nasapower_ that works with the new version of the POWER API.

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
