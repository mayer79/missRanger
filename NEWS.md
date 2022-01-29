# missRanger 2.1.5

Maintenance release,

- switching to testthat 3,
- changing the package structure, and
- bringing vignettes into right order.

# missRanger 2.1.4

## Minor changes

- Now using progress bar instead of "." to show progress (when verbose = 1).

# missRanger 2.1.2 and 2.1.3

## Maintenance update

- Fixing failing unit tests.

# missRanger 2.1.1

## Minor changes

- Allow the use of "mtry" as suggested by Thomas Lumley. Recommended values are NULL (default), 1 or a function of the number of covariables m, e.g. `mtry = function(m) max(1, m %/% 3)`. Keep in mind that `missRanger()` might use a growing set of covariables in the first iteration of the process, so passing `mtry = 2` might result in an error.

## Documentation

- Improved help pages.
- Splitted long vignette into three shorter ones.

## Other

- Added unit tests.

# missRanger 2.1.0

This is a summary of all changes since version 1.x.x.

## Major changes
* `missRanger` now also imputes and uses logical variables, character variables and further variables of mode numeric like dates and times.

* Added formula interface to specify which variables to impute (those on the left hand side) and those used to do so (those on the right hand side). Here some (pseudo) examples:

  - `. ~ .` (default): Use all variables to impute all variables. Note that only those with missing values will be imputed. Variables without missings will only be used to impute others.
  
  - `. ~ . - ID`: Use all variables except `ID` to impute all missing values.
  
  - `Species ~ Sepal.Width`: Use `Sepal.Width` to impute `Species`. Only works if `Sepal.Width` does not contain missing values. (Add it to the right hand side if it does.)
  
  - `Species + Sepal.Length ~ Species + Petal.Length`: Use `Species` and `Petal.Length` to impute `Species` and `Sepal.Length`. Only works if `Petal.Length` does not contain missing values because it does not appear on the left hand side and is therefore not imputed itself.
  
  - `. ~ 1`: Univariate imputation for all relevant columns (as nothing is selected on the right hand side).

* The first argument of `generateNA` is called `x` instead of `data` in consistency with `imputeUnivariate`.

* `imputeUnivariate` now also works for data frames and matrices.

* In PMM mode, `missRanger` relies on OOB predictions. The smaller the value of `num.trees`, the higher the risk of missing OOB predictions, which caused an error in PMM. Now, `pmm` allows for missing values in `xtrain` or `ytrain`. Thus, the algorithm will even work with `num.trees = 1`. This will be useful to impute large data sets with PMM.

## Minor changes

* The function `imputeUnivariate` has received a `seed` argument.

* The function `imputeUnivariate` has received a `v` argument, specifying columns to impute.

* The function `generateNA` offers now the possibility to use different proportions of missings for each column.

* If `verbose` is not 0, then `missRanger` will show which variables will be imputed in which order and which variables will be used for imputation.

## Minor bug fix

* The argument `returnOOB` is now effectively controlling if out-of-bag errors are attached as attribute "oob" to the resulting data frame or not. So far, it was always attached.