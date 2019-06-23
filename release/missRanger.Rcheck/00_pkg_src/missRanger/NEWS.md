# missRanger 2.1.0

This is a summary of all changes since version 1.x.x.

## Major changes
* `missRanger` now also imputes and uses logical variables and character variables as well. Performance of `missRanger` will tend to be better thanks to this fix.

* Added formula interface to specify which variables to impute (those on the left hand side) and those used to do so (those on the right hand side). Here some (pseudo) examples:

  - `. ~ .` (default): Use all variables to impute all variables. Note that only those with missing values will be imputed. Variables without missings will only be used to impute others.
  
  - `. ~ . - ID`: Use all variables except `ID` to impute all missing values.
  
  - `Species ~ Sepal.Width`: Use `Sepal.Width` to impute `Species`. Only works if `Sepal.Width` does not contain missing values.
  
  - `Species + Sepal.Length ~ Species + Petal.Length`: Use `Species` and `Petal.Length` to impute `Species` and `Sepal.Length`. Only works if `Petal.Length` does not contain missing values because it does not appear on the left hand side and is therefore not imputed itself.
  
  - `. ~ 1`: Univariate imputation for all relevant columns (as nothing is selected on the right hand side).

* The first argument of `generateNA` is called `x` instead of `data` in consistency with `imputeUnivariate`.

* `imputeUnivariate` now also works for data frames and matrices.

## Minor changes

* The function `imputeUnivariate` has received a `seed` argument.

* The function `generateNA` offers now the possibility to use different proportions of missings for each column.

## Minor bug fix

* The argument `returnOOB` is now effectively controlling if out-of-bag errors are attached as attribute "oob" to the resulting data frame or not. So far, it was always attached.