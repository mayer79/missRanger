# missRanger 2.6.0

### Major feature

Out-of-sample application is now possible! Thanks to [@jeandigitale](https://github.com/jeandigitale) for pushing the idea in [#58](https://github.com/mayer79/missRanger/issues/58).

This means you can run `imp <- missRanger(..., keep_forests = TRUE)` and then apply its models to new data via `predict(imp, newdata)`. The "missRanger" object can be saved/loaded as binary file, e.g, via `saveRDS()`/`readRDS()` for later use.

Note that out-of-sample imputation works best for rows in `newdata` with only one
missing value, counting only missings that appear in `object$to_impute` as well as in
`object$impute_by` columns. We call this the "easy case". In the "hard case", 
even multiple iterations (set by `iter`) can lead to unsatisfactory results.

### Possibly breaking changes

- Columns of special type like date/time can't be imputed anymore. You will need to convert them to numeric before imputation.
- `pmm()` is more picky: `xtrain` and `xtest` must both be either numeric, logical, or factor (with identical levels).

### Minor changes in output object

- Add original data as `data_raw`.
- Renamed `visit_seq` to `to_impute`.

### Other changes

- Now requires ranger >= 0.16.0.
- More compact vignettes.
- Better examples and README.
- Many relevant `ranger()` arguments are now explicit arguments in `missRanger()` to improve tab-completion experience:
  - num.trees = 500
  - mtry = NULL
  - min.node.size = NULL
  - min.bucket = NULL
  - max.depth = NULL
  - replace = TRUE
  - sample.fraction = if (replace) 1 else 0.632
  - case.weights = NULL
  - num.threads = NULL
  - save.memory = FALSE
- For variables that can't be used, more information is printed.
- If `keep_forests = TRUE`, the argument `data_only` is set to `FALSE` by default.
- "missRanger" object now stores `pmm.k`.

# missRanger 2.5.0

### Bug fixes

- Since Release 2.3.0, unintentionally, negative formula terms haven't been dropped, see [#62](https://github.com/mayer79/missRanger/issues/62). This is fixed now.

### Enhancements

- The vignette on multiple imputations has been revised, and a larger number of donors in predictive mean matching is being used in the example.

# missRanger 2.4.0

### Future Output API

- New argument `data_only = TRUE` to control if only the imputed data should be returned (default), or an object of class "missRanger". This object contains the imputed data and infos like OOB prediction errors, fixing [#28](https://github.com/mayer79/missRanger/issues/28). The value `FALSE` will later becoming the default in {missRanger 3.0.0}. This will be announced via deprecation cycle.

### Enhancements

- New argument `keep_forests = FALSE`. Should the random forests of the best iteration (the one that generated the final imputed data) be added to the "missRanger" object? Note that this will use a lot of memory. Only relevant if `data_only = FALSE`. This solves [#54](https://github.com/mayer79/missRanger/issues/54).

### Bug fixes

- In case the algorithm did not converge, the data of the *last* iteration was returned instead of the current one. This has been fixed.

# missRanger 2.3.0

### Major improvements

- `missRanger()` now works with syntactically wrong variable names like "1bad:variable". This solves an [old issue](https://github.com/mayer79/missRanger/issues/19), recently popping up in [this new issue](https://github.com/mayer79/missRanger/issues/51).
- `missRanger()` now works with any number of features, as long as the formula is left at its default, i.e., `. ~ .`. This solves this [issue](https://github.com/mayer79/missRanger/issues/50).

### Other changes

- Documentation improvement.
- `ranger()` is now called via the x/y interface, not the formula interface anymore.

# missRanger 2.2.1

- Switch from `importFrom` to `::` code style
- Documentation improved

# missRanger 2.2.0

### Less dependencies

- Removed {mice} from "suggested" packages.
- Removed {dplyr} from "suggested" packages.
- Removed {survival} from "suggested" packages.

### Maintenance

- Adding Github pages.
- Introduction of Github actions.

# missRanger 2.1.5 (not on CRAN)

Maintenance release,

- switching to testthat 3,
- changing the package structure, and
- bringing vignettes into right order.

# missRanger 2.1.4 (not on CRAN)

### Minor changes

- Now using progress bar instead of "." to show progress (when verbose = 1).

# missRanger 2.1.2 and 2.1.3

### Maintenance update

- Fixing failing unit tests.

# missRanger 2.1.1

### Minor changes

- Allow the use of "mtry" as suggested by Thomas Lumley. Recommended values are NULL (default), 1 or a function of the number of covariables m, e.g. `mtry = function(m) max(1, m %/% 3)`. Keep in mind that `missRanger()` might use a growing set of covariables in the first iteration of the process, so passing `mtry = 2` might result in an error.

### Documentation

- Improved help pages.
- Splitted long vignette into three shorter ones.

### Other

- Added unit tests.

# missRanger 2.1.0

This is a summary of all changes since version 1.x.x.

### Major changes
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

### Minor changes

* The function `imputeUnivariate` has received a `seed` argument.

* The function `imputeUnivariate` has received a `v` argument, specifying columns to impute.

* The function `generateNA` offers now the possibility to use different proportions of missings for each column.

* If `verbose` is not 0, then `missRanger` will show which variables will be imputed in which order and which variables will be used for imputation.

### Minor bug fix

* The argument `returnOOB` is now effectively controlling if out-of-bag errors are attached as attribute "oob" to the resulting data frame or not. So far, it was always attached.
