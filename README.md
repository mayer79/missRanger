# {missRanger} <a href='https://github.com/mayer79/missRanger'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![CRAN status](http://www.r-pkg.org/badges/version/missRanger)](https://cran.r-project.org/package=missRanger)
[![R-CMD-check](https://github.com/mayer79/missRanger/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mayer79/missRanger/actions)
[![Codecov test coverage](https://codecov.io/gh/mayer79/missRanger/branch/main/graph/badge.svg)](https://app.codecov.io/gh/mayer79/missRanger?branch=main)

[![](https://cranlogs.r-pkg.org/badges/missRanger)](https://cran.r-project.org/package=missRanger) 
[![](https://cranlogs.r-pkg.org/badges/grand-total/missRanger?color=orange)](https://cran.r-project.org/package=missRanger)

<!-- badges: end -->

## Overview

{missRanger} uses the {ranger} package to do fast missing value imputation by chained random forest. As such, it serves as an alternative implementation of the beautiful 'MissForest' algorithm, see vignette.

The main function `missRanger()` offers the option to combine random forest imputation with predictive mean matching. This firstly avoids the generation of values not present in the original data (like a value 0.3334 in a 0-1 coded variable). Secondly, this step tends to raise the variance in the resulting conditional distributions to a realistic level, a crucial element to apply multiple imputation frameworks.

## Installation

```r
# From CRAN
install.packages("missRanger")

# Development version
devtools::install_github("mayer79/missRanger")
```

## Usage

We first generate a data set with about 10% missing values in each column. 
Then those gaps are filled by `missRanger()`. In the end, the resulting data frame is displayed.

``` r
library(missRanger)
 
# Generate data with missing values in all columns
irisWithNA <- generateNA(iris, seed = 347)
 
# Impute missing values
irisImputed <- missRanger(irisWithNA, pmm.k = 3, num.trees = 100)
 
# Check results
head(irisImputed)
head(irisWithNA)
head(iris)

# Replace random forest by extremely randomized trees
irisImputed_et <- missRanger(
  irisWithNA, 
  pmm.k = 3, 
  splitrule = "extratrees", 
  num.trees = 100
)

# Using the pipe...
iris |> 
  generateNA() |> 
  missRanger(pmm.k = 5, verbose = 0) |> 
  head()
  
# More infos via `data_only = FALSE`
imp <- missRanger(irisWithNA, pmm.k = 3, data_only = FALSE, seed = 3)
summary(imp)

# missRanger object. Extract imputed data via $data
# - best iteration: 3 
# - best average OOB imputation error: 0.02058243 
# 
# Sequence of OOB prediction errors:
# 
#      Sepal.Length Sepal.Width Petal.Length Petal.Width    Species
# [1,]   1.00000000  1.03868004  0.267209559 0.103679645 0.08148148
# [2,]   0.02948771  0.05997235  0.005676231 0.007813704 0.00000000
# [3,]   0.02709505  0.06268752  0.004921649 0.008207934 0.00000000
# [4,]   0.02673459  0.06504868  0.005183209 0.008761418 0.00000000
# 
# Corresponding means:
# [1] 0.49821014 0.02059000 0.02058243 0.02114558
# 
# First rows of imputed data:
# 
#   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 2          4.9         3.0          1.4         0.2  setosa
# 3          4.7         3.2          1.6         0.2  setosa
```

Check out the vignettes for more info.
