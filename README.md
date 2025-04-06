# {missRanger} <a href='https://github.com/mayer79/missRanger'><img src='man/figures/logo.png' align="right" height="139" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/mayer79/missRanger/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mayer79/missRanger/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/mayer79/missRanger/graph/badge.svg)](https://app.codecov.io/gh/mayer79/missRanger)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/missRanger)](https://cran.r-project.org/package=missRanger)

[![](https://cranlogs.r-pkg.org/badges/missRanger)](https://cran.r-project.org/package=missRanger) 
[![](https://cranlogs.r-pkg.org/badges/grand-total/missRanger?color=orange)](https://cran.r-project.org/package=missRanger)

<!-- badges: end -->

## Overview

{missRanger} is a **multivariate imputation algorithm** based on random forests. It is a fast alternative to the famous 'MissForest' algorithm (Stekhoven and Buehlmann, 2012), and uses the {ranger} package (Wright and Ziegler, 2017) to fit the random forests. Since version 2.6.0, out-of-sample application is possible.

## Installation

```r
# From CRAN
install.packages("missRanger")

# Development version
devtools::install_github("mayer79/missRanger")
```

## Usage

```r
library(missRanger)

set.seed(3)

iris_NA <- generateNA(iris, p = 0.1)
head(iris_NA)
# Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#         5.1         3.5          1.4         0.2  setosa
#         4.9         3.0          1.4          NA  setosa
#         4.7         3.2          1.3         0.2  setosa
#         4.6         3.1          1.5         0.2    <NA>
#          NA         3.6          1.4         0.2  setosa
#         5.4         3.9          1.7         0.4    <NA>

iris_filled <- missRanger(iris_NA, pmm.k = 5, num.trees = 100)
head(iris_filled)

#   Sepal.Length Sepal.Width Petal.Length Petal.Width Species
# 1          5.1         3.5          1.4         0.2  setosa
# 2          4.9         3.0          1.4         0.2  setosa
# 3          4.7         3.2          1.3         0.2  setosa
# 4          4.6         3.1          1.5         0.2  setosa
# 5          5.2         3.6          1.4         0.2  setosa
# 6          5.4         3.9          1.7         0.4  setosa
```

## How it works

The algorithm iterates until the average out-of-bag (OOB) error of the forests stops improving. The missing values are filled by OOB predictions of the best iteration, optionally followed by predictive mean matching (PMM). The PMM step avoids values not present in the original data (like a value 0.3334 in a 0-1 coded variable). Furthermore, PMM raises the variance in the resulting conditional distributions to a more realistic level, a crucial property for **multiple imputation**.

Check-out the vignettes for more info, and for how to use `missRanger()` in multiple imputation.

## References

- Stekhoven D. J., Buehlmann, P. (2012). MissForest - non-parametric missing value imputation for mixed-type data. Bioinformatics, 28(1), 112-118.
- Marvin N. Wright, Andreas Ziegler (2017). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. Journal of Statistical Software, 77(1), 1-17. doi:10.18637/jss.v077.i01
