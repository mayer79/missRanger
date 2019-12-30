# missRanger

The `missRanger` package uses the `ranger` package to do fast missing value imputation by chained random forest. As such, it serves as an alternative implementation of the beautiful 'MissForest' algorithm, see vignette.

`missRanger` offers the option to combine random forest imputation with predictive mean matching. This firstly avoids the generation of values not present in the original data (like a value 0.3334 in a 0-1 coded variable). Secondly, this step tends to raise the variance in the resulting conditional distributions to a realistic level, a crucial element to apply multiple imputation frameworks.

## Installation
From CRAN:
``` r
install.packages("missRanger")
```

Latest version from github:
``` r
# library(devtools)
install_github("mayer79/missRanger")
```

## Examples

We first generate a data set with about 10% missing values in each column. 
Then those gaps are filled by `missRanger`. In the end, the resulting data frame is displayed.

``` r
library(missRanger)
 
# Generate data with missing values in all columns
irisWithNA <- generateNA(iris, seed = 347)
 
# Impute missing values with missRanger
irisImputed <- missRanger(irisWithNA, pmm.k = 3, num.trees = 100)
 
# Check results
head(irisImputed)
head(irisWithNA)
head(iris)

# With extra trees algorithm
irisImputed_et <- missRanger(irisWithNA, pmm.k = 3, splitrule = "extratrees", num.trees = 100)

# With `dplyr` syntax
library(dplyr)

iris %>% 
  generateNA() %>% 
  missRanger(verbose = 0) %>% 
  head()
```