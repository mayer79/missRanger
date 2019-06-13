# missRanger
 
## Description
 
This package uses the `ranger` package [1] to do fast missing value imputation by chained random forest, see [2] and [3]. 
Between the iterative model fitting, it offers the option of using predictive mean matching. This firstly avoids the 
imputation with values not present in the original data (like a value 0.3334 in a 0-1 coded variable). Secondly, predictive 
mean matching tries to raise the variance in the resulting conditional distributions to a realistic level. This would allow 
e.g. to do multiple imputation when repeating the call to missRanger(). Package `mice` utilizes the `randomForest` package with only ten trees as default.

Please check the help `?missRanger` for how to call the function and to see all options. 


## Example

This example first generates a data set with about 10% missing values in each column. 
Then those gaps are filled by `missRanger`. In the end, the resulting data frame is displayed.

``` r
library(missRanger)
 
# Generate data with missing values in all columns
irisWithNA <- generateNA(iris)
 
# Impute missing values with missRanger
irisImputed <- missRanger(irisWithNA, pmm.k = 3, num.trees = 100)
 
# Check results
head(irisImputed)
head(irisWithNA)
head(iris)

# With extra trees algorithm
irisImputed_et <- missRanger(irisWithNA, pmm.k = 3, splitrule = "extratrees", num.trees = 100)
head(irisImputed_et)
```

Since release 1.0.3, thanks to Andrew Landgraf, it is now possible to use in line with `tidyverse`.
``` r
library(tidyverse)

iris %>% 
  as.tibble %>% 
  generateNA %>% 
  missRanger(verbose = 0) %>% 
  head
```

Since release 2.0.1, following an idea of Marvin Wright, a formula interface is used to control which variables are to be imputed by which variables:

``` r
# Impute all variables with all
missRanger(irisWithNA, . ~ ., pmm.k = 3, num.trees = 100)

# Impute all variables with all except Species
missRanger(irisWithNA, . ~ . - Species, pmm.k = 3, num.trees = 100)

# Impute Species and Sepal.Width by Sepal.Width
missRanger(irisWithNA, Species + Sepal.Width ~ Species, pmm.k = 3, num.trees = 100)

# Impute all variables univariatly
missRanger(irisWithNA, . ~ 1, pmm.k = 3)
```

## How to deal with date variables etc.?
`missRanger` natively deals with numeric and character/factor variables. In real-world data sets, also other types of variables appear, e.g. date variables. These can be imputed as well, but it requires some pre- and post-processing:

1. Transform the variable to a numeric or character/factor.

2. Impute with `pmm.k` > 0, so that no new values are created.

3. Transform the imputed variable back to its original type.

### Example
``` r
library(missRanger)
library(lubridate)
library(tidyverse)

# Add a date variable to iris
iris$random_date <- seq.Date(as.Date("1998-12-17"), 
                             by = "1 day", 
                             length.out = nrow(iris))

set.seed(3234)
irisWithNA <- generateNA(iris, p = 0.2)
head(irisWithNA$random_date)
# Output: "1998-12-17" NA           NA           NA           "1998-12-21" "1998-12-22"

# Convert date to numeric, impute with PMM, convert back to date
irisImputed <- irisWithNA %>% 
  mutate(random_date = as.numeric(random_date)) %>% 
  missRanger(pmm.k = 5, num.trees = 100) %>% 
  mutate(random_date = as.Date(random_date, origin = "1970-01-01"))

head(irisImputed$random_date)
# Output: "1998-12-17" "1999-01-13" "1999-02-04" "1999-01-22" "1998-12-21" "1998-12-22"

```

## How to deal with censored variables?
There is no obvious way of how to deal with survival variables in imputation models, mostly since it is unclear of how to use them as covariables to predict other variables. 

Options discussed in [add citation] include:

- Use both status variable s and (censored) time variable t

- s and log(t)

- KM(t), and, optionally s

By KM(t), we denote the Kaplan-Meier estimate at each value of t.

The third option is the most elegant one as it explicitly deals with censoring information.

Let's go through an example to explain it:

### Example

``` r
to do

```

## How to use `missRanger` in multiple imputation settings?

to do

``` r
to do

```


## Installation
Release 1.0.4 on CRAN
```
install.packages("missRanger")
```

## References
[1]  Wright, M. N. & Ziegler, A. (2016). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. Journal of Statistical Software, in press. http://arxiv.org/abs/1508.04409. 
 
[2]  Stekhoven, D.J. and Buehlmann, P. (2012), 'MissForest - nonparametric missing value imputation for mixed-type data', Bioinformatics, 28(1) 2012, 112-118, doi: 10.1093/bioinformatics/btr597

[3]  Van Buuren, S., Groothuis-Oudshoorn, K. (2011). mice: Multivariate Imputation by Chained Equations in R. Journal of Statistical Software, 45(3), 1-67. http://www.jstatsoft.org/v45/i03/
