# missRanger

The `missRanger` package uses `ranger` [1] to do fast missing value imputation by chained random forest. As such, it serves as an alternative implementation of the beautiful 'MissForest' algorithm, introduced by Stekhoven and Buehlmann in [2].

`missRanger` offers the option to combine random forest imputation with predictive mean matching. This firstly avoids the generation of values not present in the original data (like a value 0.3334 in a 0-1 coded variable). Secondly, this step tends to raise the variance in the resulting conditional distributions to a realistic level, a crucial element to apply multiple imputation frameworks like `mice` [3].

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

Since release 2.0.1, following an idea of Marvin N. Wright, a formula interface is used to control which variables are to be imputed (on the left hand side of the formula) by which variables (those on the right hand side):

``` r
# Impute all variables with all (default behaviour). Note that variables without
# missing values will be skipped from the left hand side of the formula.
head(m <- missRanger(irisWithNA, formula = . ~ ., pmm.k = 3, num.trees = 100))

# Same
head(m <- missRanger(irisWithNA, pmm.k = 3, num.trees = 100))

# Impute all variables with all except Species
head(m <- missRanger(irisWithNA, . ~ . - Species, pmm.k = 3, num.trees = 100))

# Impute Species and Sepal.Width by Species
head(m <- missRanger(irisWithNA, Species + Sepal.Width ~ Species, pmm.k = 3, num.trees = 100))

# Impute all variables univariatly
head(m <- missRanger(irisWithNA, . ~ 1))

```

## Imputation takes too much time. What can I do?

`missRanger` is based on iteratively fitting random forests for each variable with missing values. Since the underlying random forest implementation `ranger` uses 500 trees per default, a huge number of trees might be calculated. For larger data sets, the overall process can take very long.

Here are tweaks to make things faster:

- Use less trees, e.g. by setting `num.trees = 50`. Even one single tree might be sufficient. Typically, the number of iterations until convergence will increase with fewer trees though.

- Use smaller bootstrap samples by setting e.g. `sample.fraction = 0.1`.

- Use the less greedy `splitrule = "extratrees"`.

- Use a low tree depth `max.depth = 6`.

- Use large leafs, e.g. `min.node.size = 10000`.

- Use a low `max.iter`, e.g. 1 or 2.

### Examples evaluated on a normal laptop

``` r
library(ggplot2) # for diamonds data
dim(diamonds) # 53940    10

diamonds_with_NA <- generateNA(diamonds)

# Takes 270 seconds (10 * 500 trees per iteration!)
system.time(m <- missRanger(diamonds_with_NA, pmm.k = 3))

# Takes 19 seconds
system.time(m <- missRanger(diamonds_with_NA, pmm.k = 3, num.trees = 50))

# Takes 7 seconds
system.time(m <- missRanger(diamonds_with_NA, pmm.k = 3, num.trees = 1))

# Takes 9 seconds
system.time(m <- missRanger(diamonds_with_NA, pmm.k = 3, num.trees = 50, sample.fraction = 0.1))

```

## Trick: Use `case.weights` to weight down contribution of rows with many missings

Since version 1.0.5, the underlying models can utilize case weights. This might be e.g. useful to weight down the contribution of rows with many missings.

### Example

``` r
irisWithNA <- generateNA(iris, seed = 37)
non_miss <- rowSums(!is.na(irisWithNA))
table(non_miss)

# No weighting
head(m <- missRanger(irisWithNA, num.trees = 20, pmm.k = 3, seed = 345))

# Weigthed by number of non-missing values per row. 
# The third row of Sepal.Length is imputed differently.
head(m <- missRanger(irisWithNA, num.trees = 20, pmm.k = 3, seed = 5,
                     case.weights = non_miss))

```

## How to use `missRanger` in multiple imputation settings?

For machine learning tasks, imputation is typically seen as a fixed data preparation step like dummy coding. There, multiple imputation is rarely applied as it adds another level of complexity to the analysis. This might be fine since a good validation schema will account for variation introduced by imputation. 

For tasks with focus on statistical inference (p values, standard errors, confidence intervals, estimation of effects), the extra variability introduced by imputation has to be accounted for except if only very few missing values appear. One of the standard approaches is to impute the data set multiple times, generating e.g. 10 or 100 versions of a complete data set. Then, the intended analysis (t-test, linear model etc.) is applied independently to each of the complete data sets. Their results are combined afterward in a pooling step, usually by Rubin's rule [4]. For parameter estimates, averages are taken. Their variance is basically a combination of the average squared standard errors plus the variance of the parameter estimates across the imputed data sets, leading to inflated standard errors and thus larger p values and wider confidence intervals. 

The package `mice` takes case of this pooling step. The creation of multiple complete data sets can be done by `mice` or also by `missRanger`. In the latter case, in order to keep the variance of imputed values at a realistic level, we suggest to use predictive mean matching on top of the random forest imputations. 

The following example shows how easy such workflow looks like.

``` r
set.seed(35)
irisWithNA <- generateNA(iris, p = c(0, 0.1, 0.1, 0.1, 0.1))

# Generate 100 complete data sets.
filled <- replicate(100, missRanger(irisWithNA, verbose = 0, num.trees = 100, pmm.k = 5), 
                    simplify = FALSE)
                           
# Run a linear model for each of the completed data sets.                           
models <- lapply(filled, function(x) lm(Sepal.Length ~ ., x))

# Pool the results by mice.
require(mice)
summary(pooled_fit <- pool(models))

# Gives
#                     estimate  std.error statistic        df      p.value
# (Intercept)        2.2193643 0.31988528  6.938001 102.35622 3.695848e-10
# Sepal.Width        0.5140635 0.10153973  5.062683  90.06043 2.176258e-06
# Petal.Length       0.7308352 0.08177785  8.936836  82.76528 8.926193e-14
# Petal.Width       -0.2172684 0.19621566 -1.107294  77.54475 2.715899e-01
# Speciesversicolor -0.5470729 0.31218708 -1.752388  65.88007 8.435991e-02
# Speciesvirginica  -0.7368907 0.43294402 -1.702046  64.55998 9.355793e-02

# Compare with model on original data
summary(lm(Sepal.Length ~ ., data = iris))
#                     Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)        2.17127    0.27979   7.760 1.43e-12 ***
#   Sepal.Width        0.49589    0.08607   5.761 4.87e-08 ***
#   Petal.Length       0.82924    0.06853  12.101  < 2e-16 ***
#   Petal.Width       -0.31516    0.15120  -2.084  0.03889 *  
#   Speciesversicolor -0.72356    0.24017  -3.013  0.00306 ** 
#   Speciesvirginica  -1.02350    0.33373  -3.067  0.00258 ** 

# Compare with model on first completed data
summary(lm(Sepal.Length ~ ., data = multiple_data[[1]]))
#                   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        2.20717    0.29016   7.607 3.35e-12 ***
# Sepal.Width        0.55916    0.08488   6.587 7.83e-10 ***
# Petal.Length       0.65024    0.06696   9.711  < 2e-16 ***
# Petal.Width       -0.32854    0.15536  -2.115   0.0362 *  
# Speciesversicolor -0.13651    0.19723  -0.692   0.4899    
# Speciesvirginica  -0.18012    0.27796  -0.648   0.5180  

```
The standard errors and p values of the multiple imputation are larger than of the original data set. This reflects the additional uncertainty introduced by the presence of missing values in a realistic way. In contrast, a linear model on just one imputed data set leads to as small standard errors as the original filled data set, even if 10% of the values were generated by the others. 

## How to deal with censored variables?

There is no obvious way of how to deal with survival variables as covariables in imputation models. 

Options discussed in White [5] include:

- Use both status variable s and (censored) time variable t

- s and log(t)

- surv(t), and, optionally s

By surv(t), we denote the Nelson-Aalen survival estimate at each value of t.

The third option is the most elegant one as it explicitly deals with censoring information. 

### Example

``` r
library(survival)
library(dplyr)
head(veteran)

# For illustration, we use data from a randomized two-arm trial 
# about lung cancer. The aim is to estimate the treatment effect
# of "trt" with reliable inference using Cox regression. Unfortunately, 
# we generated missing values in the covariables "age" and "karno" (performance
# status). One approach is to use multiple imputation, see the section above.
# It is recommended to use the model response in the imputation models - 
# even if it sounds wrong. In case of a censored survival response
# (i.e. consisting of a time/status pair), an elegant 
# possibility is to represent it by the estimated Nelson-Aalen estimates [5].

# Add the Nelson-Aalen survival probabilities "surv" to the data set
veteran2 <- summary(survfit(Surv(time, status) ~ 1, data = veteran), 
                times = veteran$time)[c("time", "surv")] %>% 
            as_tibble %>% 
            right_join(veteran, by = "time")

# Add missing values to some columns. We do not add missing values
# in the survival information as this is usually the response of the (Cox-) 
# modelling process following the imputation.

set.seed(96)
veteran_with_NA <- generateNA(veteran2, p = c(age = 0.1, karno = 0.1, diagtime = 0.1))

# Generate 20 complete data sets and remove "surv"
filled <- replicate(20, missRanger(veteran_with_NA, . ~ . - time - status, 
  verbose = 0, pmm.k = 3, num.trees = 100), simplify = FALSE)

filled <- lapply(filled, function(data) {data$surv <- NULL; data})

# Run a Cox proportional hazards regression for each of the completed data sets
models <- lapply(filled, function(x) coxph(Surv(time, status) ~ ., x))

# Pool the results by mice.
require(mice)
summary(pooled_fit <- pool(models))

#                       estimate   std.error   statistic        df      p.value
# trt                0.220877833 0.156103989  1.41494035 21860.788 1.571002e-01
# celltypesmallcell  0.802075691 0.212546964  3.77363985 97340.728 1.609783e-04
# celltypeadeno      1.072464023 0.234826408  4.56705032 44738.067 4.959525e-06
# celltypelarge      0.241215386 0.239557872  1.00691905 11615.999 3.139946e-01
# karno             -0.034840740 0.004220004 -8.25609188 23839.238 0.000000e+00
# diagtime          -0.001339932 0.005630740 -0.23796730 55730.472 8.119073e-01
# age               -0.005933645 0.007396932 -0.80217651  3224.462 4.225100e-01
# prior              0.001350964 0.018458043  0.07319105 32576.741 9.416545e-01

# On the original
summary(coxph(Surv(time, status) ~ ., veteran))

#                         coef  exp(coef)   se(coef)      z Pr(>|z|)    
# trt                2.946e-01  1.343e+00  2.075e-01  1.419  0.15577    
# celltypesmallcell  8.616e-01  2.367e+00  2.753e-01  3.130  0.00175 ** 
# celltypeadeno      1.196e+00  3.307e+00  3.009e-01  3.975 7.05e-05 ***
# celltypelarge      4.013e-01  1.494e+00  2.827e-01  1.420  0.15574    
# karno             -3.282e-02  9.677e-01  5.508e-03 -5.958 2.55e-09 ***
# diagtime           8.132e-05  1.000e+00  9.136e-03  0.009  0.99290    
# age               -8.706e-03  9.913e-01  9.300e-03 -0.936  0.34920    
# prior              7.159e-03  1.007e+00  2.323e-02  0.308  0.75794

```

## Trick: How to deal with date variables etc.?

`missRanger` natively deals with numeric, logical and character/factor variables. In real-world data sets, also other types of variables appear, e.g. date variables. Since release 2.1.0, such special columns are imputed as well automatically.

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
  missRanger(pmm.k = 5, num.trees = 100)

head(irisImputed$random_date)
# Output: "1998-12-17" "1998-12-18" "1998-12-19" "1999-01-02" "1998-12-21" "1998-12-22"

```

## References
[1]  Wright, M. N. & Ziegler, A. (2016). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. Journal of Statistical Software, in press. http://arxiv.org/abs/1508.04409. 
 
[2]  Stekhoven, D.J. and Buehlmann, P. (2012). MissForest - nonparametric missing value imputation for mixed-type data. Bioinformatics, 28(1), 112-118, doi: 10.1093/bioinformatics/btr597

[3]  Van Buuren, S. and Groothuis-Oudshoorn, K. (2011). mice: Multivariate Imputation by Chained Equations in R. Journal of Statistical Software, 45(3), 1-67. http://www.jstatsoft.org/v45/i03/

[4] Rubin, D.B. (1987). *Multiple Imputation for Nonresponse in Surveys.* New York: John Wiley and Sons.

[5] White IR and Royston P. (2009). Imputing missing covariate values for the Cox model. Statistics in medicine, 28(15), 1982-1998. doi:10.1002/sim.3618