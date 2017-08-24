# missRanger: An R package for fast imputation of missing values.
 
## Description
 
This package uses the `ranger` package [1] to do fast missing value imputation by chained tree ensembles, see [2] and [3]. 
Between the iterative model fitting, we offer the option of using predictive mean matching. This firstly avoids the 
imputation with values not present in the original data (like a value 0.3334 in 0-1 coded variable). Secondly, predictive 
mean matching tries to raise the variance in the resulting conditional distributions to a realistic level. This would allow 
e.g. to do multiple imputation when repeating the call to `missRanger`. 

Please check the help `?missRanger` for how to call the function and to see all options. 
The below example first generates a data set with about 10% missing values in each column. 
Then those gaps are filled by `missRanger`. In the end, the resulting data frame is displayed.
 
## Usage
```
library(missRanger)
 
# Generate data with missing values in all columns
irisWithNA <- generateNA(iris)
 
# Impute missing values with missRanger
irisImputed <- missRanger(irisWithNA, pmm.k = 3)
 
# Check results
head(irisImputed)
head(irisWithNA)
head(iris)
```
## Changes in Version 0.1.3
Now uses defaults of `ranger`. They can be changed by passing additional arguments to `missRanger`:
```
# Impute by chained extra trees, each with 200 trees.
irisWithNA <- generateNA(iris)
head(irisImputed <- missRanger(irisWithNA, pmm.k = 3, num.trees = 200, splitrule = "extratrees"))
```

Hint: If the data set is larger than a few hundreds lines, better decrease `num.trees` and/or `sample.fraction`.
## Installation
Just install from github through `devtools'.
```
# install.packages(devtools)
library(devtools) 
install_github("mayer79/missRanger") 
```
 
## References
[1]  Wright, M. N. & Ziegler, A. (2016). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. Journal of Statistical Software, in press. http://arxiv.org/abs/1508.04409. 
 
[2]  Stekhoven, D.J. and Buehlmann, P. (2012), 'MissForest - nonparametric missing value imputation for mixed-type data', Bioinformatics, 28(1) 2012, 112-118, doi: 10.1093/bioinformatics/btr597

[3]  Van Buuren, S., Groothuis-Oudshoorn, K. (2011). mice: Multivariate Imputation by Chained Equations in R. Journal of Statistical Software, 45(3), 1-67. http://www.jstatsoft.org/v45/i03/