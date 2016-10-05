# missRanger: An R package for fast imputation of missing values.

## Description
This package is written to do fast missing value imputation by chained random forests. Between the iterative model fitting, we offer the option of using predictive mean matching to add extra variability and to avoid imputation by values not being present in the original data set.

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

