## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  message = FALSE
)

## ------------------------------------------------------------------------
library(missRanger)
set.seed(84553)

head(iris)

# Generate data with missing values in all columns
head(irisWithNA <- generateNA(iris, p = 0.2))
 
# Impute missing values with missRanger
head(irisImputed <- missRanger(irisWithNA, num.trees = 100))


## ------------------------------------------------------------------------
head(irisImputed <- missRanger(irisWithNA, pmm.k = 3, num.trees = 100))

## ------------------------------------------------------------------------
head(irisImputed_et <- missRanger(irisWithNA, pmm.k = 3, splitrule = "extratrees", num.trees = 50))

## ------------------------------------------------------------------------
require(dplyr)

iris %>% 
  generateNA %>% 
  as_tibble %>% 
  missRanger(verbose = 0) %>% 
  head
  

## ------------------------------------------------------------------------
# Impute all variables with all (default behaviour). Note that variables without
# missing values will be skipped from the left hand side of the formula.
head(m <- missRanger(irisWithNA, formula = . ~ ., pmm.k = 3, num.trees = 10))

# Same
head(m <- missRanger(irisWithNA, pmm.k = 3, num.trees = 10))

# Impute all variables with all except Species
head(m <- missRanger(irisWithNA, . ~ . - Species, pmm.k = 3, num.trees = 10))

# Impute Sepal.Width by Species 
head(m <- missRanger(irisWithNA, Sepal.Width ~ Species, pmm.k = 3, num.trees = 10))

# No success. Why? Species contains missing values and thus can only be used for imputation if it is being imputed as well
head(m <- missRanger(irisWithNA, Sepal.Width + Species ~ Species, pmm.k = 3, num.trees = 10))

# Impute all variables univariatly
head(m <- missRanger(irisWithNA, . ~ 1))


## ------------------------------------------------------------------------
# Count the number of non-missing values per row
non_miss <- rowSums(!is.na(irisWithNA))
table(non_miss)

# No weighting
head(m <- missRanger(irisWithNA, num.trees = 20, pmm.k = 3, seed = 5))

# Weighted by number of non-missing values per row. 
head(m <- missRanger(irisWithNA, num.trees = 20, pmm.k = 3, seed = 5, case.weights = non_miss))


## ------------------------------------------------------------------------
irisWithNA <- generateNA(iris, p = c(0, 0.1, 0.1, 0.1, 0.1))

# Generate 20 complete data sets
filled <- replicate(20, missRanger(irisWithNA, verbose = 0, num.trees = 100, pmm.k = 5), simplify = FALSE)
                           
# Run a linear model for each of the completed data sets                          
models <- lapply(filled, function(x) lm(Sepal.Length ~ ., x))

# Pool the results by mice
require(mice)
summary(pooled_fit <- pool(models))

# Compare with model on original data
summary(lm(Sepal.Length ~ ., data = iris))


## ------------------------------------------------------------------------
require(survival)
require(dplyr)
head(veteran)
set.seed(653)

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

veteran_with_NA <- generateNA(veteran2, p = c(age = 0.1, karno = 0.1, diagtime = 0.1))

# Generate 20 complete data sets and remove "surv"
filled <- replicate(20, missRanger(veteran_with_NA, . ~ . - time - status, 
  verbose = 0, pmm.k = 3, num.trees = 50), simplify = FALSE)

filled <- lapply(filled, function(data) {data$surv <- NULL; data})

# Run a Cox proportional hazards regression for each of the completed data sets
models <- lapply(filled, function(x) coxph(Surv(time, status) ~ ., x))

# Pool the results by mice
require(mice)
summary(pooled_fit <- pool(models))

# On the original
summary(coxph(Surv(time, status) ~ ., veteran))


## ------------------------------------------------------------------------
ir <- iris
ir$s <- iris$Species == "setosa"
ir$dt <- seq(Sys.time(), by = "1 min", length.out = 150)
ir$d <- seq(Sys.Date(), by = "1 d", length.out = 150)
ir$ch <- as.character(iris$Species)
head(ir <- generateNA(ir, c(rep(0.2, 7), 0, 0)))
head(m <- missRanger(ir, pmm.k = 4))


