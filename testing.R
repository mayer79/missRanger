#=====================================================================================
#  TESTING THE PACKAGE "missRanger"
#=====================================================================================

library(ranger)
library(FNN)
lapply(list.files("r", full.names = TRUE), source)

#=====================================================================================
#  missRanger
#=====================================================================================

ir <- iris
ir$Species <- as.character(ir$Species)
ir$Species[1:3] <- NA
ir$s <- ir$Species == "setosa"
ir$Sepal.Length[1:3] <- NA

head(missRanger(ir)) 

# Check order of imputation
ir <- iris
for (i in 1:ncol(ir)) {
  ir[1:i, ncol(ir) + 1 - i] <- NA
}
head(ir)
head(missRanger(ir))

ir$Species <- NA
head(missRanger(ir, pmm.k = 5))

ir$Species <- iris$Species
head(missRanger(ir, pmm.k = 3))

n <- 100
X <- data.frame(x1 = seq_len(n), 
                x2 = log(seq_len(n)), 
                x3 = sample(LETTERS[1:3], n, replace = TRUE),
                x4 = factor(sample(LETTERS[1:3], n, replace = TRUE)),
                x5 = seq_len(n) > 50)
head(X)
X_NA <- generateNA(X, p = seq(0, 0.8, by = .2))
head(X_NA)

head(X_imp <- missRanger(X_NA))
head(X_imp <- missRanger(X_NA, pmm = 3))
head(X_imp <- missRanger(X_NA, pmm = 3, verbose = 0))
head(X_imp <- missRanger(X_NA, pmm = 3, verbose = 2, returnOOB = TRUE))
attr(X_imp, "oob")

# The formula interface
head(X_imp <- missRanger(X_NA, x2 ~ x2 + x3, pmm = 3)) # Does not use x3 because it contains NAs
head(X_imp <- missRanger(X_NA, x2 + x3 ~ x2 + x3, pmm = 3)) # If added to the lhs, x3 is used on the rhs.
head(X_imp <- missRanger(X_NA, x2 + x3 ~ 1, pmm = 3)) # Univariate imputation

irisWithNA <- generateNA(iris)
irisImputed <- missRanger(irisWithNA, pmm.k = 3, num.trees = 100)

# With extra trees algorithm
irisImputed_et <- missRanger(irisWithNA, pmm.k = 3, num.trees = 100, 
                             splitrule = "extratrees")
head(irisImputed_et)

# With one single tree
head(missRanger(irisWithNA, pmm.k = 3, num.trees = 1))

# Do not impute Species. Note: Since this variable contains missings, it cannot be used
# to impute the other variables.
head(irisImputed <- missRanger(irisWithNA, . - Species ~ ., pmm.k = 3, num.trees = 100))

# Impute univariately only.
head(irisImputed <- missRanger(irisWithNA, . ~ 1, verbose = 2))

# Use Species and Petal.Length to impute Species and Petal.Length.
head(irisImputed <- missRanger(irisWithNA, Species + Petal.Length ~ Species + Petal.Length,
                          pmm.k = 3, num.trees = 100))

ir <- iris
ir$s <- iris$Species == "setosa"
ir$dt <- seq(Sys.time(), by = "1 min", length.out = 150)
ir$d <- seq(Sys.Date(), by = "1 d", length.out = 150)
ir$ch <- as.character(iris$Species)
ir$fun <- list(mean)
ir <- generateNA(ir, c(rep(0.2, 7), 0, 0, 0.2))
head(m <- missRanger(ir, pmm.k = 4))

# Only one column, but fully missing
ir <- iris
ir$s <- NA

head(missRanger(ir))
ir$Sepal.Length[1] <- NA
missRanger(ir[1:2, ])

di <- ggplot2::diamonds
head(missRanger(di))
head(missRanger(di["color"]))
di <- generateNA(ggplot2::diamonds, p = c(color = 0.2))
head(missRanger(di, num.trees = 10, pmm.k = 5))

head(missRanger(CO2))
head(missRanger(generateNA(CO2)), pmm.k = 1)
head(missRanger(generateNA(CO2, p = c(Type = 0.2))), pmm.k = 1)
head(missRanger(generateNA(CO2, p = c(uptake = 0.7))), pmm.k = 1)
           
#=====================================================================================
#  generateNA
#=====================================================================================

generateNA(1:10, p = 0.5, seed = 3345)
generateNA(rep(Sys.Date(), 10))
generateNA(cbind(1:10, 10:1), p = 0.2)
head(generateNA(iris))
head(generateNA(iris, p = 0.2))
head(generateNA(iris, p = c(0, 1, 0.5, 0.5, 0.5)))
head(generateNA(iris, p = c(Sepal.Length = 1)))
head(generateNA(iris, p = c(Species = 0.2, Sepal.Length = 0.5)))

summary(generateNA(head(iris), p = 0.5))
generateNA(iris[1, ], p = 0.5)
generateNA(iris[1, ], p = 0.55)
generateNA(iris[1:2, ], p = 0.5)
generateNA(iris[1:5, 1], p = 0.55)

head(generateNA(iris, p = c(Species = 0.2, Sepal.Length = 0.5)))
head(generateNA(iris, p = c(Sepal.Length = 1)))

#=====================================================================================
#  imputeUnivariate
#=====================================================================================

imputeUnivariate(generateNA(1:10))
imputeUnivariate(generateNA(rep(TRUE, 10)))
imputeUnivariate(generateNA(rep(Sys.Date(), 10)))
imputeUnivariate(generateNA(cbind(1:10, 1:10), p = 0.5))

imputeUnivariate(generateNA(head(iris), p = 0.5))
imputeUnivariate(generateNA(iris[1, ], p = 0.55)) # Error
imputeUnivariate(generateNA(iris[1:2, ], p = 0.5))
imputeUnivariate(generateNA(iris[1:5, 1], p = 0.55))
imputeUnivariate(c(NA, 1:3))

imputeUnivariate(c(NA, 0, 1, 0, 1))
imputeUnivariate(c("A", "A", NA))
imputeUnivariate(as.factor(c("A", "A", NA)))
imputeUnivariate(cbind(1, NA))

head(imputeUnivariate(generateNA(iris), v = "Species"))
head(imputeUnivariate(generateNA(iris), v = c("Species", "Petal.Length")))
head(imputeUnivariate(generateNA(iris), v = "a"))

head(imputeUnivariate(generateNA(dplyr::as_tibble(iris))))

#=====================================================================================
#  pmm
#=====================================================================================

pmm(xtrain = c(0.2, 0.2, 0.8), xtest = 0.3, ytrain = c(0, 0, 1)) # 0
pmm(xtrain = c(TRUE, FALSE, TRUE), xtest = FALSE, ytrain = c(2, 0, 1)) # 0
pmm(xtrain = c(0.2, 0.8), xtest = 0.3, ytrain = c("A", "B"), k = 2) # "A" or "B"
pmm(xtrain = c("A", "A", "B"), xtest = "A", ytrain = c(2, 2, 4), k = 2) # 2
pmm(xtrain = factor(c("A", "B")), xtest = factor("C"), ytrain = 1:2) # 2
pmm(rep(Sys.Date(), 2), xtest = Sys.Date(), 3:4) # 4
pmm(rep(Sys.time(), 2), xtest = Sys.time(), 3:4) # 4
pmm(xtrain = 1, xtest = 1, ytrain = 1:2) # Error
pmm(1, xtest = Sys.Date(), ytrain = TRUE) # Works!
pmm(1:2, xtest = Sys.Date(), ytrain = c(FALSE, TRUE)) # Works!
pmm(1:10, 3:4, 1:10) # 3 4
pmm(c(NA, 1, 2), 1, ytrain = c(0, 1, NA)) # 1
pmm(c(NA, 1, 2), 1, ytrain = c(0, NA, 3)) # 3
pmm(c(NA, 1, 2), 1, ytrain = c(0, NA, NA)) # Error
pmm(c(NA, 1, 2), NA, ytrain = c(0, NA, 3)) # Error

#=====================================================================================
#  multiple imputation
#=====================================================================================

set.seed(35)
irisWithNA <- generateNA(iris, p = c(0, 0.1, 0.1, 0.1, 0.1))
multiple_data <- replicate(10, missRanger(irisWithNA, verbose = 0, 
                                          num.trees = 100, pmm.k = 5), 
                           simplify = FALSE)
multiple_models <- lapply(multiple_data, function(x) lm(Sepal.Length ~ ., x))

require(mice)
pooled_fit <- pool(multiple_models)
summary(pooled_fit)

# Gives
#                      estimate std.error  statistic       df  p.value
# (Intercept)        2.1628315 0.32342911  6.687189 59.66532 8.723584e-09
# Sepal.Width        0.5322095 0.10212819  5.211191 46.05386 4.301483e-06
# Petal.Length       0.7375857 0.08111293  9.093318 43.48983 1.282330e-11
# Petal.Width       -0.2796666 0.16684665 -1.676190 96.84094 9.692675e-02
# Speciesversicolor -0.4905450 0.30382891 -1.614544 29.82672 1.169405e-01
# Speciesvirginica  -0.6445356 0.41988752 -1.535020 29.55935 1.354145e-01

# Compare with model on original data
summary(lm(Sepal.Length ~ ., data = iris))
#                     Estimate Std. Error t value Pr(>|t|)    
#   (Intercept)        2.17127    0.27979   7.760 1.43e-12 ***
#   Sepal.Width        0.49589    0.08607   5.761 4.87e-08 ***
#   Petal.Length       0.82924    0.06853  12.101  < 2e-16 ***
#   Petal.Width       -0.31516    0.15120  -2.084  0.03889 *  
#   Speciesversicolor -0.72356    0.24017  -3.013  0.00306 ** 
#   Speciesvirginica  -1.02350    0.33373  -3.067  0.00258 ** 

#=====================================================================================
#  convert/revert
#=====================================================================================

revert(convert(head(iris)))
revert(convert(head(iris), check = TRUE))

ir <- head(iris)
ir$s <- ir$Species == "setosa"
ir$dt <- seq(Sys.time(), by = "1 min", length.out = nrow(ir))
ir$d <- seq(Sys.Date(), by = "1 d", length.out = nrow(ir))
ir$ch <- as.character(ir$Species)
ir$fun <- list(mean)
convert(head(ir))$X
convert(head(ir), check = TRUE)$X
str(ir)

# Tibble
ir <- dplyr::as_tibble(ir)
convert(head(ir))$X
convert(head(ir), check = TRUE)$X
revert(convert(head(ir)))
revert(convert(head(ir), check = TRUE))
