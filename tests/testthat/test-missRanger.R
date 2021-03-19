context("missRanger")

test_that("it works for numeric vectors with specific result", {
  expected <- c(NA, NA, 3L, NA, 5L, NA, 7L, NA, 9L, 10L)
  expect_equal(generateNA(1:10, p = 0.5, seed = 3345), expected)
})


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
ir <- generateNA(ir, c(rep(0.2, 7), 0, 0))
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

# CONVERT/REVERT
