#=====================================================================================
#  TESTING THE PACKAGE "missRanger"
#=====================================================================================

library(ranger)
library(FNN)
lapply(list.files("r", full.names = TRUE), source)

#=====================================================================================
#  Examples
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
  ir[i, 1:i] <- NA
}
head(ir)
head(missRanger(ir))

ir$Species <- NA
head(missRanger(ir, pmm.k = 5))

ir$Species <- iris$Species
head(missRanger(ir, pmm.k = 3))


#=====================================================================================
#  generateNA
#=====================================================================================

generateNA(1:10, seed = 3)
generateNA(1:10, seed = 3)
generateNA(rep(TRUE, 10))
generateNA(rep(Sys.Date(), 10))
generateNA(cbind(1:10, 1:10), p = 0.5)

summary(generateNA(head(iris), p = 0.5))
generateNA(iris[1, ], p = 0.5)
generateNA(iris[1, ], p = 0.55)
generateNA(iris[1:2, ], p = 0.5)
generateNA(iris[1:5, 1], p = 0.55)

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

#=====================================================================================
#  allVarsTwoSided
#=====================================================================================

allVarsTwoSided(Species + Sepal.Width ~ Petal.Width, iris)
allVarsTwoSided(. ~ ., iris)
allVarsTwoSided(. -Species ~ Sepal.Width, iris)
allVarsTwoSided(. ~ Sepal.Width, iris)
allVarsTwoSided(. ~ 1, iris)
allVarsTwoSided(1 ~ Species, iris)
allVarsTwoSided(1 ~ 1) # Error
allVarsTwoSided(log(SepalLength~.), data = iris) # Error
