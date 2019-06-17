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

