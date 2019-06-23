pkgname <- "missRanger"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "missRanger-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('missRanger')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("allVarsTwoSided")
### * allVarsTwoSided

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: allVarsTwoSided
### Title: Extraction of Variable Names from Two-Sided Formula.
### Aliases: allVarsTwoSided

### ** Examples

allVarsTwoSided(Species + Sepal.Width ~ Petal.Width, iris)
allVarsTwoSided(. ~ ., iris)
allVarsTwoSided(. -Species ~ Sepal.Width, iris)
allVarsTwoSided(. ~ Sepal.Width, iris)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("allVarsTwoSided", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("generateNA")
### * generateNA

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: generateNA
### Title: Adds Missing Values to a Vector, Matrix or Data Frame
### Aliases: generateNA

### ** Examples

head(generateNA(iris, p = 0.2))
head(generateNA(iris, p = c(0, 1, 0.5, 0.5, 0.5)))
generateNA(1:10, p = 0.5, seed = 3345)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("generateNA", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("imputeUnivariate")
### * imputeUnivariate

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: imputeUnivariate
### Title: Univariate Imputation
### Aliases: imputeUnivariate

### ** Examples

imputeUnivariate(c(NA, 0, 1, 0, 1))
imputeUnivariate(c("A", "A", NA))
imputeUnivariate(as.factor(c("A", "A", NA)))
head(imputeUnivariate(generateNA(iris)))



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("imputeUnivariate", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("missRanger")
### * missRanger

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: missRanger
### Title: missRanger
### Aliases: missRanger

### ** Examples

irisWithNA <- generateNA(iris)
irisImputed <- missRanger(irisWithNA, pmm.k = 3, num.trees = 100)
head(irisImputed)
head(irisWithNA)

## Not run: 
##D # With extra trees algorithm
##D irisImputed_et <- missRanger(irisWithNA, pmm.k = 3, num.trees = 100, splitrule = "extratrees")
##D head(irisImputed_et)
##D 
##D # Do not impute Species. Note: Since this variable contains missings, it won't be used
##D # for imputing other variables.
##D head(irisImputed <- missRanger(irisWithNA, . - Species ~ ., pmm.k = 3, num.trees = 100))
##D 
##D # Impute univariately only.
##D head(irisImputed <- missRanger(irisWithNA, . ~ 1))
##D 
##D # Use Species and Petal.Length to impute Species and Petal.Length.
##D head(irisImputed <- missRanger(irisWithNA, Species + Petal.Length ~ Species + Petal.Length, 
##D                                pmm.k = 3, num.trees = 100))
##D                                
##D # Multiple imputation: Fill data 20 times, run 20 analyses and pool their results.
##D require(mice)
##D filled <- replicate(20, missRanger(irisWithNA, verbose = 0, num.trees = 100, pmm.k = 5), 
##D                     simplify = FALSE)
##D models <- lapply(filled, function(x) lm(Sepal.Length ~ ., x))
##D summary(pooled_fit <- pool(models)) # Realistically inflated standard errors and p values
##D 
##D # A data set with logicals, numerics, characters and factors.
##D n <- 100
##D X <- data.frame(x1 = seq_len(n), 
##D                 x2 = log(seq_len(n)), 
##D                 x3 = sample(LETTERS[1:3], n, replace = TRUE),
##D                 x4 = factor(sample(LETTERS[1:3], n, replace = TRUE)),
##D                 x5 = seq_len(n) > 50)
##D head(X)
##D X_NA <- generateNA(X, p = seq(0, 0.8, by = .2))
##D head(X_NA)
##D 
##D head(X_imp <- missRanger(X_NA))
##D head(X_imp <- missRanger(X_NA, pmm = 3))
##D head(X_imp <- missRanger(X_NA, pmm = 3, verbose = 0))
##D head(X_imp <- missRanger(X_NA, pmm = 3, verbose = 2, returnOOB = TRUE))
##D attr(X_imp, "oob") # OOB prediction errors per column.
##D 
##D # The formula interface
##D head(X_imp <- missRanger(X_NA, x2 ~ x2 + x3, pmm = 3)) # Does not use x3 because of NAs
##D head(X_imp <- missRanger(X_NA, x2 + x3 ~ x2 + x3, pmm = 3))
##D head(X_imp <- missRanger(X_NA, x2 + x3 ~ 1, pmm = 3)) # Univariate imputation
## End(Not run)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("missRanger", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("pmm")
### * pmm

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: pmm
### Title: missRanger pmm
### Aliases: pmm

### ** Examples

pmm(xtrain = c(0.2, 0.2, 0.8), xtest = 0.3, ytrain = c(0, 0, 1)) # 0
pmm(xtrain = c(TRUE, FALSE, TRUE), xtest = FALSE, ytrain = c(2, 0, 1)) # 0
pmm(xtrain = c(0.2, 0.8), xtest = 0.3, ytrain = c("A", "B"), k = 2) # "A" or "B"
pmm(xtrain = c("A", "A", "B"), xtest = "A", ytrain = c(2, 2, 4), k = 2) # 2
pmm(xtrain = factor(c("A", "B")), xtest = factor("C"), ytrain = 1:2) # 2



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("pmm", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
