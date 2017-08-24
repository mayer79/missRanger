#=====================================================================================
# BUILD THE PACKAGE "missRanger"
#=====================================================================================

library(devtools)

stopifnot(basename(getwd()) == "missRanger")

pkg <- "release/missRanger"
unlink(pkg, force = TRUE, recursive = TRUE)
create(pkg, descr = list(
  Title = "missRanger - Imputation by Chained Random Forests",
  Description = "Fast version of the \\code{missForest} package based on \\code{ranger} (a fast implementation of random forests called 'random jungle'). We offer the option of predictive mean matching to add extra variability regarding multiple imputations and to avoid imputation by values not already being present in the data.",
  Version = "0.1.2",
  `Authors@R` = 'person("Michael", "Mayer", email = "mayermichael79@gmail.com", role = c("aut", "cre"))',
  Depends = "R (>= 3.3.2)",
  Imports = list("stats", "FNN", "ranger (>= 0.6.0)"),
  License = "GPL-3"), 
  rstudio = FALSE)

# Add R files
Rfiles <- c("generateNA.R", "imputeUnivariate.R", "missRanger.R", "pmm.R")
stopifnot(file.exists(fp <- file.path("R", Rfiles)))
file.copy(fp, file.path(pkg, "R"))

# Create Rd files
document(pkg)

# Check
check(pkg, document = FALSE, manual = TRUE, check_dir = dirname(normalizePath(pkg)))

# tar and zip file plus check
build(pkg, manual = TRUE) # tar
build(pkg, binary = TRUE) # zip

# Install the package (locally)
install(pkg) # tar

# RESTART RSTUDIO
if (FALSE) {
  library(missRanger)
  
  irisWithNA <- generateNA(iris)
  irisImputed <- missRanger(irisWithNA, pmm.k = 3)
  head(irisImputed)
  head(irisWithNA)
  head(iris)
}
