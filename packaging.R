#=====================================================================================
# BUILD THE PACKAGE "missRanger"
#=====================================================================================

library(devtools)

stopifnot(basename(getwd()) == "missRanger")

pkg <- "release/missRanger"
unlink(pkg, force = TRUE, recursive = TRUE)
create(pkg, descr = list(
            Title = "Fast Imputation of Missing Values",
            Type = "Package",
            Version = "1.0.2",
            Date = Sys.Date(),
            Description = "Alternative implementation of the beautiful 'MissForest' algorithm used to impute mixed-type data sets by chaining tree ensembles, introduced by Stekhoven, D.J. and Buehlmann, P. (2012) <doi:10.1093/bioinformatics/btr597>. Under the hood, it uses the lightning fast random jungle package 'ranger'. Between the iterative model fitting, we offer the option of using predictive mean matching. This firstly avoids imputation with values not already present in the original data (like a value 0.3334 in 0-1 coded variable). Secondly, predictive mean matching tries to raise the variance in the resulting conditional distributions to a realistic level. This would allow e.g. to do multiple imputation when repeating the call to missRanger().",
            
            `Authors@R` = "person('Michael', 'Mayer', email = 'mayermichael79@gmail.com', role = c('aut', 'cre', 'cph'))",
            Depends = "R (>= 3.4.0)",
            Imports = list("stats", "FNN (>= 1.1)", "ranger (>= 0.7.0)"),
            License = "GPL(>= 2)",
            Author = "Michael Mayer [aut, cre, cph]",
            Maintainer = "Michael Mayer <mayermichael79@gmail.com>"), 
  rstudio = FALSE)

# Add R files
Rfiles <- c("generateNA.R", "imputeUnivariate.R", "missRanger.R", "pmm.R")
stopifnot(file.exists(fp <- file.path("R", Rfiles)))
file.copy(fp, file.path(pkg, "R"))

# Create Rd files
document(pkg)

# Add further files
# devtools::use_cran_comments(pkg) (is required)
mdfiles <- c("NEWS.md", "README.md")
stopifnot(file.exists(mdfiles))
file.copy(mdfiles, pkg)

# Check
check(pkg, document = FALSE, manual = TRUE, check_dir = dirname(normalizePath(pkg)))

# tar and zip file plus check
build(pkg, manual = TRUE) # tar
# build(pkg, binary = TRUE) # zip

# Install the package (locally)
install(pkg) # tar

devtools::release(pkg)


# RESTART RSTUDIO
if (FALSE) {
  library(missRanger)
  
  irisWithNA <- generateNA(iris)
  irisImputed <- missRanger(irisWithNA, pmm.k = 3, num.trees = 100, verbose = 1)
  head(irisImputed)
  head(irisWithNA)
  head(iris)
}
