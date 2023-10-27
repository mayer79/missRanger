#=============================================================================
# Put together the package
#=============================================================================

# WORKFLOW: UPDATE EXISTING PACKAGE
# 1) Modify package content and documentation.
# 2) Increase package number in "use_description" below.
# 3) Go through this script and carefully answer "no" if a "use_*" function
#    asks to overwrite the existing files. Don't just skip that function call.
# devtools::load_all()

library(usethis)

# Sketch of description file
use_description(
  fields = list(
    Title = "Fast Imputation of Missing Values",
    Version = "2.4.0",
    Description = "Alternative implementation of the beautiful 'MissForest' algorithm used to impute 
    mixed-type data sets by chaining random forests, introduced by Stekhoven, D.J. and 
    Buehlmann, P. (2012) <doi:10.1093/bioinformatics/btr597>. Under the hood, it uses the 
    lightning fast random jungle package 'ranger'. Between the iterative model fitting, 
    we offer the option of using predictive mean matching. This firstly avoids imputation 
    with values not already present in the original data (like a value 0.3334 in 0-1 coded variable). 
    Secondly, predictive mean matching tries to raise the variance in the resulting conditional 
    distributions to a realistic level. This would allow e.g. to do multiple imputation when 
    repeating the call to missRanger(). 
    A formula interface allows to control which variables should be imputed by which.",
    `Authors@R` = "person('Michael', 'Mayer', email = 'mayermichael79@gmail.com', role = c('aut', 'cre', 'cph'))",
    Depends = "R (>= 3.5.0)",
    LazyData = NULL
  ),
  roxygen = TRUE
)

use_package("FNN", "imports")
use_package("ranger", "Imports")
use_package("stats", "Imports")
use_package("utils", "Imports")

use_gpl_license(2)

use_github_links() # use this if this project is on github

# Your files that do not belong to the package itself (others are added by "use_* function")
use_build_ignore(c("^packaging.R$", "[.]Rproj$", "^backlog$",
                   "^cran-comments.md$", "^logo.png$"), escape = FALSE)

# If your code uses the pipe operator %>%
# use_pipe()

# If your package contains data. Google how to document
# use_data()

# Add short docu in Markdown (without running R code)
use_readme_md()

# Longer docu in RMarkdown (with running R code). Often quite similar to readme.
use_vignette("missRanger")
use_vignette("multiple_imputation")
use_vignette("working_with_censoring")

# If you want to add unit tests
use_testthat()
# use_test("test-missRanger.R")

# On top of NEWS.md, describe changes made to the package
use_news_md()

# Add logo
use_logo("logo.png")

# If package goes to CRAN: infos (check results etc.) for CRAN
use_cran_comments()

# Github actions
use_github_action("check-standard")
use_github_action("test-coverage")
use_github_action("pkgdown")

# Revdep

use_revdep()

#=============================================================================
# Finish package building (can use fresh session)
#=============================================================================

library(devtools)

document()
test()
# build_vignettes()
check(manual = TRUE, cran = TRUE)
build(vignettes = FALSE)
# build(binary = TRUE)
install()

# Run only if package is public(!) and should go to CRAN
if (FALSE) {
  check_win_devel()
  check_rhub()
  
  # Takes long # devtools::install_github("r-lib/revdepcheck")
  revdepcheck::revdep_check(num_workers = 4L)

  # Wait until above checks are passed without relevant notes/warnings
  # then submit to CRAN
  release()
}
