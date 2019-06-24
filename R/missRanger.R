#' Fast Imputation of Missing Values by Chained Random Forests
#' 
#' @importFrom stats var reformulate predict setNames
#' @importFrom ranger ranger
#'
#' @description Uses the "ranger" package [1] to do fast missing value imputation by chained random forests, see [2] and [3].
#' Between the iterative model fitting, it offers the option of predictive mean matching. 
#' This firstly avoids imputation with values not present in the original data 
#' (like a value 0.3334 in a 0-1 coded variable). Secondly, predictive mean
#' matching tries to raise the variance in the resulting conditional distributions to 
#' a realistic level and, as such, allows to do multiple imputation when repeating the call to missRanger(). The iterative chaining stops as soon as \code{maxiter}
#' is reached or if the average out-of-bag estimate of performance stops improving. In the latter case, except for the first iteration, the second last (i.e. best) imputed data is returned.
#' 
#' @param data A \code{data.frame} or \code{tibble} with missing values to impute.
#' @param formula A two-sided formula specifying variables to be imputed (left hand side) and variables used to impute (right hand side). Defaults to . ~ ., i.e. use all variables to impute all variables. 
#' If e.g. all variables (with missings) should be imputed by all variables except variable "ID", use . ~ . - ID. Note that a "." is evaluated separately for each side of the formula. Further note that variables 
#' with missings must appear in the left hand side if they should be used on the right hand side.
#' @param pmm.k Number of candidate non-missing values to sample from in the predictive mean matching step. 0 to avoid this step.
#' @param maxiter Maximum number of chaining iterations.
#' @param seed Integer seed to initialize the random generator.
#' @param verbose Controls how much info is printed to screen. 0 to print nothing. 1 (default) to print a "." per iteration and variable, 2 to print the OOB prediction error per iteration and variable (1 minus R-squared for regression).
#' Furthermore, if \code{verbose} is positive, the variables used for imputation are listed as well as the variables to be imputed (in the imputation order). This will be usedful to detect if some variables are unexpectedly skipped.
#' @param returnOOB Logical flag. If TRUE, the final average out-of-bag prediction error is added to the output as attribute "oob". This does not work in the special case when the variables are imputed univariately.
#' @param case.weights Vector with non-negative case weights.
#' @param ... Arguments passed to \code{ranger}. If the data set is large, better use less trees (e.g. \code{num.trees = 20}) and/or a low value of \code{sample.fraction}. 
#' The following arguments are incompatible with \code{ranger}: \code{data}, \code{write.forest}, \code{probability}, \code{split.select.weights}, \code{dependent.variable.name}, and \code{classification}. 
#'
#' @return An imputed \code{data.frame}.
#' 
#' @title missRanger
#' @author Michael Mayer
#' 
#' @references
#' [1] Wright, M. N. & Ziegler, A. (2016). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. Journal of Statistical Software, in press. 
#' http://arxiv.org/abs/1508.04409.
#'
#' [2] Stekhoven, D.J. and Buehlmann, P. (2012). 'MissForest - nonparametric missing value imputation for mixed-type data', Bioinformatics, 28(1) 2012, 112-118. 
#' https://doi.org/10.1093/bioinformatics/btr597.
#'
#' [3] Van Buuren, S., Groothuis-Oudshoorn, K. (2011). mice: Multivariate Imputation by Chained Equations in R. Journal of Statistical Software, 45(3), 1-67. 
#' http://www.jstatsoft.org/v45/i03/
#' @export
#'
#' @examples
#' irisWithNA <- generateNA(iris)
#' irisImputed <- missRanger(irisWithNA, pmm.k = 3, num.trees = 100)
#' head(irisImputed)
#' head(irisWithNA)
#'
#' \dontrun{
#' # With extra trees algorithm
#' irisImputed_et <- missRanger(irisWithNA, pmm.k = 3, num.trees = 100, splitrule = "extratrees")
#' head(irisImputed_et)
#' 
#' # Do not impute Species. Note: Since this variable contains missings, it won't be used
#' # for imputing other variables.
#' head(irisImputed <- missRanger(irisWithNA, . - Species ~ ., pmm.k = 3, num.trees = 100))
#' 
#' # Impute univariately only.
#' head(irisImputed <- missRanger(irisWithNA, . ~ 1))
#' 
#' # Use Species and Petal.Length to impute Species and Petal.Length.
#' head(irisImputed <- missRanger(irisWithNA, Species + Petal.Length ~ Species + Petal.Length, 
#'                                pmm.k = 3, num.trees = 100))
#'                                
#' # Multiple imputation: Fill data 20 times, run 20 analyses and pool their results.
#' require(mice)
#' filled <- replicate(20, missRanger(irisWithNA, verbose = 0, num.trees = 100, pmm.k = 5), 
#'                     simplify = FALSE)
#' models <- lapply(filled, function(x) lm(Sepal.Length ~ ., x))
#' summary(pooled_fit <- pool(models)) # Realistically inflated standard errors and p values
#' 
#' # A data set with logicals, numerics, characters and factors.
#' n <- 100
#' X <- data.frame(x1 = seq_len(n), 
#'                 x2 = log(seq_len(n)), 
#'                 x3 = sample(LETTERS[1:3], n, replace = TRUE),
#'                 x4 = factor(sample(LETTERS[1:3], n, replace = TRUE)),
#'                 x5 = seq_len(n) > 50)
#' head(X)
#' X_NA <- generateNA(X, p = seq(0, 0.8, by = .2))
#' head(X_NA)
#' 
#' head(X_imp <- missRanger(X_NA))
#' head(X_imp <- missRanger(X_NA, pmm = 3))
#' head(X_imp <- missRanger(X_NA, pmm = 3, verbose = 0))
#' head(X_imp <- missRanger(X_NA, pmm = 3, verbose = 2, returnOOB = TRUE))
#' attr(X_imp, "oob") # OOB prediction errors per column.
#' 
#' # The formula interface
#' head(X_imp <- missRanger(X_NA, x2 ~ x2 + x3, pmm = 3)) # Does not use x3 because of NAs
#' head(X_imp <- missRanger(X_NA, x2 + x3 ~ x2 + x3, pmm = 3))
#' head(X_imp <- missRanger(X_NA, x2 + x3 ~ 1, pmm = 3)) # Univariate imputation
#' }
missRanger <- function(data, formula = . ~ ., pmm.k = 0L, maxiter = 10L, seed = NULL, 
                       verbose = 1, returnOOB = FALSE, case.weights = NULL, ...) {
  if (verbose) {
    cat("\nMissing value imputation by random forests\n")
  }
  
  # Some initial checks
  stopifnot(is.data.frame(data), dim(data) >= 1L, 
            inherits(formula, "formula"), 
            is.numeric(pmm.k), length(pmm.k) == 1L, pmm.k >= 0L,
            is.numeric(maxiter), length(maxiter) == 1L, maxiter >= 1L,
            !(c("formula", "data", "write.forest", "probability", 
              "split.select.weights", "dependent.variable.name",
              "classification", "case.weights") %in% names(list(...))))
  
  if (!is.null(case.weights)) {
    stopifnot(length(case.weights) == nrow(data), !anyNA(case.weights))
  }
  
  if (!is.null(seed)) {
    set.seed(seed)
  }  
  
  # Select variables to be imputed. We will visit them from few to many missings.
  relevantVars <- allVarsTwoSided(formula, data[1, ])

  ok <- vapply(data[, relevantVars[[1]], drop = FALSE], FUN.VALUE = TRUE,
               function(z) (is.numeric(z) || is.factor(z) || is.character(z) || 
                            is.logical(z)) && anyNA(z) && !all(is.na(z)))  
  data.na <- is.na(data[, relevantVars[[1]][ok], drop = FALSE])
  visit.seq <- names(sort(colSums(data.na)))
  
  # ranger does not allow for logical responses and converts character 
  # responses to character. Here, we convert these types to factors in a way that
  # can be reverted before exiting.
  wasCharacter <- visit.seq[vapply(data[, visit.seq, drop = FALSE], is.character, TRUE)]
  wasLogical <- visit.seq[vapply(data[, visit.seq, drop = FALSE], is.logical, TRUE)]
  if (length(zz <- c(wasCharacter, wasLogical))) {
    data[, zz] <- lapply(data[, zz, drop = FALSE], as.factor)
  }
  
  if (verbose) {
    cat("\n  Variables to impute: ")
    cat(visit.seq, sep = ", ")
  }
  
  # Select inital ("completed") and final ("imputeBy") covariables for the random forests.
  ok <- relevantVars[[2]] %in% visit.seq |
    !vapply(data[, relevantVars[[2]], drop = FALSE], anyNA, TRUE)
  imputeBy <- relevantVars[[2]][ok]
  completed <- setdiff(imputeBy, visit.seq)
  
  if (verbose) {
    cat("\n  Variables used to impute: ")
    cat(imputeBy, sep = ", ")
  }
  
  if (!length(visit.seq)) {
    if (verbose) {
      cat("\n")
    }
    
    return(data)
  }

  # Initialization  
  j <- 1L             # iterator
  crit <- TRUE        # criterion on OOB prediction error to keep iterating
  verboseDigits <- 4L # formatting of OOB prediction errors (if verbose = 2)
  predError <- setNames(rep(1, length(visit.seq)), visit.seq)
  
  if (verbose >= 2) {
    cat("\n", abbreviate(visit.seq, minlength = verboseDigits + 2L), sep = "\t")
  }
  
  # Looping over iterations and variables to impute
  while (crit && j <= maxiter) {
    if (verbose) {
      cat("\niter ", j, ":\t", sep = "")
    }
    data.last <- data
    predErrorLast <- predError
    
    for (v in visit.seq) {
      v.na <- data.na[, v]
      
      if (length(completed) == 0L) {
        data[[v]] <- imputeUnivariate(data[[v]])
      } else {
        fit <- ranger(formula = reformulate(completed, response = v), 
                      data = data[!v.na, union(v, completed), drop = FALSE],
                      case.weights = case.weights[!v.na],
                      ...)
        pred <- predict(fit, data[v.na, completed, drop = FALSE])$predictions
        data[v.na, v] <- if (pmm.k) pmm(xtrain = fit$predictions, 
                                        xtest = pred, 
                                        ytrain = data[[v]][!v.na], 
                                        k = pmm.k) else pred
        predError[[v]] <- fit$prediction.error / (if (fit$treetype == "Regression") var(data[[v]][!v.na]) else 1)
        
        if (is.nan(predError[[v]])) {
          predError[[v]] <- 0
        }
      }
      
      if (j == 1L && (v %in% imputeBy)) {
        completed <- union(completed, v)
      }
      
      if (verbose == 1) {
        cat(".")
      } else if (verbose >= 2) {
        cat(format(round(predError[[v]], verboseDigits), nsmall = verboseDigits), "\t")
      }
    }

    j <- j + 1L
    crit <- mean(predError) < mean(predErrorLast)
  }
  
  if (verbose) {
    cat("\n")
  }
  
  if (j == 2L || (j == maxiter && crit)) {
    data.last <- data
    predErrorLast <- predError
  }
  
  if (returnOOB) {
    attr(data.last, "oob") <- predErrorLast 
  }
  
  # Undo the conversions from character and logical to factors.
  if (length(wasCharacter)) {
    data.last[, wasCharacter] <- lapply(data.last[, wasCharacter, drop = FALSE], as.character)
  }
  if (length(wasLogical)) {
    data.last[, wasLogical] <- lapply(data.last[, wasLogical, drop = FALSE], as.logical)
  }
  
  data.last
}

