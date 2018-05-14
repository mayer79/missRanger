#' Fast Imputation of Missing Values by Chained Tree Ensembles
#' 
#' @importFrom stats var reformulate predict
#' @importFrom ranger ranger
#'
#' @description Uses the "ranger" package [1] to do fast missing value imputation by chained tree ensembles, see [2] and [3].
#' Between the iterative model fitting, it offers the option of predictive mean matching. This firstly avoids imputation
#' with values not present in the original data (like a value 0.3334 in a 0-1 coded variable). Secondly, predictive mean
#' matching tries to raise the variance in the resulting conditional distributions to a realistic level and, as such, 
#' allows to do multiple imputation when repeating the call to missRanger(). The iterative chaining stops as soon as \code{maxiter}
#' is reached or if the average out-of-bag estimate of performance stops improving. In the latter case, except for the first iteration,
#' the second last (i.e. best) imputed data is returned.
#' @param data A \code{data.frame} with missing values to impute.
#' @param maxiter Maximum number of chaining iterations.
#' @param pmm.k Number of candidate non-missing values to sample from in the predictive mean matching step. 0 to avoid this step.
#' @param seed Integer seed to initialize the random generator.
#' @param verbose Controls how much info is printed to screen. 0 to print nothing. 1 (default) to print a "." per iteration and 
#'                variable, 2 to print the OOB prediction error per iteration and variable (1 minus R-squared for regression).
#' @param ... Arguments passed to \code{ranger}. If the data set is large, better use less trees 
#' (e.g. \code{num.trees = 100}) and/or a low value of \code{sample.fraction}. 
#' The following arguments are incompatible: \code{formula}, \code{data}, \code{write.forest}, 
#' \code{probability}, \code{split.select.weights}, \code{dependent.variable.name}, and \code{classification}. 
#'
#' @return An imputed \code{data.frame}.
#' @references
#' [1] Wright, M. N. & Ziegler, A. (2016). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. Journal of Statistical Software, in press. http://arxiv.org/abs/1508.04409.
#'
#' [2] Stekhoven, D.J. and Buehlmann, P. (2012). 'MissForest - nonparametric missing value imputation for mixed-type data', Bioinformatics, 28(1) 2012, 112-118, doi: 10.1093/bioinformatics/btr597
#'
#' [3] Van Buuren, S., Groothuis-Oudshoorn, K. (2011). mice: Multivariate Imputation by Chained Equations in R. Journal of Statistical Software, 45(3), 1-67. http://www.jstatsoft.org/v45/i03/
#' @export
#'
#' @examples
#' irisWithNA <- generateNA(iris)
#' irisImputed <- missRanger(irisWithNA, pmm.k = 3, num.trees = 100)
#' head(irisImputed)
#' head(irisWithNA)
#'
#' # With extra trees algorithm
#' irisImputed_et <- missRanger(irisWithNA, pmm.k = 3, num.trees = 100, splitrule = "extratrees")
#' head(irisImputed_et)
missRanger <- function(data, maxiter = 10L, pmm.k = 0L, seed = NULL, verbose = 1, ...) {
  if (verbose > 0) {
    cat("\nMissing value imputation by chained tree ensembles\n")
  }
  
  stopifnot(is.data.frame(data), dim(data) >= 1L, 
            is.numeric(maxiter), length(maxiter) == 1L, maxiter >= 1L,
            is.numeric(pmm.k), length(pmm.k) == 1L, pmm.k >= 0L,
            !(c("formula", "data", "write.forest", "probability", 
              "split.select.weights", "dependent.variable.name",
              "classification") %in% names(list(...))))
  
  if (!is.null(seed)) {
    set.seed(seed)
  }  
  
  allVars <- names(which(vapply(data, function(z) (is.factor(z) || is.numeric(z)) && any(!is.na(z)), TRUE)))
  
  if (verbose > 0 && length(allVars) < ncol(data)) {
    cat("\n  Variables ignored in imputation (wrong data type or all values missing: ")
    cat(setdiff(names(data), allVars), sep = ", ")
  }
  
  stopifnot(length(allVars) > 1L)
  data.na <- is.na(data[, allVars, drop = FALSE])
  count.seq <- sort(colMeans(data.na))
  visit.seq <- names(count.seq)[count.seq > 0]
  
  if (!length(visit.seq)) {
    return(data)
  }
  
  verboseDigits <- 4  # prediction of OOB prediction errors (if verbose = 2)
  j <- 1L             # iterator
  predError <- rep(1, length(visit.seq))
  names(predError) <- visit.seq
  crit <- TRUE        # criterion on OOB prediction error to keep iterating
  completed <- setdiff(allVars, visit.seq)
  
  if (verbose >= 2) {
    cat("\n", abbreviate(visit.seq, minlength = verboseDigits + 2), sep = "\t")
  }
  while (crit && j <= maxiter) {
    if (verbose > 0) {
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
                      data = data[!v.na, union(v, completed)],
                      ...)
        pred <- predict(fit, data[v.na, allVars])$predictions
        data[v.na, v] <- if (pmm.k) pmm(xtrain = fit$predictions, 
                                        xtest = pred, 
                                        ytrain = data[[v]][!v.na], 
                                        k = pmm.k) else pred
        predError[[v]] <- fit$prediction.error / (if (fit$treetype == "Regression") var(data[!v.na, v]) else 1)
        
        if (is.nan(predError[[v]])) {
          predError[[v]] <- 0
        }
      }
      
      completed <- union(completed, v)
      if (verbose == 1) {
        cat(".")
      } else if (verbose >= 2) {
        cat(format(round(predError[[v]], verboseDigits), nsmall = verboseDigits), "\t")

      }
    }

    j <- j + 1L
    crit <- mean(predError) < mean(predErrorLast)
  }
  
  if (verbose > 0) {
    cat("\n")
  }
  if (j == 2L || (j == maxiter && crit)) data else data.last
}
