#' Missing Values Imputation by Chained Random Forests
#' 
#' @importFrom stats var reformulate predict
#' @importFrom ranger ranger
#'
#' @description Uses the "ranger" package [1] to do fast missing value imputation by chained random forests, see [2] and [3].
#' Between the iterative model fitting, we offer the option of using predictive mean matching. This firstly avoids the imputation
#' with values not present in the original data (like a value 0.3334 in 0-1 coded variable). Secondly, predictive mean
#' matching tries to raise the variance in the resulting conditional distributions to a realistic level.
#' This would allow e.g. to do multiple imputation when repeating the call to "missRanger".
#' @param data A \code{data.frame} with missing values to impute.
#' @param maxiter Maximum number of chaining iterations.
#' @param pmm.k Number of candidate non-missing values to sample from in the predictive mean matching step. 0 to avoid this step.
#' @param seed Integer seed to initialize the random generator.
#' @param ... Arguments passed to \code{ranger}. Don't use \code{formula}, \code{data} or \code{seed}. They are already handled by the algorithm. Not
#'        all \code{ranger} options do make sense (e.g. \code{write.forest = FALSE} will cause the algorithm to crash.
#'        If the data set is large, better use less trees \code{num.trees = 100} and/or a low value of \code{sample.fraction}. 
#'
#' @return A \code{data.frame} as \code{data} but with imputed missing values.
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
#' irisImputed <- missRanger(irisWithNA, pmm.k = 3)
#' head(irisImputed)
#' head(irisWithNA)
#' head(iris)
missRanger <- function(data, maxiter = 10L, pmm.k = 0L, seed = NULL, ...) {
  cat("Missing value imputation by chained random forests")
  
  stopifnot(is.data.frame(data), dim(data) >= 1L, 
            is.numeric(maxiter), length(maxiter) == 1L, maxiter >= 1L,
            is.numeric(pmm.k), length(pmm.k) == 1L, pmm.k >= 0L,
            !(c("formula", "data", "write.forest", "probability", 
              "split.select.weights", "dependent.variable.name",
              "classification") %in% names(list(...))))
  
  set.seed(seed)
  allVars <- names(which(vapply(data, function(z) (is.factor(z) || is.numeric(z)) && any(!is.na(z)), 
                                TRUE)))
  
  if (length(allVars) < ncol(data)) {
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
  
  j <- 1L
  predError <- rep(1, length(visit.seq))
  names(predError) <- visit.seq
  crit <- TRUE
  completed <- setdiff(allVars, visit.seq)
  
  while (crit && j <= maxiter) {
    cat("\n  missRanger iteration ", j, ":", sep = "")
    data.last <- data
    predErrorLast <- predError
    
    for (v in visit.seq) {
      v.na <- data.na[, v]
      
      if (length(completed) == 0L) {
        data[, v] <- imputeUnivariate(data[, v])
      } else {
        fit <- ranger(formula = reformulate(completed, response = v), 
                      data = data[!v.na, union(v, completed)],
                      ...)
        pred <- predict(fit, data[v.na, allVars])$predictions
        data[v.na, v] <- if (pmm.k) pmm(xtrain = fit$predictions, 
                                        xtest = pred, 
                                        ytrain = data[!v.na, v], 
                                        k = pmm.k) else pred
        predError[[v]] <- fit$prediction.error / (if (fit$treetype == "Regression") var(data[!v.na, v]) else 1)
        
        if (is.nan(predError[[v]])) {
          predError[[v]] <- 0
        }
      }
      
      completed <- union(completed, v)
      cat(".")
    }
    
    cat("done")
    j <- j + 1L
    crit <- mean(predError) < mean(predErrorLast)
  }
  
  cat("\n")
  if (j == 2L || (j == maxiter && crit)) data else data.last
}
