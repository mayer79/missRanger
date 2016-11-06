#' Missing Values Imputation by Chained Random Forests
#'
#' @description Uses the `ranger` package [1] to do fast missing value imputation by chained random forests, see [2] and [3].
#' Between the iterative model fitting, we offer the option of using predictive mean matching. This firstly avoids the imputation
#' with values not present in the original data (like a value 0.3334 in 0-1 coded variable). Secondly, predictive mean
#' matching tries to raise the variance in the resulting conditional distributions to a realistic level.
#' This would allow e.g. to do multiple imputation when repeating the call to `missRanger'.
#' @param data A \code{data.frame} with missing values to impute.
#' @param n.max Maximum size of bootstrap samples in the bagging steps of the random forests.
#' @param maxiter Maximum number of chaining iterations.
#' @param pmm.k Number of candidate non-missing values to sample from in the predictive mean matching step. Use \code{pmm.k = 0} to avoid this step.
#' @param seed Integer seed to initialize the random generator.
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
missRanger <- function(data, n.max = 10000, maxiter = 10, pmm.k = 0, seed = NULL) {
  cat("\nMissing value imputation by chained random forests\n")
  if (!is.null(seed)) {
    set.seed(seed)
  }
  allVars <- names(which(sapply(data, function(z) (is.factor(z) || is.numeric(z)) && any(!is.na(z)))))
  if (length(allVars) < ncol(data)) {
    cat("\n  Variables ignored in imputation: ")
    cat(setdiff(names(data), allVars), sep = ", ")
  }
  stopifnot(length(allVars) > 1)
  data.na <- is.na(data[, allVars, drop = FALSE])

  count.seq <- sort(colMeans(data.na))
  visit.seq <- names(count.seq)[count.seq > 0]
  if (!length(visit.seq)) {
    return(data)
  }
  frac <- min(1, n.max/nrow(data))
  k <- 1
  predError <- rep(1, length(visit.seq))
  names(predError) <- visit.seq
  crit <- TRUE
  completed <- setdiff(allVars, visit.seq)
  while (crit && k <= maxiter) {
    cat("\n  missRanger iteration ", k, ":", sep = "")
    data.last <- data
    predErrorLast <- predError
    for (v in visit.seq) {
      v.na <- data.na[, v]
      if (length(completed) == 0) {
        data[, v] <- imputeUnivariate(data[, v])
      } else {
        #factorHandling <- if (is.numeric(data[, v]) || is.factor(data[, v]) && length(levels(data[, v])) <= 2) "order" else "ignore"
        factorHandling <- "ignore"
        fit <- ranger(reformulate(completed, response = v), data = data[!v.na, union(v, completed)], num.trees = 100,
                      sample.fraction = frac, write.forest = TRUE, respect.unordered.factors = factorHandling)

        pred <- predict(fit, data[v.na, allVars])$predictions
        data[v.na, v] <- if (pmm.k) pmm(fit$predictions, pred, data[!v.na, v], pmm.k) else pred
        predError[[v]] <- fit$prediction.error/(if (fit$treetype == "Regression") var(data[!v.na, v]) else 1)
        if (is.nan(predError[[v]])) {
          predError[[v]] <- 0
        }
      }
      completed <- union(completed, v)
      cat(".")
    }
    cat("done")
    k <- k + 1
    crit <- mean(predError) < mean(predErrorLast)
  }
  cat("\n")
  if (k == maxiter && crit) data else data.last
}
