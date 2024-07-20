#' Predictive Mean Matching
#'
#' For each value in the prediction vector `xtest`, one of the closest `k`
#' values in the prediction vector `xtrain` is randomly chosen and its observed
#' value in `ytrain` is returned. Note that `xtrain` and `xtest` must be both either
#' numeric, logical, or factor-valued. `ytest` can be of any type.
#'
#' @param xtrain Vector with predicted values in the training data.
#'   Must be numeric, logical, or factor-valued.
#' @param xtest Vector as `xtrain` with predicted values in the test data.
#'   Missing values are not allowed.
#' @param ytrain Vector of the observed values in the training data. Must be of same 
#'   length as `xtrain`.
#' @param k Number of nearest neighbours (donors) to sample from.
#' @param seed Integer random seed.
#' @returns Vector of the same length as `xtest` with values from `xtrain`.
#' @export
#' @examples 
#' pmm(xtrain = c(0.2, 0.3, 0.8), xtest = c(0.7, 0.2), ytrain = 1:3, k = 1)  # c(3, 1)
pmm <- function(xtrain, xtest, ytrain, k = 1L, seed = NULL) {
  stopifnot(
    (is.numeric(xtrain) && is.numeric(xtest)) ||
      (is.factor(xtrain) && is.factor(xtest)) ||
      (is.logical(xtrain) && is.logical(xtest)),
    length(xtrain) == length(ytrain),
    length(xtest) >= 1L,
    !anyNA(xtest),
    k >= 1L
  )
  
  # Filter on complete train data
  ok <- !is.na(xtrain) & !is.na(ytrain)
  if (!any(ok)) {
    stop("'xtrain' and 'ytrain' need at least one complete observation")
  }
  xtrain <- xtrain[ok]
  ytrain <- ytrain[ok]
  
  # Handle trivial case
  u <- unique(ytrain)
  if (length(u) == 1L) {
    return(rep(u, length(xtest)))
  }
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  if (is.factor(xtrain) && !identical(levels(xtrain), levels(xtest))) {
    stop("Incompatible factor levels in 'xtrain' and 'xtest'")  
  }
  
  if (!is.numeric(xtrain)) {
    xtrain <- as.numeric(xtrain)
    xtest <- as.numeric(xtest)
  }
  
  # PMM based on k-nearest neightbour
  k <- min(k, length(xtrain))
  nn <- FNN::knnx.index(xtrain, xtest, k)
  take <- t(stats::rmultinom(length(xtest), 1L, rep(1L, k)))
  
  return(ytrain[rowSums(nn * take)])
}
