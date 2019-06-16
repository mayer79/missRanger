#' Predictive Mean Matching
#' 
#' @importFrom stats rmultinom
#' @importFrom FNN knnx.index
#'
#' @description For each value in the prediction vector \code{xtest}, 
#' one of the closest \code{k} values in the prediction vector \code{xtrain} 
#' is randomly chosen and its observed value in \code{ytrain} is returned. 
#' 
#' @param xtrain Vector with predicted values in the training data. 
#'        Can be of type logical, numeric, character, or factor.
#' @param xtest Vector with predicted values in the test data.
#' @param ytrain Vector of observed values in the training data.
#' @param k Number of nearest neighbours to sample from.
#' @param seed Integer random seed.
#'
#' @return Vector of the same length as \code{xtest} with values from \code{xtrain}.
#' @export
#'
#' @examples 
#' pmm(xtrain = c(0.2, 0.2, 0.8), xtest = 0.3, ytrain = c(0, 0, 1), k = 1) # 0
#' pmm(xtrain = c(TRUE, FALSE, TRUE), xtest = FALSE, ytrain = c(2, 0, 1), k = 1) # 0
#' pmm(xtrain = c(0.2, 0.8), xtest = 0.3, ytrain = c("A", "B"), k = 2) # "A" or "B"
#' pmm(xtrain = c("A", "A", "B"), xtest = "A", ytrain = c(2, 2, 4), k = 2) # 2
#' pmm(xtrain = factor(c("A", "B")), xtest = factor("C"), ytrain = 1:2, k = 1) # 2
pmm <- function(xtrain, xtest, ytrain, k = 1L, seed = NULL) {
  stopifnot(length(xtrain) >= 1L, length(xtest) >= 1L, 
            length(xtrain) == length(ytrain), 
            mode(xtrain) %in% c("logical", "numeric", "character"),
            mode(xtrain) == mode(xtest),
            k >= 1L)
  
  # Handle trivial case
  if (length(ytrain) == 1L) {
    return(rep(ytrain, length(xtest)))
  }
  
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # STEP 1: Turn xtrain and xtest into numbers.
  
  # Handles the case of inconsistent factor levels of xtrain and xtest.
  if (is.factor(xtrain) && (nlevels(xtrain) != nlevels(xtest) || 
                            !all(levels(xtrain) == levels(xtest)))) {
    xtrain <- as.character(xtrain)
    xtest <- as.character(xtest)
  }
  
  # Turns character vectors into factors.
  if (is.character(xtrain)) {
    lvl <- unique(c(xtrain, xtest))
    xtrain <- factor(xtrain, levels = lvl)
    xtest <- factor(xtest, levels = lvl)
  } 
  
  # Turns everything into numbers.
  if (!is.numeric(xtrain) && mode(xtrain) %in% c("logical", "numeric")) {
    xtrain <- as.numeric(xtrain)
    xtest <- as.numeric(xtest)
  } 
  
  # STEP 2: PMM based on k-nearest neightbour.
  
  k <- min(k, length(xtrain))
  nn <- knnx.index(xtrain, xtest, k)
  take <- t(rmultinom(length(xtest), 1L, rep(1L, k)))
  ytrain[rowSums(nn * take)]
}
