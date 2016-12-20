#' Predictive Mean Matching
#' 
#' @importFrom stats rmultinom
#'
#' @description This function is used internally only but might help others to implement an efficient way of doing predictive mean matching on top of any prediction based missing value imputation. It works as follows:
#' For each predicted value of a vector \code{xtest}, the closest \code{k} predicted values of another vector \code{xtrain} are identified by k-nearest neighbour. Then, one of those neighbours are randomly picked and its corresponding observed value in \code{ytrain} is returned.
#' @param xtrain Vector with predicted values in the training data set.
#' @param xtest Vector with predicted values in the test data set.
#' @param ytrain Vector with observed response in the training data set.
#' @param k Number of nearest neighbours to choose from. Set \code{k = 0} if no predictive mean matching is to be done.
#'
#' @return Vector with predicted values in the test data set based on preditive mean matching.
#' @export
#'
#' @examples pmm(xtrain = c(0.2, 0.2, 0.8), xtest = 0.3, ytrain = c(0, 0, 1), k = 1)
pmm <- function(xtrain, xtest, ytrain, k = 1) {
  stopifnot(length(xtrain) == length(ytrain), k >= 1L)
  if (is.factor(xtrain) || is.character(xtrain)) {
    xtrain <- as.character(xtrain)
    xtest  <- as.character(xtest)
    lvl <- levels(factor(c(xtrain, xtest)))
    xtrain <- as.numeric(factor(xtrain, levels = lvl))
    xtest <- as.numeric(factor(xtest, levels = lvl))
  }
  k <- min(k, length(xtrain))
  nn <- FNN::knnx.index(data.frame(x = xtrain), data.frame(x = xtest), k)
  take <- t(rmultinom(length(xtest), 1, rep(1, k)))
  ytrain[rowSums(nn*take)]
}
