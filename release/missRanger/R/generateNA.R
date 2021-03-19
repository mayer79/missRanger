#' Adds Missing Values to a Vector, Matrix or Data Frame
#'
#' Takes a vector, matrix or \code{data.frame} and replaces some values by \code{NA}. 
#' 
#' @param x A vector, matrix or \code{data.frame}.
#' @param p Proportion of missing values to add to \code{x}. In case \code{x} is a \code{data.frame}, \code{p} can also be a vector of probabilities per column or a named vector (see examples).
#' @param seed An integer seed.
#'
#' @return \code{x} with missing values.
#' 
#' @export
#'
#' @examples 
#' generateNA(1:10, p = 0.5, seed = 3345)
#' generateNA(rep(Sys.Date(), 10))
#' generateNA(cbind(1:10, 10:1), p = 0.2)
#' head(generateNA(iris))
#' head(generateNA(iris, p = 0.2))
#' head(generateNA(iris, p = c(0, 1, 0.5, 0.5, 0.5)))
#' head(generateNA(iris, p = c(Sepal.Length = 1)))
#' head(generateNA(iris, p = c(Species = 0.2, Sepal.Length = 0.5)))
generateNA <- function(x, p = 0.1, seed = NULL) {
  stopifnot(p >= 0, p <= 1, is.atomic(x) || is.data.frame(x))
  
  if (!is.null(seed)) {
    set.seed(seed)  
  }
  
  generateNaVec <- function(z, p) {
    n <- length(z)
    z[sample(n, round(p * n))] <- NA
    z
  }
  
  # vector or matrix
  if (is.atomic(x)) {
    return(generateNaVec(x, p))
  } 
  
  # data frame
  v <- if (is.null(names(p))) names(x) else intersect(names(p), names(x))
  x[, v] <- Map(generateNaVec, x[, v, drop = FALSE], p)

  x
}
