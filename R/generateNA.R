#' Adds Missing Values to a Vector, Matrix or Data Frame
#'
#' @description Takes a vector, matrix or data frame and replaces some values by NA. 
#' 
#' @param x A vector, matrix or \code{data.frame}.
#' @param p Proportion of missing values to add to \code{x}. 
#'          If \code{x} is a \code{data.frame}, pass a vector 
#'          to use different values for different columns of \code{x}.
#' @param seed An integer seed.
#'
#' @return \code{x} with missing values.
#' 
#' @export
#'
#' @examples 
#' head(generateNA(iris, p = 0.2))
#' head(generateNA(iris, p = c(0, 1, 0.5, 0.5, 0.5)))
#' generateNA(1:10, p = 0.5, seed = 3345)
generateNA <- function(x, p = 0.1, seed = NULL) {
  stopifnot(p >= 0, p <= 1, is.atomic(x) || is.data.frame(x))
  
  if (!is.null(seed)) {
    set.seed(seed)  
  }
  
  generateNaVec <- function(z, p) {
    n <- length(z)
    z[sample(n, p * n)] <- NA
    z
  }
  
  if (is.atomic(x)) {
    return(generateNaVec(x, p))
  } 
  
  x[] <- Map(generateNaVec, x, p)
 
  x
}
