#' Adds Missing Values
#'
#' Takes a vector, matrix or `data.frame` and replaces some values by `NA`. 
#' 
#' @param x A vector, matrix or `data.frame`.
#' @param p Proportion of missing values to add to `x`. In case `x` is a `data.frame`, 
#'   `p` can also be a vector of probabilities per column or a named vector.
#' @param seed An integer seed.
#' @returns `x` with missing values.
#' @export
#' @examples 
#' generateNA(1:10, p = 0.5)
#' head(generateNA(iris, p = 0.2))
generateNA <- function(x, p = 0.1, seed = NULL) {
  stopifnot(
    p >= 0,
    p <= 1,
    is.atomic(x) || is.data.frame(x)
  )
  
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

  return(x)
}

