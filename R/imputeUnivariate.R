#' Univariate Imputation
#'
#' @description Fills missing values of a vector, matrix or data frame 
#' by sampling with replacement from the non-missing values. For data frames, 
#' this sampling is done by column.
#' 
#' @param x A vector, matrix or data frame.
#' @param seed An integer seed.
#'
#' @return \code{x} without missing values.
#' @export
#'
#' @examples
#' imputeUnivariate(c(NA, 0, 1, 0, 1))
#' imputeUnivariate(c("A", "A", NA))
#' imputeUnivariate(as.factor(c("A", "A", NA)))
#' 
#' # Impute a whole data set univariately
#' ir <- generateNA(iris)
#' head(imputeUnivariate(ir))
imputeUnivariate <- function(x, seed = NULL) {
  stopifnot(is.atomic(x) || is.data.frame(x))
  
  if (!is.null(seed)) {
    set.seed(seed)  
  }
  
  imputeVec <- function(z) {
    na <- is.na(z)
    if ((s <- sum(na))) {
      z[na] <- sample(z[!na], s, replace = TRUE)
    }
    z
  }
  
  if (is.atomic(x)) {
    return(imputeVec(x))
  } 
 
  x[] <- lapply(x, imputeVec)

  x
}
  