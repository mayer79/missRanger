#' Univariate Imputation
#'
#' Fills missing values of a vector, matrix or data frame by sampling with replacement
#' from the non-missing values. For data frames, this sampling is done within column.
#' 
#' @param x A vector, matrix or data frame.
#' @param v A character vector of column names to impute (only relevant if `x` 
#'   is a data frame). The default `NULL` imputes all columns.
#' @param seed An integer seed.
#' @returns `x` with imputed values.
#' @export
#' @examples
#' imputeUnivariate(c(NA, 0, 1, 0, 1))
#' imputeUnivariate(c("A", "A", NA))
#' imputeUnivariate(as.factor(c("A", "A", NA)))
#' head(imputeUnivariate(generateNA(iris)))
#' head(imputeUnivariate(generateNA(iris), v = "Species"))
#' head(imputeUnivariate(generateNA(iris), v = c("Species", "Petal.Length")))
imputeUnivariate <- function(x, v = NULL, seed = NULL) {
  stopifnot(is.atomic(x) || is.data.frame(x))
  
  if (!is.null(seed)) {
    set.seed(seed)  
  }
  
  imputeVec <- function(z) {
    na <- is.na(z)
    if ((s <- sum(na))) {
      if (s == length(z)) {
        stop("No non-missing elements to sample from.")
      }
      z[na] <- sample(z[!na], s, replace = TRUE)
    }
    z
  }
  
  # vector or matrix
  if (is.atomic(x)) {
    return(imputeVec(x))
  } 
 
  # data frame
  v <- if (is.null(v)) names(x) else intersect(v, names(x))
  x[, v] <- lapply(x[, v, drop = FALSE], imputeVec)

  x
}
  