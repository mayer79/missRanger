#' Univariate Imputation
#'
#' @description Fills missing values of a vector of any type by sampling 
#' with replacement from the non-missing values. Requires at least one 
#' non-missing value to run.
#' 
#' @param x A vector of any type possibly containing missing values.
#'
#' @return A vector of the same length and type as \code{x} but without missing values.
#' @export
#'
#' @examples
#' imputeUnivariate(c(NA, 0, 1, 0, 1))
#' imputeUnivariate(c("A", "A", NA))
#' imputeUnivariate(as.factor(c("A", "A", NA)))
imputeUnivariate <- function(x) {
  stopifnot(is.atomic(x), length(x) >= 1L)
  ok <- !is.na(x)
  if (any(!ok)) {
    x[!ok] <- sample(x[ok], sum(!ok), replace = TRUE)
  }
  x
}
