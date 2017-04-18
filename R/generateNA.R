#' Adds Missing Values to a Data Set
#'
#' @description Takes a data frame and replaces randomly part of the values by missing values. This can be useful when testing functions that work with missings.
#' @param data A \code{data.frame}.
#' @param p Proportion of missing values to approximatly add to each column of \code{data}.
#'
#' @return A version of \code{data} with missing values.
#' @export
#'
#' @examples head(generateNA(iris))
generateNA <- function(data, p = 0.1) {
  n <- nrow(data)
  data[] <- lapply(data, function(z) {z[sample(n, floor(n * p))] <- NA; z})
  data
}
