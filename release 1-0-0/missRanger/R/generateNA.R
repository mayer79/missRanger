#' Adds Missing Values to a Data Set
#'
#' @description Takes a data frame and replaces randomly part of the values by missing values. 
#' 
#' @param data A \code{data.frame}.
#' @param p Proportion of missing values to approximately add to each column of \code{data}.
#' @param seed An integer seed.
#'
#' @return \code{data} with missing values.
#' 
#' @export
#'
#' @examples head(generateNA(iris))
generateNA <- function(data, p = 0.1, seed = NULL) {
  stopifnot(is.numeric(p), length(p) == 1L, p >= 0, p <= 1, 
            is.data.frame(data), (di <- dim(data)) >= 1L)
  
  if (!is.null(seed)) {
    set.seed(seed)  
  }

  v <- sample(c(TRUE, FALSE), prod(di), replace = TRUE, prob = c(p, 1 - p))
  dim(v) <- di
  data[v] <- NA
  data
}

