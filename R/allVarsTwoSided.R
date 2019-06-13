#' Extraction of Variable Names from Two-Sided Formula.
#'
#' @importFrom stats terms.formula reformulate
#'
#' @description Takes a formula and a data frame and returns all variable names in both the lhs and the rhs. 
#' lrs and rhs are evaluated separately.
#' @param formula A two-sided formula object.
#' @param data A \code{data.frame}.
#'
#' @return A \code{list} with two character vectors of variable names.
#' 
#' @export
#'
#' @examples 
#' allVarsTwoSided(Species + Sepal.Width ~ Petal.Width, iris)
#' allVarsTwoSided(. ~ ., iris)
#' allVarsTwoSided(.-Species ~ Sepal.Width, iris)
#' allVarsTwoSided(. ~ Sepal.Width, iris)
allVarsTwoSided <- function(formula, data) {
  stopifnot(inherits(formula, "formula"), 
            length(formula <- as.character(formula)) == 3L)
  
  lapply(formula[2:3], function(z) attr(terms.formula(
    reformulate(z), data = data), "term.labels"))
}


