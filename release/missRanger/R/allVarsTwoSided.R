#' Extraction of Variable Names from Two-Sided Formula.
#'
#' @importFrom stats terms.formula reformulate
#'
#' @description Takes a formula and a data frame and returns all variable names in both the left hand side and the right hand side. 
#' Dots (".") are evaluated separately within both sides of the formula. Functions like "log" etc. are not supported.
#' 
#' @author Michael Mayer
#' 
#' @param formula A two-sided formula object.
#' @param data A \code{data.frame} used to evaluate any "." appearing in the formula.
#'
#' @return A \code{list} with two character vectors of variable names.
#' 
#' @export
#'
#' @examples 
#' allVarsTwoSided(Species + Sepal.Width ~ Petal.Width, iris)
#' allVarsTwoSided(. ~ ., iris)
#' allVarsTwoSided(. -Species ~ Sepal.Width, iris)
#' allVarsTwoSided(. ~ Sepal.Width, iris)
allVarsTwoSided <- function(formula, data) {
  stopifnot(inherits(formula, "formula"), 
            length(formula <- as.character(formula)) == 3L,
            is.data.frame(data))
  
  lapply(formula[2:3], function(z) attr(terms.formula(
    reformulate(z), data = data), "term.labels"))
}


