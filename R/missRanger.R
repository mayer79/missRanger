#' Fast Imputation of Missing Values by Chained Random Forests
#' 
#' Uses the {ranger} package (Wright & Ziegler) to do fast missing value imputation by 
#' chained random forests, see Stekhoven & Buehlmann and Van Buuren & Groothuis-Oudshoorn.
#' Between the iterative model fitting, it offers the option of predictive mean matching. 
#' This firstly avoids imputation with values not present in the original data 
#' (like a value 0.3334 in a 0-1 coded variable). 
#' Secondly, predictive mean matching tries to raise the variance in the resulting 
#' conditional distributions to a realistic level. This allows to do multiple imputation 
#' when repeating the call to \code{missRanger()}. 
#' The iterative chaining stops as soon as \code{maxiter} is reached or if the average 
#' out-of-bag estimate of performance stops improving. 
#' In the latter case, except for the first iteration, the second last (i.e. best) 
#' imputed data is returned.
#' 
#' A note on \code{mtry}: Be careful when passing a non-default \code{mtry} to 
#' \code{ranger()} because the number of available covariates might be growing during 
#' the first iteration, depending on the missing pattern. 
#' Values \code{NULL} (default) and 1 are safe choices. 
#' Additionally, recent versions of \code{ranger()} allow \code{mtry} to be a 
#' single-argument function of the number of available covariables, 
#' e.g. \code{mtry = function(m) max(1, m %/% 3)}.
#' 
#' @param data A \code{data.frame} or \code{tibble} with missing values to impute.
#' @param formula A two-sided formula specifying variables to be imputed 
#' (left hand side) and variables used to impute (right hand side). 
#' Defaults to \code{. ~ .}, i.e., use all variables to impute all variables. 
#' For instance, if all variables (with missings) should be imputed by all variables 
#' except variable "ID", use \code{. ~ . - ID}. Note that a "." is evaluated 
#' separately for each side of the formula. Further note that variables with missings 
#' must appear in the left hand side if they should be used on the right hand side.
#' @param pmm.k Number of candidate non-missing values to sample from in the 
#' predictive mean matching steps. 0 to avoid this step.
#' @param maxiter Maximum number of chaining iterations.
#' @param seed Integer seed to initialize the random generator.
#' @param verbose Controls how much info is printed to screen. 
#' 0 to print nothing. 1 (default) to print a progress bar per iteration, 
#' 2 to print the OOB prediction error per iteration and variable 
#' (1 minus R-squared for regression).
#' Furthermore, if \code{verbose} is positive, the variables used for imputation are 
#' listed as well as the variables to be imputed (in the imputation order). 
#' This will be useful to detect if some variables are unexpectedly skipped.
#' @param returnOOB Logical flag. If TRUE, the final average out-of-bag prediction error
#' is added to the output as attribute "oob". This does not work in the special case 
#' when the variables are imputed univariately.
#' @param case.weights Vector with non-negative case weights.
#' @param ... Arguments passed to \code{ranger::ranger()}. If the data set is large, 
#' better use less trees (e.g. \code{num.trees = 20}) and/or a low value of 
#' \code{sample.fraction}. 
#' The following arguments are incompatible, amongst others: 
#' \code{write.forest}, \code{probability}, \code{split.select.weights}, 
#' \code{dependent.variable.name}, and \code{classification}. 
#'
#' @return An imputed \code{data.frame}.
#' 
#' @references
#' \enumerate{
#'   \item Wright, M. N. & Ziegler, A. (2016). ranger: A Fast Implementation of Random Forests for High Dimensional Data in C++ and R. Journal of Statistical Software, in press. <arxiv.org/abs/1508.04409>.
#'   \item Stekhoven, D.J. and Buehlmann, P. (2012). 'MissForest - nonparametric missing value imputation for mixed-type data', Bioinformatics, 28(1) 2012, 112-118. https://doi.org/10.1093/bioinformatics/btr597.
#'   \item Van Buuren, S., Groothuis-Oudshoorn, K. (2011). mice: Multivariate Imputation by Chained Equations in R. Journal of Statistical Software, 45(3), 1-67. http://www.jstatsoft.org/v45/i03/
#' }
#' @export
#'
#' @examples
#' irisWithNA <- generateNA(iris, seed = 34)
#' irisImputed <- missRanger(irisWithNA, pmm.k = 3, num.trees = 100)
#' head(irisImputed)
#' head(irisWithNA)
missRanger <- function(data, formula = . ~ ., pmm.k = 0L, maxiter = 10L, 
                       seed = NULL, verbose = 1, returnOOB = FALSE, 
                       case.weights = NULL, ...) {
  if (verbose) {
    cat("\nMissing value imputation by random forests\n")
  }
  
  # 1) INITIAL CHECKS
  bad_args <- c(
    "write.forest", 
    "probability", 
    "split.select.weights",  
    "dependent.variable.name", 
    "classification"
  )
  stopifnot(
    "'data' should be a data.frame!" = is.data.frame(data), 
    "'data' should have at least one row and column!" = dim(data) >= 1L, 
    "'formula' should be a formula!" = inherits(formula, "formula"), 
    length(formula <- as.character(formula)) == 3L,
    "'pmm.k' must be numeric!" = is.numeric(pmm.k), 
    "'pmm.k' must be a single number!" = length(pmm.k) == 1L, 
    "'pmm.k' should not be negative!" = pmm.k >= 0L,
    "'maxiter' must be numeric!" = is.numeric(maxiter), 
    "'maxiter' must be a single number!" = length(maxiter) == 1L, 
    "'maxiter' should not be negative!" = maxiter >= 1L,
    "incompatible ranger arguments" = !(bad_args  %in% names(list(...)))
  )
  if (!is.null(case.weights)) {
    stopifnot(
      "Wrong number of 'case.weights'!" = length(case.weights) == nrow(data), 
      "Missing values in 'case.weights'!" = !anyNA(case.weights)
    )
  }
  
  if (!is.null(seed)) {
    set.seed(seed)
  }  
  
  # 2) SELECT AND CONVERT VARIABLES TO IMPUTE
  
  # Extract lhs and rhs from formula
  relevantVars <- lapply(formula[2:3], function(z) attr(stats::terms.formula(
    stats::reformulate(z), data = data[1L, ]), "term.labels"))
  
  # Pick variables from lhs with some but not all missings
  toImpute <- relevantVars[[1L]][vapply(data[, relevantVars[[1L]], drop = FALSE], 
                FUN.VALUE = TRUE, function(z) anyNA(z) && !all(is.na(z)))]
  
  # Try to convert special variables to numeric/factor
  # in order to be safely predicted by ranger
  converted <- convert(data[, toImpute, drop = FALSE], check = TRUE)
  data[, toImpute] <- converted$X
  
  # Remove variables that cannot be safely converted
  visitSeq <- setdiff(toImpute, converted$bad)
  
  if (verbose) {
    cat("\n  Variables to impute:\t\t")
    cat(visitSeq, sep = ", ")
  }
  
  if (!length(visitSeq)) {
    if (verbose) {
      cat("\n")
    }
    return(data)
  }
  
  # Get missing indicators and order variables by number of missings
  dataNA <- is.na(data[, visitSeq, drop = FALSE])
  visitSeq <- names(sort(colSums(dataNA)))
  
  # 3) SELECT VARIABLES USED TO IMPUTE
  
  # Variables on the rhs should either appear in "visitSeq" 
  # or do not contain any missings
  imputeBy <- relevantVars[[2L]][relevantVars[[2L]] %in% visitSeq | 
     !vapply(data[, relevantVars[[2L]], drop = FALSE], anyNA, TRUE)]
  completed <- setdiff(imputeBy, visitSeq)
  
  if (verbose) {
    cat("\n  Variables used to impute:\t")
    cat(imputeBy, sep = ", ")
    cat("\n")
  }

  # 4) IMPUTATION
  
  # Initialization  
  j <- 1L
  crit <- TRUE
  verboseDigits <- 4L
  predError <- stats::setNames(rep(1, length(visitSeq)), visitSeq)
  
  if (verbose >= 2) {
    cat("\n", abbreviate(visitSeq, minlength = verboseDigits + 2L), sep = "\t")
  }
  
  # Looping over iterations and variables to impute
  while (crit && j <= maxiter) {
    if (verbose) {
      if (verbose == 1) {
        i <- 1L
        cat("\n")
        cat(paste("iter", j))
        cat("\n")
        pb <- utils::txtProgressBar(0, length(visitSeq), style = 3)
      } else if (verbose >= 2) {
        cat("\niter ", j, ":\t", sep = "")
      }
    }
    
    dataLast <- data
    predErrorLast <- predError
    
    for (v in visitSeq) {
      v.na <- dataNA[, v]
      
      if (length(completed) == 0L) {
        data[[v]] <- imputeUnivariate(data[[v]])
      } else {
        fit <- ranger::ranger(
          formula = stats::reformulate(completed, response = v),
          data = data[!v.na, union(v, completed), drop = FALSE],
          case.weights = case.weights[!v.na], 
          ...
        )
        pred <- stats::predict(fit, data[v.na, completed, drop = FALSE])$predictions
        data[v.na, v] <- if (pmm.k) pmm(
          xtrain = fit$predictions, xtest = pred, ytrain = data[[v]][!v.na], k = pmm.k
        ) else pred
        predError[[v]] <- fit$prediction.error / (
          if (fit$treetype == "Regression") stats::var(data[[v]][!v.na]) else 1
        )
        
        if (is.nan(predError[[v]])) {
          predError[[v]] <- 0
        }
      }
      
      if (j == 1L && (v %in% imputeBy)) {
        completed <- union(completed, v)
      }
      
      if (verbose) {
        if (verbose == 1) {
          utils::setTxtProgressBar(pb, i)
          i <- i + 1L
        } else if (verbose >= 2) {
          cat(format(round(predError[[v]], verboseDigits), 
                     nsmall = verboseDigits), "\t")  
        }
      }
    }

    j <- j + 1L
    crit <- mean(predError) < mean(predErrorLast)
  }
  
  if (verbose) {
    cat("\n")
  }
  
  if (j == 2L || (j == maxiter && crit)) {
    dataLast <- data
    predErrorLast <- predError
  }
  
  if (returnOOB) {
    attr(dataLast, "oob") <- predErrorLast 
  }
  
  # Revert the conversions
  revert(converted, X = dataLast)
}

#' A version of \code{typeof()} internally used by \code{missRanger()}.
#'
#' Returns either "numeric" (double or integer), "factor", "character", "logical", 
#' "special" (mode numeric, but neither double nor integer) or "" (otherwise).
#' \code{missRanger} requires this information to deal with response types not natively 
#' supported by \code{ranger::ranger()}.
#' 
#' @param object Any object.
#' @return A string.
typeof2 <- function(object) {
  if (is.numeric(object)) "numeric" else
    if (is.factor(object)) "factor" else
      if (is.character(object)) "character" else
        if (is.logical(object)) "logical" else
          if (mode(object) == "numeric") "special" else ""
}  

#' Conversion of non-factor/non-numeric variables.
#'
#' Converts non-factor/non-numeric variables in a data frame to factor/numeric. 
#' Stores information to revert back.
#' 
#' @param X A data frame.
#' @param check If \code{TRUE}, the function checks if the converted columns can be 
#' reverted without changes.
#' @return A list with the following elements: \code{X} is the converted data frame, 
#' \code{vars}, \code{types}, \code{classes} are the names, types and classes of the 
#' converted variables. Finally, \code{bad} names variables in \code{X} that should 
#' have been converted but could not. 
convert <- function(X, check = FALSE) {
  stopifnot(is.data.frame(X))
  
  if (!ncol(X)) {
    return(list(X = X, bad = character(0), vars = character(0), 
                types = character(0), classes = character(0)))
  }
  
  types <- vapply(X, typeof2, FUN.VALUE = "")
  bad <- types == "" | if (check) mapply(function(a, b) 
    isFALSE(all.equal(a, b)), X, revert(convert(X))) else FALSE
  types <- types[!(types %in% c("numeric", "factor") | bad)]
  vars <- names(types)
  classes <- lapply(X[, vars, drop = FALSE], class)
  
  X[, vars] <- lapply(X[, vars, drop = FALSE], function(v) 
    if (is.character(v) || is.logical(v)) as.factor(v) else as.numeric(v))
  
  list(X = X, bad = names(X)[bad], vars = vars, types = types, classes = classes)
}

#' Revert conversion.
#' 
#' Reverts conversions done by \code{convert()}.
#' @param con A list returned by \code{convert()}.
#' @param X A data frame with some columns to be converted back according to the 
#' information stored in \code{converted}.
#' @return A data frame.
revert <- function(con, X = con$X) {
  stopifnot(c("vars", "types", "classes") %in% names(con), is.data.frame(X))
  
  if (!length(con$vars)) {
    return(X)
  }
  
  f <- function(v, ty, cl) {
    switch(
      ty, 
      logical = as.logical(v), 
      character = as.character(v),
      special = {class(v) <- cl; v}, 
      v
    )
  }
  X[, con$vars] <- Map(f, X[, con$vars, drop = FALSE], con$types, con$classes)
  X
}

