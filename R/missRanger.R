#' Fast Imputation of Missing Values by Chained Random Forests
#' 
#' @description
#' Uses the "ranger" package (Wright & Ziegler) to do fast missing value imputation by 
#' chained random forests, see Stekhoven & Buehlmann and Van Buuren & Groothuis-Oudshoorn.
#' Between the iterative model fitting, it offers the option of predictive mean matching. 
#' This firstly avoids imputation with values not present in the original data 
#' (like a value 0.3334 in a 0-1 coded variable). 
#' Secondly, predictive mean matching tries to raise the variance in the resulting 
#' conditional distributions to a realistic level. This allows to do multiple imputation 
#' when repeating the call to [missRanger()]. 
#' 
#' @details
#' The iterative chaining stops as soon as `maxiter` is reached or if the average 
#' out-of-bag (OOB) prediction errors stop reducing. 
#' In the latter case, except for the first iteration, the second last (= best) 
#' imputed data is returned.
#' 
#' OOB prediction errors are quantified as 1 - R^2 for numeric variables, and as 
#' classification error otherwise. If a variable has been imputed only univariately,
#' the value is 1.
#' 
#' A note on `mtry`: Be careful when passing a non-default `mtry` to 
#' [ranger::ranger()] because the number of available covariates might be growing during 
#' the first iteration, depending on the missing pattern. 
#' Values `NULL` (default) and 1 are safe choices. 
#' Additionally, recent versions of [ranger::ranger()] allow `mtry` to be a 
#' single-argument function of the number of available covariables, 
#' e.g., `mtry = function(m) max(1, m %/% 3)`.
#' 
#' @param data A `data.frame` with missing values to impute.
#' @param formula A two-sided formula specifying variables to be imputed 
#'   (left hand side) and variables used to impute (right hand side). 
#'   Defaults to `. ~ .`, i.e., use all variables to impute all variables. 
#'   For instance, if all variables (with missings) should be imputed by all variables 
#'   except variable "ID", use `. ~ . - ID`. Note that a "." is evaluated 
#'   separately for each side of the formula. Further note that variables with missings 
#'   must appear in the left hand side if they should be used on the right hand side.
#' @param pmm.k Number of candidate non-missing values to sample from in the 
#'   predictive mean matching steps. 0 to avoid this step.
#' @param maxiter Maximum number of chaining iterations.
#' @param seed Integer seed to initialize the random generator.
#' @param verbose Controls how much info is printed to screen. 
#'   0 to print nothing. 1 (default) to print a progress bar per iteration, 
#'   2 to print the OOB prediction error per iteration and variable 
#'   (1 minus R-squared for regression).
#'   Furthermore, if `verbose` is positive, the variables used for imputation are 
#'   listed as well as the variables to be imputed (in the imputation order). 
#'   This will be useful to detect if some variables are unexpectedly skipped.
#' @param returnOOB Logical flag. If TRUE, the final average out-of-bag prediction 
#'   errors per variable is added to the resulting data as attribute "oob". 
#'   Only relevant when `data_only = TRUE` (and when forests are grown).
#' @param case.weights Vector with non-negative case weights.
#' @param data_only If `TRUE` (default), only the imputed data is returned.
#'   Otherwise, a "missRanger" object with additional information is returned.
#' @param keep_forests Should the random forests of the final imputations
#'   be returned? The default is `FALSE`. Setting this option will use a lot of memory.
#'   Only relevant when `data_only = TRUE` (and when forests are grown).
#' @param ... Arguments passed to [ranger::ranger()]. If the data set is large, 
#'   better use less trees (e.g. `num.trees = 20`) and/or a low value of 
#'   `sample.fraction`. The following arguments are incompatible, amongst others: 
#'   `write.forest`, `probability`, `split.select.weights`, 
#'   `dependent.variable.name`, and `classification`. 
#' @returns 
#'   If `data_only` an imputed `data.frame`. Otherwise, a "missRanger" object with
#'   the following elements that can be extracted via `$`:
#'   - `data`: The imputed data.
#'   - `forests`: When `keep_forests = TRUE`, a list of "ranger" models used to 
#'     generate the imputed data. `NULL` otherwise.
#'   - `visit_seq`: Variables to be imputed (in this order).
#'   - `impute_by`: Variables used for imputation.
#'   - `best_iter`: Best iteration.
#'   - `pred_errors`: Per-iteration OOB prediction errors (1 - R^2 for regression,
#'     classification error otherwise).
#'   - `mean_pred_errors`: Per-iteration averages of OOB prediction errors.
#'   
#' @references
#'   1. Wright, M. N. & Ziegler, A. (2016). ranger: A Fast Implementation of 
#'     Random Forests for High Dimensional Data in C++ and R. Journal of Statistical 
#'     Software, in press. <arxiv.org/abs/1508.04409>.
#'   2. Stekhoven, D.J. and Buehlmann, P. (2012). 'MissForest - nonparametric missing 
#'     value imputation for mixed-type data', Bioinformatics, 28(1) 2012, 112-118. 
#'     https://doi.org/10.1093/bioinformatics/btr597.
#'   3. Van Buuren, S., Groothuis-Oudshoorn, K. (2011). mice: Multivariate Imputation 
#'     by Chained Equations in R. Journal of Statistical Software, 45(3), 1-67. 
#'     http://www.jstatsoft.org/v45/i03/
#' @export
#' @examples
#' irisWithNA <- generateNA(iris, seed = 34)
#' irisImputed <- missRanger(irisWithNA, pmm.k = 3, num.trees = 100)
#' head(irisImputed)
#' head(irisWithNA)
#' 
#' # Extended output
#' imp <- missRanger(irisWithNA, pmm.k = 3, num.trees = 100, data_only = FALSE)
#' head(imp$data)
#' imp$pred_errors
#' 
#' # If you even want to keep the random forests of the best iteration
#' imp <- missRanger(
#'   irisWithNA, pmm.k = 3, num.trees = 100, data_only = FALSE, keep_forests = TRUE
#' )
#' imp$forests$Species
#' imp$forests$Sepal.Width
#' imp$pred_errors[imp$best_iter, "Sepal.Width"]  # 1 - R-squared
missRanger <- function(data, formula = . ~ ., pmm.k = 0L, maxiter = 10L, 
                       seed = NULL, verbose = 1, returnOOB = FALSE, case.weights = NULL, 
                       data_only = TRUE, keep_forests = FALSE, ...) {
  if (verbose) {
    cat("\nMissing value imputation by random forests\n")
  }
  
  # 1) INITIAL CHECKS
  bad_args <- c(
    "write.forest", "probability", "split.select.weights",  
    "dependent.variable.name", "classification"
  )
  stopifnot(
    "'data' should be a data.frame!" = is.data.frame(data), 
    "'data' should have at least one row and column!" = dim(data) >= 1L, 
    "'formula' should be a formula!" = inherits(formula, "formula"), 
    "Don't load {formula.tools}. It breaks base R's as.character()" = 
      length(formula <- as.character(formula)) == 3L,
    "'pmm.k' should not be negative!" = pmm.k >= 0L,
    "'maxiter' should be a positiv number!" = maxiter >= 1L,
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
  parsef <- function(z) {
    if (z == ".") {
      return(colnames(data))
    }
    all.vars(stats::terms.formula(stats::reformulate(z), data = data[1L, ]))
  }
  relevant_vars <- lapply(formula[2:3], parsef)
  
  # Pick variables from lhs with some but not all missings
  pick <- vapply(
    data[, relevant_vars[[1L]], drop = FALSE], 
    FUN = function(z) anyNA(z) && !all(is.na(z)),
    FUN.VALUE = TRUE
  )
  to_impute <- relevant_vars[[1L]][pick]
  
  # Try to convert special variables to numeric/factor
  # in order to be safely predicted by ranger
  converted <- convert(data[, to_impute, drop = FALSE], check = TRUE)
  data[, to_impute] <- converted$X
  
  # Remove variables that cannot be safely converted
  visit_seq <- setdiff(to_impute, converted$bad)
  
  if (verbose) {
    cat("\n  Variables to impute:\t\t")
    cat(visit_seq, sep = ", ")
  }
  
  if (!length(visit_seq)) {
    if (verbose) {
      cat("\n")
    }
    if (data_only) {
      return(data) 
    } else {
      out <- structure(
        list(
          data = data,
          forests = NULL,
          visit_seq = c(),
          impute_by = c(),
          best_iter = 0L,
          pred_errors = NULL,
          mean_pred_errors = NULL
        ), 
        class = "missRanger"
      )  
      return(out)
    }
  }
  
  # Get missing indicators and order variables by number of missings
  data_NA <- is.na(data[, visit_seq, drop = FALSE])
  visit_seq <- names(sort(colSums(data_NA)))
  
  # 3) SELECT VARIABLES USED TO IMPUTE
  
  # Variables on the rhs should either appear in "visit_seq" 
  # or do not contain any missings
  impute_by <- relevant_vars[[2L]][relevant_vars[[2L]] %in% visit_seq | 
     !vapply(data[, relevant_vars[[2L]], drop = FALSE], anyNA, TRUE)]
  completed <- setdiff(impute_by, visit_seq)
  
  if (verbose) {
    cat("\n  Variables used to impute:\t")
    cat(impute_by, sep = ", ")
    cat("\n")
  }

  # 4) IMPUTATION
  
  # Initialization  
  j <- 1L
  crit <- TRUE
  dig <- 4L
  pred_error <- stats::setNames(rep(1, length(visit_seq)), visit_seq)
  pred_errors <- list()
  if (keep_forests) {
    forests <- list()
  }
  
  if (verbose >= 2) {
    cat("\n", abbreviate(visit_seq, minlength = dig + 2L), sep = "\t")
  }
  
  # Looping over iterations and variables to impute
  while (crit && j <= maxiter) {
    if (verbose) {
      if (verbose == 1) {
        i <- 1L
        cat("\n")
        cat(paste("iter", j))
        cat("\n")
        pb <- utils::txtProgressBar(0, length(visit_seq), style = 3)
      } else if (verbose >= 2) {
        cat("\niter ", j, ":\t", sep = "")
      }
    }
    
    data_last <- data
    pred_error_last <- pred_error
    if (keep_forests) {
      forests_last <- forests
    }

    for (v in visit_seq) {
      v.na <- data_NA[, v]
      
      if (length(completed) == 0L) {
        data[[v]] <- imputeUnivariate(data[[v]])
      } else {
        fit <- ranger::ranger(
          y = data[[v]][!v.na],
          x = data[!v.na, completed, drop = FALSE],
          case.weights = case.weights[!v.na],
          ...
        )
        pred <- stats::predict(fit, data[v.na, completed, drop = FALSE])$predictions
        
        data[v.na, v] <- if (pmm.k) pmm(
          xtrain = fit$predictions, xtest = pred, ytrain = data[[v]][!v.na], k = pmm.k
        ) else pred
        
        if (fit$treetype == "Regression") {
          pred_error[[v]] <- 1 - fit$r.squared
        } else {  # Classification error
          pred_error[[v]] <- fit$prediction.error
        }
        
        if (is.nan(pred_error[[v]])) {
          pred_error[[v]] <- 0
        }

        if (keep_forests) {
          forests[[v]] <- fit
        }
      }
      
      if (j == 1L && (v %in% impute_by)) {
        completed <- union(completed, v)
      }
      
      if (verbose) {
        if (verbose == 1) {
          utils::setTxtProgressBar(pb, i)
          i <- i + 1L
        } else if (verbose >= 2) {
          cat(format(round(pred_error[[v]], dig), nsmall = dig), "\t")  
        }
      }
    }
    
    pred_errors[[j]] <- pred_error
    crit <- mean(pred_error) < mean(pred_error_last)
    j <- j + 1L
  }
  
  if (verbose) {
    cat("\n")
  }
  
  # We take the current iteration if (a) the iteration before did not impute yet
  # or (b) we had to stop before performance worsened
  if (j == 2L || (j > maxiter && crit)) {
    data_last <- data
    pred_error_last <- pred_error
    best_iter <- j - 1L
    if (keep_forests) {
      forests_last <- forests
    }
  } else {
    best_iter <- j - 2L
  }
  
  # Revert the conversions
  data_last <- revert(converted, X = data_last)
  
  if (data_only) {
    if (returnOOB) {
      attr(data_last, "oob") <- pred_error_last 
    }
    return(data_last)
  }
  
  out <- list(
    data = data_last,
    forests = if (keep_forests) forests_last,
    visit_seq = visit_seq,
    impute_by = impute_by,
    best_iter = best_iter,
    pred_errors = do.call(rbind, pred_errors),
    mean_pred_errors = vapply(pred_errors, FUN = mean, FUN.VALUE = numeric(1))
  )
  class(out) <- "missRanger"
  return(out)
}

# Helper functions

#' A version of [typeof()] internally used by [missRanger()].
#'
#' Returns either "numeric" (double or integer), "factor", "character", "logical", 
#' "special" (mode numeric, but neither double nor integer) or "" (otherwise).
#' [missRanger()] requires this information to deal with response types not natively 
#' supported by [ranger::ranger()].
#' 
#' @noRd
#' @param object Any object.
#' @returns A string.
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
#' @noRd
#' @param X A `data.frame`.
#' @param check If `TRUE`, the function checks if the converted columns can be 
#'   reverted without changes.
#' @returns 
#'   A list with the following elements: `X` is the converted data frame, 
#'   `vars`, `types`, `classes` are the names, types and classes of the 
#'   converted variables. Finally, `bad` names variables in `X` that should 
#'   have been converted but could not. 
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
#' Reverts conversions done by [convert()].
#' 
#' @noRd
#' @param con A list returned by [convert()].
#' @param X A data frame with some columns to be converted back according to the 
#'   information stored in \code{converted}.
#' @returns A data frame.
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
