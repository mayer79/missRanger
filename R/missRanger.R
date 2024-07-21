#' Fast Imputation of Missing Values by Chained Random Forests
#' 
#' Uses the "ranger" package (Wright & Ziegler) to do fast missing value imputation by
#' chained random forests, see Stekhoven & Buehlmann and Van Buuren & Groothuis-Oudshoorn.
#' Between the iterative model fitting, it offers the option of predictive mean matching.
#' This firstly avoids imputation with values not present in the original data
#' (like a value 0.3334 in a 0-1 coded variable).
#' Secondly, predictive mean matching tries to raise the variance in the resulting
#' conditional distributions to a realistic level. This allows to do multiple imputation
#' when repeating the call to [missRanger()].
#' 
#' The iterative chaining stops as soon as `maxiter` is reached or if the average
#' out-of-bag (OOB) prediction errors stop reducing.
#' In the latter case, except for the first iteration, the second last (= best)
#' imputed data is returned.
#' 
#' OOB prediction errors are quantified as 1 - R^2 for numeric variables, and as
#' classification error otherwise. If a variable has been imputed only univariately,
#' the value is 1.
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
#' @param num.trees Number of trees passed to [ranger::ranger()].
#' @param mtry Number of covariates considered per split. The default `NULL` equals
#'   the rounded down root of the number of features. Can be a function, e.g.,
#'   `function(p) trunc(p/3)`. Passed to [ranger::ranger()]. Note that during the
#'   first iteration, the number of features is growing. Thus, a fixed value can lead to
#'   an error. Using a function like `function(p) min(p, 2)` will fix such problem.
#' @param min.node.size Minimal node size passed to [ranger::ranger()].
#'   By default 1 for classification and 5 for regression.
#' @param min.bucket Minimal terminal node size passed to [ranger::ranger()].
#'   The default `NULL` means 1.
#' @param max.depth Maximal tree depth passed to [ranger::ranger()].
#'   `NULL` means unlimited depth. 1 means single split trees.
#' @param replace Sample with replacement passed to [ranger::ranger()].
#' @param sample.fraction Fraction of rows per tree passed to [ranger::ranger()].
#'   The default: use all rows when `replace = TRUE` and 0.632 otherwise.
#' @param case.weights Optional case weights passed to [ranger::ranger()].
#' @param num.threads Number of threads passed to [ranger::ranger()].
#'   The default `NULL` uses all threads.
#' @param save.memory Slow but memory saving mode of [ranger::ranger()].
#' @param maxiter Maximum number of iterations.
#' @param seed Integer seed.
#' @param verbose A value in 0, 1, 2 contolling the verbosity.
#' @param returnOOB Should the final average OOB prediction errors be added
#'   as data attribute "oob"? Only relevant when `data_only = TRUE`.
#' @param data_only If `TRUE` (default), only the imputed data is returned.
#'   Otherwise, a "missRanger" object with additional information is returned.
#' @param keep_forests Should the random forests of the last relevant iteration
#'   be returned? The default is `FALSE`. Setting this option will use a lot of memory.
#'   Only relevant when `data_only = TRUE`.
#' @param ... Additional arguments passed to [ranger::ranger()]. Not all make sense.
#' @returns 
#'   If `data_only = TRUE` an imputed `data.frame`. Otherwise, a "missRanger" object
#'   with the following elements:
#'   - `data`: The imputed data.
#'   - `data_raw`: The original data provided.
#'   - `forests`: When `keep_forests = TRUE`, a list of "ranger" models used to 
#'     generate the imputed data. `NULL` otherwise.
#'   - `to_impute`: Variables to be imputed (in this order).
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
#' set.seed(34)
#' iris2 <- generateNA(iris)
#' 
#' imp1 <- missRanger(iris2, pmm.k = 5, num.trees = 50, seed = 1)
#' head(imp1)
#' 
#' # Extended output
#' imp2 <- missRanger(iris2, pmm.k = 5, num.trees = 50, data_only = FALSE, seed = 1)
#' summary(imp2)
#' identical(imp1, imp2$data)  # TRUE
#' 
#' # Univariate imputation of Species and Sepal.Width
#' imp3 <- missRanger(iris2, Species + Sepal.Width ~ 1)
missRanger <- function(
    data,
    formula = . ~ .,
    pmm.k = 0L,
    num.trees = 500,
    mtry = NULL,
    min.node.size = NULL,
    min.bucket = NULL,
    max.depth = NULL,
    replace = TRUE,
    sample.fraction = if (replace) 1 else 0.632,
    case.weights = NULL,
    num.threads = NULL,
    save.memory = FALSE,
    maxiter = 10L,
    seed = NULL,
    verbose = 1,
    returnOOB = FALSE,
    data_only = TRUE,
    keep_forests = FALSE,
    ...
  ) {
  if (verbose) {
    message("Missing value imputation by random forests\n")
  }
  
  # 1) INITIAL CHECKS
  bad_args <- c(
    "write.forest", 
    "probability", 
    "quantreg", 
    "oob.error", 
    "dependent.variable.name", 
    "classification"
  )
  stopifnot(
    "'data' should be a data.frame!" = is.data.frame(data), 
    "'data' should have at least one row and one column!" = dim(data) >= 1L, 
    "'pmm.k' should not be negative!" = pmm.k >= 0L,
    "'maxiter' should be positive!" = maxiter >= 1L,
    "Incompatible ranger() arguments in ..." = !(bad_args  %in% names(list(...)))
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
  
  if (!data_only) {
    data_raw <- data
  }

  lhs_rhs <- .formula_parser(formula, data[1L, ])
  to_impute <- lhs_rhs[[1L]]  # lhs
  impute_by <- lhs_rhs[[2L]]  # rhs
  
  # 2) SELECT VARIABLES TO IMPUTE
  
  # 2a) Pick variables with some but not all missings
  ok <- vapply(
    data[, to_impute, drop = FALSE], 
    FUN = function(z) anyNA(z) && !all(is.na(z)),
    FUN.VALUE = logical(1L)
  )
  to_impute <- to_impute[ok]
  
  # 2b) Drop variables incompatible as responses in ranger()
  #  Note: We *could* do univariate imputation though. But at this stage we do not
  #  know this yet in all cases: impute_by might still contain bad variables.
  ok <- vapply(
    data[, to_impute, drop = FALSE], 
    FUN = function(z) .check_response(z),
    FUN.VALUE = logical(1L)
  )
  if (verbose && !all(ok)) {
    message(
      paste("Can't impute these variables (wrong type): ",
            paste(to_impute[!ok], collapse = ", "))
    )
  }
  to_impute <- to_impute[ok]
  
  if (length(to_impute) == 0L) {
    # Nothing to do...
    if (verbose) {
      cat("\n")
    }
    if (data_only) {
      return(data) 
    } else {
      out <- structure(
        list(
          data = data,
          data_raw = data_raw,
          forests = NULL,
          to_impute = c(),
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
  
  # Get missing indicators, and sort variables by increasing number of missings
  data_NA <- is.na(data[, to_impute, drop = FALSE])
  to_impute <- names(sort(colSums(data_NA)))
  
  # 3) SELECT VARIABLES USED TO IMPUTE
  
  # Variables should either appear in "to_impute" or do not contain any missings
  ok <- impute_by %in% to_impute |
    !vapply(data[, impute_by, drop = FALSE], FUN = anyNA, FUN.VALUE = logical(1L))
  if (verbose && !all(ok)) {
    message("Can't use these features for imputation because they contain missing values 
            and do not appear on the LHS of the formula.")
    message(paste(impute_by[!ok], collapse = ", "))
  }  
  impute_by <- impute_by[ok]
  
  # 3b) Drop variables that can't be used as features in ranger()
  ok <- vapply(
    data[, impute_by, drop = FALSE],
    FUN = function(z) .check_feature(z),
    FUN.VALUE = logical(1L)
  )
  if (verbose && !all(ok)) {
    message("Dropping features of incompatible mode() to impute:")
    message(paste(impute_by[!ok], collapse = ", "))
  }
  impute_by <- impute_by[ok]
  
  # 3c) Drop constant features (NA does not count as value)
  ok <- vapply(
    data[, impute_by, drop = FALSE],
    FUN = function(z) length(unique(z[!is.na(z)])) > 1L,
    FUN.VALUE = logical(1L)
  )
  if (verbose && !all(ok)) {
    message("Skip constant features for imputation:")
    message(paste(impute_by[!ok], collapse = ", "))
  }
  impute_by <- impute_by[ok]
  

  if (verbose) {
    cat("\n  Variables to impute:\t\t")
    cat(to_impute, sep = ", ")
    cat("\n  Variables used to impute:\t")
    cat(impute_by, sep = ", ")
    cat("\n")
  }

  # 4) IMPUTATION
  
  # Initialization
  completed <- setdiff(impute_by, to_impute)  # Immediately used as features in ranger()
  j <- 1L                                     # Which iteration?
  crit <- TRUE                                # Iterate until criterium is FALSE
  dig <- 4L                                   # Only used if verbose = 2
  pred_error <- rep(1, length(to_impute))     # Within iteration OOB errors per feature
  names(pred_error) <- to_impute
  pred_errors <- list()                       # Keeps OOB errors per iteration
  if (keep_forests) {
    forests <- list()
  }
  
  if (verbose >= 2) {
    cat("\n", abbreviate(to_impute, minlength = dig + 2L), sep = "\t")
  }
  
  # Looping over iterations and variables to impute
  while (crit && j <= maxiter) {
    if (verbose) {
      if (verbose == 1) {
        i <- 1L
        cat("\niter", j, "\n")
        pb <- utils::txtProgressBar(0, length(to_impute), style = 3)
      } else if (verbose >= 2) {
        cat("\niter ", j, ":\t", sep = "")
      }
    }

    data_last <- data
    pred_error_last <- pred_error
    if (keep_forests) {
      forests_last <- forests
    }

    for (v in to_impute) {
      v.na <- data_NA[, v]

      if (length(completed) == 0L) {
        data[[v]] <- imputeUnivariate(data[[v]])
      } else {
        y <- data[[v]][!v.na]
        is_char <- is.character(y)
        if (is_char) {
          y <- as.factor(y)
        }
        
        fit <- ranger::ranger(
          num.trees = num.trees,
          mtry = mtry,
          min.node.size = min.node.size,
          min.bucket = min.bucket,
          max.depth = max.depth,
          replace = replace,
          sample.fraction = sample.fraction,
          case.weights = if (!is.null(case.weights)) case.weights[!v.na],
          num.threads = num.threads,
          save.memory = save.memory,
          x = data[!v.na, completed, drop = FALSE],
          y = y,
          ...
        )

        pred <- stats::predict(fit, data[v.na, completed, drop = FALSE])$predictions
        
        if (pmm.k >= 1L) {
          pred <- pmm(xtrain = fit$predictions, xtest = pred, ytrain = y, k = pmm.k)
        } else if (is.logical(y)) {
          pred <- as.logical(pred)
        } else if (is_char) {
          pred <- as.character(pred)
        }

        data[v.na, v] <- pred
        
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
  
  if (data_only) {
    if (returnOOB) {
      attr(data_last, "oob") <- pred_error_last 
    }
    return(data_last)
  }
  
  out <- list(
    data = data_last,
    data_raw = data_raw,
    forests = if (keep_forests) forests_last,
    to_impute = to_impute,
    impute_by = impute_by,
    best_iter = best_iter,
    pred_errors = do.call(rbind, pred_errors),
    mean_pred_errors = vapply(pred_errors, FUN = mean, FUN.VALUE = numeric(1))
  )
  class(out) <- "missRanger"
  
  return(out)
}


# HELPER FUNCTIONS

# Extracts colnames of data from a string like "a + b + c"
.string_parser <- function(z, data) {
  if (z == ".") {
    return(colnames(data))
  }
  out <- attr(stats::terms.formula(stats::reformulate(z), data = data), "term.labels")
  return(trimws(out, whitespace = "`"))  # Remove annoying enclosing backticks
}

# Returns list with lhs and rhs variable name vectors
.formula_parser <- function(formula, data) {
  if (!inherits(formula, "formula")) {
    stop("'formula' should be a formula!")
  }
  formula <- as.character(formula)
  if (length(formula) != 3L) {
    stop("Formula must have left and right hand side. If it has: Don't load {formula.tools}. It breaks base R's as.character()")
  }
  return(lapply(formula[2:3], FUN = .string_parser, data = data))
}

# Checks if response type can be used in ranger (or easily converted to)
.check_response <- function(x) {
  # is.numeric(1L) -> TRUE
  return(is.numeric(x) || is.factor(x) || is.character(x) || is.logical(x))
}

# Checks if feature type can be used in ranger (assumption)
.check_feature <- function(x) {
  # factor/integer/Date -> "numeric"
  return(mode(x) %in% c("numeric", "character", "logical"))
}

