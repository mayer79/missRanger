#' Print Method
#' 
#' Print method for an object of class "missRanger".
#'
#' @param x An object of class "missRanger".
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @examples
#' CO2_ <- generateNA(CO2, seed = 1)
#' imp <- missRanger(CO2_, pmm.k = 5, data_only = FALSE, num.threads = 1)
#' imp
print.missRanger <- function(x, ...) {
  b <- x$best_iter
  cat("missRanger object. Extract imputed data via $data\n")
  cat("- best iteration:", b, "\n")
  cat("- best average OOB imputation error:", x$mean_pred_errors[b], "\n")
  invisible(x)
}

#' Summary Method
#' 
#' Summary method for an object of class "missRanger".
#' 
#' @param object An object of class "missRanger".
#' @param ... Further arguments passed from other methods.
#' @returns Invisibly, the input is returned.
#' @export
#' @examples
#' CO2_ <- generateNA(CO2, seed = 1)
#' imp <- missRanger(CO2_, pmm.k = 5, data_only = FALSE, num.threads = 1)
#' summary(imp)
summary.missRanger <- function(object, ...) {
  print(object)
  cat("\nSequence of OOB prediction errors:\n\n")
  print(object$pred_errors)
  cat("\nMean performance per iteration:\n")
  print(object$mean_pred_errors)
  cat("\nFirst rows of imputed data:\n\n")
  print(utils::head(object$data, 3L))
  invisible(object)
}


#' Predict Method
#' 
#' Impute missing values on new data based on an object of class "missRanger".
#' 
#' @param object 'missRanger' object.
#' @param newdata A `data.frame` with missing values to impute.
#' @param pmm.k Number of candidate predictions of the original dataset
#'   for predictive mean matching (PMM). By default the same as during fitting.
#' @param iter Number of prediction iterations. Set to 0 for univariate imputation.
#' @param seed Integer seed used for initial univariate imputation and PMM.
#' @param verbose Should info be printed? (1 = yes/default, 0 for no).
#' @param ... Currently not used.
#' @export
#' @examples
#' iris2 <- generateNA(iris, seed = 20, p = c(Sepal.Length = 0.2, Species = 0.1))
#' imp <- missRanger(iris2, pmm.k = 5, num.trees = 100, keep_forests = TRUE, seed = 2)
#' predict(imp, head(iris2), seed = 3)
predict.missRanger <- function(
    object, newdata, pmm.k = object$pmm.k, iter = 3L, seed = NULL, verbose = 1, ...
  ) {
  stopifnot(
    "'newdata' should be a data.frame!" = is.data.frame(newdata),
    "'newdata' should have at least one row!" = nrow(newdata) >= 1L,
    "'iter' should not be negative!" = iter >= 0L,
    "'pmm.k' should not be negative!" = pmm.k >= 0L,
    "No random forests found in 'object'. Use missRanger(..., keep_forests = TRUE)." =
      !is.null(object$forests)
  )
  data_raw <- object$data_raw
  
  # WHICH VARIABLES TO IMPUTE?
  
  # (a) Only those in newdata
  to_impute <- intersect(object$to_impute, colnames(newdata))
  
  # (b) Only those with missings, and in decreasing order
  #     to minimize impact of univariate imputations
  to_fill <- is.na(newdata[, to_impute, drop = FALSE])
  m <- sort(colSums(to_fill), decreasing = TRUE)
  to_impute <- names(m[m > 0])
  to_fill <- to_fill[, to_impute, drop = FALSE]
  
  if (length(to_impute) == 0L) {
    return(newdata)
  }
  
  # CHECKS FOR VARIABLES USED TO IMPUTE
  
  impute_by <- object$impute_by
  if (!all(impute_by %in% colnames(newdata))) {
    stop(
      "Variables not present in 'newdata': ",
      paste(setdiff(impute_by, colnames(newdata)), collapse = ", ")
    )
  }
  
  # We currently won't do multivariate imputation if variable not to be imputed 
  # has missing values
  only_impute_by <- setdiff(impute_by, to_impute)
  if (length(only_impute_by) > 0L && anyNA(newdata[, only_impute_by, drop = FALSE])) {
    stop(
      "Missing values in ", paste(only_impute_by, collapse = ", "), " not allowed."
    )
  }
  
  for (v in union(to_impute, impute_by)) {
    v_new <- newdata[[v]]
    v_orig <- data_raw[[v]]
    # class() distinguishes numeric, integer, logical, factor, character, Date, ...
    # - variables in to_impute are numeric, integer, logical, factor, or character
    # - variables in impute_by can also be of *mode* numeric, which includes Dates
    if (!identical(class(v_new), class(v_orig))) {
      stop("Inconsistency between 'newdata' and original data in variable ", v)
    }
    
    # Factor inconsistencies are not okay if we predict it
    if (v %in% to_impute && is.factor(v_new) && !identical(levels(v_new), levels(v_orig))) {
      if (all(levels(v_new) %in% levels(v_orig))) {
        newdata[[v]] <- factor(v_new, levels(v_orig), ordered = is.ordered(v_orig))
        if (verbose >= 1) {
          message("\nExtending factor levels of '", v, "' to those in original data")
        }
      } else {
        stop("New factor levels seen in variable to impute: ", v)
      }
    }
  }

  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # UNIVARIATE IMPUTATION
  
  for (v in to_impute) {
    bad <- to_fill[, v]
    v_orig <- data_raw[[v]]
    newdata[[v]][bad] <- sample(v_orig[!is.na(v_orig)], size = sum(bad), replace = TRUE)
  }
  
  if (length(impute_by) == 0L || iter == 0L) {
    if (verbose >= 1) {
      message("\nOnly univariate imputations done")
    }  
    return(newdata)
  }
  
  # MULTIVARIATE IMPUTATION
  
  # Do we have a random forest for all variables with missings?
  forests_missing <- setdiff(to_impute, names(object$forests))
  if (verbose >= 1 && length(forests_missing) > 0L) {
    message(
      "\n", paste(forests_missing, collapse = ", "), 
      "without random forest. Univariate imputation done for this variable."
    )
  }
  to_impute <- setdiff(to_impute, forests_missing)
  
  for (j in seq_len(iter)) {
    for (v in to_impute) {
      y <- newdata[[v]]
      pred <- predict(object$forests[[v]], newdata[to_fill[, v], ])$predictions
      if (pmm.k >= 1) {
        xtrain <- object$forests[[v]]$predictions
        ytrain <- data_raw[[v]]
        if (anyNA(ytrain)) {
          ytrain <- ytrain[!is.na(ytrain)]  # To align with OOB predictions
        }
        pred <- pmm(xtrain = xtrain, xtest = pred, ytrain = ytrain, k = pmm.k)
      } else if (is.logical(y)) {
        pred <- as.logical(pred)
      } else if (is.character(y)) {
        pred <- as.character(pred)
      }
      newdata[[v]][to_fill[, v]] <- pred
    }
  }
  return(newdata)
}

