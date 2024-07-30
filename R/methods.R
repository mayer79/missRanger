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
#' @description
#' Impute missing values on `newdata` based on an object of class "missRanger".
#' 
#' For multivariate imputation, use `missRanger(..., keep_forests = TRUE)`. 
#' For univariate imputation, no forests are required. 
#' This can be enforced by `predict(..., iter = 0)` or via `missRanger(. ~ 1, ...)`.
#' 
#' Note that out-of-sample imputation works best for rows in `newdata` with only one
#' missing value (actually counting only missings in variables used as covariates 
#' in random forests). We call this the "easy case". In the "hard case", 
#' even multiple iterations (set by `iter`) can lead to unsatisfactory results.
#' 
#' @details
#' The out-of-sample algorithm works as follows:
#' 1. Impute univariately all relevant columns by randomly drawing values 
#'    from the original, unimputed data. This step will only impact "hard case" rows.
#' 2. Replace univariate imputations by predictions of random forests. This is done
#'    sequentially over variables in decreasing order of missings in "hard case" rows
#'    (to minimize the impact of univariate imputations). Optionally, this is followed
#'    by predictive mean matching (PMM).
#' 3. Repeat Step 2 for "hard case" rows multiple times.
#' 
#' @param object 'missRanger' object.
#' @param newdata A `data.frame` with missing values to impute.
#' @param pmm.k Number of candidate predictions of the original dataset
#'   for predictive mean matching (PMM). By default the same value as during fitting.
#' @param iter Number of iterations for "hard case" rows. 0 for univariate imputation.
#' @param num.threads Number of threads used by ranger's predict function.
#'   The default `NULL` uses all threads.
#' @param seed Integer seed used for initial univariate imputation and PMM.
#' @param verbose Should info be printed? (1 = yes/default, 0 for no).
#' @param ... Passed to the predict function of ranger.
#' @export
#' @examples
#' iris2 <- generateNA(iris, seed = 20, p = c(Sepal.Length = 0.2, Species = 0.1))
#' imp <- missRanger(iris2, pmm.k = 5, num.trees = 100, keep_forests = TRUE, seed = 2)
#' predict(imp, head(iris2), seed = 3)
predict.missRanger <- function(
    object,
    newdata,
    pmm.k = object$pmm.k,
    iter = 4L,
    num.threads = NULL,
    seed = NULL,
    verbose = 1L,
    ...
  ) {
  stopifnot(
    "'newdata' should be a data.frame!" = is.data.frame(newdata),
    "'newdata' should have at least one row!" = nrow(newdata) >= 1L,
    "'iter' should not be negative!" = iter >= 0L,
    "'pmm.k' should not be negative!" = pmm.k >= 0L
  )
  data_raw <- object$data_raw
  
  # WHICH VARIABLES TO IMPUTE?
  
  # (a) Only those in newdata
  to_impute <- intersect(object$to_impute, colnames(newdata))
  
  # (b) Only those with missings
  to_fill <- is.na(newdata[, to_impute, drop = FALSE])
  missing_counts <- colSums(to_fill)
  to_impute <- to_impute[missing_counts > 0L]
  to_fill <- to_fill[, to_impute, drop = FALSE]
  
  if (length(to_impute) == 0L) {
    return(newdata)
  }
  
  # CHECK VARIABLES USED TO IMPUTE
  
  impute_by <- object$impute_by
  if (!all(impute_by %in% colnames(newdata))) {
    stop(
      "Variables not present in 'newdata': ",
      paste(setdiff(impute_by, colnames(newdata)), collapse = ", ")
    )
  }
  
  # We currently don't do multivariate imputation if variable not to be imputed 
  # has missing values
  only_impute_by <- setdiff(impute_by, to_impute)
  if (length(only_impute_by) > 0L && anyNA(newdata[, only_impute_by])) {
    stop(
      "Missing values in ", paste(only_impute_by, collapse = ", "), " not allowed."
    )
  }
  
  # CONSISTENCY CHECKS WITH 'data_raw'
  
  for (v in union(to_impute, impute_by)) {
    v_new <- newdata[[v]]
    v_orig <- data_raw[[v]]
    
    if (all(is.na(v_new))) {
      next  # NA can be of wrong class!
    }
    # class() distinguishes numeric, integer, logical, factor, character, Date, ...
    # - variables in to_impute are numeric, integer, logical, factor, or character
    # - variables in impute_by can also be of *mode* numeric, which includes Dates
    if (!identical(class(v_new), class(v_orig))) {
      stop("Inconsistency between 'newdata' and original data in variable ", v)
    }
    
    # Factor inconsistencies are not okay in 'to_impute'
    if (
      v %in% to_impute && is.factor(v_new) && !identical(levels(v_new), levels(v_orig))
    ) {
      if (all(levels(v_new) %in% levels(v_orig))) {
        newdata[[v]] <- factor(v_new, levels(v_orig), ordered = is.ordered(v_orig))
        if (verbose >= 1L) {
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
  # has no effect for "easy case" rows, but is not very expensive
  
  for (v in to_impute) {
    bad <- to_fill[, v]
    v_orig <- data_raw[[v]]
    donors <- sample(v_orig[!is.na(v_orig)], size = sum(bad), replace = TRUE)
    if (all(bad)) {
      # Handles e.g. case when original is factor, but newdata has all NA of numeric type
      newdata[[v]] <- donors
    } else {
      newdata[[v]][bad] <- donors
    }
  }
  
  if (length(impute_by) == 0L || iter < 1L) {
    if (verbose >= 1L) {
      message("\nOnly univariate imputations done")
    }  
    return(newdata)
  }
  
  # MULTIVARIATE IMPUTATION
  
  if (is.null(object$forests)) {
    stop("No random forests in 'object'. Use missRanger(, keep_forests = TRUE).")
  }
  
  # Do we have a random forest for all variables with missings?
  # This can fire only if the first iteration in missRanger() was the best, and only
  # for maximal one variable.
  forests_missing <- setdiff(to_impute, names(object$forests))
  if (length(forests_missing) > 0L) {
    if (verbose >= 1L) {
      message(
        "\nNo random forest for ", forests_missing, 
        ". Univariate imputation done for this variable."
      )
    }
    to_impute <- setdiff(to_impute, forests_missing)
  }
  
  # Do we have rows of "hard case"? If no, a single iteration is sufficient.
  easy <- rowSums(to_fill[, intersect(to_impute, impute_by), drop = FALSE]) <= 1L
  if (all(easy)) {
    iter <- 1L
  } else {
    # We impute first the column with most missings in *hard case* rows to minimize
    # impact of univariate imputations
    missing_counts <- colSums(to_fill[, to_impute, drop = FALSE] & !easy)
    to_impute <- to_impute[order(missing_counts, decreasing = TRUE)]
  }
  
  for (j in seq_len(iter)) {
    for (v in to_impute) {
      pred <- stats::predict(
        object$forests[[v]],
        newdata[to_fill[, v], ],
        num.threads = num.threads,
        verbose = verbose >= 1L,
        ...
      )$predictions
      if (pmm.k >= 1) {
        xtrain <- object$forests[[v]]$predictions
        ytrain <- data_raw[[v]]
        if (anyNA(ytrain)) {
          ytrain <- ytrain[!is.na(ytrain)]  # To align with OOB predictions
        }
        pred <- pmm(xtrain = xtrain, xtest = pred, ytrain = ytrain, k = pmm.k)
      } else if (is.logical(newdata[[v]])) {
        pred <- as.logical(pred)
      } else if (is.character(newdata[[v]])) {
        pred <- as.character(pred)
      }
      newdata[[v]][to_fill[, v]] <- pred
    }
    if (j == 1L) {
      to_fill <- to_fill & !easy
      
      # Remove features that were missing in easy case rows only
      missing_counts <- colSums(to_fill[, to_impute, drop = FALSE])
      to_impute <- to_impute[missing_counts > 0L]
    }
  }
  return(newdata)
}

