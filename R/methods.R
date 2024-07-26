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
#'   for predictive mean matching (PMM).
#' @param iter Number of random forest iterations. Set to 0 for univariate imputation.
#' @param seed Integer seed used for initial univariate imputation and PMM.
#' @param verbose Should info be printed? Default is 1 (yes). Set to 0 for no info.
#' @param ... Currently not used.
#' @export
#' @examples
#' iris2 <- generateNA(iris, seed = 1)
#' imp <- missRanger(iris2, pmm.k = 5, num.trees = 100, keep_forests = TRUE)
#' predict(imp, head(iris2), seed = 9)
predict.missRanger <- function(
    object, newdata, pmm.k = 5L, iter = 3L, seed = NULL, verbose = 1, ...
  ) {
  stopifnot(
    "'data' should be a data.frame!" = is.data.frame(newdata),
    "'data' should have at least one row and one column!" = dim(newdata) >= 1L,
    "'niter' should not be negative!" = iter >= 0L,
    "'pmm.k' should not be negative!" = pmm.k >= 0L,
    "No random forests found in 'object'. Use missRanger(..., keep_forests = TRUE)." =
      !is.null(object$forests)
  )
  # More checks
  # - align factor levels
  # - calculate impute_by
  # - calculate to_impute
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  data_raw <- object$data_raw
  impute_by <- object$impute_by
  to_impute <- object$impute_to
  
  to_fill <- is.na(newdata[to_impute])
  
  # UNIVARIATE IMPUTATION
  for (v in impute_by) {
    bad <- is.na(newdata[[v]])
    if (any(bad)) {
      orig <- data_raw[[v]]
      newdata[[v]][bad] <- sample(orig[!is.na(orig)], size = sum(bad), replace = TRUE)
    }
  }

  # MULTIVARIATE IMPUTATION
  
  # Do we have a random forest for all variables with missings
  forests_missing <- setdiff(to_impute, names(object$forests))
  if (verbose && length(forests_missing > 0L)) {
    message(
      paste(
        "\nFeature",
        paste(forests_missing, collapse = ", "),
        "without random forest. Univariate imputation done.")
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

