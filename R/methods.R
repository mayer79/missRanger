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
  cat("\nCorresponding means:\n")
  print(object$mean_pred_errors)
  cat("\nFirst rows of imputed data:\n\n")
  print(utils::head(object$data, 3L))
  invisible(object)
}
