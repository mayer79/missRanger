iris2 <- generateNA(iris, seed = 1, p = 0.3)

test_that("all missings are filled for multivariate and univariate imputation", {
  imp <- missRanger(iris2, verbose = 0L, seed = 1L, num.trees = 20L, data_only = FALSE)
  expect_true(imp$best_iter > 1L)
  expect_true(!anyNA(imp$data))
  
  imp <- missRanger(iris2, . ~ 1, verbose = 0L, seed = 1L, data_only = FALSE)
  expect_equal(imp$best_iter, 1L)
  expect_true(!anyNA(imp$data))
})

#===================================================
# ARGUMENTS
#===================================================

test_that("pmm.k produces values in the original data", {
  imp1 <- missRanger(
    iris2, num.trees = 20L, verbose = 0L, pmm.k = 0L, seed = 1L, data_only = FALSE
  )
  imp2 <- missRanger(
    iris2, num.trees = 20L, verbose = 0L, pmm.k = 3L, seed = 1L, data_only = FALSE
  )
  
  expect_true(imp1$best_iter > 1L)
  expect_true(imp2$best_iter > 1L)
  
  expect_false(isTRUE(all.equal(imp1$data, imp2$data)))
  expect_true(all(imp2$data$Sepal.Length %in% iris2$Sepal.Length))
})

test_that("num.trees has an effect", {
  imp1 <- missRanger(iris2, num.trees = 20L, verbose = 0L, seed = 1L, data_only = FALSE)
  imp2 <- missRanger(iris2, num.trees = 10L, verbose = 0L, seed = 1L, data_only = FALSE)
  
  expect_true(imp1$best_iter > 1L)
  expect_true(imp2$best_iter > 1L)
  
  expect_false(isTRUE(all.equal(imp1$data, imp2$data)))
})

test_that("mtry has an effect", {
  imp1 <- missRanger(iris2, num.trees = 20L, verbose = 0L, seed = 1L, data_only = FALSE)
  imp2 <- missRanger(
    iris2, num.trees = 20L, verbose = 0L, seed = 1L, mtry = 1, data_only = FALSE
  )
  imp3 <- missRanger(
    iris2,
    num.trees = 20L,
    verbose = 0L,
    seed = 1L,
    mtry = function(m) min(m, 4),
    data_only = FALSE
  )
  
  expect_true(imp1$best_iter > 1L)
  expect_true(imp2$best_iter > 1L)
  expect_true(imp3$best_iter > 1L)
  
  expect_false(isTRUE(all.equal(imp1$data, imp2$data)))
  expect_false(isTRUE(all.equal(imp1$data, imp3$data)))
  expect_false(isTRUE(all.equal(imp2$data, imp3$data)))
})

test_that("min.node.size has an effect", {
  imp1 <- missRanger(
    iris2, num.trees = 20L, verbose = 0L, seed = 1L, min.node.size = 10, data_only = FALSE
  )
  imp2 <- missRanger(
    iris2, num.trees = 20L, verbose = 0L, seed = 1L, min.node.size = 20, data_only = FALSE
  )

  expect_true(imp1$best_iter > 1L)
  expect_true(imp2$best_iter > 1L)
  
  expect_false(isTRUE(all.equal(imp1$data, imp2$data)))
})

test_that("min.bucket has an effect", {
  imp1 <- missRanger(
    iris2, num.trees = 20L, verbose = 0L, seed = 1L, min.bucket = 10, data_only = FALSE
  )
  imp2 <- missRanger(
    iris2, num.trees = 20L, verbose = 0L, seed = 1L, min.bucket = 20, data_only = FALSE
  )
  
  expect_true(imp1$best_iter > 1L)
  expect_true(imp2$best_iter > 1L)
  
  expect_false(isTRUE(all.equal(imp1$data, imp2$data)))
})

test_that("max.depth has an effect", {
  imp1 <- missRanger(
    iris2, num.trees = 20L, verbose = 0L, seed = 1L, max.depth = 1, data_only = FALSE
  )
  imp2 <- missRanger(
    iris2, num.trees = 20L, verbose = 0L, seed = 1L, max.depth = 2, data_only = FALSE
  )
  
  expect_true(imp1$best_iter > 1L)
  expect_true(imp2$best_iter > 1L)
  
  expect_false(isTRUE(all.equal(imp1$data, imp2$data)))
})

test_that("replace has an effect", {
  imp1 <- missRanger(
    iris2, num.trees = 20L, verbose = 0L, seed = 1L, replace = TRUE, data_only = FALSE
  )
  imp2 <- missRanger(
    iris2, num.trees = 20L, verbose = 0L, seed = 1L, replace = FALSE, data_only = FALSE
  )
  
  expect_true(imp1$best_iter > 1L)
  expect_true(imp2$best_iter > 1L)
  
  expect_false(isTRUE(all.equal(imp1$data, imp2$data)))
})

test_that("sample.fraction has an effect", {
  imp1 <- missRanger(
    iris2,
    num.trees = 20L,
    verbose = 0L,
    seed = 1L,
    sample.fraction = 1,
    data_only = FALSE
  )
  imp2 <- missRanger(
    iris2,
    num.trees = 20L,
    verbose = 0L,
    seed = 1L,
    sample.fraction = 0.5,
    data_only = FALSE
  )
  
  expect_true(imp1$best_iter > 1L)
  expect_true(imp2$best_iter > 1L)
  
  expect_false(isTRUE(all.equal(imp1$data, imp2$data)))
})

test_that("case.weights works (only if multivariate imputation is being done)", {
  imp1 <- missRanger(iris2, num.trees = 20L, verbose = 0L, seed = 1L, data_only = FALSE)
  imp2 <- missRanger(
    iris2,
    num.trees = 20L,
    verbose = 0L,
    seed = 1L,
    case.weights = rep(1, nrow(iris)),
    data_only = FALSE
  )
  imp3 <- missRanger(
    iris2,
    num.trees = 20L,
    verbose = 0L,
    seed = 1L,
    case.weights = 1:nrow(iris),
    data_only = FALSE
  )
  
  expect_true(imp1$best_iter > 1L)
  expect_true(imp2$best_iter > 1L)
  expect_true(imp3$best_iter > 1L)
  
  expect_equal(imp1$data, imp2$data)
  expect_false(isTRUE(all.equal(imp1$data, imp3$data)))
  
  expect_error(missRanger(iris2, num.trees = 3L, verbose = 0L, case.weights = 1:7))
})

test_that("seed works", {
  imp1 <- missRanger(iris2, num.trees = 20L, verbose = 0L, seed = 1L, data_only = FALSE)
  imp2 <- missRanger(iris2, num.trees = 20L, verbose = 0L, seed = 1L, data_only = FALSE)
  imp3 <- missRanger(iris2, num.trees = 20L, verbose = 0L, seed = 2L, data_only = FALSE)
  
  expect_true(imp1$best_iter > 1L)
  expect_true(imp2$best_iter > 1L)
  expect_true(imp3$best_iter > 1L)
  
  expect_equal(imp1$data, imp2$data)
  expect_false(identical(imp1$data, imp3$data))
})

test_that("verbose can be suppressed", {
  expect_silent(missRanger(iris2, num.trees = 3L, verbose = 0L))
  capture_output(expect_message(missRanger(iris2, num.trees = 3L, verbose = 1L)))
  capture_output(expect_message(missRanger(iris2, num.trees = 3L, verbose = 2L)))
})

test_that("returnOOB works", {
  imp1 <- missRanger(
    iris2, num.trees = 20L, verbose = 0L, returnOOB = TRUE
  )
  imp2 <- missRanger(
    iris2, num.trees = 20L, verbose = 0L, returnOOB = FALSE
  )
  
  expect_true(length(attributes(imp1)$oob) == ncol(iris))
  expect_null(attributes(imp2)$oob)
})

#===================================================
# DATA TYPE CONSISTENCY
#===================================================

n <- 200L

X <- data.frame(
  int = seq_len(n), 
  double = log(seq_len(n)), 
  char = rep(LETTERS[1:4], n %/% 4),
  fact = factor(rep(LETTERS[1:2], n %/% 2)),
  logi = seq_len(n) > n %/% 3,
  date = seq.Date(as.Date("2001-01-01"), length.out = n, by = "1 d")
)

X_NA <- generateNA(X[1:5], p = 0.2, seed = 1L)

test_that("Imputing a logical produces a logical (normal, pmm, univariate)", {
  X2 <- generateNA(X, p = c(logi = 0.3, fact = 0.3), seed = 1L)
  
  imp1 <- missRanger(
    X2, num.trees = 20L, verbose = 0L, pmm.k = 0, seed = 1L, data_only = FALSE
  )
  imp2 <- missRanger(
    X2, num.trees = 20L, verbose = 0L, pmm.k = 3, seed = 1L, data_only = FALSE
  )
  imp3 <- missRanger(X2, . ~ 1, verbose = 0L, seed = 1L, data_only = FALSE)

  expect_true(imp1$best_iter > 1L)
  expect_true(imp2$best_iter > 1L)
  expect_true(imp3$best_iter == 1L)
    
  for (imp in list(imp1, imp2, imp3)) {
    expect_true(is.logical(imp$data$logi))
  }
})

test_that("Imputing a factor produces a factor (normal, pmm, univariate)", {
  X2 <- generateNA(X, p = c(fact = 0.3))
  imp1 <- missRanger(
    X2, num.trees = 20L, verbose = 0L, pmm.k = 0, seed = 1L, data_only = FALSE
  )
  imp2 <- missRanger(
    X2, num.trees = 20L, verbose = 0L, pmm.k = 3, seed = 1L, data_only = FALSE
  )
  imp3 <- missRanger(X2, . ~ 1, verbose = 0L, seed = 1L, data_only = FALSE)
  
  expect_true(imp1$best_iter > 1L)
  expect_true(imp2$best_iter > 1L)
  expect_true(imp3$best_iter == 1L)
  
  for (imp in list(imp1, imp2, imp3)) {
    expect_true(is.factor(imp$data$fact))
    expect_equal(levels(imp$data$fact), levels(X$fact))
  }
})

test_that("Imputing a character produces a character (normal, pmm, univariate)", {
  X2 <- generateNA(X, p = c(char = 0.3))
  imp1 <- missRanger(
    X2, num.trees = 20L, verbose = 0L, pmm.k = 0, seed = 1L, data_only = FALSE
  )
  imp2 <- missRanger(
    X2, num.trees = 20L, verbose = 0L, pmm.k = 3, seed = 1L, data_only = FALSE
  )
  imp3 <- missRanger(X2, . ~ 1, verbose = 0L, seed = 1L, data_only = FALSE)
  
  expect_true(imp1$best_iter > 1L)
  expect_true(imp2$best_iter > 1L)
  expect_true(imp3$best_iter == 1L)
  
  for (imp in list(imp1, imp2, imp3)) {
    expect_true(is.character(imp$data$char))
  }
})

test_that("Imputing a int produces an int or double (normal, pmm, univariate)", {
  X2 <- generateNA(X, p = c(int = 0.3), seed = 1L)
  
  imp1 <- missRanger(
    X2, num.trees = 20L, verbose = 0L, pmm.k = 0, seed = 1L, data_only = FALSE
  )
  imp2 <- missRanger(
    X2, num.trees = 20L, verbose = 0L, pmm.k = 3, seed = 1L, data_only = FALSE
  )
  imp3 <- missRanger(X2, . ~ 1, verbose = 0L, seed = 1L, data_only = FALSE)
  
  expect_true(imp1$best_iter > 1L)
  expect_true(imp2$best_iter > 1L)
  expect_true(imp3$best_iter == 1L)
  
  for (imp in list(imp1, imp2, imp3)) {
    expect_true(is.numeric(imp$data$int))
  }
})

# SELECTION OF "to_impute" and "impute_by"

# Are two vectors identical up to sorting?
.setequal <- function(x, y) {
  (length(x) == length(y)) && all(sort(x) == sort(y))
}

test_that("Only features with missings, but not all missings, are being imputed", {
  X2 <- generateNA(X, p = c(int = 1, double = 0.2), seed = 1L)
  imp <- missRanger(X2, num.trees = 20L, verbose = 0L, seed = 1L, data_only = FALSE)
  
  expect_equal(imp$to_impute, "double")
  expect_equal(unname(colSums(is.na(imp$data))), c(n, 0, 0, 0, 0, 0))
})

test_that("Date variables (and other non-standard variable types) are not imputed", {
  X2 <- generateNA(X, p = c(date = 0.3, int = 0.1), seed = 1L)
  imp <- missRanger(X2, num.trees = 20L, verbose = 0L, seed = 1L, data_only = FALSE)
  
  expect_equal(imp$to_impute, "int")
  expect_true(anyNA(imp$data$date))
})

test_that("Features with missings that are not imputed are not used for imputation", {
  X2 <- generateNA(X, p = c(int = 0.1, double = 0.2), seed = 1L)
  imp <- missRanger(
    X2, . - double ~ ., num.trees = 20L, verbose = 0L, seed = 1L, data_only = FALSE
  )
  
  expect_equal(imp$impute_by, setdiff(colnames(X2), "double"))
})

test_that("Constant features are not used for imputation", {
  X2 <- generateNA(X, p = c(int = 0.1, double = 0.2), seed = 1L) |> 
    transform(const = 1)
  imp <- missRanger(
    X2, num.trees = 20L, verbose = 0L, seed = 1L, data_only = FALSE
  )
  
  expect_equal(imp$impute_by, setdiff(colnames(X2), "const"))
})

test_that("list variables (and other strange types) cannot be used for imputation", {
  X2 <- generateNA(X, p = c(int = 0.1, double = 0.2), seed = 1L)
  X2$list <- as.list(replicate(n, NULL))
  imp <- missRanger(X2, num.trees = 20L, verbose = 0L, seed = 1L, data_only = FALSE)
  
  expect_equal(imp$impute_by, setdiff(colnames(X), "list"))
})

test_that("Multiple problems together", {
  X2 <- generateNA(X, p = 0.2, seed = 1L)
  X2$list <- as.list(replicate(n, NULL))
  X2$const <- 1
  
  imp <- missRanger(
    X2, . - double ~ ., num.trees = 20L, verbose = 0L, seed = 1L, data_only = FALSE
  )
  
  xpected <- setdiff(colnames(X), c("double", "date"))  
  expect_equal(imp$impute_by, xpected)
  expect_true(.setequal(imp$to_impute, xpected))
})

# FORMULA PARSING

test_that("formula interface works with specified left and right side", {
  imp <- missRanger(
    X_NA,
    int ~ int + double,
    pmm = 3L,
    num.trees = 20L,
    verbose = 0L,
    seed = 1L,
    data_only = FALSE
  )
  na_per_col <- colSums(is.na(imp$data))
  
  expect_equal(na_per_col[[1L]], 0L)
  expect_true(all(na_per_col[-1L] >= 1L))
  expect_equal(imp$to_impute, "int")
  expect_equal(imp$impute_by, "int")
})

test_that("formula interface works with unspecified right side", {
  imp <- missRanger(
    X_NA,
    int + double ~ .,
    pmm = 3L,
    num.trees = 20L,
    verbose = 0L,
    seed = 1L,
    data_only = FALSE
  )
  na_per_col <- colSums(is.na(imp$data))
  
  expect_equal(unname(na_per_col[1:2]), c(0, 0))
  expect_true(all(na_per_col[-(1:2)] >= 1L))
  expect_true(.setequal(imp$to_impute, c("int", "double")))
  expect_equal(imp$impute_by, c("int", "double"))
})

test_that("formula interface works with unspecified left side", {
  imp <- missRanger(
    X_NA, . ~ int, pmm = 3L, num.trees = 20L, verbose = 0L, seed = 1L, data_only = FALSE
  )
  expect_true(!anyNA(imp$data))
  expect_true(.setequal(imp$to_impute, colnames(X_NA)))
  expect_equal(imp$impute_by, "int")
})

test_that("dropping columns on left side leaves missing values", {
  imp <- missRanger(
    X_NA,
    . - int ~ .,
    pmm = 3L,
    num.trees = 20L,
    verbose = 0L,
    seed = 1L,
    data_only = FALSE
  )
  expect_equal(
    unname(colSums(is.na(imp$data)) > 0),
    c(TRUE, FALSE, FALSE, FALSE, FALSE)
  )
  xpected <- setdiff(colnames(X_NA), "int")
  expect_true(.setequal(imp$to_impute, xpected))
  expect_equal(imp$impute_by, xpected)
})

test_that("dropping columns on right side has an impact", {
  imp1 <- missRanger(
    X_NA, . ~ . - int, num.trees = 20L, verbose = 0L, seed = 1L, data_only = FALSE
  )
  imp2 <- missRanger(X_NA, num.trees = 20L, verbose = 0L, seed = 1L)
  
  expect_false(identical(imp1$data, imp2))
  expect_true(.setequal(imp1$to_impute, colnames(X_NA)))
  expect_equal(imp1$impute_by, setdiff(colnames(X_NA), "int"))
})

test_that("empty rhs equals univariate imputation", {
  imp1 <- missRanger(X_NA, . ~ 1, num.trees = 20L, verbose = 0L, seed = 1L)
  imp2 <- imputeUnivariate(X_NA, seed = 1L)

  imp3 <- missRanger(X_NA, int + char ~ 1, num.trees = 20L, verbose = 0L, seed = 1L)
  imp4 <- imputeUnivariate(X_NA, seed = 1L, v = c("int", "char"))
  
  expect_equal(imp1, imp2)
  expect_equal(imp3, imp4)
})

test_that("non-syntactic column names work with or without formula", {
  X_NA2 <- X_NA
  colnames(X_NA2)[1:2] <- c("1bad name", "2 also bad")
  
  imp1 <- missRanger(X_NA, num.trees = 20L, verbose = 0L, seed = 1L)
  imp2 <- missRanger(X_NA2, num.trees = 20L, verbose = 0L, seed = 1L)
  imp3 <- missRanger(
    . - `1bad name` ~ - ., data = X_NA2, num.trees = 20L, verbose = 0L, seed = 1L
  )
  
  expect_equal(colnames(X_NA2), colnames(imp2))
  expect_equal(imp1, setNames(imp2, colnames(imp1)))
  expect_false(anyNA(imp2))
  expect_equal(
    unname(colSums(is.na(imp3)) == 0L),
    c(FALSE, TRUE, TRUE, TRUE, TRUE)
  )
})

