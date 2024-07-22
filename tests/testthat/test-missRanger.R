iris2 <- generateNA(iris, p = 0.2, seed = 1L)

test_that("all missings are filled for multivariate and univariate imputation", {
  imp <- missRanger(iris2, verbose = 0L, seed = 1L, num.trees = 20)
  expect_true(!anyNA(imp))
  
  imp <- missRanger(iris2, . ~ 1, verbose = 0L, seed = 1L)
  expect_true(!anyNA(imp))
})

#===================================================
# TEST ARGUMENTS
#===================================================

imp1 <- missRanger(iris2, num.trees = 20L, verbose = 0L, seed = 1L)

test_that("pmm.k produces values present in the original data", {
  imp2 <- missRanger(iris2, num.trees = 20L, verbose = 0L, pmm.k = 3L, seed = 1L)
  
  expect_false(isTRUE(all.equal(imp1, imp2)))
  expect_true(all(imp2$Sepal.Length %in% iris2$Sepal.Length))
})

test_that("num.trees has an effect", {
  imp2 <- missRanger(iris2, num.trees = 10L, verbose = 0L, seed = 1L)
  expect_false(isTRUE(all.equal(imp1, imp2)))
})

test_that("mtry has an effect", {
  imp2 <- missRanger(iris2, num.trees = 20L, verbose = 0L, seed = 1L, mtry = 1)
  imp3 <- missRanger(
    iris2, num.trees = 20L, verbose = 0L, seed = 1L, mtry = function(m) min(m, 4)
  )
  
  expect_false(isTRUE(all.equal(imp1, imp2)))
  expect_false(isTRUE(all.equal(imp1, imp3)))
  expect_false(isTRUE(all.equal(imp2, imp3)))
})

test_that("min.node.size has an effect", {
  imp2 <- missRanger(
    iris2, num.trees = 20L, verbose = 0L, seed = 1L, min.node.size = 20
  )
  expect_false(isTRUE(all.equal(imp1, imp2)))
})

test_that("min.bucket has an effect", {
  imp2 <- missRanger(iris2, num.trees = 20L, verbose = 0L, seed = 1L, min.bucket = 20)
  expect_false(isTRUE(all.equal(imp1, imp2)))
})

test_that("max.depth has an effect", {
  imp2 <- missRanger(iris2, num.trees = 20L, verbose = 0L, seed = 1L, max.depth = 1)
  expect_false(isTRUE(all.equal(imp1, imp2)))
})

test_that("replace has an effect", {
  imp2 <- missRanger(iris2, num.trees = 20L, verbose = 0L, seed = 1L, replace = FALSE)
  expect_false(isTRUE(all.equal(imp1, imp2)))
})

test_that("sample.fraction has an effect", {
  imp2 <- missRanger(
    iris2, num.trees = 20L, verbose = 0L, seed = 1L, sample.fraction = 0.5
  )
  expect_false(isTRUE(all.equal(imp1, imp2)))
})

test_that("case.weights works", {
  imp2 <- missRanger(
    iris2, num.trees = 20L, verbose = 0L, seed = 1L, case.weights = rep(1, nrow(iris))
  )
  imp3 <- missRanger(
    iris2, num.trees = 20L, verbose = 0L, seed = 1L, case.weights = 1:nrow(iris)
  )
  
  expect_equal(imp1, imp2)
  expect_false(isTRUE(all.equal(imp1, imp3)))
  
  expect_error(missRanger(iris2, num.trees = 3L, verbose = 0L, case.weights = 1:7))
})

test_that("seed works", {
  imp2 <- missRanger(iris2, num.trees = 20L, verbose = 0L, seed = 1L)
  imp3 <- missRanger(iris2, num.trees = 20L, verbose = 0L, seed = 2L)
  
  expect_equal(imp1, imp2)
  expect_false(identical(imp1, imp3))
})

test_that("verbose can be suppressed", {
  expect_silent(missRanger(iris2, num.trees = 3L, verbose = 0L))
  capture_output(expect_message(missRanger(iris2, num.trees = 3L, verbose = 1L)))
  capture_output(expect_message(missRanger(iris2, num.trees = 3L, verbose = 2L)))
})

test_that("returnOOB works", {
  imp2 <- missRanger(iris2, num.trees = 20L, verbose = 0L, returnOOB = TRUE)
  
  expect_true(length(attributes(imp2)$oob) == ncol(iris))
  expect_null(attributes(imp1)$oob)
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

test_that("Imputing retains data type (with/without PMM and univariately)", {
  imp1 <- missRanger(X_NA, num.trees = 20L, verbose = 0L, pmm.k = 0, seed = 1L)
  imp2 <- missRanger(X_NA, num.trees = 20L, verbose = 0L, pmm.k = 3, seed = 1L)
  imp3 <- missRanger(X_NA, . ~ 1, verbose = 0L, seed = 1L)

  for (imp in list(imp1, imp2, imp3)) {
    expect_true(is.numeric(imp$double))
    expect_true(is.numeric(imp$int))
    expect_true(is.logical(imp$logi))
    expect_true(is.character(imp$char))
   
    expect_true(is.factor(imp$fact))
    expect_equal(levels(imp$fact), levels(X$fact))
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
  
  suppressMessages(
    capture_output(
      imp <- missRanger(X2, . - double ~ ., num.trees = 20L, seed = 1L, data_only = FALSE) 
    )
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
  
  expect_equal(unname(na_per_col["int"]), 0L)
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
  
  expect_equal(unname(na_per_col[c("int", "double")]), c(0, 0))
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

