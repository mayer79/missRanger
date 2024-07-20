irisWithNA <- generateNA(iris, seed = 1, p = 0.3)

test_that("all missings are filled", {
  imp <- missRanger(irisWithNA, pmm.k = 3L, verbose = 0L, seed = 1L, num.trees = 50L)
  expect_true(!anyNA(imp))
})

test_that("pmm.k works", {
  imp1 <- missRanger(
    irisWithNA, maxiter = 3L, num.trees = 5L, verbose = 0L, pmm.k = 0L, seed = 1L
  )
  imp2 <- missRanger(
    irisWithNA, maxiter = 3L, num.trees = 5L, verbose = 0L, pmm.k = 3L, seed = 1L
  )
  
  expect_false(isTRUE(all.equal(imp1, imp2)))
  expect_false(all(imp1$Sepal.Length %in% irisWithNA$Sepal.Length))
  expect_true(all(imp2$Sepal.Length %in% irisWithNA$Sepal.Length))
})

test_that("num.trees has an effect", {
  imp1 <- missRanger(irisWithNA, num.trees = 10L, verbose = 0L, seed = 1L)
  imp2 <- missRanger(irisWithNA, num.trees = 20L, verbose = 0L, seed = 1L)
  
  expect_false(isTRUE(all.equal(imp1, imp2)))
})

test_that("mtry has an effect", {
  imp1 <- missRanger(irisWithNA, num.trees = 10L, verbose = 0L, seed = 1L)
  imp2 <- missRanger(irisWithNA, num.trees = 10L, verbose = 0L, seed = 1L, mtry = 1)
  imp3 <- missRanger(
    irisWithNA, num.trees = 10L, verbose = 0L, seed = 1L, mtry = function(m) min(m, 4)
  )
  
  expect_false(isTRUE(all.equal(imp1, imp2)))
  expect_false(isTRUE(all.equal(imp1, imp3)))
  expect_false(isTRUE(all.equal(imp2, imp3)))
})

test_that("min.node.size has an effect", {
  imp1 <- missRanger(
    irisWithNA, num.trees = 10L, verbose = 0L, seed = 1L, min.node.size = 10
  )
  imp2 <- missRanger(
    irisWithNA, num.trees = 10L, verbose = 0L, seed = 1L, min.node.size = 20
  )

  expect_false(isTRUE(all.equal(imp1, imp2)))
})

test_that("min.bucket has an effect", {
  imp1 <- missRanger(
    irisWithNA, num.trees = 10L, verbose = 0L, seed = 1L, min.bucket = 10
  )
  imp2 <- missRanger(
    irisWithNA, num.trees = 10L, verbose = 0L, seed = 1L, min.bucket = 20
  )
  
  expect_false(isTRUE(all.equal(imp1, imp2)))
})

test_that("max.depth has an effect", {
  imp1 <- missRanger(
    irisWithNA, num.trees = 10L, verbose = 0L, seed = 1L, max.depth = 1
  )
  imp2 <- missRanger(
    irisWithNA, num.trees = 10L, verbose = 0L, seed = 1L, max.depth = 2
  )
  
  expect_false(isTRUE(all.equal(imp1, imp2)))
})

test_that("replace has an effect", {
  imp1 <- missRanger(
    irisWithNA, num.trees = 10L, verbose = 0L, seed = 1L, replace = TRUE
  )
  imp2 <- missRanger(
    irisWithNA, num.trees = 10L, verbose = 0L, seed = 1L, replace = FALSE
  )
  
  expect_false(isTRUE(all.equal(imp1, imp2)))
})

test_that("sample.fraction has an effect", {
  imp1 <- missRanger(
    irisWithNA, num.trees = 10L, verbose = 0L, seed = 1L, sample.fraction = 1
  )
  imp2 <- missRanger(
    irisWithNA, num.trees = 10L, verbose = 0L, seed = 1L, sample.fraction = 0.5
  )
  
  expect_false(isTRUE(all.equal(imp1, imp2)))
})

test_that("case.weights works", {
  imp1 <- missRanger(irisWithNA, num.trees = 10L, verbose = 0L, seed = 1L)
  imp2 <- missRanger(
    irisWithNA,
    num.trees = 10L,
    verbose = 0L,
    seed = 1L,
    case.weights = rep(1, nrow(iris))
  )
  imp3 <- missRanger(
    irisWithNA, num.trees = 10L, verbose = 0L, seed = 1L, case.weights = 1:nrow(iris)
  )
  
  expect_true(isTRUE(all.equal(imp1, imp2)))
  expect_false(isTRUE(all.equal(imp1, imp3)))
  expect_error(missRanger(irisWithNA, num.trees = 3L, verbose = 0L, case.weights = 1:7))
})

test_that("seed works", {
  imp1 <- missRanger(irisWithNA, maxiter = 3L, num.trees = 5L, verbose = 0L, seed = 1L)
  imp2 <- missRanger(irisWithNA, maxiter = 3L, num.trees = 5L, verbose = 0L, seed = 1L)
  imp3 <- missRanger(irisWithNA, maxiter = 3L, num.trees = 5L, verbose = 0L, seed = 2L)
  
  expect_equal(imp1, imp2)
  expect_false(identical(imp1, imp3))
})

test_that("verbose works", {
  expect_silent(missRanger(irisWithNA, num.trees = 3L, maxiter = 3L, verbose = 0L))
  
  capture_output(
    expect_no_error(
      missRanger(irisWithNA, num.trees = 3L, verbose = 1L, maxiter = 3L)
    )
  )
  
  capture_output(
    expect_no_error(
      missRanger(irisWithNA, num.trees = 3L, verbose = 2L, maxiter = 3L)
    )
  )
})

test_that("returnOOB works", {
  imp1 <- missRanger(
    irisWithNA, maxiter = 3L, num.trees = 5L, verbose = 0L, returnOOB = TRUE
  )
  imp2 <- missRanger(
    irisWithNA, maxiter = 3L, num.trees = 5L, verbose = 0L, returnOOB = FALSE
  )
  
  expect_true(length(attributes(imp1)$oob) == ncol(iris))
  expect_null(attributes(imp2)$oob)
})

test_that("Special columns like dates can't be filled but work as features", {
  ir1 <- transform(
    iris[1:2], 
    date = seq.Date(as.Date("2001-01-01"), length.out = nrow(iris), by = "1 d")
  ) |> 
    generateNA()
  
  expect_error(missRanger(ir1, maxiter = 3L, num.trees = 5L, verbose = 0L))
  
  ir2 <- transform(
    irisWithNA[1:2],
    date = seq.Date(as.Date("2001-01-01"), length.out = nrow(iris), by = "1 d")
  )
  
  imp1 <- missRanger(
    ir2,
    maxiter = 3L,
    num.trees = 20L,
    verbose = 0L,
    seed = 1L,
    data_only = FALSE,
    mtry = 1
  )
  imp2 <- missRanger(
    ir2, 
    . ~ . - Sepal.Width,
    maxiter = 3L,
    num.trees = 20L,
    verbose = 0L,
    seed = 1L,
    data_only = FALSE,
    mtry = 1
  )
  
  expect_false(identical(imp1$data, imp2$data))
  expect_equal(sort(imp1$to_impute), c("Sepal.Length", "Sepal.Width"))
  expect_equal(sort(imp2$to_impute), c("Sepal.Length", "Sepal.Width"))
  expect_equal(imp1$impute_by, c("Sepal.Length", "Sepal.Width", "date"))
  expect_equal(imp2$impute_by, c("Sepal.Length", "date"))
})

# FORMULA PARSING
n <- 20L
X <- data.frame(
  x1 = seq_len(n), 
  x2 = log(seq_len(n)), 
  x3 = rep(LETTERS[1:4], n %/% 4),
  x4 = factor(rep(LETTERS[1:2], n %/% 2)),
  x5 = seq_len(n) > n %/% 3
)

X_NA <- generateNA(X, p = seq(0.2, 0.8, length.out = ncol(X)), seed = 13L)

test_that("formula interface works with specified left and right side", {
  imp <- missRanger(
    X_NA, x2 ~ x2 + x3, pmm = 3L, num.trees = 5L, verbose = 0L, data_only = FALSE
  )
  na_per_col <- colSums(is.na(imp$data))
  
  expect_equal(na_per_col[[2L]], 0L)
  expect_true(all(na_per_col[-2L] >= 1L))
  expect_equal(imp$to_impute, "x2")
  expect_equal(imp$impute_by, "x2")
})

test_that("formula interface works with unspecified right side", {
  imp <- missRanger(
    X_NA, x2 + x3 ~ ., pmm = 3L, num.trees = 5L, verbose = 0L, data_only = FALSE
  )
  na_per_col <- colSums(is.na(imp$data))
  
  expect_equal(unname(na_per_col[2:3]), c(0, 0))
  expect_true(all(na_per_col[-c(2:3)] >= 1L))
  expect_equal(sort(imp$to_impute), c("x2", "x3"))
  expect_equal(sort(imp$impute_by), c("x2", "x3"))
})

test_that("formula interface works with unspecified left side", {
  imp <- missRanger(
    X_NA, . ~ x1, pmm = 3L, num.trees = 5L, verbose = 0L, data_only = FALSE
  )
  expect_true(!anyNA(imp$data))
  expect_equal(sort(imp$to_impute), sort(colnames(X_NA)))
  expect_equal(imp$impute_by, "x1")
})

test_that("dropping columns on left side leave missing values", {
  imp <- missRanger(
    X_NA, . - x1 ~ ., pmm = 3L, num.trees = 5L, verbose = 0L, data_only = FALSE
  )
  expect_equal(
    colSums(is.na(imp$data)) > 0,
    c(x1 = TRUE, x2 = FALSE, x3 = FALSE, x4 = FALSE, x5 = FALSE)
  )
  xpected <- setdiff(colnames(X_NA), "x1")
  expect_equal(sort(imp$to_impute), sort(xpected))
  expect_equal(imp$impute_by, xpected)
})

test_that("dropping columns on right side has an impact", {
  imp1 <- missRanger(
    X_NA, . ~ . - x1, num.trees = 5L, verbose = 0L, seed = 1L, data_only = FALSE
  )
  imp2 <- missRanger(X_NA, num.trees = 5L, verbose = 0L, seed = 1L)
  
  expect_false(identical(imp1$data, imp2))
  expect_equal(sort(imp1$to_impute), sort(colnames(X_NA)))
  expect_equal(imp1$impute_by, setdiff(colnames(X_NA), "x1"))
})

test_that("non-syntactic column names work with or without formula", {
  X_NA2 <- X_NA
  colnames(X_NA2) <- paste(1:5, colnames(X_NA))
  
  imp <- missRanger(X_NA, maxiter = 3L, num.trees = 5L, verbose = 0L, seed = 1L)
  imp2 <- missRanger(X_NA2, maxiter = 3L, num.trees = 5L, verbose = 0L, seed = 1L)
  imp3 <- missRanger(
    `1 x1` + `2 x2` + `3 x3` + `4 x4` + `5 x5` ~ `1 x1` + `2 x2` + `3 x3` + `4 x4` + `5 x5`,
    data = X_NA2, 
    maxiter = 3L, 
    num.trees = 5L, 
    verbose = 0L, 
    seed = 1L
  )
  
  imp4 <- missRanger(
    . ~ `1 x1` + `2 x2` + `3 x3` + `4 x4` + `5 x5`,
    data = X_NA2, 
    maxiter = 3L, 
    num.trees = 5L, 
    verbose = 0L, 
    seed = 1L
  )
  
  imp5 <- missRanger(
    `1 x1` + `2 x2` + `3 x3` + `4 x4` + `5 x5` ~ .,
    data = X_NA2, 
    maxiter = 3L, 
    num.trees = 5L, 
    verbose = 0L, 
    seed = 1L
  )
  
  imp6 <- missRanger(
    . - `1 x1` ~ . - `1 x1`,
    data = X_NA2, 
    maxiter = 3L, 
    num.trees = 5L, 
    verbose = 0L, 
    seed = 1L
  )
  
  expect_equal(colnames(X_NA2), colnames(imp2))
  expect_equal(imp, setNames(imp2, colnames(imp)))
  expect_equal(imp2, imp3)
  expect_equal(imp2, imp4)
  expect_equal(imp2, imp5)
  
  expect_equal(
    unname(colSums(is.na(imp6)) == 0L),
    c(FALSE, TRUE, TRUE, TRUE, TRUE)
  )
})

test_that("Extremely wide datasets are handled", {
  # https://github.com/mayer79/missRanger/issues/50
  set.seed(1L)
  data <- as.data.frame(matrix(rnorm(385 * 20000), nrow = 385L, ncol = 20000L))
  data[5L, 5L] <- NA
  expect_no_error(
    missRanger(data, num.trees = 3L, verbose = 0L, maxiter = 3, max.depth = 1)
  )
  rm(data)
})

