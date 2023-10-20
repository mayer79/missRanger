irisWithNA <- generateNA(iris, seed = 1, p = 0.3)

test_that("all missings are filled", {
  imp <- missRanger(irisWithNA, pmm.k = 3L, verbose = 0L, seed = 1L, num.trees = 50L)
  expect_true(!anyNA(imp))
})

test_that("messages are suppressed with verbose=0", {
  expect_silent(missRanger(irisWithNA, maxiter = 3L, num.trees = 50L, verbose = 0L))
})

test_that("seed works", {
  imp1 <- missRanger(irisWithNA, maxiter = 3L, num.trees = 50L, verbose = 0L, seed = 1L)
  imp2 <- missRanger(irisWithNA, maxiter = 3L, num.trees = 50L, verbose = 0L, seed = 1L)
  expect_equal(imp1, imp2)
})

test_that("returnOOB works", {
  imp1 <- missRanger(
    irisWithNA, maxiter = 3L, num.trees = 50L, verbose = 0L, returnOOB = TRUE
  )
  expect_true(length(attributes(imp1)$oob) == 5L)
})

n <- 20L
X <- data.frame(
  x1 = seq_len(n), 
  x2 = log(seq_len(n)), 
  x3 = rep(LETTERS[1:4], n %/% 4),
  x4 = factor(rep(LETTERS[1:2], n %/% 2)),
  x5 = seq_len(n) > n %/% 3
)
X_NA <- generateNA(X, p = seq(0.2, 0.8, length.out = ncol(X)), seed = 13L)
imp <- missRanger(X_NA, maxiter = 3L, num.trees = 20L, verbose = 0L, seed = 1L)

test_that("variable type is respected (integer might get double)", {
  expect_true(!anyNA(imp))
  expect_equal(sapply(imp[-1L], class), sapply(X_NA[-1L], class))
  expect_true(class(imp[, 1L]) %in% c("integer", "numeric"))
})

test_that("non-syntactic column names work", {
  X_NA2 <- X_NA
  colnames(X_NA2) <- paste(1:5, colnames(X_NA))
  imp2 <- missRanger(X_NA2, maxiter = 3L, num.trees = 20L, verbose = 0L, seed = 1L)
  imp3 <- missRanger(
    `1 x1` + `2 x2` + `3 x3` + `4 x4` + `5 x5` ~ `1 x1` + `2 x2` + `3 x3` + `4 x4` + `5 x5`,
    data = X_NA2, 
    maxiter = 3L, 
    num.trees = 20L, 
    verbose = 0L, 
    seed = 1L
  )
  
  imp4 <- missRanger(
    . ~ `1 x1` + `2 x2` + `3 x3` + `4 x4` + `5 x5`,
    data = X_NA2, 
    maxiter = 3L, 
    num.trees = 20L, 
    verbose = 0L, 
    seed = 1L
  )
  
  imp5 <- missRanger(
    `1 x1` + `2 x2` + `3 x3` + `4 x4` + `5 x5` ~ .,
    data = X_NA2, 
    maxiter = 3L, 
    num.trees = 20L, 
    verbose = 0L, 
    seed = 1L
  )
  expect_equal(colnames(X_NA2), colnames(imp2))
  expect_equal(imp, setNames(imp2, colnames(imp)))
  expect_equal(imp2, imp3)
  expect_equal(imp2, imp4)
  expect_equal(imp2, imp5)
})

test_that("pmm.k works regarding value range in double columns", {
  imp <- missRanger(X_NA, maxiter = 3L, num.trees = 20L, verbose = 0L, pmm.k = 3L)
  expect_true(all(imp$x2 %in% X$x2))
  
  imp <- missRanger(X_NA, maxiter = 3L, num.trees = 20L, verbose = 0L, pmm.k = 0L)
  expect_false(all(imp$x2 %in% X$x2))
})

test_that("pmm.k works regarding value range in integer columns", {
  imp <- missRanger(X_NA, maxiter = 3L, num.trees = 20L, verbose = 0L, pmm.k = 3L)
  expect_true(all(imp$x1 %in% X$x1))
  
  imp <- missRanger(X_NA, maxiter = 3L, num.trees = 20L, verbose = 0L, pmm.k = 0L)
  expect_false(all(imp$x1 %in% X$x1))
})

test_that("formula interface works with specified left and right side", {
  imp <- missRanger(X_NA, x2 ~ x2 + x3, pmm = 3L, num.trees = 20L, verbose = 0L)
  na_per_col <- colSums(is.na(imp))
  
  expect_equal(na_per_col[[2L]], 0L)
  expect_true(all(na_per_col[-2L] >= 1L))
})

test_that("formula interface works with unspecified right side", {
  imp <- missRanger(X_NA, x2 ~ ., pmm = 3L, num.trees = 20L, verbose = 0L)
  na_per_col <- colSums(is.na(imp))
  
  expect_equal(na_per_col[[2L]], 0L)
  expect_true(all(na_per_col[-2L] >= 1L))
})

test_that("formula interface works with unspecified left side", {
  imp <- missRanger(X_NA, . ~ x1, pmm = 3L, num.trees = 20L, verbose = 0L)
  expect_true(!anyNA(imp))
})

test_that("date and datetime columns work", {
  n <- 20L
  Xd <- data.frame(
    time = seq(Sys.time(), by = "1 min", length.out = n),
    date = seq(Sys.Date(), by = "1 d", length.out = n)
  )
  Xd_NA <- generateNA(Xd, p = 0.3, seed = 13L)
  
  imp <- missRanger(Xd_NA, maxiter = 3L, num.trees = 20L, verbose = 0L, pmm.k = 0L)
  expect_true(expect_true(!anyNA(imp)))
  expect_equal(sapply(imp, class), sapply(Xd_NA, class))
})

test_that("mtry works", {
  imp1 <- missRanger(
    irisWithNA, 
    num.trees = 30L, 
    verbose = 0L, 
    seed = 1L, 
    maxiter = 3L,
    mtry = function(m) max(1, m %/% 3)
  )
  
  imp2 <- missRanger(
    irisWithNA, 
    num.trees = 30L, 
    verbose = 0L, 
    seed = 1L, 
    mtry = 1L, 
    maxiter = 3L
  )
  
  imp3 <- missRanger(
    irisWithNA, 
    num.trees = 30L, 
    verbose = 0L, 
    seed = 1L, 
    maxiter = 3L
  )
  
  expect_equal(imp1, imp2)
  expect_false(isTRUE(all.equal(imp1, imp3)))
})

test_that("verbose >= 1 does not produce an error", {
  capture_output(
    expect_no_error(
      missRanger(
        irisWithNA, 
        num.trees = 3L, 
        verbose = 1L, 
        seed = 1L, 
        maxiter = 1L,
      )
    )
  )

  capture_output(
    expect_no_error(
      missRanger(
        irisWithNA, 
        num.trees = 3L, 
        verbose = 2L, 
        seed = 1L, 
        maxiter = 1L,
      )
    )
  )
})

test_that("Too few case.weights give an error", {
  expect_error(
    missRanger(
      irisWithNA,
      num.trees = 3L,
      verbose = 0L,
      seed = 1L,
      maxiter = 1L,
      case.weights = 1:10
    )
  )
})

test_that("Extremely wide datasets are handled", {
  set.seed(1L)
  data <- matrix(rnorm(385 * 20000), nrow = 385L, ncol = 20000L)
  data[5L, 5L] <- NA
  expect_no_error(missRanger(as.data.frame(data), num.trees = 3L, verbose = 0L))
})
