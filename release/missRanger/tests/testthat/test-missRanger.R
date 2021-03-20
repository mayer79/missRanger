context("missRanger")

irisWithNA <- generateNA(iris, seed = 1, p = 0.3)
irisWithNA2 <- irisWithNA[c(1:2, 50:51, 100:101), ]

test_that("results are reproducible", {
  imp <- missRanger(irisWithNA, pmm.k = 3, verbose = 0, seed = 1, num.trees = 50)
  expect_true(!anyNA(imp))
  
  expected <- structure(list(
    Sepal.Length = c(5.1, 5.5, 4.7, 4.6, 5, 5), 
    Sepal.Width = c(3.1, 3, 3.2, 3.1, 3.6, 3.9), 
    Petal.Length = c(1.9, 3.5, 1.3, 1.5, 1.4, 1.7), 
    Petal.Width = c(0.4, 1, 0.2, 0.2, 0.2, 0.4), 
    Species = structure(c(1L, 1L, 1L, 1L, 1L, 1L), 
                        .Label = c("setosa", "versicolor", "virginica"), class = "factor")), 
    row.names = c(NA, 6L), class = "data.frame")
  expect_equal(imp[1:6, ], expected)
})

test_that("messages are suppressed with verbose=0", {
  expect_silent(missRanger(irisWithNA2, maxiter = 3, num.trees = 1, verbose = 0))
})

test_that("seed works", {
  imp1 <- missRanger(irisWithNA2, maxiter = 3, num.trees = 1, verbose = 0, seed = 1)
  imp2 <- missRanger(irisWithNA2, maxiter = 3, num.trees = 1, verbose = 0, seed = 1)
  expect_equal(imp1, imp2)
})

test_that("returnOOB works", {
  imp1 <- missRanger(irisWithNA2, maxiter = 3, num.trees = 1, 
                     verbose = 0, returnOOB = TRUE)
  expect_true(length(attributes(imp1)$oob) == 5L)
})

n <- 20
X <- data.frame(x1 = seq_len(n), 
                x2 = log(seq_len(n)), 
                x3 = rep(LETTERS[1:4], n %/% 4),
                x4 = factor(rep(LETTERS[1:2], n %/% 2)),
                x5 = seq_len(n) > n %/% 3)
X_NA <- generateNA(X, p = seq(0.2, 0.8, length.out = ncol(X)), seed = 13)

test_that("variable type is respected (integer might get double)", {
  imp <- missRanger(X_NA, maxiter = 3, num.trees = 20, verbose = 0)
  expect_true(!anyNA(imp))
  expect_equal(sapply(imp[-1], class), sapply(X_NA[-1], class))
  expect_true(class(imp[, 1]) %in% c("integer", "numeric"))
})

test_that("pmm.k works regarding value range in double columns", {
  imp <- missRanger(X_NA, maxiter = 3, num.trees = 20, verbose = 0, pmm.k = 3)
  expect_true(all(imp$x2 %in% X$x2))
  
  imp <- missRanger(X_NA, maxiter = 3, num.trees = 20, verbose = 0, pmm.k = 0)
  expect_false(all(imp$x2 %in% X$x2))
})

test_that("pmm.k works regarding value range in integer columns", {
  imp <- missRanger(X_NA, maxiter = 3, num.trees = 20, verbose = 0, pmm.k = 3)
  expect_true(all(imp$x1 %in% X$x1))
  
  imp <- missRanger(X_NA, maxiter = 3, num.trees = 20, verbose = 0, pmm.k = 0)
  expect_false(all(imp$x1 %in% X$x1))
})

test_that("formula interface works with specified left and right side", {
  imp <- missRanger(X_NA, x2 ~ x2 + x3, pmm = 3, num.trees = 20, verbose = 0)
  na_per_col <- colSums(is.na(imp))
  
  expect_equal(na_per_col[[2]], 0L)
  expect_true(all(na_per_col[-2] >= 1L))
})

test_that("formula interface works with unspecified right side", {
  imp <- missRanger(X_NA, x2 ~ ., pmm = 3, num.trees = 20, verbose = 0)
  na_per_col <- colSums(is.na(imp))
  
  expect_equal(na_per_col[[2]], 0L)
  expect_true(all(na_per_col[-2] >= 1L))
})

test_that("formula interface works with unspecified left side", {
  imp <- missRanger(X_NA, . ~ x1, pmm = 3, num.trees = 20, verbose = 0)
  expect_true(!anyNA(imp))
})

test_that("date and datetime columns work", {
  n <- 20
  Xd <- data.frame(time = seq(Sys.time(), by = "1 min", length.out = n),
                  date = seq(Sys.Date(), by = "1 d", length.out = n))
  Xd_NA <- generateNA(Xd, p = 0.3, seed = 13)
  
  imp <- missRanger(Xd_NA, maxiter = 3, num.trees = 20, verbose = 0, pmm.k = 0)
  expect_true(expect_true(!anyNA(imp)))
  expect_equal(sapply(imp, class), sapply(Xd_NA, class))
})

test_that("a too small mtry causes an error", {
  expect_error(missRanger(irisWithNA2, num.trees = 20, verbose = 0, mtry = 3))
})

test_that("mtry works", {
  imp1 <- missRanger(irisWithNA, num.trees = 3, verbose = 0, seed = 1, maxiter = 3,
                     mtry = function(m) max(1, m %/% 3))
  imp2 <- missRanger(irisWithNA, num.trees = 3, verbose = 0, seed = 1, mtry = 1, maxiter = 3)
  imp3 <- missRanger(irisWithNA, num.trees = 3, verbose = 0, seed = 1, maxiter = 3)
  expect_equal(imp1, imp2)
  expect_false(isTRUE(all.equal(imp1, imp3)))
})
