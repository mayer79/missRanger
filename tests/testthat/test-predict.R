# Out-of-sample (OOS) Application

iris1 <- generateNA(iris, p = c(Sepal.Width = 0.2, Species = 0.1), seed = 1L)
iris2 <- generateNA(iris, p = 0.2, seed = 1L)

test_that("OOS can work in some cases without forests", {
  imp <- missRanger(iris1, verbose = 0, seed = 1L, num.trees = 10, data_only = FALSE)
  
  # There are no missings
  expect_equal(predict(imp, head(iris)), head(iris))
  
  # We use univariate imputation at prediction stage
  expect_message(
    pred <- predict(imp, tail(iris1), iter = 0L), 
    "Only univariate imputations done"
  )
  expect_true(!anyNA(pred))
  
  # We use univariate imputation at missRanger() stage
  imp <- missRanger(
    iris1, . ~ 1, verbose = 0, seed = 1L, num.trees = 10, data_only = FALSE
  )
  
  expect_message(
    pred <- predict(imp, tail(iris1)),
    "Only univariate imputations done"
  )
  expect_true(!anyNA(pred))
  
  # But usually, it gives an error
  imp <- missRanger(iris1, verbose = 0, seed = 1L, num.trees = 10, data_only = FALSE)
  
  # There are no missings
  expect_error(predict(imp, tail(iris1)), "No random forests")
})

test_that("OOS removes all missings with and without PMM", {
  imp <- missRanger(iris2, verbose = 0, seed = 1L, num.trees = 10, keep_forests = TRUE)
  
  expect_true(!anyNA(predict(imp, iris2)))
  expect_true(!anyNA(predict(imp, iris2, pmm.k = 1L)))
  
  imp <- missRanger(iris1, verbose = 0, seed = 1L, num.trees = 10, keep_forests = TRUE)
  
  expect_true(!anyNA(predict(imp, iris1)))
  expect_true(!anyNA(predict(imp, iris1, pmm.k = 1L)))
  
  expect_error(predict(imp, iris2), "not allowed.")
})

test_that("OOS is picky about wrong input", {
  imp <- missRanger(iris2, verbose = 0, seed = 1L, num.trees = 10, keep_forests = TRUE)
  
  one_row <- iris2[1L, ]
  one_row[, "Species"] <- "new species"
  expect_error(predict(imp, one_row), "Inconsistency between.")
  
  one_row <- iris2[1L, ]
  one_row[, "Sepal.Length"] <- "new species"
  expect_error(predict(imp, one_row), "Inconsistency between.")
})

test_that("OOS expects columns to be present, but extra columns are okay", {
  imp <- missRanger(iris2, verbose = 0, seed = 1L, num.trees = 10, keep_forests = TRUE)
  
  expect_error(predict(imp, head(iris2[, -5])), "Variables not present.")
  expect_no_error(predict(imp, iris2 |> transform(additional = 1)))
})

test_that("OOS can deal with single-iteration fits, when one forest is missing", {
  imp <- missRanger(
    iris2, verbose = 0, seed = 1L, num.trees = 10, keep_forests = TRUE, maxiter = 1L
  )
  
  expect_message(pred <- predict(imp, iris2), "No random forest")
  expect_no_message(pred <- predict(imp, iris2, verbose = FALSE))
})

test_that("OOS seed works", {
  imp <- missRanger(iris2, verbose = 0, seed = 1L, num.trees = 10, keep_forests = TRUE)
  
  x1 <- predict(imp, iris2, seed = 1L)
  x2 <- predict(imp, iris2, seed = 1L)
  x3 <- predict(imp, iris2, seed = 2L)
  
  expect_equal(x1, x2)
  expect_false(identical(x1, x3))
})

test_that("OOS PMM works", {
  imp <- missRanger(iris2, verbose = 0, seed = 1L, num.trees = 10, keep_forests = TRUE)
  
  x1 <- predict(imp, iris2, seed = 1L)
  x2 <- predict(imp, iris2, seed = 1L, pmm.k = 5)
  
  expect_false(identical(x1, x2))
  
  # Fitted with PMM
  imp <- missRanger(
    iris2, verbose = 0, seed = 1L, num.trees = 10, keep_forests = TRUE, pmm.k = 5L
  )
  
  x1 <- predict(imp, iris2, seed = 1L, pmm.k = 0)
  x2 <- predict(imp, iris2, seed = 1L, pmm.k = 5)
  
  expect_false(identical(x1, x2))
})

test_that("OOS iter works", {
  imp <- missRanger(iris2, verbose = 0, seed = 1L, num.trees = 10, keep_forests = TRUE)
  
  x1 <- predict(imp, iris2, seed = 1L, iter = 1, pmm.k = 4L)
  x2 <- predict(imp, iris2, seed = 1L, iter = 4, pmm.k = 4L)
  
  expect_false(identical(x1, x2))
})

test_that("OOS is not working when there are missings in impute_by", {
  imp <- missRanger(
    iris1,
    Species ~ Sepal.Length,
    verbose = 0,
    seed = 1L,
    num.trees = 10,
    keep_forests = TRUE
  )
  
  new_row <- iris[1L, ]
  new_row[, c("Sepal.Length", "Species")] <- NA

  expect_error(predict(imp, new_row), "not allowed")
})

test_that("OOS does 1 iter if only easy case", {
  imp <- missRanger(iris2, verbose = 0, seed = 1L, num.trees = 10, keep_forests = TRUE)
  
  new_row <- iris[1L, ]
  new_row[, "Sepal.Length"] <- NA
  
  x1 <- predict(imp, new_row, seed = 1L, pmm.k = 5L, iter = 10L)
  x2 <- predict(imp, new_row, seed = 1L, pmm.k = 5L, iter = 1L)
  
  expect_equal(x1, x2)
})

test_that("OOS does not fail if there is all missings, even of wrong type", {
  imp <- missRanger(iris2, verbose = 0, seed = 1L, num.trees = 10, keep_forests = TRUE)
  
  new_rows <- iris[1:2, ]
  new_rows[] <- NA
  
  expect_no_error(x1 <- predict(imp, new_rows, seed = 1L))
})

n <- 200L

X <- data.frame(
  int = seq_len(n), 
  double = log(seq_len(n)), 
  char = rep(LETTERS[1:4], n %/% 4),
  fact = factor(rep(LETTERS[1:2], n %/% 2)),
  logi = seq_len(n) > n %/% 3
)

X_NA <- generateNA(X[1:5], p = 0.2, seed = 1L)

test_that("OOS does not fail if there is all missings, even of wrong type (MORE TYPES)", {
  imp1 <- missRanger(
    X_NA, num.trees = 20L, verbose = 0L, seed = 1L, keep_forests = TRUE
  )
  
  new_rows <- X[1:2, ]
  new_rows[] <- NA
  
  expect_no_error(x1 <- predict(imp1, new_rows, seed = 1L, pmm.k = 1L))
  expect_equal(lapply(x1, class), lapply(X, class))
  
  # Without PMM, ints might turn into doubles
  expect_no_error(x1 <- predict(imp1, new_rows, seed = 1L, pmm.k = 0L))
  xp <- list(int = "numeric", double = "numeric", char = "character", 
             fact = "factor", logi = "logical")
  expect_equal(lapply(x1, class), xp)
})

test_that("non-syntactic column names work", {
  X_NA2 <- X_NA
  colnames(X_NA2)[1:2] <- c("1bad name", "2 also bad")
  
  imp1 <- missRanger(X_NA2, num.trees = 20L, verbose = 0L, seed = 1L, keep_forests = TRUE)
  
  expect_equal(colnames(predict(imp1, head(X_NA2))), colnames(X_NA2))
})

