test_that("it works for numeric vectors", {
  x <- c(NA, 1:10, NA)
  filled <- imputeUnivariate(x)
  expect_true(!anyNA(filled))
})

test_that("it gives same value for numeric vector", {
  x <- c(NA, NA, 1:10)
  filled <- imputeUnivariate(x, seed = 1L)
  expect_equal(filled[1:2], c(9L, 4L))
})

test_that("it works for character vectors", {
  x <- c(LETTERS[1:10], NA, NA)
  filled <- imputeUnivariate(x)
  expect_true(!anyNA(filled))
})

test_that("it works for factors", {
  x <- factor(c(LETTERS[1:10], NA, NA))
  filled <- imputeUnivariate(x)
  expect_true(!anyNA(filled))
})

test_that("it works for boolean vectors", {
  x <- c(TRUE, TRUE, FALSE, NA, NA)
  filled <- imputeUnivariate(x)
  expect_true(!anyNA(filled))
})

test_that("it works for date vectors", {
  x <- generateNA(rep(Sys.Date(), 10L), p = 0.5)
  filled <- imputeUnivariate(x)
  expect_true(!anyNA(filled))
})

test_that("it works for matrix objects", {
  x <- generateNA(cbind(1:10, 1:10), p = 0.5)
  filled <- imputeUnivariate(x)
  expect_true(!anyNA(filled))
})

test_that("it works for data.frames", {
  x <- generateNA(head(iris), p = 0.5)
  filled <- imputeUnivariate(x)
  expect_true(!anyNA(filled))
})

test_that("it can impute only certain columns", {
  x <- generateNA(iris, seed = 10L)
  filled <- imputeUnivariate(x, v = c("Species", "Petal.Length"))
  expect_true(!anyNA(filled[["Species"]]))
  expect_true(anyNA(filled[["Petal.Width"]]))
})

test_that("it fails when all values in a column are missing", {
  x <- rep(NA, 10L)
  expect_error(imputeUnivariate(x))
})
