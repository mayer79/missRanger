context("generateNA")

test_that("it works for numeric vectors", {
  expected <- c(NA, NA, 3L, NA, 5L, NA, 7L, NA, 9L, 10L)
  expect_equal(generateNA(1:10, p = 0.5, seed = 3345), expected)
})

test_that("it works for datetime vectors", {
  x <- as.Date("2020-01-02")
  expected <- c(x, NA, x, NA)
  expect_equal(generateNA(rep(x, 4), p = 0.5, seed = 3345), expected)
})

test_that("it works for matrix", {
  x <- cbind(1:3, 3:1)
  expected <- cbind(1:3, c(3, 2, NA))
  expect_equal(generateNA(x, p = 0.2, seed = 3345), expected)
})

test_that("it works for data.frame", {
  x <- iris[1:6, ]
  expect_equal(generateNA(x, p = 0.2, seed = 3345)[6, 1], NA_real_)
  expect_equal(generateNA(x, p = c(0, 1, 0, 0, 0))[1, 2], NA_real_)
  expect_equal(generateNA(x, p = c(Sepal.Length = 1))[1, 1], NA_real_)
})

test_that("it works for data.frame with one row", {
  x <- iris[1, ]
  expect_equal(generateNA(x, p = 0.5), x)
  expect_equal(generateNA(x, p = 0.55)[1, 1], NA_real_)
})

