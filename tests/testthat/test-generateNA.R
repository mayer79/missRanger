test_that("it works for numeric vectors with specific result", {
  expected <- c(NA, NA, 3L, NA, 5L, NA, 7L, NA, 9L, 10L)
  expect_equal(generateNA(1:10, p = 0.5, seed = 3345), expected)
})

test_that("it works for character vectors", {
  x <- LETTERS[1:10]
  expect_true(anyNA(generateNA(x, p = 0.5)))
})

test_that("it works for factors", {
  x <- factor(LETTERS[1:10])
  expect_true(anyNA(generateNA(x, p = 0.5)))
})

test_that("it works for datetime vectors", {
  x <- as.Date("2020-01-02")
  expect_true(anyNA(generateNA(rep(x, 4), p = 0.5)))
})

test_that("it works for matrix object", {
  x <- cbind(1:3, 3:1)
  expect_true(anyNA(generateNA(x, p = 0.2)))
})

test_that("p has an effect", {
  x <- 1:100
  high <- generateNA(x, p = 0.5, seed = 1L)
  low <- generateNA(x, p = 0.2, seed = 1L)
  expect_true(sum(is.na(high)) > sum(is.na(low)))
})

test_that("it works for data.frame", {
  x <- iris[1:6, ]
  expect_true(anyNA(generateNA(x, p = 0.2)))
  
  holes <- generateNA(x, p = c(0, 1, 0, 0, 0))
  expect_true(all(is.na(holes[2L])))
  expect_true(!anyNA(holes[-2L]))
  
  holes <- generateNA(x, p = c(Sepal.Length = 1))
  expect_true(all(is.na(holes[1L])))
  expect_true(!anyNA(holes[-1L]))
})

test_that("it works for data.frame with one row", {
  x <- iris[1L, ]
  expect_true(!anyNA(generateNA(x, p = 0.5)))
  expect_true(all(is.na(generateNA(x, p = 0.55))))
})

