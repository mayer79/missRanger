test_that("pmm picks the right reference based on numeric values", {
  expect_equal(
    pmm(xtrain = c(0.2, 0.2, 0.8), xtest = 0.3, ytrain = c(0, 0, 1)), 0
  )
  expect_equal(pmm(1:10, 3:4, 1:10), 3:4)
})

test_that("pmm picks the right reference based on logical values", {
  expect_equal(
    pmm(xtrain = c(TRUE, FALSE, TRUE), xtest = FALSE, ytrain = c(2, 0, 1)), 0
  )
})

test_that("pmm picks the right reference based on factor values", {
  xtrain <- factor(c("A", "B"))
  xtest <- factor("B", levels = levels(xtrain))
  expect_equal(
    pmm(xtrain = xtrain, xtest = xtest, ytrain = c(2, 0), k = 1L), 0
  )
})

test_that("character, Dates and incompatible factors lead to an error", {
  expect_error(pmm(xtrain = c("A", "A", "B"), xtest = "A", ytrain = c(2, 2, 4), k = 2))
  expect_error(pmm(xtrain = factor(c("A", "B")), xtest = factor("C"), ytrain = 1:2))
})

test_that("pmm gives error if ytrain is of wrong length", {
  expect_error(pmm(xtrain = 1, xtest = 1, ytrain = 1:2))
})

test_that("pmm works with NA in xtrain and NA in ytrain", {
  expect_equal(pmm(c(NA, 1, 2), 1, ytrain = c(0, NA, 3)), 3)
})

test_that("pmm gives error when xtest contains NA", {
  expect_error(pmm(c(1, 1, 2), NA, ytrain = c(0, 1, 2)))
})

