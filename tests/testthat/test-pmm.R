context("pmm")

test_that("it works for numeric vectors with specific result", {
  expected <- c(NA, NA, 3L, NA, 5L, NA, 7L, NA, 9L, 10L)
  expect_equal(generateNA(1:10, p = 0.5, seed = 3345), expected)
})

pmm(xtrain = c(0.2, 0.2, 0.8), xtest = 0.3, ytrain = c(0, 0, 1)) # 0
pmm(xtrain = c(TRUE, FALSE, TRUE), xtest = FALSE, ytrain = c(2, 0, 1)) # 0
pmm(xtrain = c(0.2, 0.8), xtest = 0.3, ytrain = c("A", "B"), k = 2) # "A" or "B"
pmm(xtrain = c("A", "A", "B"), xtest = "A", ytrain = c(2, 2, 4), k = 2) # 2
pmm(xtrain = factor(c("A", "B")), xtest = factor("C"), ytrain = 1:2) # 2
pmm(rep(Sys.Date(), 2), xtest = Sys.Date(), 3:4) # 4
pmm(rep(Sys.time(), 2), xtest = Sys.time(), 3:4) # 4
pmm(xtrain = 1, xtest = 1, ytrain = 1:2) # Error
pmm(1, xtest = Sys.Date(), ytrain = TRUE) # Works!
pmm(1:2, xtest = Sys.Date(), ytrain = c(FALSE, TRUE)) # Works!
pmm(1:10, 3:4, 1:10) # 3 4
pmm(c(NA, 1, 2), 1, ytrain = c(0, 1, NA)) # 1
pmm(c(NA, 1, 2), 1, ytrain = c(0, NA, 3)) # 3
pmm(c(NA, 1, 2), 1, ytrain = c(0, NA, NA)) # Error
pmm(c(NA, 1, 2), NA, ytrain = c(0, NA, 3)) # Error

