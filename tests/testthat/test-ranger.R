test_that("x/y API of ranger does not care about feature order in x at prediction", {
  fit <- ranger::ranger(y = iris[[1L]], x = iris[, 2:5], num.trees = 10L)
  pred1 <- predict(fit, iris[1:5, 2:5])
  pred2 <- predict(fit, iris[1:5, 5:2])
  expect_equal(pred1, pred2)
})

test_that("x/y API of ranger does not care about extra columns during prediction", {
  fit <- ranger::ranger(y = iris[[1L]], x = iris[, 2:5], num.trees = 10L)
  pred1 <- predict(fit, iris[1:5, 2:5])
  pred2 <- predict(fit, iris[1:5, ])
  expect_equal(pred1, pred2)
})


test_that("ranger can safely predict on character feature", {
  ir <- transform(iris, Species = as.character(Species))
  fit <- ranger::ranger(Sepal.Length ~ ., data = ir)
  
  pred1 <- predict(fit, ir[c(1, 51, 101), ])$predictions
  pred2 <- c(
    predict(fit, ir[c(1), ])$predictions,
    predict(fit, ir[c(51), ])$predictions,
    predict(fit, ir[c(101), ])$predictions
  )
  expect_equal(pred1, pred2)
})
