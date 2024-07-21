# STABILITY OF COVARIATES

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

test_that("ranger can flexibly predict on feature being character at fit time", {
  ir <- transform(iris, Species = as.character(Species))
  
  # Fit on character
  fit <- ranger::ranger(y = ir[[1L]], x = ir["Species"], num.trees = 10L)
  
  # Predict on character
  pred1 <- predict(fit, ir[c(1L, 51L, 101L), ])$predictions
  
  # Predict on data with just one row
  pred2 <- c(
    predict(fit, ir[1L, ])$predictions,
    predict(fit, ir[51L, ])$predictions,
    predict(fit, ir[101L, ])$predictions
  )

  # Predict on factor (iris instead of ir)
  pred3 <- predict(fit, iris[c(1L, 51L, 101L), ])$predictions
  
  expect_equal(pred1, pred2)
  expect_equal(pred1, pred3)
  expect_no_error(predict(fit, transform(ir[1:2, ], Species = "new value")))
  expect_no_error(predict(fit, transform(ir[1:2, ], Species = factor("new value"))))
})

test_that("ranger can flexibly predict on feature being factor at fit time", {
  # Fit on factor
  fit <- ranger::ranger(y = iris[[1L]], x = iris[5L], num.trees = 10L)
  
  # Predict on factor with consistent levels
  pred1 <- predict(fit, iris[c(1L, 51L, 101L), ])$predictions
  
  # Incomplete factor levels at prediction time
  pred2 <- c(
    predict(fit, droplevels(iris[1L, ]))$predictions,
    predict(fit, droplevels(iris[51L, ]))$predictions,
    predict(fit, droplevels(iris[101L, ]))$predictions
  )
  
  # Character at prediction time
  ir <- transform(iris, Species = as.character(Species))
  pred3 <- c(
    predict(fit, ir[1L, ])$predictions,
    predict(fit, ir[51L, ])$predictions,
    predict(fit, ir[101L, ])$predictions
  )
  
  expect_equal(pred1, pred2)
  expect_equal(pred1, pred3)
  
  # Even unseen factor levels or character strings
  expect_no_error(predict(fit, transform(iris[1L, ], Species = factor("new value"))))
  expect_no_error(predict(fit, transform(iris[1L, ], Species = "new value")))
})

test_that("ranger does not care if integer is represented as double", {
  y <- iris$Sepal.Length
  x1 <- round(iris["Sepal.Width"])  # Double
  x2 <- transform(x1, Sepal.Width = as.integer(Sepal.Width))
  
  fit1 <- ranger::ranger(y = y, x = x1, num.trees = 10L, seed = 1L)
  fit2 <- ranger::ranger(y = y, x = x2, num.trees = 10L, seed = 1L)
  
  # Compare predictions on "native" data type
  expect_equal(
    predict(fit1, x1[1:5, , drop = FALSE])$predictions,
    predict(fit2, x2[1:5, , drop = FALSE])$predictions
  )
  
  # Compare predictions for one model applied to "wrong" data type
  for (model in list(fit1, fit2)) {
    expect_equal(
      predict(model, x1[1:5, , drop = FALSE])$predictions,
      predict(model, x2[1:5, , drop = FALSE])$predictions
    )
  }
})

test_that("ranger does not care if logical is represented as double", {
  y <- iris$Sepal.Length
  x1 <- iris["Sepal.Width"] > median(iris$Sepal.Width)
  x2 <- transform(x1, Sepal.Width = as.double(Sepal.Width))
  
  fit1 <- ranger::ranger(y = y, x = x1, num.trees = 10L, seed = 1L)
  fit2 <- ranger::ranger(y = y, x = x2, num.trees = 10L, seed = 1L)
  
  # Compare predictions on "native" data type
  expect_equal(
    predict(fit1, x1[1:5, , drop = FALSE])$predictions,
    predict(fit2, x2[1:5, , drop = FALSE])$predictions
  )
  
  # Compare predictions for one model applied to "wrong" data type
  for (model in list(fit1, fit2)) {
    expect_equal(
      predict(model, x1[1:5, , drop = FALSE])$predictions,
      predict(model, x2[1:5, , drop = FALSE])$predictions
    )
  }
})

test_that("date feature work (and can be represented as number)", {
  y <- seq(-1, 1, length.out = 100)^2
  x1 <- data.frame(time = seq.Date(as.Date("2024-01-01"), by = "1 d", length.out = 100))
  x2 <- transform(x1, time = as.numeric(time))
  
  fit1 <- ranger::ranger(y = y, x = x1, num.trees = 10L, seed = 1L)
  fit2 <- ranger::ranger(y = y, x = x2, num.trees = 10L, seed = 1L)
  
  # Compare predictions
  expect_equal(
    predict(fit1, x1[1:5, , drop = FALSE])$predictions,
    predict(fit2, x2[1:5, , drop = FALSE])$predictions
  )
  
  # Compare predictions for one model applied to "wrong" data type
  for (model in list(fit1, fit2)) {
    expect_equal(
      predict(model, x1[1:5, , drop = FALSE])$predictions,
      predict(model, x2[1:5, , drop = FALSE])$predictions
    )
  }
})

# STABILITY OF PREDICTION CLASSES
test_that("ranger works with logical responses, but predictions are 0 or 1", {
  # This tests shows that predictions should be converted to logical
  y <- iris$Sepal.Length > median(iris$Sepal.Length)
  x <- iris[2:4]
  
  fit <- ranger::ranger(y = y, x = x, num.trees = 50L, seed = 1L)
  pred <- predict(fit, iris)$predictions
  
  expect_equal(fit$treetype, "Classification")
  expect_type(pred, "double")
  expect_true(all(pred %in% c(0, 1)))
  y[1:5] <- pred[1:5]
  expect_type(y, "double")
})

test_that("factor responses result in factor predictions with right levels", {
  y <- iris$Species
  x <- iris[2:4]
  
  fit <- ranger::ranger(y = y, x = x, num.trees = 10L)
  pred <- predict(fit, iris)$predictions
  
  expect_equal(fit$treetype, "Classification")
  expect_s3_class(pred, "factor")
  expect_equal(levels(pred), levels(y))
})

test_that("character responses are not accepted", {
  # Shows that character responses have to be converted to character
  y <- as.character(iris$Species)
  x <- iris[2:4]
  
  expect_error(fit <- ranger::ranger(y = y, x = x, num.trees = 10L))
})

test_that("date responses are not accepted", {
  # Shows that special classes (even if mode is numeric) are not possible
  y <- seq.Date(as.Date("2004-01-01"), length.out = nrow(iris), by = "1 d")
  x <- iris[2:4]
  
  expect_error(fit <- ranger::ranger(y = y, x = x, num.trees = 10L))
})

