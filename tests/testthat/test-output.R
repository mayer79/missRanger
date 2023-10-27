CO2_ <- generateNA(CO2, seed = 1, p = 0.3)
i1 <- missRanger(
  CO2_, seed = 1, verbose = 0, returnOOB = TRUE, data_only = TRUE, num.trees = 20
)
oob <- attr(i1, "oob")
attr(i1, "oob") <- NULL

i2 <- missRanger(
  CO2_, seed = 1, verbose = 0, returnOOB = TRUE, data_only = FALSE, num.trees = 20
)

test_that("imputed data and OOB error does not depend on 'data_only' option", {
  expect_equal(i1, i2$data)
  expect_equal(oob, i2$pred_errors[i2$best_iter, ])
})

test_that("print() gives no error", {
  capture_output(expect_no_error(print(i2)))
})

test_that("summary() gives no error", {
  capture_output(expect_no_error(summary(i2)))
})

test_that("best_iter may also equal to maxiter", {
  i1 <- missRanger(
    CO2_, seed = 1, verbose = 0, returnOOB = TRUE, 
    data_only = TRUE, num.trees = 20, maxiter = 2
  )
  
  oob <- attr(i1, "oob")
  attr(i1, "oob") <- NULL
  
  i2 <- missRanger(
    CO2_, seed = 1, verbose = 0, returnOOB = TRUE, 
    data_only = FALSE, num.trees = 20, maxiter = 2
  )
  
  expect_equal(i1, i2$data)
  expect_equal(i2$best_iter, 2L)
  expect_equal(oob, i2$pred_errors[i2$best_iter, ])
})
