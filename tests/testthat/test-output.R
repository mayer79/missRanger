CO2_ <- generateNA(CO2, seed = 1, p = 0.3)
i1 <- missRanger(
  CO2_, seed = 1, verbose = 0, returnOOB = TRUE, data_only = TRUE, num.trees = 20
)
oob <- attr(i1, "oob")
attr(i1, "oob") <- NULL

i2 <- missRanger(CO2_, seed = 1, verbose = 0, data_only = FALSE, num.trees = 20)

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
    CO2_, seed = 1, verbose = 0, data_only = FALSE, num.trees = 20, maxiter = 2
  )
  
  expect_equal(i1, i2$data)
  expect_equal(i2$best_iter, 2L)
  expect_equal(oob, i2$pred_errors[i2$best_iter, ])
})

test_that("Output options work also in the case no imputation happens", {
  i1 <- missRanger(
    CO2_, 1 ~ ., seed = 1, verbose = 0, returnOOB = TRUE, 
    data_only = TRUE, num.trees = 20, maxiter = 2
  )
  
  oob <- attr(i1, "oob")
  attr(i1, "oob") <- NULL
  
  i2 <- missRanger(
    CO2_, 1 ~ ., seed = 1, verbose = 0, data_only = FALSE, num.trees = 20, maxiter = 2
  )
  
  expect_equal(i1, i2$data)
  expect_equal(i2$best_iter, 0L)
  expect_equal(oob, i2$pred_errors[i2$best_iter, ])
})

test_that("Output options work also in the univariate imputation case", {
  i1 <- missRanger(
    CO2_, . ~ 1, seed = 1, verbose = 0, returnOOB = TRUE, 
    data_only = TRUE, num.trees = 20, maxiter = 2
  )
  
  oob <- attr(i1, "oob")
  attr(i1, "oob") <- NULL
  
  i2 <- missRanger(
    CO2_, . ~ 1, seed = 1, verbose = 0, data_only = FALSE, num.trees = 20, maxiter = 2
  )
  
  expect_equal(i1, i2$data)
  expect_equal(i2$best_iter, 1L)
  expect_equal(oob, i2$pred_errors[i2$best_iter, ])
  capture_output(expect_no_error(print(i2)))
  capture_output(expect_no_error(summary(i2)))
})

test_that("forests can be attached and OOB errors are consistent with them", {
  i3 <- missRanger(
    CO2_, seed = 1, verbose = 0, data_only = FALSE, num.trees = 20, keep_forests = TRUE
  )
  b <- i3$best_iter
  expect_equal(length(i3$forests), ncol(CO2_))
  
  # Non-numeric variable
  oob_Plant <- c(Plant = i3$forests$Plant$prediction.error)
  expect_equal(oob_Plant, i3$pred_errors[b, "Plant"])
  
  oob_uptake <- 1 - c(uptake = i3$forests$uptake$r.squared)
  expect_equal(oob_uptake, i3$pred_errors[b, "uptake"])
})

test_that("OOB errors of forests are consistent also when maxiter is reached", {
  i3 <- missRanger(
    CO2_, seed = 1, verbose = 0, data_only = FALSE, 
    num.trees = 20, keep_forests = TRUE, maxiter = 2
  )
  b <- i3$best_iter
  expect_equal(b, 2)
  
  # Non-numeric variable
  oob_Plant <- c(Plant = i3$forests$Plant$prediction.error)
  expect_equal(oob_Plant, i3$pred_errors[b, "Plant"])
  
  oob_uptake <- 1 - c(uptake = i3$forests$uptake$r.squared)
  expect_equal(oob_uptake, i3$pred_errors[b, "uptake"])
})

test_that("In univariate case, no forests can be attached", {
  i3 <- missRanger(
    CO2_, . ~ 1, seed = 1, verbose = 0, data_only = FALSE, 
    num.trees = 20, keep_forests = TRUE
  )
  b <- i3$best_iter
  expect_equal(length(i3$forests), 0L)
  expect_equal(i3$mean_pred_errors, 1)
})


