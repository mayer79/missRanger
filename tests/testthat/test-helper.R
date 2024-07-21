test_that(".formula_parser() works", {
  expect_error(.formula_parser(~ a))
  expect_error(.formula_parser("a ~ b"))
  
  expect_equal(
    .formula_parser(. ~ ., data = iris),
    list(names(iris), names(iris))
  )
  
  expect_equal(
    .formula_parser(Species + Sepal.Width ~ ., data = iris),
    list(c("Species", "Sepal.Width"), names(iris))
  )
  
  expect_equal(
    .formula_parser(. ~ Species + Sepal.Width, data = iris),
    list(names(iris), c("Species", "Sepal.Width"))
  )
  
  expect_equal(
    .formula_parser(. ~ . - Species, data = iris),
    list(names(iris), names(iris[1:4]))
  )
  
  expect_equal(
    .formula_parser(. - Species ~ ., data = iris),
    list(names(iris[1:4]), names(iris))
  )
  
  expect_equal(
    .formula_parser(. - Species ~ . - Species - Sepal.Length, data = iris),
    list(names(iris[1:4]), names(iris[2:4]))
  )
})

test_that(".check_response() works", {
  expect_true(.check_response(c(1L, 2L)))
  expect_true(.check_response(c(1.0, 2.0)))
  expect_true(.check_response(c(TRUE, FALSE)))
  expect_true(.check_response(LETTERS[1:5]))
  expect_true(.check_response(factor(LETTERS[1:5])))
  expect_false(.check_response(as.Date("2009-01-01")))
  expect_false(.check_response(list(a = 1, b = 2)))
})

test_that(".check_feature() works", {
  expect_true(.check_feature(c(1L, 2L)))
  expect_true(.check_feature(c(1.0, 2.0)))
  expect_true(.check_feature(c(TRUE, FALSE)))
  expect_true(.check_feature(LETTERS[1:5]))
  expect_true(.check_feature(factor(LETTERS[1:5])))
  expect_true(.check_feature(as.Date("2009-01-01")))
  expect_false(.check_feature(list(a = 1, b = 2)))
})

