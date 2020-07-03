library("testthat")
context("insertRowAndKeepAttr")

test_that("Keeps attributes", {
  test <- matrix(1:4, ncol = 2)
  attr(test, "wow") <- 1000
  test <- insertRowAndKeepAttr(test, 1)

  expect_equal(attr(test, "wow"), 1000)
  expect_equal(nrow(test), 3)
})

test_that("Keeps class", {
  test <- matrix(1:4, ncol = 2)
  attr(test, "wow") <- 1000
  class(test) <- c("new_class", class(test))
  test <- insertRowAndKeepAttr(test, 1)

  expect_true("new_class" %in% class(test)[1])
})