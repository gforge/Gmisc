library(testthat)

test_that("Check initialization, copy, and dimensions",{
  expect_error(transitionClass$new())

  trn_mtrx <- matrix(1, ncol=2, nrow=2)
  a <- transitionClass$new(trn_mtrx)
  expect_equal(length(dim(a$transitions)),
               length(dim(trn_mtrx)) + 1)
  expect_equal(a$getDim(),
               dim(trn_mtrx))

  a$addTransitions(trn_mtrx)
  expect_equal(a$getDim(),
               dim(trn_mtrx))

  expect_equal(tail(dim(a$transitions), 1),
               2)

  b <- a$copy()
  a$addTransitions(trn_mtrx)
  expect_equal(b$getDim(),
               dim(trn_mtrx))
  expect_equal(tail(dim(b$transitions), 1),
               2)
  expect_equal(tail(dim(a$transitions), 1),
               3)

  err_mtrx <- matrix(1, ncol=1, nrow=2)
  expect_error(a$addTransitions(err_mtrx))
  err_mtrx <- matrix(1, ncol=2, nrow=1)
  expect_error(a$addTransitions(err_mtrx))
  err_mtrx <- matrix("1", ncol=2, nrow=1)
  expect_error(a$addTransitions(err_mtrx))
  err_mtrx <- matrix("1", ncol=2, nrow=2)
  expect_error(a$addTransitions(err_mtrx))
})

