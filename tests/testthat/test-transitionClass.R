library(testthat)

test_that("Check initialization, copy, and dimensions",{
  expect_error(getRefClass("transitionClass")$new(),
               regexp = "transition matrix")

  trn_mtrx <- matrix(1, ncol=2, nrow=2, dimnames = list(LETTERS[1:2], letters[1:2]))
  a <- getRefClass("transitionClass")$new(trn_mtrx)
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

test_that("Check box size",{
  trn_mtrx <- matrix(1:4, ncol=2, nrow=2)
  a <- getRefClass("transitionClass")$new(trn_mtrx)
  expect_error(a$boxSizes())
  expect_equal(a$boxSizes(1),
               rowSums(trn_mtrx))
  expect_equal(a$boxSizes(2),
               colSums(trn_mtrx))

  trn_mtrx <- array(1:8, dim = c(2, 2, 2))
  a <- getRefClass("transitionClass")$new(trn_mtrx)
  expect_equivalent(a$boxSizes(1),
                    rowSums(trn_mtrx[,,1]) + rowSums(trn_mtrx[,,2]))
  expect_equivalent(attr(a$boxSizes(1), "prop"),
                    rowSums(trn_mtrx[,,1])/(rowSums(trn_mtrx[,,1]) + rowSums(trn_mtrx[,,2])))
  expect_equivalent(a$boxSizes(2),
                    colSums(trn_mtrx[,,1]) + colSums(trn_mtrx[,,2]))
  expect_equivalent(attr(a$boxSizes(2), "prop"),
                    colSums(trn_mtrx[,,1])/(colSums(trn_mtrx[,,1]) + colSums(trn_mtrx[,,2])))
})
