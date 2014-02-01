library('testthat')
context('insertRowAndKeepAttr')

test <- matrix(1:4, ncol=2)
attr(test, 'wow') <- 1000
test <- insertRowAndKeepAttr(test, 1)

test_that("Keeps attributes", { 
  expect_equal(attr(test, 'wow'), 1000)
})

test <- matrix(1:4, ncol=2)
attr(test, 'wow') <- 1000
class(test) <- c("new_class", class(test))
test <- insertRowAndKeepAttr(test, 1)
test_that("Keeps class", { 
  expect_true("new_class" == class(test)[1])
})
