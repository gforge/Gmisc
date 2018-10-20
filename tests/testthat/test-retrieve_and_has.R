library('testthat')
context('retrieve and has')

test_that("Basic retrieve", { 
  var <- "a"
  expect_equal(retrieve(var, "1"), var)
  
  var <- list(b = 1:10)
  expect_equal(retrieve(var, "b"), 1:10)
  
  var <- list(b = list(c = 1, d = list(1,2,3,list(e = 22))))
  expect_equal(retrieve(var, "b.d.4.e"), 22)

  expect_true(is.na(retrieve(var, "b.ee")))
})


test_that("Basic has", { 
  var <- "a"
  expect_true(has(var, "1"))
  
  var <- list(b = 1:10)
  expect_true(has(var, "b"))
  
  var <- list(b = list(c = 1, d = list(1,2,3,list(e = 22))))
  expect_true(has(var, "b.d.4.e"))
  
  expect_false(has(var, "b.ee"))
})
