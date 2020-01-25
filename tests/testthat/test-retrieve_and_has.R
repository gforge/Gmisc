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

  var <- list(list(c = 1, d = 2))
  expect_equal(retrieve(var, "1.c"), 1)
})

test_that("Retrieve with special character", { 
  var <- list(b = list(c = 1, `a.d` = list(1,2,3,list(e = 22))))
  expect_equal(retrieve(var, "b.a\\.d.4.e"), 22)

  var <- list(b = list(c = 1, `a[evilname]d` = list(1,2,3,list(e = 22))))
  expect_equal(retrieve(var, "b.a\\[evilname\\]d.4.e"), 22)
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
