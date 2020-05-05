library('testthat')
context('pathJoin')

test_that("pathJoin basics", { 
  expect_equal(pathJoin("one", "two/three", "four"), 
               file.path("one", "two/three", "four"))
  expect_equal(pathJoin("one", "two//three/", "four"), 
               file.path("one", "two/three", "four"))
})