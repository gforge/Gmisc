library('testthat')
context('getTicks')

test_that("Check the getTicks function", 
{ 
  expect_true(length(getTicks(0, 1)) > 2,
              info="Ticks should return more than 2 ticks regardless of span size")
  expect_true(length(getTicks(0, .01)) > 2,
              info="Ticks should return more than 2 ticks regardless of span size")

  expect_true(length(getTicks(0, 10)) > 2,
              info="Ticks should return more than 2 ticks regardless of span size")
  
  expect_true(length(getTicks(0, 100)) < 30,
              info="Ticks should not fill every even number for larger spans")

  expect_true(all(c(.5, 1,2,4) %in% getTicks(0.1, 8, exp=TRUE)),
              info="Exponential ticks should contain 2^...")
  
})