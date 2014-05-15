library('testthat')
context('pvalueFormatter')

test_that("Add zero", { 
  expect_equal(pvalueFormatter(.5, two_dec.limit=10^-1), "0.50")
  expect_equal(pvalueFormatter(.06, two_dec.limit=10^-1), "0.06")
  expect_equal(pvalueFormatter(.06, two_dec.limit=10^-2), "0.060")
  expect_equal(pvalueFormatter(.06451, two_dec.limit=10^-3), "0.065")
  expect_equal(pvalueFormatter(.00006451, sig.limit=10^-3), "&lt; 0.001")
})
