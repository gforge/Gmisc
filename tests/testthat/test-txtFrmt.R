library('testthat')
context('txtPval')

test_that("Add zero", {
  expect_equal(txtPval(.5, two_dec_lim=10^-1), "0.50")
  expect_equal(txtPval(.06, two_dec_lim=10^-1), "0.06")
  expect_equal(txtPval(.06, two_dec_lim=10^-2), "0.060")
  expect_equal(txtPval(.06451, two_dec_lim=10^-3), "0.065")
  expect_equal(txtPval(.00006451, sig_lim=10^-3), "&lt; 0.001")
})
